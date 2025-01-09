############# reproduction land take per inhabitant working paper
#### compare global land cover (GLC) area changes with official Norwegian statistics based on AR5

#### libraries
library(tidyverse)
library(sf)

main_path <- "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/APPLICATIONS/2025/UPLAND_2/test_site_selection/DATA_TROND"

#### 0.Base data

## NOR municipalities 2023

### SOURCE:
shp_path <- paste0(main_path,"/2023/kommuner_2023_land.shp")
kommuner_land <- st_read(shp_path) |> st_transform(25833)

## GLC per municipality
### SOURCE: script?
KomGLCkm2 <- read.csv(file.path(main_path, "fcKomGLCkm2_tibble.csv"),encoding = "UTF-8") 


## SSB stats (AR5 per municipality)
#SOURCE: SSB table 10781
KomSSBkm2 <- read.csv(file.path(main_path, "ssb_10781.csv")) 

## population
#SOURCE: SSB table 06913
pop<-read.csv(file.path(main_path, "ssb_06913.csv")) 


### 1. PROCESSING built up area changes
## to compare the GLC and the SSB data set, a selection of 2015 - 2022 is made for both data sets. In a second step, a linear transformation is applied.

## 1.1 Crop GLC, SSB and population to 2015 - 2022
KomSSBkm2 <- KomSSBkm2 %>%
  mutate(
    # Remove the "K-" prefix
    cleaned_col = str_remove(navn, "^K-")
  ) %>%
  separate(
    cleaned_col, into = c("kommunenummer", "KomNavn"), sep = " ", convert = TRUE
  )%>%group_by(kommunenummer,KomNavn)%>%summarise(across(where(is.numeric), sum, .names = "sum_{.col}"))%>%select(sum_X2015,sum_X2016,sum_X2017,sum_X2018,sum_X2019,sum_X2020,sum_X2021,sum_X2022)

KomGLCkm2<-KomGLCkm2%>%select(kommunenummer, KomNavn, ImpAreaKm2_2015,ImpAreaKm2_2016,ImpAreaKm2_2017,ImpAreaKm2_2018,ImpAreaKm2_2019,ImpAreaKm2_2020,ImpAreaKm2_2021,ImpAreaKm2_2022)

pop<-pop%>%select(region,folk_2015,folk_2022)


## 1.2 Linear regression for SSB and GLC areas
KomSSBkm2 <- KomSSBkm2 %>%
  rowwise() %>%
  mutate(
    # Extract the area values and years for the period 2000 to 2022
    lm_data = list(tibble(
      year = 2015:2022,
      area = c_across(sum_X2015:sum_X2022)  # Explicitly specify the columns from 2000 to 2022
    )),
    # Fit the linear model and extract coefficients
    lm_fit = list(lm(area ~ year, data = lm_data)),
    intercept2 = coef(lm_fit)[1],
    slope2 = coef(lm_fit)[2]
  ) %>%
  ungroup() %>%
  select(-lm_data, -lm_fit)%>%# Remove temporary columns
  mutate(#calc estimate for 2015 and 2022
    est_SSBkm2_2015 = intercept2 + slope2 * 2015,
    est_SSBkm2_2022 = intercept2 + slope2 * 2022
  )

KomGLCkm2 <- KomGLCkm2 %>%
  rowwise() %>%
  mutate(
    # Extract the area values and years for the period 2000 to 2022
    lm_data = list(tibble(
      year = 2015:2022,
      area = c_across(ImpAreaKm2_2015:ImpAreaKm2_2022)  # Explicitly specify the columns from 2000 to 2022
    )),
    # Fit the linear model and extract coefficients
    lm_fit = list(lm(area ~ year, data = lm_data)),
    intercept2 = coef(lm_fit)[1],
    slope2 = coef(lm_fit)[2]
  ) %>%
  ungroup() %>%
  select(-lm_data, -lm_fit)%>%# Remove temporary columns
  mutate(#calc estimate for 2015 and 2022
    est_GLCkm2_2015 = intercept2 + slope2 * 2015,
    est_GLCkm2_2022 = intercept2 + slope2 * 2022
  )

### 2. Calculate built up area per capita (BPC) for 2015 and 2022 for both data sets
# 2.1 join population to area data sets
KomGLCkm2<-merge(KomGLCkm2,pop, by.x="kommunenummer", by.y="region")
KomSSBkm2<-merge(KomSSBkm2,pop, by.x="kommunenummer", by.y="region")

# 2.2 BPC
KomGLCkm2$GLC_BPC_2015 = KomGLCkm2$est_GLCkm2_2015/KomGLCkm2$folk_2015
KomGLCkm2$GLC_BPC_2022 = KomGLCkm2$est_GLCkm2_2022/KomGLCkm2$folk_2022

KomSSBkm2$SSB_BPC_2015 = KomSSBkm2$est_SSBkm2_2022/KomSSBkm2$folk_2022
KomSSBkm2$SSB_BPC_2022 = KomSSBkm2$est_SSBkm2_2022/KomSSBkm2$folk_2022

# 2.3 Delta of population and BPC
delta_pop<-function(A1,A0,POP1,POP0){
  # Validate input

  # Compute APOP
  APOP <- (A1 - A0) * (log(POP1 / POP0) / log(A1 / A0))
  
  return(APOP) 
}

delta_bpc<-function(A1,A0,bpc1,bpc0){
  # Validate input
  if (A1 <= 0 || A0 <= 0 || bpc1 <= 0 || bpc0 <= 0) {
    stop("All inputs must be positive and greater than zero.")
  }
  
  # Compute Abpc
  ABPC <- (A1 - A0) * (log(bpc1 / bpc0) / log(A1 / A0))
  
  return(ABPC) 
}

KomGLCkm2$delta_GLC_pop<-delta_pop(KomGLCkm2$est_GLCkm2_2022,KomGLCkm2$est_GLCkm2_2015,KomGLCkm2$folk_2022,KomGLCkm2$folk_2015)
