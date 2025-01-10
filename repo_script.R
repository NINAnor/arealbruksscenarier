############# reproduction land take per inhabitant working paper
#### compare global land cover (GLC) area changes with official Norwegian statistics based on AR5

#### libraries
library(tidyverse)
library(sf)



################# functions
delta<-function(x1,x0,y1,y0){
  # Validate input
  
  # Compute APOP
  APOP <- (x1 - x0) * (log(y1 / y0) / log(x1 / x0))
  
  return(APOP) 
}
# annual growth rate function
AGR<-function(x1,x0,n){
  AGR<-((x1/x0)^(1/n)-1)*100
}


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

## municipalities to regions (fylke)
regio<-read.csv(file.path(main_path, "kom_fylke.csv")) 



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

# 1.3 remove municipalities with land area in either start or end of the timeline
KomGLCkm2[KomGLCkm2==0] <- NA
KomGLCkm2<-KomGLCkm2[complete.cases(KomGLCkm2),]

KomSSBkm2[KomSSBkm2==0] <- NA
KomSSBkm2<-KomSSBkm2[complete.cases(KomSSBkm2),]


pop[pop==0] <- NA
pop<-pop[complete.cases(pop),]


### 2. Calculate built up area per capita (BPC) for 2015 and 2022 for both data sets
# 2.1 join population to area data sets
KomGLCkm2<-merge(KomGLCkm2,pop, by.x="kommunenummer", by.y="region")
KomSSBkm2<-merge(KomSSBkm2,pop, by.x="kommunenummer", by.y="region")


# 2.2 BPC
KomGLCkm2$GLC_BPC_2015 = KomGLCkm2$est_GLCkm2_2015/KomGLCkm2$folk_2015
KomGLCkm2$GLC_BPC_2022 = KomGLCkm2$est_GLCkm2_2022/KomGLCkm2$folk_2022

KomSSBkm2$SSB_BPC_2015 = KomSSBkm2$est_SSBkm2_2015/KomSSBkm2$folk_2015
KomSSBkm2$SSB_BPC_2022 = KomSSBkm2$est_SSBkm2_2022/KomSSBkm2$folk_2022


# 2.3 Delta of population and BPC
KomSSBkm2$delta_SSB_pop<-delta(KomSSBkm2$est_SSBkm2_2022,KomSSBkm2$est_SSBkm2_2015,KomSSBkm2$folk_2022,KomSSBkm2$folk_2015)
KomSSBkm2$delta_SSB_bpc<-delta(KomSSBkm2$est_SSBkm2_2022,KomSSBkm2$est_SSBkm2_2015,KomSSBkm2$SSB_BPC_2022,KomSSBkm2$SSB_BPC_2015)

KomGLCkm2$delta_GLC_pop<-delta(KomGLCkm2$est_GLCkm2_2022,KomGLCkm2$est_GLCkm2_2015,KomGLCkm2$folk_2022,KomGLCkm2$folk_2015)
KomGLCkm2$delta_GLC_bpc<-delta(KomGLCkm2$est_GLCkm2_2022,KomGLCkm2$est_GLCkm2_2015,KomGLCkm2$GLC_BPC_2022,KomGLCkm2$GLC_BPC_2015)

# 2.4 Annual growth rate (AGR) of population and area use per capita
KomGLCkm2$AGR_BPC_GLC<-AGR(KomGLCkm2$GLC_BPC_2022,KomGLCkm2$GLC_BPC_2015,8)
KomSSBkm2$AGR_BPC_SSB<-AGR(KomSSBkm2$SSB_BPC_2022,KomSSBkm2$SSB_BPC_2015,8)


#KomSSBkm2$AGR_BPC_SSB<-((as.double(KomSSBkm2$SSB_BPC_2022)/as.double(KomSSBkm2$SSB_BPC_2015))^(1/8)-1)*100

# 2.5 merge GLC and SSB
master<-merge(KomSSBkm2,KomGLCkm2,by ="kommunenummer")
master<-master%>%select(kommunenummer,KomNavn,folk_2015.x,folk_2022.x,est_SSBkm2_2015,est_SSBkm2_2022,est_GLCkm2_2015,est_GLCkm2_2022,SSB_BPC_2015,SSB_BPC_2022,
                        GLC_BPC_2015,GLC_BPC_2022,delta_SSB_pop,delta_SSB_bpc,delta_GLC_pop,delta_GLC_bpc,AGR_BPC_GLC,AGR_BPC_SSB)

### 3. Establish 9 groups of population - area development
master$AGR_pop<-AGR(master$folk_2022.x,master$folk_2015.x,8)

#3.1 Use the same thresholds for both periods for consistency
thresholds_pop <- quantile(master$AGR_pop, probs = c(0.33, 0.67), na.rm = TRUE)
#and for land area change
thresholds_bpc_GLC <- quantile(master$AGR_BPC_GLC, probs = c(0.33, 0.67), na.rm = TRUE)
thresholds_bpc_SSB <- quantile(master$AGR_BPC_SSB, probs = c(0.33, 0.67), na.rm = TRUE)

# Classify changes using thresholds
master <- master %>%
  mutate(
    # Population change classes
    pop_change_class = case_when(
      AGR_pop < thresholds_pop[1] ~ "decline",
      AGR_pop > thresholds_pop[2] ~ "incline",
      TRUE ~ "stable"
    ),
    
    # BPC change classes
    bpc_ssb_change_class = case_when(
      AGR_BPC_SSB < thresholds_bpc_SSB[1] ~ "small_change",
      AGR_BPC_SSB > thresholds_bpc_SSB[2] ~ "big_incline",
      TRUE ~ "moderate_incline"
    ),
    bpc_glc_change_class = case_when(
      AGR_BPC_GLC < thresholds_bpc_GLC[1] ~ "small_change",
      AGR_BPC_GLC > thresholds_bpc_GLC[2] ~ "big_incline",
      TRUE ~ "moderate_incline"
    )
  )


### 4 select communities for UPLAND that bpc_ssb_change_class == bpc_glc_change_class
master<-master%>%filter(bpc_glc_change_class==bpc_ssb_change_class)

# 4.1 make the final 9 classes
master$comb_class <- paste(
  "pop", master$pop_change_class,
  "bpc", master$bpc_glc_change_class,
  sep = "_"
)

# Define the reverse mapping for the combined categories
category_labels <- c(
  "a) Overflødig utbygd areal" = "pop_decline_bpc_small_change",
  "b) Stillstand i utbygd areal" = "pop_decline_bpc_moderate_incline",
  "c) Nedgang-spredning" = "pop_decline_bpc_big_incline",
  "d) Stillstand i utbygd areal" = "pop_stable_bpc_small_change",
  "e) Moderat arealvekst" = "pop_stable_bpc_moderate_incline",
  "f) Ekspansjon i utbygd areal" = "pop_stable_bpc_big_incline",
  "g) Arealeffektiv vekst" = "pop_incline_bpc_small_change",
  "i) Rask vekst i utbygd areal" = "pop_incline_bpc_moderate_incline",
  "h) Proporsjonal vekst" = "pop_incline_bpc_big_incline"
)

# Apply the labels to both historical and future periods
master$cat_NOR <- factor(
  master$comb_class,
  levels = category_labels,
  labels = names(category_labels)
)

# 4.2 join Fylke and select from UPLAND target fylke
master<-merge(master,regio,by.x = "kommunenummer",by.y="kommunenummer")
target_fylke<-c("Agder","Innlandet","Trøndelag","Akershus")
master_sel<-master%>%filter(fylkesnavn %in% target_fylke)

# 4.3 join centrality index to remove 1 and 6
cent<-read.csv(file.path(main_path, "sentralitet_ssb.csv")) 
master_sel<-merge(master_sel,cent,by.x = "kommunenummer",by.y="knr.2024")
#remove 1 and 6
master_sel<-master_sel%>%filter(Klasse.2023<6 & Klasse.2023>1)

# 4.4 filter only by interesting classes 
target_class<-c("g) Arealeffektiv vekst","i) Rask vekst i utbygd areal","c) Nedgang-spredning")
master_sel<-master_sel%>%filter(cat_NOR %in% target_class)
###### stats by fylke and cat_NOR
counts<-master_sel%>%group_by(fylkesnavn,cat_NOR)%>%summarise(cnt = n())

write.csv(master_sel,"glc_eq_ssb_selection_upland.csv")
