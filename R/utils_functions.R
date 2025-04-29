#functions
install_if_needed <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
  invisible(lapply(packages, library, character.only = TRUE))
}