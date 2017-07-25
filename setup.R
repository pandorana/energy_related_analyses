libs <- c("ProjectTemplate", "tidyverse", "stringr", "lubridate", "forcats", "modelr", "broom", "car", "knitr", 
          "SGL", "gridExtra", "lm.beta", "Hmisc")
check_install_packages <- function(pack) {
  if (is.na(match(pack, installed.packages())))
    install.packages(pack)
}
lapply(libs, check_install_packages)
library(ProjectTemplate)
load.project()
rm(list = c("libs","check_install_packages"))
cat("\014")
