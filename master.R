# master script
# install.packages(c('rms', 'MBESS', 'censReg', 'tidyverse', 
# 'car', 'BayesFactor', 'psych', 'devtools', 'knitr')))
#library(devtools)
#install_github("Joe-Hilgard/hilgard")
library(knitr)

source("0_data_aggregation.R")
source("1_data_cleaning.R")
source("2_analysis.R")
source("3_plotting.R")
knit("Results.Rmd")

