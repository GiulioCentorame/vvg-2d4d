# master script
# install.packages(c('rms', 'MBESS', 'censReg', 'tidyverse', 'broom',
# 'car', 'BayesFactor', 'psych', 'devtools', 'knitr', 'compute.es',
# 'GPArotation', 'ordinal'))
# library(devtools)
# install_github("Joe-Hilgard/hilgard")
library(knitr)

source("0_data_aggregation.R")
source("1_data_cleaning.R")
source("2_analysis.R")
source("3_plotting.R")
knit("Results.Rmd")
knit("Supplement.Rmd")

# sink session_info() output for package versioning
sink("session_info.txt")
devtools::session_info()
sink(file = NULL)
