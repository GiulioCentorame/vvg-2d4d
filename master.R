# master script
#install.packages(c('car', 'reshape2', 'xlsx', 'BayesFactor', 'rms', 'psych', 'devtools'))
#library(devtools)
#install_github("Joe-Hilgard/hilgard")
library(knitr)

source("0_data_aggregation.R")
source("1_data_cleaning.R")
source("2_analysis.R")
source("3_plotting.R")

