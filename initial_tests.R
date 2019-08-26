


### ----------- Alexandra Bannach-Brown
## Complete reporting of clinical trials
# ---- link clinical trials.gov with pubmed abstract & with EuPMC and automatically assess PICO


#### install base packages
install.packages("devtools")
library(devtools)

# clinicaltrials.gov API package
install_github("sachsmc/rclinicaltrials")
library(rclinicaltrials)

# -------------- 1. Set-Up Link with ClinicalTrials.gov  ----------------------------------------------------


#count trials - run on 26/07/2019

clinicaltrials_count(query = "ALL") # returns all records 311985

# find out how to use advanced settings
advanced_search_terms


# Clinical trials that are completed, intervention studies with results
clinicaltrials_count(query = c("type=Intr", "rslt=With", "recr=Completed", "ALL")) 
# 29737

#Clinical trials that are completed, intervention studies with results - registered/recieved since 2010 until 31.12.2017
clinicaltrials_count(query = c("type=Intr", "rslt=With", "recr=Completed", "ALL", "rcv_s=01/01/2010", "rcv_e=12/31/2017")) 
# 16202

y_test10 <- clinicaltrials_download(query = c("type=Intr", "rslt=With", "recr=Completed", "ALL", "rcv_s=01/01/2010", "rcv_e=12/31/2017"), count = 10, include_results = TRUE)

## -------------- EuPMC --------------------------------------
# install packages
install.packages("fulltext")
