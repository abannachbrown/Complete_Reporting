### ----------- Alexandra Bannach-Brown

### ------------- PRISMA-A Reporting 


##########################
### read in prisma endorsers
##########################

library(readxl)
PRISMA_endorsers <- read_excel("PRISMA_endorsers_website_05072019.xlsx", 
                               col_names = FALSE)

names(PRISMA_endorsers) <- "endorsers"



##############################
### read in articles from epistemonikos 
###############################

# # install.packages("RefManageR")
# library(RefManageR)
# # ?RefManageR
# 
# 
# # User RefManageR to import bibtex references and convert to tibble
# bib <- ReadBib("epistemonikos_srs_bibtex_20190807.txt")
# SRs_table  <- as.data.frame(bib)
# 
# 
# ### try with revtools
# #install.packages("revtools")
# library(revtools)
# screen_duplicates()
# 
# SRs_table <- read_bibliography("epistemonikos_srs_ris20190807.ris")

guess_encoding("Epistemonikos_srs_excel20190807.txt")

### import data set (tab delimited export from endnote library)
SRs_table <- read_delim("Epistemonikos_srs_excel20190807.txt", 
                         "\t", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE
                        #, encoding = "CP1252" #doesnt work
                        )

## check default encoding
Sys.getlocale("LC_CTYPE")
# check supported encoding
iconvlist()

### issues with encoding of windows-1252 or CP-1252


## check the file has imported correctly
#### needs to have the exact date of each publication & 11263 articles
head(SRs_table)

summary(SRs_table)

###################
###### Clean data 
###########################
library(tidyverse)

# convert journal name column name
names(SRs_table)[4] <- "Journal"


# remove articles with blank Title or Abstract

# length(which(is.na(SRs_table$Title)))
# [1] 19

# length(which(is.na(SRs_table$Abstract)))
# [1] 301

## at least 301 articles without abstract


# filter dataset to be left with full title and abstract
SRs_table_clean <- SRs_table %>% 
                  filter(!is.na(Abstract) & !is.na(Title) & Journal != "Book")

# (!is.na(Journal) | Journal != "Book"))

#check how many journal feilds are blank
length(which(is.na(SRs_table_clean$Journal)))

summary(SRs_table_clean)

###### filter by date - need to have month and 
# load lubridate package for date wrangling
library(lubridate)


unique(SRs_table_clean$Date)

SRs_table_clean$Date_format <- parse_date_time(SRs_table_clean$Date, orders = c("dmy", "ymd"))

length(which(is.na(SRs_table_clean$Date_format)))


SRs_table_clean2 <- SRs_table_clean %>% 
  filter(!is.na(Date_format))


SRs_table_clean2$date_year <- year(SRs_table_clean2$Date_format)
SRs_table_clean2$date_month <- month(SRs_table_clean2$Date_format)


SRs_3 <- SRs_table_clean2 %>% 
        filter(date_year >= "2009" & date_year <= "2017")

# view summary to check all months are between 1 and 12
summary(SRs_3)


# view distribution of months & years
hist(SRs_3$date_month)
hist(SRs_3$date_year)


# form dates
SRs_3$date_year_month <- ymd(paste(SRs_3$date_year, SRs_3$date_month, "01", sep="-"))


## save histogram to current working directory
png("histogram_articles.png")
hist(SRs_3$date_year_month, breaks = "months")
dev.off()

# 
# ## remove unclean text
# # install.packages("textclean")
library(textclean)


SRs_table_clean$Journal <- replace_non_ascii(SRs_table_clean$Journal, replacement = " ")



#### remove journals with Cochrane in  ### EXCLUSION CRITERIA

# which docs contain Cochrane in Journal field
which(grepl("Cochrane", SRs_3$Journal))

#subset the data to exclude cochrane
SRs_4 <- SRs_3 %>%  filter(!grepl("Cochrane",Journal))




################################
### subset dataframe into intervention and control
######################
SRs_4$Journal <- tolower(SRs_4$Journal)

PRISMA_endorsers$endorsers <- tolower(PRISMA_endorsers$endorsers)

intervention_papers <- subset(SRs_4, Journal %in% PRISMA_endorsers$endorsers)

control_papers <- subset(SRs_4, !(Journal %in% PRISMA_endorsers$endorsers))


##############################################
## additionally: randomly select rows for 

intervention_papers_rand25 <- sample_n(intervention_papers, 25)


control_papers_rand25 <- sample_n(control_papers, 25)

########################
# code to later select paper that were'nt selected before
#####################

# #data in the sample
# in_int <- intervention_papers[intervention_papers_rand25,]
# 
# #data not in the sample
# out_int <- intervention_papers[-intervention_papers_rand25,]

#### then resample from the out fun


#######################################################
##### create blinded papers for upload
########################################################

intervention_papers_blind <- intervention_papers %>% 
                                select(ID, Title, Abstract, Authors)


control_papers_blind <- control_papers %>% 
                          select(ID, Title, Abstract, Authors)


blind_inter_25 <- intervention_papers_rand25 %>% 
                  select(ID, Title, Abstract, Authors)


blind_cont_25 <- control_papers_rand25 %>% 
                  select(ID, Title, Abstract, Authors)





########################################
#### export to ENdnote or bibtex file

BibFile_ <- BibEntry(bibtype = "Article", key = intervention_papers$ID, title = intervention_papers$Title,
         author = intervention_papers$Authors, journaltitle = intervention_papers$Journal,
         date = intervention_papers$date_year_month, pubstate = "forthcoming")


WriteBib(as.BibEntry(BibFile_), "example2.bib")




