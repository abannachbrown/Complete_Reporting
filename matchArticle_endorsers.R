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

# guess_encoding("Epistemonikos_srs_excel20190807.txt")

### import data set (tab delimited export from endnote library)
library(tidyverse)
SRs_table <- read_delim("Epistemonikos_srs_excel20190807.txt", 
                         "\t", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE
                        #, encoding = "CP1252" #doesnt work
                        )

## check default encoding
#Sys.getlocale("LC_CTYPE")
# check supported encoding
#iconvlist()

### issues with encoding of windows-1252 or CP-1252


## check the file has imported correctly
#### needs to have the exact date of each publication & 11263 articles
head(SRs_table)

summary(SRs_table)
# not the same as 11263

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


### PILOT PAPERS 
intervention_papers_2009_1 <-intervention_papers %>% 
                                subset(date_year == 2009 & 6 >= date_month)   
                                # %>%
                                # sample_n(25)

intervention_papers_2009 <-intervention_papers %>% 
  subset(date_year == 2009 #& 6 >= date_month
         )   
# %>%
# sample_n(25)
  

intervention_papers_2009_2 <-intervention_papers %>% 
  subset(date_year == 2009 & date_month > 6)   
# %>%
# sample_n(25)


control_papers_rand25 <- sample_n(control_papers, 25)

control_papers_2009_1 <-control_papers %>% 
  subset(date_year == 2009 & 6 >= date_month)  %>%  
  sample_n(25)


control_papers_2009_2 <-control_papers %>% 
  subset(date_year == 2009 & date_month > 6)  %>%  
  sample_n(25)

control_papers_2009 <-control_papers %>% 
  subset(date_year == 2009 
         #& 6 >= date_month
         )  %>%  
  sample_n(25)

########################
## additional pilot papers 2012

control_papers_2012_2 <-control_papers %>% 
  subset(date_year == 2012 & date_month > 6)  %>%  
  sample_n(25)


intervention_papers_2012_2 <-intervention_papers %>% 
  subset(date_year == 2012 & date_month > 6) %>%
  sample_n(25)


##### sample size calc say 45 papers per


## 2009
########################
# code to later select paper that were'nt selected before
#####################
 #import pilot papers
control_papers_2009 <- read.delim("C:/Users/abannach/OneDrive - Bond University/PRISMA-A Project/Complete_Reporting/control_papers_2009.tsv")
control_papers_2012 <- read.delim("C:/Users/abannach/OneDrive - Bond University/PRISMA-A Project/Complete_Reporting/control_papers_2012.tsv")
intervention_papers_2009.tsv <- read.delim("C:/Users/abannach/OneDrive - Bond University/PRISMA-A Project/Complete_Reporting/intervention_papers_2009.tsv.txt")
intervention_papers_2012 <- read.delim("C:/Users/abannach/OneDrive - Bond University/PRISMA-A Project/Complete_Reporting/intervention_papers_2012.tsv")

# intervention
# #data in the sample 
 in_int_2009 <- subset(intervention_papers, (Title %in% intervention_papers_2009.tsv$Title))
# 
# #data not in the sample
 out_int_2009 <- subset(intervention_papers, !(Title %in% intervention_papers_2009.tsv$Title))

 # #data in the sample
 in_int_2012 <- subset(intervention_papers, (Title %in% intervention_papers_2012$Title))
 # 
 # #data not in the sample
 out_int_2012 <- subset(intervention_papers, !(Title %in% intervention_papers_2012$Title))
 
 
 ## control
 # #data in the sample 
 in_cont_2009 <- subset(control_papers, (Title %in% control_papers_2009$Title))
 # 
 # #data not in the sample
 out_cont_2009 <- subset(control_papers, !(Title %in% control_papers_2009$Title))
 
 # #data in the sample
 in_cont_2012 <- subset(control_papers, (Title %in% control_papers_2012$Title))
 # 
 # #data not in the sample
 out_cont_2012 <- subset(control_papers, !(Title %in% control_papers_2012$Title))
 
 
 
#### then resample from the out fun
## resample 2009
 
 control_papers_2009_1_re <-out_cont_2009 %>% 
   subset(date_year == 2009 & 6 >= date_month)  %>%  
   sample_n(20)
 
 control_papers_2009_2_re <-out_cont_2009 %>% 
   subset(date_year == 2009 & date_month > 6)  %>%  
   sample_n(20)
 
 
 intervention_papers_2009_1_re <-out_int_2009 %>% 
   subset(date_year == 2009 & 6 >= date_month) # %>%
 #sample_n(45)
 
 intervention_papers_2009_2_re <-out_int_2009 %>% 
   subset(date_year == 2009 & date_month > 6) #%>%
 #sample_n(45)
 
 
 ## resample 2012
 
 control_papers_2012_1_re <-out_cont_2012 %>% 
   subset(date_year == 2012 & 6 >= date_month)  %>%  
   sample_n(45)
 
 control_papers_2012_2_re <-out_cont_2012 %>% 
   subset(date_year == 2012 & date_month > 6)  %>%  
   sample_n(20)
 
 
 intervention_papers_2012_1_re <-out_int_2012 %>% 
   subset(date_year == 2012 & 6 >= date_month) # %>%
# sample_n(45)
 
 intervention_papers_2012_2_re <-out_int_2012 %>% 
   subset(date_year == 2012 & date_month > 6) # %>%
 #sample_n(20)
 


#############################
control_papers_2010_1 <-control_papers %>% 
  subset(date_year == 2010 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2010_2 <-control_papers %>% 
  subset(date_year == 2010 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2010_1 <-intervention_papers %>% 
  subset(date_year == 2010 & 6 >= date_month) # %>%
#sample_n(45)

intervention_papers_2010_2 <-intervention_papers %>% 
  subset(date_year == 2010 & date_month > 6) #%>%
  #sample_n(45)


#### 2011


control_papers_2011_1 <-control_papers %>% 
  subset(date_year == 2011 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2011_2 <-control_papers %>% 
  subset(date_year == 2011 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2011_1 <-intervention_papers %>% 
  subset(date_year == 2011 & 6 >= date_month) # %>%
#sample_n(45)

intervention_papers_2011_2 <-intervention_papers %>% 
  subset(date_year == 2011 & date_month > 6) #%>%
#sample_n(45)



### 2012 - need to resample so they are not the same



### 2013


control_papers_2013_1 <-control_papers %>% 
  subset(date_year == 2013 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2013_2 <-control_papers %>% 
  subset(date_year == 2013 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2013_1 <-intervention_papers %>% 
  subset(date_year == 2013 & 6 >= date_month) # %>%
#sample_n(45)

intervention_papers_2013_2 <-intervention_papers %>% 
  subset(date_year == 2013 & date_month > 6) %>%
sample_n(45)


### 2014

control_papers_2014_1 <-control_papers %>% 
  subset(date_year == 2014 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2014_2 <-control_papers %>% 
  subset(date_year == 2014 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2014_1 <-intervention_papers %>% 
  subset(date_year == 2014 & 6 >= date_month) # %>%
#sample_n(45)

intervention_papers_2014_2 <-intervention_papers %>% 
  subset(date_year == 2014 & date_month > 6) %>%
  sample_n(45)


##### 2015

control_papers_2015_1 <-control_papers %>% 
  subset(date_year == 2015 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2015_2 <-control_papers %>% 
  subset(date_year == 2015 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2015_1 <-intervention_papers %>% 
  subset(date_year == 2015 & 6 >= date_month) # %>%
# sample_n(45)

intervention_papers_2015_2 <-intervention_papers %>% 
  subset(date_year == 2015 & date_month > 6) %>%
  sample_n(45)

##### 2016

control_papers_2016_1 <-control_papers %>% 
  subset(date_year == 2016 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2016_2 <-control_papers %>% 
  subset(date_year == 2016 & date_month > 6)  %>%  
  sample_n(45)


intervention_papers_2016_1 <-intervention_papers %>% 
  subset(date_year == 2016 & 6 >= date_month)  %>%
 sample_n(45)

intervention_papers_2016_2 <-intervention_papers %>% 
  subset(date_year == 2016 & date_month > 6) %>%
  sample_n(45)


##### 2017

control_papers_2017_1 <-control_papers %>% 
  subset(date_year == 2017 & 6 >= date_month)  %>%  
  sample_n(45)

control_papers_2017_2 <-control_papers %>% 
  subset(date_year == 2017 & date_month > 6) # %>%  
#  sample_n(45)


intervention_papers_2017_1 <-intervention_papers %>% 
  subset(date_year == 2017 & 6 >= date_month) # %>%
#  sample_n(45)

intervention_papers_2017_2 <-intervention_papers %>% 
  subset(date_year == 2017 & date_month > 6)# %>%
# sample_n(45)


##########################################
# 2/12/2019 merge all sampled articles together

all_control <- do.call("rbind", list(control_papers_2009_1_re, control_papers_2009_2_re, 
                                     control_papers_2010_1, control_papers_2010_2, 
                                     control_papers_2011_1, control_papers_2011_2, 
                                     control_papers_2012_1_re, control_papers_2012_2_re, 
                                     control_papers_2013_1, control_papers_2013_2, 
                                     control_papers_2014_1, control_papers_2014_2, 
                                     control_papers_2015_1, control_papers_2015_2, 
                                     control_papers_2016_1, control_papers_2016_2, 
                                     control_papers_2017_1, control_papers_2017_2))

all_int <- do.call("rbind", list(intervention_papers_2009_1_re, intervention_papers_2009_2_re, 
                                 intervention_papers_2010_1, intervention_papers_2010_2, 
                                 intervention_papers_2011_1, intervention_papers_2011_2, 
                                 intervention_papers_2012_1_re, intervention_papers_2012_2_re, 
                                 intervention_papers_2013_1, intervention_papers_2013_2, 
                                 intervention_papers_2014_1, intervention_papers_2014_2, 
                                 intervention_papers_2015_1, intervention_papers_2015_2, 
                                 intervention_papers_2016_1, intervention_papers_2016_2, 
                                 intervention_papers_2017_1, intervention_papers_2017_2))


########################

write_tsv()

# write_tsv(quote=FALSE, row.names = FALSE, sep="\t", na='') %>% 
  
  removeNewline <- function(text){
    print("-- Start to remove \r \n \f \t")
    text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
    return(text)
  }
  
control_download <-  control_papers_2009 %>%
  mutate(
    title = removeNewline(Title),
    surname = removeNewline(Authors),
    firstname = "",
    csurname = "",
    cfirstname = "",
    cauthororder = "",
    publicationName = removeNewline(Journal),
    doi = "",
    url = "",
    abstract = removeNewline(Abstract),
    keywords = "ID",
    URL = "",
    authorAddress = "",
    referenceType = "",
    pdfPath = ""
  ) %>%
  select(
    title = title,
    firstname = firstname,
    surname = surname,
    csurname = csurname,
    cfirstname = cfirstname,
    cauthororder = cauthororder,
    publicationName = publicationName,
    alternateName = Journal,
    abstract = abstract,
    URL = URL,
    authorAddress = authorAddress,
    year = Year,
    DOI = doi,
    referenceType = referenceType,
    pdfPath = pdfPath,
    keywords = ID
  )

names(control_download) <- c( "Title",
                          "First Author First Name",
                          "First Author Surname",
                          "Corresponding Author First Name",
                          "Corresponding Author Surname",
                          "Corresponding Author Order",
                          "Publication Name",
                          "Alternate Name",
                          "Abstract",
                          "Url",
                          "Author Address",
                          "Year",
                          "DOI",
                          "Reference Type",
                          "PDF Relative Path",
                          "Keywords"
)

                          
write.table(control_download, file = "control_papers_2009.tsv", quote=FALSE, row.names = FALSE, sep="\t", na='' )



removeNewline <- function(text){
  print("-- Start to remove \r \n \f \t")
  text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
  return(text)
}

intervention_download_pilot <-  intervention_papers_2009 %>%
  mutate(
    title = removeNewline(Title),
    surname = removeNewline(Authors),
    firstname = "",
    csurname = "",
    cfirstname = "",
    cauthororder = "",
    publicationName = removeNewline(Journal),
    doi = "",
    url = "",
    abstract = removeNewline(Abstract),
    keywords = "ID",
    URL = "",
    authorAddress = "",
    referenceType = "",
    pdfPath = ""
  ) %>%
  select(
    title = title,
    firstname = firstname,
    surname = surname,
    csurname = csurname,
    cfirstname = cfirstname,
    cauthororder = cauthororder,
    publicationName = publicationName,
    alternateName = Journal,
    abstract = abstract,
    URL = URL,
    authorAddress = authorAddress,
    year = Year,
    DOI = doi,
    referenceType = referenceType,
    pdfPath = pdfPath,
    keywords = ID
  )

names(intervention_download_pilot) <- c( "Title",
                              "First Author First Name",
                              "First Author Surname",
                              "Corresponding Author First Name",
                              "Corresponding Author Surname",
                              "Corresponding Author Order",
                              "Publication Name",
                              "Alternate Name",
                              "Abstract",
                              "Url",
                              "Author Address",
                              "Year",
                              "DOI",
                              "Reference Type",
                              "PDF Relative Path",
                              "Keywords"
)


write.table(intervention_download_pilot, file = "intervention_papers_2009.tsv", quote=FALSE, row.names = FALSE, sep="\t", na='' )


#######################################################\
### download SyRF format for full papers
##################################################

removeNewline <- function(text){
  print("-- Start to remove \r \n \f \t")
  text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
  return(text)
}

all_int_download <-  all_int %>%
  mutate(
    title = removeNewline(Title),
    surname = removeNewline(Authors),
    firstname = "",
    csurname = "",
    cfirstname = "",
    cauthororder = "",
    publicationName = removeNewline(Journal),
    doi = "",
    url = "",
    abstract = removeNewline(Abstract),
    keywords = "ID",
    URL = "",
    authorAddress = "",
    referenceType = "",
    pdfPath = ""
  ) %>%
  select(
    title = title,
    firstname = firstname,
    surname = surname,
    csurname = csurname,
    cfirstname = cfirstname,
    cauthororder = cauthororder,
    publicationName = publicationName,
    alternateName = Journal,
    abstract = abstract,
    URL = URL,
    authorAddress = authorAddress,
    year = Year,
    DOI = doi,
    referenceType = referenceType,
    pdfPath = pdfPath,
    keywords = ID
  )

names(all_int_download) <- c( "Title",
                                         "First Author First Name",
                                         "First Author Surname",
                                         "Corresponding Author First Name",
                                         "Corresponding Author Surname",
                                         "Corresponding Author Order",
                                         "Publication Name",
                                         "Alternate Name",
                                         "Abstract",
                                         "Url",
                                         "Author Address",
                                         "Year",
                                         "DOI",
                                         "Reference Type",
                                         "PDF Relative Path",
                                         "Keywords"
)


write.table(all_int_download, file = "all_int_download.txt", quote=FALSE, row.names = FALSE, sep="\t", na='' )


all_control_download <-  all_control %>%
  mutate(
    title = removeNewline(Title),
    surname = removeNewline(Authors),
    firstname = "",
    csurname = "",
    cfirstname = "",
    cauthororder = "",
    publicationName = removeNewline(Journal),
    doi = "",
    url = "",
    abstract = removeNewline(Abstract),
    keywords = "ID",
    URL = "",
    authorAddress = "",
    referenceType = "",
    pdfPath = ""
  ) %>%
  select(
    title = title,
    firstname = firstname,
    surname = surname,
    csurname = csurname,
    cfirstname = cfirstname,
    cauthororder = cauthororder,
    publicationName = publicationName,
    alternateName = Journal,
    abstract = abstract,
    URL = URL,
    authorAddress = authorAddress,
    year = Year,
    DOI = doi,
    referenceType = referenceType,
    pdfPath = pdfPath,
    keywords = ID
  )

names(all_control_download) <- c( "Title",
                                         "First Author First Name",
                                         "First Author Surname",
                                         "Corresponding Author First Name",
                                         "Corresponding Author Surname",
                                         "Corresponding Author Order",
                                         "Publication Name",
                                         "Alternate Name",
                                         "Abstract",
                                         "Url",
                                         "Author Address",
                                         "Year",
                                         "DOI",
                                         "Reference Type",
                                         "PDF Relative Path",
                                         "Keywords"
)


write.table(all_control_download, file = "all_control_download.tsv", quote=FALSE, row.names = FALSE, sep="\t", na='' )



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

# BibFile_ <- BibEntry(bibtype = "Article", key = intervention_papers$ID, title = intervention_papers$Title,
#          author = intervention_papers$Authors, journaltitle = intervention_papers$Journal,
#          date = intervention_papers$date_year_month, pubstate = "forthcoming")
# 
# 
# WriteBib(as.BibEntry(BibFile_), "example2.bib")




