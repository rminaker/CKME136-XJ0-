### == CKME136 XJ0  ==


#Load packages
library(rtweet) #For collecting tweets
library(tidyverse) #For wrangling data
library(summarytools) #Form summarizing data sets and selecting variable to include and exclude


#Set working directory and path for saving CSV
wd=setwd("B:\\canpoli_ryerson")
path="B:\\canpoli_ryerson"


### -------------- GET_TIMELINES DATA -------------- 

#Collect tweet data - TIMELINES (each party leaders personal account activity)
#The get_timelines functions collects all required tweets in each pull. No need to joing multiple data sets.
canpoli.tmls=rtweet::get_timelines(c(
  "@MaximeBernier",
  "@JustinTrudeau", 
  "@AndrewScheer",
  "@theJagmeetSingh",
  "@ElizabethMay"),
  n = 5000)

#Save tweet timeline data (complete set) as CSV
rtweet::save_as_csv(canpoli.tmls, file_name="canpoli.tmls.complete", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


### -------------- SEARCH_TWEETS DATA -------------- 

#Collect tweet data - SEARCH
canpoli.srch=search_tweets(
  "@MaximeBernier OR
  @AndrewScheer OR
  @JustinTrudeau OR
  @theJagmeetSingh OR
  @ElizabethMay", n = 1000000, retryonratelimit = TRUE, parse=TRUE, since_id=	1098091827784241154)

#For each additonal pull, get the max status_id to ensure only new data is collected in subsequent pulls
staus_id_max=canpoli.srch %>%                
  summarise(staus_id_max= max(status_id)) 

#Save tweet search data (complete set) as CSV
save_as_csv(canpoli.srch, file_name="canpoli_03-10-2019", prepend_ids = TRUE, na = "",fileEncoding = "UTF-8")



### -------------- FILTER DATA SETS FOR REQUIRED VAIABLES ONLY -------------- 

#Load the two complete data sets

#Load complete get_timelines data set - 88 variables | ~20 MB
canpoli.tmls.complete=read_csv("canpoli.tmls.complete.csv") 

#Load complete search_tweets data set - 88 variables | ~140 MB
canpoli.search.complete=read_csv("canpoli_01-02-19-2019.csv") 


#Use summary tools package to analyze data and identify required columns
#This step only required once and is applicable for both TIMELINE and SEARCH_TWEETS data (both data sets contain the same variables)

canpoli.tmls.summary=summarytools::dfSummary(canpoli.tmls.complete)
summarytools::view(canpoli.tmls.summary)

#Create a list of required variables
req_var=c(1:6,12:15,30,71,72,76)

#Filter TIMELINE and SEARCH data sets using the list of required variables

#Create filtered timelines data set - 18 variables | ~7 MB (-65% file size)
canpoli.tmls.filtered=canpoli.tmls.complete %>% select(req_var)

#Create filtered search data set - 18 variables | 30 MB (-78% file size)
canpoli.search.filtered=canpoli.search.complete %>% select(req_var)


### -------------- APPLY ANY OTHER PRE-PROSSESING  -------------- 

#Subtract 5 hours (18000 seconds) from the created_at varible to get Ottawa time (GMT-5)
#This is not accurate for all tweets, but more accurate than GMT-0

canpoli.tmls.filtered$created_at=canpoli.tmls.filtered$created_at-18000
canpoli.search.filtered$created_at=canpoli.search.filtered$created_at-18000


### -------------- SAVE FILTERED DATA SETS  -------------- 
#Filered data sets are saved and used for analysis and machine learning in seperate R files

#Save filtered timelines data set to .CSV
write_csv(canpoli.tmls.filtered, path = "B:\\canpoli_ryerson\\canpoli.tmls.filtered.csv")

#Save filtered search data set to .CSV
write_csv(canpoli.search.filtered, path = "B:\\canpoli_ryerson\\canpoli.search.filtered.csv")


