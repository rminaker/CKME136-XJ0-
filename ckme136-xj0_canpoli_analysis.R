### == CKME136 XJ0 ==
## RScript #2- Text mining and discovery 


#Load packages
library(tidyverse) #For wrangling data
library(tidytext) #For extra wrangling capabilities for text data
library(syuzhet) #For sentiment analysis
library(tm) #For text mining
library(wordcloud) #Wordclouds
library(SnowballC) #For word stemming


#Set working directory and path for saving CSV
wd=setwd("B:\\canpoli_ryerson")
path="B:\\canpoli_ryerson"

### -------------- LOAD FILTERed DATA SETS-------------- 

#Load filtered get_timelines data set
canpoli.tmls.filtered=read_csv("canpoli.tmls.filtered.csv") 

#Load filtered search_tweets data set 
canpoli.search.complete=read_csv("canpoli.srch.filtered.csv") 


### -------------- ANALYSIS ON TWEET_TIMELINE DATA (NON-TEXT) -------------- 

#Text mining/analysis to look for pattern/insights on 6 non-text variales:
# - created_at, source, favorite_count, retweet_count, followers

#Filter on greater than Jan 01, 2019, not including retweets, and English tweets only
canpoli.tmls.filtered= canpoli.tmls.filtered %>% dplyr::filter(created_at > "2019-01-01", lang=="en", is_retweet=="FALSE")


#Number of tweets by party leader (comparison over time)
canpoli.tmls.filtered %>%
  dplyr::group_by(name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_line() +
  #ggplot2::theme_classic() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Source of tweets by party leader (comparison)
ggplot(canpoli.tmls.filtered, aes(x = name, fill = source)) +
  geom_bar() +
  theme(legend.title = element_blank())+
  #theme_classic()+
  #facet_wrap(~ source)+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Sources of tweets by Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


#Number of followers by party leader (comparison)
#All leaders
options(scipen=10000)
canpoli.tmls.filtered %>%
  group_by(name) %>%
  summarize(avgfollowers = median(followers_count)) %>% 
  arrange(desc(avgfollowers)) %>% 
  ggplot(., aes(x = name, y=avgfollowers, fill=avgfollowers)) + 
  geom_bar(stat='identity')+
  theme(legend.title = element_blank())+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Number of followers of Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#All leaders excluding Justin Trudeau
canpoli.tmls.filtered %>%
  filter(name != "Justin Trudeau") %>% 
  group_by(name) %>%
  summarize(avgfollowers = median(followers_count)) %>% 
  arrange(desc(avgfollowers)) %>% 
  ggplot(., aes(x = name, y=avgfollowers, fill =avgfollowers)) +
  geom_bar(stat='identity')+
  theme(legend.title = element_blank())+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Number of followers of Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Number of tweets favourited by party leader (comparison)
#Count of tweets favourited
ggplot(canpoli.tmls.filtered, aes(x = name, y=favorite_count, fill=favorite_count)) +
  geom_bar(stat='identity') +
  theme(legend.title = element_blank())+
  #theme_classic()+
  #facet_wrap(~ source)+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Tweets favourited by Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Average of tweets favourited
canpoli.tmls.filtered %>% group_by(name) %>%     
  summarise(Number_of_Tweets = n_distinct(status_id), Number_of_Likes = sum(favorite_count)) %>% 
  mutate(Avg_Likes=Number_of_Likes/Number_of_Tweets)%>%
  ggplot(., aes(x = name, y=Avg_Likes, fill =Avg_Likes)) +
  geom_bar(stat='identity')+
  theme(legend.title = element_blank())+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Number of followers of Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
  
#Number of tweets retweeted by party leader (comparison)
ggplot(canpoli.tmls.filtered, aes(x = name, y=retweet_count, fill=retweet_count)) +
  geom_bar(stat='identity') +
  theme(legend.title = element_blank())+
  #theme_classic()+
  #facet_wrap(~ source)+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Canadian federal party leaders - retweeted tweets",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Hashtags by party leader (comparison)
#Create a seperate chart for each party leader to examine main themes
canpoli.tmls.filtered %>%
  #filter(name == "Justin Trudeau") %>% 
  #filter(name == "Andrew Sheer") %>% 
  #filter(name == "Jagmeet Singh") %>% 
  #filter(name == "Maxime Bernier") %>% 
  #filter(name == "Elizabeth May") %>% 
  select(name, hashtags) %>% 
  mutate(tolower(hashtags)) %>% 
  unnest_tokens(word, hashtags) %>%
  count(name,word, sort = TRUE) %>%
  filter(!is.na(word), n>1) %>%
  ungroup() %>% 
  ggplot(., aes(x = word, y=n,fill =name)) +
  geom_bar(stat='identity')+
  ggtitle("piped ggplot2")+
  theme(legend.title = element_blank())+
  coord_flip()+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Hashtags used by Canadian federal party leaders",
    subtitle = "Twitter status (tweet) counts aggregated from January 01, 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


#Hashtags by public in mentions
#TBC

### -------------- ANALYSIS ON TWEET_TIMELINE DATA (TEXT) -------------- 
#Text mining/analysis to look for pattern/insights [text] variale:

#Create a copy of the df before processing
tweet_text=canpoli.tmls.filtered

#Pre-clean the [text] variable using regex
#Original regex:
#\\r|\\n|'|'|\.|!|\?|\*|-|-|:|"|\/|&gt|;|\dk|@|#|\$|%|\^|\(|\)|_|\+\=|&|`|~|<|"|\{|\}|\[|\]|\\d

#Remove URLs
tweet_text$text=gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tweet_text$text) 
#Remove everything else except for text
tweet_text$text=gsub("\\\r|\\\n|'|'|\\.|!|\\?|\\*|-|-|:|"|\\/|&gt|;|\\dk|@|\$|%|\\^|\\(|\\)|_|\\+\\=|&|`|~|\"|\\{|\\}|\\[|\\]|\\d","",canpoli.tmls.filtered$text) 
#Examine some tweets to verify outcome is correct
tweet_text$text[1:5]

#create a corpus for eventual use in text analysis using wordcloud
tweets_corpus = VCorpus(VectorSource(tweet_text$text))
#tweets_corpus

#Clean the corpus
tweets_corpus=tm_map(tweets_corpus, content_transformer(tolower))
tweets_corpus=tm_map(tweets_corpus, stripWhitespace)
tweets_corpus=tm_map(tweets_corpus, removePunctuation)
tweets_corpus=tm_map(tweets_corpus, removeNumbers)
tweets_corpus=tm_map(tweets_corpus, removeWords, stopwords("en"))
tweets_corpus=tm_map(tweets_corpus, removeWords, c("the","via","amp","will","for", "youre","hey"))

## creating a copy of the corpus to be used as a dictionary
#myCorpusDic=tweets_corpus 

#tweets_corpus=tm_map(tweets_corpus, stemDocument)
#tweets_corpus=tm_map(tweets_corpus, content_transformer(stemCompletion),dictionary=myCorpusDic, type="prevalent")

#Create a tern document matrix 
tdm=TermDocumentMatrix(tweets_corpus)
m=as.matrix(tdm)

#Sum and sort word frequencies
word_freqs = sort(rowSums(m), decreasing = TRUE) 
#word_freqs

# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)
#dm[1:100,]

#Create a wordcloud of frequent terms to extract themes from twitter TIMELINE data
#Will create multiple word clouds - each leader's timeline and each public search mentioning leader
wordcloud(dm$word, dm$freq, 
          random.order = FALSE, 
          colors = brewer.pal(5, "Dark2"),
          min.freq = 10,
          rot.per=0.35,
          scale=c(5,1),
          max.words=100
)
