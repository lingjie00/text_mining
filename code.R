library(tidyverse)
library(tidytext)
library(textdata)
library(knitr)

path = "/Users/lingjie/Desktop/git/text_mining"
filename = "mayday.csv"
stop_word_lst = stop_words$word
senti_file = get_sentiments("nrc")

##########
#programs#
##########

#input filename and stop words and return list of words without stop words
get_words = function(path, filename,stop_word_lst) {
  setwd(path)
  #import file
  text = read.csv(filename,sep="|",header=FALSE,stringsAsFactors=FALSE)
  names(text) = c("sentence")
  #filter stop words
  words = text %>% unnest_tokens(word,sentence) %>% filter(!word%in%stop_word_lst)
  return(words)
}

words = get_words(path,filename, stop_word_lst)

## get count of different sentiments
get_total_sentiments = function(senti){
  nrow(inner_join(words,get_sentiments(senti),by="word"))
}

feelings = c("bing","afinn","loughran","nrc")
senti_count = sapply(feelings,get_total_sentiments)

#input list of words and sentiments and generate a inner join of two lists
senti = inner_join(words,senti_file,by="word")

#input the sentiment required and output only the specific sentiment
get_senti_words = function(feeling) {
  table = senti %>% filter(sentiment==feeling) %>% arrange(word)
  kable(table)
}

##########
#Analysis#
##########

#top 20 unique words
word_table = words %>% group_by(word) %>% 
  summarize(n=n(), percent = round(n()/nrow(words)*100)) %>% arrange(desc(n))
head(word_table,20)

#get sentiment table
senti_table = senti %>% group_by(sentiment) %>% 
  summarize(n=n(), percent = round(n()/nrow(senti)*100) ) %>% arrange(desc(n))
senti_table

senti_lst = unique(senti$sentiment)
senti_lst