library(tidyverse)
library(tidytext)
library(textdata)
library(knitr)

path = "/Users/lingjie/Desktop/git/text_mining/data"
filename = "mayday.csv"
stop_word_lst = stop_words$word
senti_file = get_sentiments("nrc")
setwd(path)

##########
#programs#
##########

#input filename and stop words and return list of words without stop words
get_words = function(filename) {
  #import file
  text = read.csv(filename,sep="|",header=FALSE,stringsAsFactors=FALSE)
  names(text) = c("sentence")
  #filter stop words
  words = text %>% unnest_tokens(word,sentence) %>% filter(!word%in%stop_word_lst)
  return(words)
}

# get count of different sentiments
get_total_sentiments = function(senti){
  nrow(inner_join(words,get_sentiments(senti),by="word"))
}

#input list of words and sentiments and generate a inner join of two lists
senti_fn = function(){
  inner_join(words,senti_file,by="word")
}

#input the sentiment required and output only the specific sentiment
get_senti_words = function(senti, feeling) {
  table = senti %>% filter(sentiment==feeling) %>% arrange(word)
  kable(table)
}

#add words into stop words
add_stop_words = function(word) {
  new_stop_word_lst = c(stop_word_lst,word)
  assign("stop_word_lst",new_stop_word_lst,envir = .GlobalEnv)
}

##########
#Analysis#
##########

additional_stop_words = c("it’s")
sapply(additional_stop_words,add_stop_words)

words = get_words(filename)
senti = senti_fn()

feelings = c("bing","afinn","loughran","nrc")
senti_count = sapply(feelings,get_total_sentiments)

#top 20 unique words
word_table = words %>% group_by(word) %>% 
  summarize(n=n(), percent = round(n()/nrow(words)*100)) %>% arrange(desc(n))

#get sentiment table
senti_table = senti %>% group_by(sentiment) %>% 
  summarize(n=n(), percent = round(n()/nrow(senti)*100) ) %>% arrange(desc(n))

senti_lst = unique(senti$sentiment)

#########
#Reports#
#########
head(word_table,10)
senti_table
senti_lst