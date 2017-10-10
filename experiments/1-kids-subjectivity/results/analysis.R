library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

setwd("~/Desktop/adjs!/kids-adjectives/experiments/1-kids-subjectivity/Submiterator-master")

num_round_dirs = 12
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/kids-subjectivity.csv', sep='')) %>% #'round1/kids-subjectivity.csv')) %>% #for just 1
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid", "class","predicate","slide_number","response","language"))
unique(d$language)

length(unique(d$workerid)) # n=XXX