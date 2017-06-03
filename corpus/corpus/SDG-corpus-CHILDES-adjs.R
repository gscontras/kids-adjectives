#library(ggplot2)
#library(plyr)
library(tidyverse)
theme_set(theme_bw(18))
setwd("~/git/adjective_ordering/corpus_results/")
#source("rscripts/helpers.r")

#load("data/r.RData")
rs = read.table("data/swbd.tab", sep="\t", header=T, quote="")
rs$Corpus = "swbd"
nrow(rs) # 15'744 cases in swbd
rb = read.table("data/bncs.tab", sep="\t", header=T, quote="")
rb$Corpus = "bncs" 
nrow(rb) # 201'261 cases in bncs
head(rb)
rbw = read.table("data/bncw.tab", sep="\t", header=T, quote="")
rbw$Corpus = "bncw"
nrow(rbw) # 270'695 (already restricted) cases in bncw, over 4 million without restriction
head(rbw)


###### ALSO NEED THESE

# data.frame with only the data points for the adjectives that we tested in the faultless disagreement study
adjs = data.frame(Adjective = c('ancient',
                                'dead',
                                'fresh',
                                'new',
                                'old',
                                'ole',
                                'rotten',
                                'wobble',
                                'young',
                                'black',
                                'blue',
                                'brown',
                                'gold',
                                'golden',
                                'gray',
                                'green',
                                'grey',
                                'orange',
                                'pink',
                                'purple',
                                'red',
                                'silver',
                                'tawny',
                                'white',
                                'yellow',
                                'big',
                                'bitsy',
                                'chubby',
                                'crust',
                                'eensie',
                                'fat',
                                'flat',
                                'fleece',
                                'huge',
                                'itsy',
                                'itty',
                                'large',
                                'little',
                                'long',
                                'podgy',
                                'puff',
                                'short',
                                'small',
                                'spin',
                                'tall',
                                'teeny',
                                'thick',
                                'thin',
                                'tiny',
                                'tubby',
                                'wee',
                                'weensie',
                                'weeny',
                                'wide',
                                'plastic',
                                'wooden',
                                'wool',
                                'damp',
                                'dry',
                                'hard',
                                'right',
                                'rough',
                                'sharp',
                                'smooth',
                                'soft',
                                'wet',
                                'misshapen',
                                'oval',
                                'round',
                                'square',
                                'alright',
                                'awful',
                                'bad',
                                'brilliant',
                                'crummy',
                                'good',
                                'grand',
                                'great',
                                'horrible',
                                'horrid',
                                'nasty',
                                'nice',
                                'okay',
                                'ordinary',
                                'poor',
                                'thirst'), 
                  Class = c('age',
                            'age',
                            'age',
                            'age',
                            'age',
                            'age',
                            'age',
                            'age',
                            'age',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'color',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'dimension',
                            'material',
                            'material',
                            'material',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'physical',
                            'shape',
                            'shape',
                            'shape',
                            'shape',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value',
                            'value'))
str(adjs)
row.names(adjs) = adjs$Adjective

#######################################################################
########### HERE'S WHERE ALL THE INTERESTING STUFF STARTS #############
#######################################################################

d = rbind(rb,rs)#,rbw)
rb = d
d = merge(rb, rbw, all=T)
nrow(d)
rb=d
# plot mean distance from noun for all adjectives we tested
d_exp = droplevels(rb[rb$Adjective %in% adjs$Adjective | rb$PrevAdjective %in% adjs$Adjective | rb$PrevPrevAdjective %in% adjs$Adjective,])
nrow(d_exp)
table(d_exp$Corpus)
table(d_exp$Corpus,d_exp$PrevAdj)
table(d_exp$Corpus,d_exp$PrevPrevAdj) # only 111 cases with 3 adjs
head(d_exp[d_exp$PrevPrevAdj == "yes",])
d_exp[d_exp$PrevAdjective == "",]$PrevAdjective = NA
d_exp[d_exp$PrevPrevAdjective == "",]$PrevPrevAdjective = NA


# plot adjective's mean distance from noun by class, only for cases where there's more than one prenominal modifier -- THIS IS THE STUFF YOU"RE REPORTING IN THE PAPER
d_subexp = d_exp[d_exp$PrevAdj == "yes" | d_exp$PrevPrevAdj == "yes",]
nrow(d_subexp) #total of  cases

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective, Corpus, Noun) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered) #total:  cases
head(gathered)
summary(gathered)
gathered$DistanceFromNoun = 1
gathered[gathered$Position == "PrevAdjective",]$DistanceFromNoun = 2
gathered[gathered$Position == "PrevPrevAdjective",]$DistanceFromNoun = 3
unique(paste(gathered$Position,gathered$DistanceFromNoun))
head(gathered)
gathered$Class = adjs[as.character(gathered$Adjective),]$Class
summary(gathered)
# remove all the adjectives that didn't occur in the experiment
gathered = droplevels(gathered[gathered$Adjective %in% adjs$Adjective,])
nrow(gathered) # to plot: 50857 cases
summary(gathered)

table(gathered$Corpus)

nrow(gathered[gathered$Noun %in% nouns$Noun,])
d_subexp[d_subexp$Noun %in% nouns$Noun,]
bynoun = droplevels(gathered[gathered$Noun %in% nouns$Noun,])
bynoun$NounClass = nouns[as.character(bynoun$Noun),]$NounClass
nrow(bynoun)

sort(table(bynoun$Class),decreasing=T)
round(sort(prop.table(table(bynoun$Class)),decreasing=T),3)

# calculate mean distance from noun by adjective
nrow(gathered) # 50857 total cases
nrow(gathered[gathered$DistanceFromNoun < 3,]) # 49675 adj-adj cases
two_adj <- gathered[gathered$DistanceFromNoun < 3,]
two_adj$Distance = two_adj$DistanceFromNoun - 1
head(two_adj)


# calculate means and CIs
bootsSummary <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE, n_boots_samps=10000) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     bootsci_high = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["97.5%"]],
                     bootsci_low = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["2.5%"]]
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  return(datac)
}
agr_class = bootsSummary(data=two_adj, measurevar="Distance", groupvars=c("Class"))
#write.csv(agr_class,"~/git/kids-adjectives/corpus/corpus/SDG-CHILDES-adjectives.csv")

two_adj$new_class = NA
two_adj[two_adj$Class=="dimension"|two_adj$Class=="age"|two_adj$Class=="physical",]$new_class = "dimension-age-physical"
two_adj[two_adj$Class=="value",]$new_class = "value"
two_adj[two_adj$Class=="shape"|two_adj$Class=="color"|two_adj$Class=="material",]$new_class = "shape-color-material"
agr_new_class = bootsSummary(data=two_adj, measurevar="Distance", groupvars=c("new_class"))
#write.csv(agr_new_class,"~/git/kids-adjectives/corpus/corpus/SDG-CHILDES-adjectives-subj-bins.csv")


