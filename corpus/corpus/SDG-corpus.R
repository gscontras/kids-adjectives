#library(ggplot2)
#library(plyr)
library(tidyverse)
theme_set(theme_bw(18))
#setwd("~/Documents/git/CoCoLab/adjective_ordering/corpus_results/")
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
adjs = data.frame(Adjective = c('red', 'yellow', 'green', 'blue', 'purple', 'brown', 'big', 'small', 'huge', 'tiny', 'short', 'long', 'wooden', 'plastic', 'metal', 'smooth', 'hard', 'soft', 'old', 'new', 'rotten', 'fresh', 'good', 'bad', 'round', 'square'), Class = c('color', 'color', 'color', 'color', 'color', 'color', 'size', 'size', 'size', 'size', 'size', 'size', 'material', 'material', 'material', 'texture', 'texture', 'texture', 'age', 'age', 'age', 'age', 'quality', 'quality', 'shape', 'shape'))
str(adjs)
row.names(adjs) = adjs$Adjective

nouns = data.frame(Noun = c("apple","banana","carrot","cheese","tomato","orange","cherry","cranberry","grape","grapefruit","pear","pomegranate","raspberry","strawberry","watermelon","beet","pepper","radish","onion","potato","rhubarb","apricot","cantaloupe","fig","kiwi","lemon","mango","nectarine","papaya","peach","persimmon","pumpkin","rutabaga","squash","corn","pineapple","tangerine","cauliflower","garlic","ginger","artichoke","kohlrabi","mushroom","parsnip","shallot","turnip","avocado","lime","pea","arugula","asparagus","broccoli","sprouts","cabbage","bean","celery","cucumber","leek","okra","spinach","zucchini","blackberry","blueberry","currant","plum","raisin","olive","eggplant","apples","bananas","carrots","cheeses","tomatoes","oranges","cherries","cranberries","grapes","grapefruits","pears","pomegranates","raspberries","strawberries","watermelons","beets","peppers","radishes","onions","potatoes","apricots","cantaloupes","figs","kiwis","lemons","mangoes","nectarines","papayas","peaches","persimmons","pumpkins","rutabagas","pineapples","tangerines","artichokes","kohlrabis","mushrooms","parsnips","shallots","turnips","avocadoes","limes","peas","cabbages","beans","celeries","cucumbers","leeks","zucchinis","watercress","blackberries","blueberries","currants","plums","raisins","olives","eggplants","chair","couch","fan","tv","desk","recliner","stool","sofa","bench","seat","bed","futon","hammock","mattress","table","television","bookcase","shelf","bookshelf","cabinet","closet","pantry","chest","drawer","nightstand","sideboard","wardrobe","chairs","couches","fans","tvs","desks","recliners","stools","sofas","benches","seats","beds","futons","hammocks","mattresses","tables","televisions","bookcases","shelves","bookshelves","cabinets","closets","pantries","chests","drawers","nightstands","sideboards","wardrobes"), NounClass = c(rep("food",125),rep("furniture",54)))
row.names(nouns) = nouns$Noun

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
nrow(d_subexp) #total of 35721 cases

gathered = d_subexp %>% 
  select(Adjective, PrevAdjective, PrevPrevAdjective, Corpus, Noun) %>% 
  gather(Position, Adjective,  Adjective:PrevPrevAdjective)
gathered = gathered[!is.na(gathered$Adjective),]
nrow(gathered) #total: 107163 cases
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
nrow(gathered) # to plot: 39199 cases
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
nrow(gathered) # 39199 total cases
nrow(gathered[gathered$DistanceFromNoun < 3,]) # 38418 adj-adj cases
two_adj <- gathered[gathered$DistanceFromNoun < 3,]
two_adj$Distance = two_adj$DistanceFromNoun - 1
head(two_adj)

#get rid of dplyr
detach("package:dplyr", unload=TRUE) 

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
#write.csv(agr_class,"~/Documents/git/cocolab/adjective_ordering/experiments/analysis/corpus_class_averages.csv")

two_adj$new_class = NA
two_adj[two_adj$Class=="size"|two_adj$Class=="age"|two_adj$Class=="texture",]$new_class = "dimension-age-physical"
two_adj[two_adj$Class=="quality",]$new_class = "value"
two_adj[two_adj$Class=="shape"|two_adj$Class=="color"|two_adj$Class=="material",]$new_class = "shape-color-material"
agr_new_class = bootsSummary(data=two_adj, measurevar="Distance", groupvars=c("new_class"))
#write.csv(agr_new_class,"~/Documents/git/meaningLab/kids-adjectives/corpus/SDG-subj-bins.csv")