library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)


# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 


setwd("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master")

d = read.table("subjectivity-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("subjectivity-subject_information.tsv",sep="\t",header=T)
head(s)

d$language = s$language[match(d$workerid,s$workerid)]

d <- d[d$language != "Bosnian" & d$language != "Russian",]

unique(d$workerid) # n=28

aggregate(response~class,data=d,mean)

#d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))


#load in naturalness preferences
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)

#####
## compare subjectivity/faultless and naturalness
#####

# explainable variance
o$workerID = o$workerid + 1
o$response = o$correctresponse
o$class = o$correctclass
#library(plyr)
prophet(splithalf_class(o, 100), 2) # 0.99 class configuration
prophet(splithalf_pred(o, 100), 2) # 0.98 predicate configuration
d$workerID = d$workerid
prophet(splithalf_pred(d, 100), 2) # 0.98

s_agr_pred = aggregate(response~predicate,data=d,mean)
s_agr_class = aggregate(response~class,data=d,mean)

## SUBJECTIVITY
# PREDICATE
o_agr_pred$subjectivity = s_agr_pred$response[match(o_agr_pred$predicate,s_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$subjectivity) # r = .92, r2 = .85
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.7510,  0.9036 )  
# CLASS
o_agr_class$subjectivity = s_agr_class$response[match(o_agr_class$correctclass,s_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$subjectivity) # r = .92, r2 = .85
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.1890,  0.9561 )  

# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity-new.pdf",height=3,width=3.5)


#####
## configuration analysis
#####

# load in order preference
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/analysis/naturalness-configuration-duplicated.csv",header=T)
head(o)
o$predicate = o$correct_configuration
o$class = o$correctclass
o$workerID = o$workerid + 1
o$response = o$correctresponse
# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf_class(o, 100), 2) # 0.97 class configuration
prophet(splithalf_pred(o, 100), 2) # 0.82 predicate configuration



o_agr = aggregate(correctresponse~correctclass+correctclass1+correctclass2,data=o,mean)

# CLASS add in subjectivity difference
o_agr$class1_s = s_agr_class$response[match(o_agr$correctclass1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$correctclass2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$correctresponse) # r = .90, r2 = .80
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6801,  0.8787 )  

o_agr_pred = aggregate(correctresponse~correct_configuration+correctpred1+correctpred2+class,data=o,mean)

# PREDICATE add in subjectivity difference
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$correctpred2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$correctresponse) # r = .81, r2 = .66
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6142,  0.6980 ) 

## CLASS
# plot order preference against subjectivity
ggplot(o_agr, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()

## PREDICATE
# plot order preference against subjectivity
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/short-paper/plots/naturalness-subjectivity.pdf",height=3,width=3.5)
#ggsave("../results/adjective-difference.pdf",height=3,width=3.5)






















#### comparing faultless disagreement and subjectivity

f = read.table("~/Documents/git/CoCoLab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)

f = subset(f,select=c("workerid","class","predicate","slide_number","response"))
f$experiment = "faultless disagreement"
d = subset(d,select=c("workerid","class","predicate","slide_number","response"))
d$experiment = "subjectivity"
all_d = rbind(d,f)


## predicate plot by class
c_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("class","experiment"))
class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response,fill=experiment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  xlab("adjective class") +
  #facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
#ggsave("../results/class_plot_comparison.pdf",height=3)

## predicate plot by class
p_s = bootsSummary(data=all_d, measurevar="response", groupvars=c("class","predicate","experiment"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response,fill=experiment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot_comparison.png",height=5,width=7)








#### just subjectivity


## predicate plot by class
c_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("subjectivity\n")+
  xlab("adjective class") +
  #facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
ggsave("../results/class_plot.pdf",height=5)

## predicate plot by class
p_s = bootsSummary(data=d, measurevar="response", groupvars=c("class","predicate"))
p_s$predicate <- factor(p_s$predicate,ordered=is.ordered(p_s$predicate))
pred_plot <- ggplot(p_s, aes(x=reorder(predicate,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("subjectivity\n")+
  xlab("predicate") +
  facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
pred_plot
ggsave("../results/pred_plot.pdf",height=5)


