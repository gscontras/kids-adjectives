library(hydroGOF)
library(ggplot2)
library(lme4)
library(lmerTest)
require(MuMIn)
library(plyr)

setwd("~/git/kids-adjectives/corpus/analysis/")

# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

s = read.csv("SDG_subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)

#load in corpus distances
c = read.csv("CHILDES_adjs.csv",header=T)
head(c)
#only look at overlapping adjectives
c = c[c$overlap==1,]


## SUBJECTIVITY
# PREDICATE
c$subjectivity = s_agr_pred$response[match(c$word,s_agr_pred$predicate)]

# Age 2 produced: 14 adjs
two_p = c[c$age==2&c$child.p.or.d=="produced",]
gof(two_p$av.distance,two_p$subjectivity) # r = -.22, r2 = .05
results <- boot(data=two_p, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0001,  0.3018 )   
# Age 2 directed: 18 adjs
two_d = c[c$age==2&c$child.p.or.d=="directed",]
gof(two_d$av.distance,two_d$subjectivity) # r = .00, r2 = .00
results <- boot(data=two_d, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0000,  0.0000 ) 

# Age 3 produced: 14 adjs
three_p = c[c$age==3&c$child.p.or.d=="produced",]
gof(three_p$av.distance,three_p$subjectivity) # r = .47, r2 = .22
results <- boot(data=three_p, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0017,  0.5210 )     
# Age 3 directed: 18 adjs
three_d = c[c$age==3&c$child.p.or.d=="directed",]
gof(three_d$av.distance,three_d$subjectivity) # r = .42, r2 = .18
results <- boot(data=three_d, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0016,  0.5133 )     

# Age 4 produced: 12 adjs
four_p = c[c$age==4&c$child.p.or.d=="produced",]
gof(four_p$av.distance,four_p$subjectivity) # r = -.36, r2 = .13
results <- boot(data=four_p, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0009,  0.4678 )    
# Age 4 directed: 19 adjs
four_d = c[c$age==4&c$child.p.or.d=="directed",]
gof(four_d$av.distance,four_d$subjectivity) # r = .16, r2 = .03
results <- boot(data=four_d, statistic=rsq, R=10000, formula=av.distance~subjectivity)
boot.ci(results, type="bca") # 95%   ( 0.0000,  0.2467 )     

#####
##PLOT distance against subjectivity
#####

c$facet = paste(c$age,c$child.p.or.d)

#count number of observations per facet
c.cor <- ddply(.data=c, 
                 .(facet), 
                 summarize, 
                 n=paste("n =", length(word)))
#calculate r2
lm_eqn = function(df){
  m = lm(av.distance ~ subjectivity, df);
  eq <- substitute(~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 1)))
  as.character(as.expression(eq));                 
}
eq <- ddply(c,.(facet),lm_eqn)

# plot 
ggplot(c, aes(x=subjectivity,y=av.distance)) +
  #geom_point(color="red") +
  geom_text(aes(label=word),color="black",alpha=.75)+
  geom_smooth(method=lm,se=FALSE,color="red",alpha=.1) +
  xlab("\nadult subjectivity")+
  ylab("corpus distance\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  facet_wrap(~facet,ncol = 2)+
  geom_text(data=c.cor, aes(x=.45, y=1, label=n),color="blue", inherit.aes=FALSE, parse=FALSE) +
  geom_text(data=eq,aes(x = 0.45, y = 0.88,label=V1),color="blue", parse = TRUE, inherit.aes=FALSE) +
  theme_bw()
#ggsave("plots/distance-subjectivity.png",width=7.5,height=6)


# plot order preference against subjectivity with text label
ggplot(o_agr_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  geom_text(aes(label=correctpred1),color="black")+
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity-labelled.pdf",height=4,width=5.5)


# plots with error bars

head(s)
head(o)
s_s = bootsSummary(data=s, measurevar="response", groupvars=c("predicate","class"))
o_s = bootsSummary(data=o, measurevar="correctresponse", groupvars=c("correctpred1","correctclass1"))
sos <- o_s
sos$subjectivity = s_s$response[match(sos$correctpred1,s_s$predicate)]
sos$s_high = s_s$bootsci_high[match(sos$correctpred1,s_s$predicate)]
sos$s_low = s_s$bootsci_low[match(sos$correctpred1,s_s$predicate)]
ggplot(sos, aes(x=subjectivity,y=correctresponse)) +
  #geom_point() +
  geom_text(aes(label=correctpred1),color="black",size=2)+
  #geom_smooth(method=lm,color="black") +
  geom_abline(slope=1,intercept=0)+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high,color="gray"),alpha=0.5)+
  geom_errorbarh(aes(xmin=s_low, xmax=s_high,color="gray"),alpha=0.5)+
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  theme(legend.position="none")
#ggsave("results/naturalness-subjectivity-errorbars.pdf",height=4,width=5.5)
# add in frequency and length 
lf = read.table("../../corpus_results/data/sampled_adjectives_with_freq.txt",sep="\t",header=T)
head(lf)
sos$freq = lf$logProbability[match(sos$correctpred1,lf$Adjective)]
sos$length = lf$Length[match(sos$correctpred1,lf$Adjective)]
ggplot(sos, aes(x=subjectivity,y=correctresponse,color=freq)) +
  geom_point(size=1) +
  geom_text(aes(label=correctpred1),size=2.5)+
  #geom_smooth(method=lm,color="black") +
  geom_abline(slope=1,intercept=0)+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high,alpha=0.1))+
  geom_errorbarh(aes(xmin=s_low, xmax=s_high,alpha=0.1))+
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  facet_wrap(~length)
  #theme(legend.position="none")
ggsave("results/naturalness-subjectivity-faceted.pdf",height=6,width=8.5)



# plot order preference against subjectivity with class facet and r2

lm_eqn = function(df){
  m = lm(correctresponse ~ subjectivity, df);
  eq <- substitute(~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#eq <- ddply(o_agr_pred,.(correctclass1),lm_eqn)
eq <- ddply(o_no_sup_pred,.(correctclass1),lm_eqn)

ggplot(o_no_sup_pred, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  #geom_text(aes(label=correctpred1),color="black")+
  geom_smooth(method=lm,se=FALSE,color="black") +
  xlab("\nsubjectivity")+
  ylab("naturalness\n")+
  ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  geom_text(data=eq,aes(x = 0.5, y = 0.2,label=V1), parse = TRUE, inherit.aes=FALSE) +
  facet_wrap(~correctclass1)#,scales="free_y")


#####
## configuration analysis
#####

# load in order preference
head(o)
o$predicate = o$correct_configuration
o$class = o$correctclass
o$workerID = o$workerid + 1
o$response = o$correctresponse
# get Spearman-Brown prophecy (explainable variance)
prophet(splithalf_class(o, 100), 2) # 0.95 class configuration
prophet(splithalf_pred(o, 100), 2) # 0.65 predicate configuration


##SUBJECTIVITY
# CLASS add in subjectivity difference
o_agr = aggregate(correctresponse~correctclass+correctclass1+correctclass2,data=o,mean)
o_agr$class1_s = s_agr_class$response[match(o_agr$correctclass1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$correctclass2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$correctresponse) # r = .83, r2 = .69
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6042,  0.7563 )  

# PREDICATE add in subjectivity difference
o_agr_pred = aggregate(correctresponse~correct_configuration+correctpred1+correctpred2+class,data=o,mean)
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$correctpred2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$correctresponse) # r = .54, r2 = .29
results <- boot(data=o_agr_pred, statistic=rsq, R=100, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.5929,  0.6769 )

## plot order preference against subjectivity
# CLASS
ggplot(o_agr, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity_class-difference.pdf",height=3,width=3.5)

#PREDICATE
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity_difference.pdf",height=3,width=3.5)

# PREDICATE FACETED BY CLASS CONFIGURATION
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()+
  facet_wrap(~class)
#ggsave("results/naturalness-subjectivity_class-facet.pdf",height=12,width=17)

#####
## configuration without superlatives
#####
head(o_no_sup)
# CLASS add in subjectivity difference
o_agr = aggregate(correctresponse~correctclass+correctclass1+correctclass2,data=o_no_sup,mean)
o_agr$class1_s = s_agr_class$response[match(o_agr$correctclass1,s_agr_class$class)]
o_agr$class2_s = s_agr_class$response[match(o_agr$correctclass2,s_agr_class$class)]
o_agr$s_diff = (o_agr$class1_s-o_agr$class2_s)
#compare subjectivity with order-preference
gof(o_agr$s_diff,o_agr$correctresponse) # r = .86, r2 = .74
results <- boot(data=o_agr, statistic=rsq, R=10000, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ( 0.6614,  0.7938 ) 
# PREDICATE add in subjectivity difference
o_agr_pred = aggregate(correctresponse~correct_configuration+correctpred1+correctpred2+correctclass,data=o_no_sup,mean)
o_agr_pred$predicate1_s = s_agr_pred$response[match(o_agr_pred$correctpred1,s_agr_pred$predicate)]
o_agr_pred$predicate2_s = s_agr_pred$response[match(o_agr_pred$correctpred2,s_agr_pred$predicate)]
o_agr_pred$s_diff = (o_agr_pred$predicate1_s-o_agr_pred$predicate2_s)
#compare subjectivity with order-preference
gof(o_agr_pred$s_diff,o_agr_pred$correctresponse) # r = .58, r2 = .33
results <- boot(data=o_agr_pred, statistic=rsq, R=100, formula=correctresponse~s_diff)
boot.ci(results, type="bca") # 95%   ???
## plot order preference against subjectivity
# CLASS
ggplot(o_agr, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("~/Documents/git/cocolab/adjective_ordering/writing/long-paper/plots/naturalness-subjectivity_class-difference_no-sup.png",height=3,width=3.5)
#PREDICATE
ggplot(o_agr_pred, aes(x=s_diff,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nsubjectivity difference")+
  ylab("configuration naturalness\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("results/naturalness-subjectivity_difference_no-sup.pdf",height=3,width=3.5)



#################################################
## REGRESSION ANALYSES
#################################################

#load in subjectivity scores
s = read.csv("results/subjectivity-expanded_results.csv",header=T)
head(s)
s_agr_pred = aggregate(response~predicate,data=s,mean)

#load in length and frequency
lf = read.table("../../corpus_results/data/sampled_adjectives_with_freq.txt",sep="\t",header=T)
head(lf)

#load in naturalness preferences
#o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference-duplicated.csv",header=T)
#non-duplicated data
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/12-order-preference-expanded/Submiterator-master/order-preference.csv",header=T)
head(o)
o$correctpred1 <- o$predicate1
o$correctpred2 <- o$predicate2
o$correctresponse <- o$response
o <- o[o$makes_sense=="yes",]

o$freq1 = lf$logProbability[match(o$correctpred1,lf$Adjective)]
o$freq2 = lf$logProbability[match(o$correctpred2,lf$Adjective)]
o$length1 = lf$Length[match(o$correctpred1,lf$Adjective)]
o$length2 = lf$Length[match(o$correctpred2,lf$Adjective)]
o$sub1 = s_agr_pred$response[match(o$correctpred1,s_agr_pred$predicate)]
o$sub2 = s_agr_pred$response[match(o$correctpred2,s_agr_pred$predicate)]
o$sup1 = 0
o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last",]$sup1 = 1
o$sup2 = 0
o[o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]$sup2 = 1

m = lmer(correctresponse~
           slide_number+
           class_configuration+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+
           sub1+
           sub2+
           sup1+
           sup2+
           sub1:sup1+
           #sub1:sup2+
           #sub2:sup1+
           sub2:sup2+
           sub1:sub2+
           sup1:sup2+
           sub1:length1+
           #sub1:length2+
           #sub2:length1+
           sub2:length2+
           sub1:freq1+
           #sub1:freq2+
           #sub2:freq1+
           sub2:freq2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           (1|workerid)+(1|noun),data=o)
summary(m)

m_no_config = lmer(correctresponse~
           slide_number+
           #class_configuration+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+
           sub1+
           sub2+
           sup1+
           sup2+
           sub1:sup1+
           #sub1:sup2+
           #sub2:sup1+
           sub2:sup2+
           sub1:sub2+
           sup1:sup2+
           sub1:length1+
           #sub1:length2+
           #sub2:length1+
           sub2:length2+
           sub1:freq1+
           #sub1:freq2+
           #sub2:freq1+
           sub2:freq2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           (1|workerid)+(1|noun),data=o)
summary(m_no_config)

anova(m,m_no_config)

## DIFFERENCE SCORES
o$subDiff = o$sub1 - o$sub2
o$lengthDiff = o$length1 - o$length2
o$freqDiff = o$freq1 - o$freq2

md = lmer(correctresponse~
           slide_number+
           sup1+
           sup2+
           subDiff+
           lengthDiff+
           freqDiff+
          #sup1:sup2+
          #sup1:subDiff+
          #sup2:subDiff+
          #sup1:lengthDiff+
          #sup2:lengthDiff+
          #sup1:freqDiff+
          #sup2:freqDiff+
          subDiff:lengthDiff+
          subDiff:freqDiff+
          lengthDiff:freqDiff+
         (1|workerid)+(1|noun),data=o)
summary(md)

m_sub = lmer(correctresponse~
            #slide_number+
            #sup1+
            #sup2+
            subDiff+
            #lengthDiff+
            #freqDiff+
            #sup1:sup2+
            #sup1:subDiff+
            #sup2:subDiff+
            #sup1:lengthDiff+
            #sup2:lengthDiff+
            #sup1:freqDiff+
            #sup2:freqDiff+
            #subDiff:lengthDiff+
            #subDiff:freqDiff+
            #lengthDiff:freqDiff+
            (1|workerid)+(1|noun),data=o)
summary(m_sub)


## NO SUPERLATIVES
o_no_sup <- o[o$correctpred1!="best"&o$correctpred1!="biggest"&o$correctpred1!="closest"&o$correctpred1!="last"&o$correctpred2!="best"&o$correctpred2!="biggest"&o$correctpred2!="closest"&o$correctpred2!="last",]
#o_no_sup <- o_no_sup[o_no_sup$makes_sense=="yes",]
o_no_sup$subDiff = o_no_sup$sub1 - o_no_sup$sub2
o_no_sup$lengthDiff = o_no_sup$length1 - o_no_sup$length2
o_no_sup$freqDiff = o_no_sup$freq1 - o_no_sup$freq2
# explainable variance
o_no_sup$workerID = o_no_sup$workerid + 1
o_no_sup$response = o_no_sup$correctresponse
o_no_sup$predicate = o_no_sup$correctpred1
prophet(splithalf_pred(o_no_sup, 100), 2) # 0.98

md_sup = lmer(correctresponse~
            slide_number+
            subDiff+
            lengthDiff+
            freqDiff+
            subDiff:lengthDiff+
            subDiff:freqDiff+
            lengthDiff:freqDiff+
            (1|workerid)+(1|noun),data=o_no_sup)
summary(md_sup)

md_sup_config = lmer(correctresponse~
                slide_number+
                class_configuration+
                subDiff+
                lengthDiff+
                freqDiff+
                subDiff:lengthDiff+
                subDiff:freqDiff+
                lengthDiff:freqDiff+
                (1|workerid)+(1|noun),data=o_no_sup)
summary(md_sup_config)

anova(md_sup,md_sup_config)


m_no_sup = lmer(correctresponse~
           slide_number+
           #makes_sense+
           length1+
           length2+
           freq1+
           freq2+ 
           sub1+
           sub2+
           #sup1+
           #sup2+
           #sub1:sup1+
           #sub2:sup2+
           sub1:sub2+
           #sup1:sup2+
           #sub1:makes_sense+
           #sub2:makes_sense+
           #sup1:makes_sense+
           #sup2:makes_sense+
           sub1:length1+
           sub2:length2+
           sub1:freq1+
           sub2:freq2+
           (1|workerid)+(1|noun),data=o_no_sup)
summary(m_no_sup)



r.squaredGLMM(md)
r.squaredGLMM(m_sub)
#r.squaredGLMM(md_sup)
#r.squaredGLMM(md_sup_config)

r.squaredGLMM(m)
r.squaredGLMM(m_no_config)


# add in frequency and length 
lf = read.table("../../corpus_results/data/sampled_adjectives_with_freq.txt",sep="\t",header=T)
head(lf)
o_agr_pred$freq = lf$logProbability[match(o_agr_pred$correctpred1,lf$Adjective)]
o_agr_pred$length = lf$Length[match(o_agr_pred$correctpred1,lf$Adjective)]
head(o_agr_pred)

m = glm(correctresponse~subjectivity+freq+length,data=o_agr_pred)
summary(m)
r.squaredGLMM(m)
