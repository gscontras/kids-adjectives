

## load in order preferences

d = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/3-order-preference/Submiterator-master/order-preference-trials.tsv",sep="\t",header=T)
head(d)
d$pred1_noun = paste(d$predicate1,d$noun)
d$pred2_noun = paste(d$predicate2,d$noun)

## load in faultless

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
#f_agr = aggregate(response~class,data=f,mean)
f_agr = aggregate(response~predicate*noun,data=f,mean)
f_agr$pred_noun = paste(f_agr$predicate,f_agr$noun)
#d$class1_f = f_agr$response[match(d$predicate1,f_agr$predicate) & match(d$noun,f_agr$noun)]
#d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
#d$class2_f = f_agr$response[match(d$class2,f_agr$class)]
d$pred1_f = f_agr$response[match(d$pred1_noun,f_agr$pred_noun)]
d$pred2_f = f_agr$response[match(d$pred2_noun,f_agr$pred_noun)]
#d$f_diff = (d$class1_f-d$class2_f)
d$f_diff = (d$pred1_f-d$pred2_f)

## load in subjectivity

s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
s_agr = aggregate(response~class,data=s,mean)
d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
d$class2_s = s_agr$response[match(d$class2,s_agr$class)]
d$s_diff = (d$class1_s-d$class2_s)

## load in subjectivity with noun

s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/7-subjectivity-noun/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
s_agr = aggregate(response~predicate*noun,data=s,mean)
s_agr$pred_noun = paste(s_agr$predicate,s_agr$noun)
#d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
#d$class2_s = s_agr$response[match(d$class2,s_agr$class)]
d$pred1_s = s_agr$response[match(d$pred1_noun,s_agr$pred_noun)]
d$pred2_s = s_agr$response[match(d$pred2_noun,s_agr$pred_noun)]
#d$s_diff = (d$class1_s-d$class2_s)
d$s_diff = (d$pred1_s-d$pred2_s)

full = d

d = na.omit(d)
d$residual_fdiff =  residuals(lm(f_diff~s_diff, data=d))
d$residual_sdiff =  residuals(lm(s_diff~f_diff, data=d))
pairscor.fnc(d[,c("response","f_diff","s_diff","residual_fdiff","residual_sdiff","slide_number")])

m.1 = lmer(response~residual_fdiff+s_diff+slide_number+(1|workerid),data=d)
summary(m.1)

m.1a = lmer(response~residual_fdiff+slide_number+(1|workerid),data=d)

m.1b = lmer(response~s_diff+slide_number+(1|workerid),data=d)

m.0 = lmer(response~slide_number+(1|workerid) + (1|noun),data=d)
m.0s = lmer(response~s_diff+slide_number+(1|workerid) + (1|noun),data=d)
m.0f = lmer(response~f_diff+slide_number+(1|workerid) + (1|noun),data=d)

anova(m.0,m.0s)
anova(m.0,m.0f)
anova(m.1a,m.1)
anova(m.1b,m.1)


m.2 = lmer(response~residual_sdiff+f_diff+slide_number+(1|workerid),data=d)
summary(m.2)

m = lmer(response~f_diff+s_diff+slide_number+(1|workerid),data=d)
summary(m)


## plot 

d_s = aggregate(response~f_diff*s_diff,data=d,mean)

ggplot(d_s, aes(x=s_diff,y=response,color=f_diff)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")

ggplot(d_s, aes(x=f_diff,y=response,color=s_diff)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")