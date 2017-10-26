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

s = read.csv("../../experiments/1-kids-subjectivity/results/adjective-subjectivity.csv",header=T) #new numbers
#s = read.csv("SDG_subjectivity-expanded_results.csv",header=T) #SDG numbers
head(s)
s_agr_pred = s
#s_agr_pred = aggregate(response~predicate,data=s,mean)

#load in corpus distances
c = read.csv("CHILDES_adjs.csv",header=T)
head(c)
#only look at overlapping adjectives
#c = c[c$overlap==1,]

# histogram
ggplot(c, aes(x=tokens)) +
  geom_histogram() +
  facet_wrap(~facet,ncol = 2) +
  theme_bw()


## SUBJECTIVITY
# PREDICATE
c$subjectivity = s_agr_pred$response[match(c$word,s_agr_pred$predicate)]
c = c[!is.na(c$subjectivity),]

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
  geom_text(data=c.cor, aes(x=.45, y=0.88, label=n),color="blue", inherit.aes=FALSE, parse=FALSE) +
  #geom_text(data=eq,aes(x = 0.45, y = 0.88,label=V1),color="blue", parse = TRUE, inherit.aes=FALSE) +
  theme_bw()
#ggsave("plots/distance-subjectivity.png",width=7.5,height=6)




## 5+ tokens plot

c5 = c[c$tokens>4,]

c5$facet = paste(c5$age,c5$child.p.or.d)

#count number of observations per facet
c5.cor <- ddply(.data=c5, 
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
eq <- ddply(c5,.(facet),lm_eqn)

# plot 
ggplot(c5, aes(x=subjectivity,y=av.distance)) +
  #geom_point(color="red") +
  geom_text(aes(label=word),color="black",alpha=.75)+
  geom_text(aes(label=tokens,y=av.distance+.08),color="red",alpha=.75)+
  geom_line(stat='smooth', method = "lm", se=FALSE, color = "blue", alpha=0.3)+
  #geom_smooth(method=lm,se=FALSE,color="blue",alpha=.1) +
  xlab("\nadult subjectivity")+
  ylab("corpus distance\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  facet_wrap(~facet,ncol = 2)+
  geom_text(data=c5.cor, aes(x=.45, y=1, label=n),color="blue", inherit.aes=FALSE, parse=FALSE) +
  geom_text(data=eq,aes(x = 0.45, y = 0.88,label=V1),color="blue", parse = TRUE, inherit.aes=FALSE) +
  theme_bw()
#ggsave("plots/distance-subjectivity-5+.png",width=7.5,height=6)
