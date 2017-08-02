library(ggplot2)
library(lme4)
library(hydroGOF)
library(tidyr)
library(dplyr)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/13-subjectivity-expanded/Submiterator-master")

num_round_dirs = 22
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/subjectivity-expanded.csv', sep='')) %>%
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid", "class","predicate","slide_number","response","language"))
unique(d$language)
d = d[d$language!="Chinese"&d$language!="Spanish, English"&d$language!="Spanish"&d$language!="Hindi"&d$language!="spanish"&d$language!="",]
unique(d$language)

length(unique(d$workerid)) # n=189

table(d$predicate)
table(d$class)

#write.csv(d,"../results/subjectivity-expanded_results.csv")


## predicate plot by class
c_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
#c_s = aggregate(response~class,data=d,mean)

class_plot <- ggplot(c_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,response,is.ordered=T), width=0.1),position=position_dodge(width=0.9))+
  ylab("subjectivity\n")+
  xlab("adjective class") +
  #facet_wrap(~class,scale="free_x") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
class_plot
ggsave("../results/class_subjectivity.pdf",height=3)



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


