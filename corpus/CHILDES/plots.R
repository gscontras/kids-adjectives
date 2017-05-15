library(ggplot2)
library(reshape2)
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

d = read.csv("~/git/kids-adjectives/corpus/CHILDES/data/data.csv",header=T)
head(d)

d <- d[d$numadj=="2",]

d <- d[d$age!="24",]

#d$age = factor(d$age,labels = c("2","3","4","adult"))
#d$age = factor(d$age,levels = c("2","3","4","adult"))
d$label = factor(d$label,labels = c("adult","child-directed","child-produced"))
d$class = factor(d$class,labels = c("age","color","dimension","dimensiomn\nage\nphysical","material","physical","shape","shape\ncolor\nmaterial","value"))
d$class = factor(d$class,levels = c("dimension","value","age","physical","shape","color","material","dimensiomn\nage\nphysical","shape\ncolor\nmaterial"))


## semantic class plots
ggplot(data=d[d$binned=="no"&d$label=="adult",],aes(x=class,y=distance,fill=age))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  geom_errorbar(aes(ymin=CI1, ymax=CI2, x=class, width=0.1),position=position_dodge(.9))+
  xlab("")+
  #labs(fill="experiment") +
  facet_grid(.~label, scales = "free_x")+
  ylab("")+
  ylim(0,1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#E76BF3"))
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/adult-plot.pdf",width=3,height=4.5)
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/adult-plot-short.pdf",width=2.2,height=2.1)


ggplot(data=d[d$binned=="no"&d$label!="adult",],aes(x=class,y=distance,fill=age))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  geom_errorbar(aes(ymin=CI1, ymax=CI2, x=class, width=0.1),position=position_dodge(.9))+
  xlab("semantic class")+
  #labs(fill="experiment") +
  facet_grid(age~label, scales = "free_x")+
  ylab("distance\n")+
  ylim(0,1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/individual-age-plot-vertical.pdf",width=4.2,height=4)

## subjectivity level plots

d$class = factor(d$class,levels = c("dimension","value","age","physical","shape","color","material","dimensiomn\nage\nphysical","shape\ncolor\nmaterial"))
d$class = factor(d$class,labels = c("dimension","high","age","physical","shape","color","material","middle","low"))

ggplot(data=d[d$binned=="yes"&d$label=="adult",],aes(x=class,y=distance,fill=age))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  geom_errorbar(aes(ymin=CI1, ymax=CI2, x=class, width=0.1),position=position_dodge(.9))+
  xlab("")+
  #labs(fill="experiment") +
  facet_grid(.~label, scales = "free_x")+
  ylab("")+
  ylim(0,1)+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#E76BF3"))
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/adult-plot-binned.pdf",width=3,height=4.5)
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/adult-plot-binned-short.pdf",width=2.2,height=1.6)

ggplot(data=d[d$binned=="yes"&d$label!="adult",],aes(x=class,y=distance,fill=age))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  geom_errorbar(aes(ymin=CI1, ymax=CI2, x=class, width=0.1),position=position_dodge(.9))+
  xlab("\n\nsubjectivity level")+
  #labs(fill="experiment") +
  facet_grid(age~label, scales = "free_x")+
  ylab("")+
  ylim(0,1)+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))
#ggsave("~/git/kids-adjectives/corpus/CHILDES/plots/individual-age-plot-binned-vertical.pdf",width=4.1,height=3.9)



