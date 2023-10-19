#bar graphs

df<-data.frame(mean=c(24,25,27,24),sd=c(1.1,2.1,1.5,1.8),Category=as.factor(c("A","B","C","D")))
df
library(ggplot2)
ggplot(df,aes(x=Category))+geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")
