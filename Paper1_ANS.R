library(ggplot2)
library(tidyverse)
library(plotrix)
col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"

data <- data.frame(
  c(0.99999, 0.9999998, 1.000000, 1.00000001, 1.000000),
  c(0.8838, 0.7836, 0.6908,NA, 0.6833),
  c(0.3098, 0.5366, 0.5071, 0.6422, 0.6583),
  c(0.8732, 0.5079, 1.4487, 1.9918, 0.1083),
  c(0.7817, 1.0721, 0.7777, 1.0718, 0.3583)
)
colnames(data) <- c('CONT','RES', 'PIC', 'OXR', 'PID')

data2 <- 
  data %>%
  gather(key='Sample')


data_avg <-
  data2 %>%
  group_by(Sample) %>%
  summarise(avg=mean(value, na.rm=TRUE))
data_sem <-
  data2 %>%
  group_by(Sample) %>%
  summarise(sem=std.error(value, na.rm=TRUE))
data_summary <-
  data_avg %>%
  inner_join(data_sem, by=c("Sample"))

data_summary$Sample <- factor(data_summary$Sample, levels=c("CONT","RES",'PIC','OXR','PID'))

myPalette <- c(col_CONT,col_RES, col_PIC, col_OXR, col_PID)
# Bar graph
ggplot(data_summary) +
  geom_bar(mapping=aes(x=Sample, y=avg,fill=Sample), position="dodge", stat="identity") +
  #scale_alpha_discrete(range=c(0.95, 0.65)) + 
  geom_errorbar(mapping=aes(x=Sample, y=avg,ymax=avg+sem,ymin=avg-sem), size=0.8, width=0.2, position=position_dodge(.9))+
  scale_fill_manual(values=myPalette) + 
  labs(x=NULL, y='ANS Fluorescence\n(Fraction of Control)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        strip.text.x = element_text(size = 20, face='bold'),
        axis.title.y=element_text(margin=margin(0,20,0,0))) +
  geom_hline(yintercept = 1, linetype=2) +
  geom_path(x=c(1,1,1,2,2), y=c(1.05,1.1,1.1,1.1,1.05), size=1) +
  annotate("text", x=1.5, y=1.1, label="***", size=10) + 
  geom_path(x=c(2.0,2.0,3,3,3), y=c(1.05, 1.12, 1.12, 1.12, 1.05), size=1) +
  annotate("text", x=2.5, y=1.12, label="*", size=10) +
  geom_path(x=c(1,1,3,3,3), y=c(1.05,1.2,1.2,1.2,1.05), size=1) +
  annotate("text", x=2, y=1.2, label="***", size=10) +
  scale_y_continuous(limits=c(0, 1.5),breaks=seq(0, 1.5, by = 0.25))