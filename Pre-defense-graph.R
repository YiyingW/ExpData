library(tidyverse)

# A representative aggregation curve for paper1
one_agg <- read.csv("aggregation_curve.csv", as.is=FALSE)
one_agg_tidy <- 
  one_agg %>%
  gather(CONT:PID,key='Sample', value='y')
one_agg_tidy$Sample <- factor(one_agg_tidy$Sample, levels=c("CONT","RES",'PIC','OXR','PID'))
ggplot(data=one_agg_tidy) +
  geom_point(mapping = aes(x=Time, y=y, color=Sample), size=4) +
  geom_line(mapping = aes(x=Time, y=y, color=Sample), size=1.5) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(x='Time (min)', y='Fraction of Control Plateau') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.title=element_blank(), legend.text=element_text(size=15), legend.position=c(0.92,0.85)) 

# Paper1 aggregation plateau reduction
paper1_agg_plateau <- read.csv("paper1_agg_plateau.csv", as.is=FALSE)
agg_plateau_tidy <- 
  paper1_agg_plateau %>%
  gather(RES:PID,key='Sample', value='y')
agg_plateau_tidy$Sample <- factor(agg_plateau_tidy$Sample, levels=c("RES",'PIC','OXR','PID'))
agg_plateau_to_plot_mean <-
  agg_plateau_tidy %>%
  group_by(Sample) %>%
  summarise(avg=mean(y))
agg_plateau_to_plot_sd <-
  agg_plateau_tidy %>%
  group_by(Sample) %>%
  summarise(sem=std.error(y))
agg_plateau_to_plot <-
  agg_plateau_to_plot_mean %>%
  inner_join(agg_plateau_to_plot_sd, by='Sample')
agg_plateau_to_plot$Sample <- factor(agg_plateau_to_plot$Sample, levels=c("RES",'PIC','OXR','PID'))
myPalette <- c("#A3A500", "#00BF7D","#00B0F6", "#E76BF3")
ggplot(data=agg_plateau_to_plot) +
  geom_bar(mapping=aes(x=Sample, y=avg, fill=Sample), stat='identity',position='dodge') +
  scale_fill_manual(values=myPalette) + 
  geom_errorbar(aes(x=Sample,y=avg,ymax=avg+sem,ymin=avg-sem), size=1, width=0.5) +
  labs(x=NULL, y='Plateau\n(Fraction of Control)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        axis.title.y=element_text(margin=margin(0,20,0,0))) +
  geom_hline(yintercept = 1, linetype=2) +
  annotate("text", x=2, y=0.07, label="***", size=10) +
  geom_path(x=c(1,1,2,2), y=c(1.02,1.1,1.1,1.02), size=1) +
  annotate("text", x=1.5, y=1.125, label="+++", size=10) + 
  geom_path(x=c(2.0,2.0,3,3), y=c(1.02,1.13,1.13,1.02), size=1) +
  annotate("text", x=2.5, y=1.155, label="+", size=10) +
  geom_path(x=c(2.0,2.0,4,4), y=c(1.02,1.2,1.2,1.02), size=1) +
  annotate("text", x=3, y=1.225, label="+", size=10)

# Paper1 aggregation lag extension
paper1_agg_lag <- read.csv("paper1_agg_lag.csv", as.is=FALSE)
agg_lag_tidy <- 
  paper1_agg_lag %>%
  gather(RES:PID,key='Sample', value='y')
agg_lag_tidy$Sample <- factor(agg_lag_tidy$Sample, levels=c("RES",'PIC','OXR','PID'))
agg_lag_to_plot_mean <-
  agg_lag_tidy %>%
  group_by(Sample) %>%
  summarise(avg=mean(y))
agg_lag_to_plot_sem <-
  agg_lag_tidy %>%
  group_by(Sample) %>%
  summarise(sem=std.error(y))
agg_lag_to_plot <-
  agg_lag_to_plot_mean %>%
  inner_join(agg_lag_to_plot_sem, by='Sample')
agg_plateau_to_plot$Sample <- factor(agg_lag_to_plot$Sample, levels=c("RES",'PIC','OXR','PID'))
myPalette <- c("#A3A500", "#00BF7D","#00B0F6", "#E76BF3")
ggplot(data=agg_lag_to_plot) +
  geom_bar(mapping=aes(x=Sample, y=avg, fill=Sample), stat='identity',position='dodge') +
  scale_fill_manual(values=myPalette) + 
  geom_errorbar(aes(x=Sample,y=avg,ymax=avg+sem,ymin=avg-sem), size=1, width=0.5) +
  labs(x=NULL, y='Lag\n(Fraction of Control)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        axis.title.y=element_text(margin=margin(0,20,0,0))) +
  geom_hline(yintercept = 1, linetype=2) +
  annotate("text", x=2, y=0.1, label="Cannot be", size=5) +
  annotate("text", x=2, y=0.06, label="determined", size=5) +
  scale_y_continuous(limits=c(0, 1.25),breaks=seq(0, 1.25, by = 0.25))
