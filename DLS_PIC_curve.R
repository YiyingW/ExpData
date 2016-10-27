readings <- read.csv("data/DLS_PIC_curve.csv")
to_plot_avg <- 
  readings %>%
  group_by(Time, Sample) %>%
  summarise(avg=mean(Fraction))
to_plot_sem <- 
  readings %>%
  group_by(Time, Sample) %>%
  summarise(sem=std.error(Fraction))
to_plot <-
  to_plot_avg %>%
  inner_join(to_plot_sem)
myPalette <- c("#F8766D", "#00BF7D")
ggplot(data=to_plot) +
  geom_point(mapping=aes(x=Time, y=avg, col=factor(Sample)), size=2.5)+
  scale_color_manual(values=myPalette) +
  geom_line(mapping=aes(x=Time, y=avg, col=factor(Sample)), size=1.5) +
  geom_errorbar(aes(x=Time,y=avg,ymax=avg+sem,ymin=avg-sem,
                    col=factor(Sample)), size=1, width=0.5, alpha=0.3) + 
  labs(x='Time (min)', y='Intensity\nFraction of Control Plateau') +
  theme(axis.text=element_text(face = 'bold',size=15), axis.title=element_text(face = 'bold',size=15),
        legend.title=element_blank(), legend.text=element_text(size=12),
        axis.title.y=element_text(margin=margin(0,20,0,0))) 

ggplot(data=readings, aes(x=Time, y=Fraction, col=Sample)) +
  #geom_point(size=2)+
  scale_color_manual(values=myPalette) +
  geom_smooth(mapping=aes(x=Time, y=Fraction), se=TRUE)

plateau <-
  data.frame(CONT=c(1,0.960266,1,1.135069),
             PIC=c(0.704245,0.932327,0.292359,1.157310))
plateau_to_plot <-
  plateau %>%
  gather(key=Sample)
plateau_avg <-
  plateau_to_plot %>% 
  group_by(Sample) %>%
  summarise(avg=mean(value))
plateau_sem <-
  plateau_to_plot %>% 
  group_by(Sample) %>%
  summarise(sem=std.error(value))
  
plateau_to_plot <-
  plateau_to_plot %>% 
  inner_join(plateau_avg) %>%
  inner_join(plateau_sem)

ggplot(data=plateau_to_plot) +
  geom_bar(mapping=aes(x=Sample, y=avg, fill=Sample), stat='identity',position='dodge') +
  scale_fill_manual(values=myPalette) + 
  geom_errorbar(aes(x=Sample,y=avg,ymax=avg+sem,ymin=avg-sem), size=1, width=0.5) +
  labs(x=NULL, y='Plateau Intensity\n(Fraction of Control Plateau)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        axis.title.y=element_text(margin=margin(0,20,0,0))) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_y_continuous(limits=c(0, 1.1),breaks=seq(0, 1.05, by = 0.25))
  
  
  
