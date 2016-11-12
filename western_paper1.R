
col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"

western <- read.csv("data/western_data.csv")

western_avg <-
  western %>%
  group_by(Sample, Size) %>%
  summarise(avg=mean(value))
western_sem <-
  western %>%
  group_by(Sample, Size) %>%
  summarise(sem=std.error(value))

western_summary <-
  western_avg %>%
  inner_join(western_sem, by=c("Sample","Size"))
western_summary$Sample <- factor(western_summary$Sample, levels=c("RES",'PIC','OXR','PID'))
western$Sample <- factor(western$Sample, levels=c("RES",'PIC','OXR','PID'))
myPalette <- c(col_RES, col_PIC, col_OXR, col_PID)
# Bar graph
ggplot(western_summary) +
  geom_bar(mapping=aes(x=Sample, y=avg,fill=Sample), position="dodge", stat="identity") +
  facet_wrap(~Size, nrow=1) +
  #scale_alpha_discrete(range=c(0.95, 0.65)) + 
  geom_errorbar(mapping=aes(x=Sample, y=avg,ymax=avg+sem,ymin=avg-sem), size=0.8, width=0.2, position=position_dodge(.9))+
  scale_fill_manual(values=myPalette) + 
  labs(x=NULL, y='6E10 Intensity\n(Fraction of Control)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        strip.text.x = element_text(size = 20, face='bold'),
        axis.title.y=element_text(margin=margin(0,20,0,0)))+
  geom_hline(yintercept = 1, linetype=2) 





