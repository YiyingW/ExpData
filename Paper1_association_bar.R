
col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"

data <- data.frame(
  c(1.11118, 0.573265, 0.714),
  c(0.090179,0.085202, 0.1025),
  c(0.926498,0.719794,0.8064),
  c(1.535516, 1.424165, 1.3256)
)
colnames(data) <- c('RES', 'PIC', 'OXR', 'PID')

data2 <- 
  data %>%
  gather(key='Sample')


data_avg <-
  data2 %>%
  group_by(Sample) %>%
  summarise(avg=mean(value))
data_sem <-
  data2 %>%
  group_by(Sample) %>%
  summarise(sem=std.error(value))
data_summary <-
  data_avg %>%
  inner_join(data_sem, by=c("Sample"))

data_summary$Sample <- factor(data_summary$Sample, levels=c("RES",'PIC','OXR','PID'))

myPalette <- c(col_RES, col_PIC, col_OXR, col_PID)
# Bar graph
ggplot(data_summary) +
  geom_bar(mapping=aes(x=Sample, y=avg,fill=Sample), position="dodge", stat="identity") +
  #scale_alpha_discrete(range=c(0.95, 0.65)) + 
  geom_errorbar(mapping=aes(x=Sample, y=avg,ymax=avg+sem,ymin=avg-sem), size=0.8, width=0.2, position=position_dodge(.9))+
  scale_fill_manual(values=myPalette) + 
  labs(x=NULL, y='Relative Association Rate') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        strip.text.x = element_text(size = 20, face='bold'),
        axis.title.y=element_text(margin=margin(0,20,0,0)))+
  geom_hline(yintercept = 1, linetype=2) +
  annotate("text", x=2, y=0.1, label="***", size=10) +
  annotate("text", x=4, y=1.485, label="*", size=10) +
  geom_path(x=c(1,1,2,2),  y=c(1.55,1.65,1.65,1.55), size=1) +
  annotate("text", x=1.5, y=1.685, label="+", size=10) + 
  geom_path(x=c(2.0,2.0,3,3), y=c(1.55,1.68,1.68,1.55), size=1) +
  annotate("text", x=2.5, y=1.715, label="++", size=10) +
  geom_path(x=c(2.0,2.0,4,4),  y=c(1.55,1.75,1.75,1.55), size=1) +
  annotate("text", x=3, y=1.785, label="++", size=10) +
  scale_y_continuous(limits=c(0, 1.85),breaks=seq(0, 1.85, by = 0.25))