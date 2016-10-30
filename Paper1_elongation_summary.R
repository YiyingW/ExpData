library(plotrix)
res_avg <- mean(c(1.21,1.25,1.73))
res_sem <- std.error(c(1.21,1.25,1.73))
pic_avg <- mean(c(0.54,0.45,0.664,0.477))
pic_sem <- std.error(c(0.54,0.45,0.664,0.477))
oxr_avg <- mean(c(0.7))
oxr_sem <- std.error(c(0.7))
pid_avg <- mean(c(0.642))
pid_sem <- std.error(c(0.642))


col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"
myPalette <- c(col_RES, col_PIC, col_OXR, col_PID)

avg <- c(res_avg, pic_avg, oxr_avg, pid_avg)
sem <- c(res_sem, pic_sem, oxr_sem, pid_sem)
sample <- c("RES", "PIC", "OXR", "PID")
data <- data.frame(avg, sem, sample)
data$sample <- factor(data$sample, levels = c("RES", "PIC", "OXR", "PID"))
ggplot(data) +
  geom_bar(mapping=aes(x=sample, y=avg,fill=sample), position="dodge", stat="identity") +
  #scale_alpha_discrete(range=c(0.95, 0.65)) + 
  geom_errorbar(mapping=aes(x=sample, y=avg,ymax=avg+sem,ymin=avg-sem), size=0.8, width=0.2, position=position_dodge(.9))+
  scale_fill_manual(values=myPalette) + 
  labs(x=NULL, y='Elongation Rate\n(Fraction of Control)') +
  theme(axis.text=element_text(face = 'bold',size=20), axis.title=element_text(face = 'bold',size=20),
        legend.position='none', 
        strip.text.x = element_text(size = 20, face='bold'),
        axis.title.y=element_text(margin=margin(0,20,0,0)))+
  geom_hline(yintercept = 1, linetype=2)