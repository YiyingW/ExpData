raw_data <- read.csv("data/Paper1_elongation/m1_2.csv")
#yellow <- c("#E69F00",  "#CC79A7")

col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"

myPalette <- c(col_RES, col_PIC, col_OXR, col_PID)

raw_data$sample <- factor(raw_data$sample, levels=c("RES",'PIC','OXR','PID'))
ann_text <- data.frame(time=2, delta_mass=0.5, sample=factor("RES", levels = c(
  "RES", "PIC", "OXR", "PID")))

ggplot(data = raw_data) +
  geom_point(mapping=aes(x=time, y=delta_mass, color=sample, shape=when,alpha=when), size=3) +
  geom_smooth(mapping=aes(x=time, y=delta_mass, color=sample, shape=when,alpha=when),method=lm, se=FALSE) +
  facet_wrap(~sample, nrow=2) +
  scale_alpha_manual(values=c(0.5,0.9)) +
  scale_color_manual(values=myPalette) +
  labs(x='Time (min)', y=bquote('Mass ('*mu~ 'g/' ~ cm^-2*')'))+
  theme(axis.text=element_text(face = 'bold',size=15), axis.title=element_text(face = 'bold',size=15),
        legend.title=element_blank(),
        legend.position="none",
        strip.text.x = element_text(size = 15, face='bold'),
        axis.title.y=element_text(face = 'bold',size=15)) 
