raw_data <- read.csv("data/Paper1_association/association_curve2copy.csv")
data <- raw_data

col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"
col_negative <- "#999999"

data$Sample <- factor(data$Sample, levels=c("CONT","RES",'PID'))
myPalette <- c(col_CONT, col_RES, col_PID)

ggplot(data=data) +
  geom_point(mapping=aes(x=time, y=r, color=Sample)) +
  scale_color_manual(values=myPalette) +
  geom_smooth(mapping=aes(x=time, y=r, color=Sample),method=lm, alpha=0.5, se=FALSE) +
  
  labs(x='Time (min)', y= expression(paste(Delta,italic('R'['H']), "(nm)"))) +
  theme(axis.text=element_text(face = 'bold',size=15), axis.title=element_text(face = 'bold',size=15),
        legend.title=element_blank(),
        legend.position=c(0.1,0.8),
        legend.text =element_text(size=15, face='bold'),
        axis.title.y=element_text(face = 'bold',size=15)) 