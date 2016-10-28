cont <- read.csv("data/Paper_1_ANS/CONT_ans.csv", as.is=TRUE)
res <- read.csv("data/Paper_1_ANS/RES_ans.csv", as.is=TRUE)
pic <- read.csv("data/Paper_1_ANS/PIC_ans.csv", as.is=TRUE)
oxr <- read.csv("data/Paper_1_ANS/OXR_ans.csv", as.is=TRUE)
pid <- read.csv("data/Paper_1_ANS/PID_ans.csv", as.is=TRUE)

cont_ <-
  cont %>%
  mutate(Sample=rep("CONT", nrow(cont)))
res_ <-
  res %>%
  mutate(Sample=rep("RES", nrow(res))) 
pic_ <-
  pic %>%
  mutate(Sample=rep("PIC", nrow(pic))) 
oxr_ <-
  oxr %>%
  mutate(Sample=rep("OXR", nrow(oxr))) 
pid_ <-
  pid %>%
  mutate(Sample=rep("PID", nrow(pid))) 

data <-
  cont_ %>%
  rbind(res_) %>%
  rbind(pic_) %>%
  rbind(oxr_) %>%
  rbind(pid_)


col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"

data$Sample <- factor(data$Sample, levels=c("CONT","RES",'PIC','OXR','PID'))
ggplot(data=data) +
  geom_point(mapping=aes(x=wavelength, y=blank)) +
  geom_line(mapping=aes(x=wavelength, y=blank), size=0.5) +
  geom_point(mapping=aes(x=wavelength, y=sample, color=Sample)) +
  geom_line(mapping=aes(x=wavelength, y=sample, color=Sample), size=0.5) +
  facet_wrap(~Sample, nrow=1) + 
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_continuous(limits = c(405, 600)) +
  geom_ribbon(data=data%>%filter(wavelength>=450,wavelength<=550), aes(x=wavelength, ymin=blank, ymax=sample, fill=Sample), alpha=0.8) + 
  geom_ribbon(data=data%>%filter(wavelength>=450,wavelength<=550), aes(x=wavelength, ymin=0, ymax=blank), fill='black', alpha=0.8) +
  labs(x='Wavelength (nm)', y='Fluorescence Emission (A.U.)') +
  theme(axis.text=element_text(face = 'bold',size=10), axis.title=element_text(face = 'bold',size=12),
        legend.position='none', 
        strip.text.x = element_text(size = 15, face='bold'),
        axis.title.y=element_text(margin=margin(0,20,0,0)))
