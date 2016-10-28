raw_data <- read.csv("data/Paper1_elongation/protofibril_binding.csv")

proto <- 
  raw_data %>%
  filter(time <=100) 
proto$sample <- rep("Aggregates", nrow(proto))
proto <-
  proto %>%
  select(time, mass, sample)
m1 <-
  raw_data %>%
  filter(time >100, time<=150)
m1$sample <- rep("Monomer", nrow(m1))
m1 <-
  m1 %>%
  select(time, mass, sample)
pic <-
  raw_data %>%
  filter(time>150, time<=200)
pic$sample <- rep("Compound", nrow(pic))
pic <-
  pic %>%
  select(time, mass, sample)
m2 <- raw_data %>%
  filter(time>200)
m2$sample <- rep("Monomer", nrow(m2))
m2 <-
  m2 %>%
  select(time, mass, sample)





data <- rbind(proto, m1, pic, m2)
rhg_cols <- c( "#F27314", "#F8A31B", 
              "#E2C59F", "#B6C5CC", "#8E9CA3", "#556670")
ggplot(data = data) +
  geom_point(mapping=aes(x=time, y=mass, color=sample), size=3) +
  scale_color_manual(values=rhg_cols) +
  annotate("text", x=25, y=2, label="Aggregates", size=5, fontface='bold',color="#F27314") +
  annotate("text", x=120, y=3, label="Monomer", size=5, fontface='bold',color="#E2C59F") +
  annotate("text", x=180, y=3.5, label="Compound", size=5, fontface='bold',color="#F8A31B") +
  annotate("text", x=220, y=3.8, label="Monomer", size=5, fontface='bold',color="#E2C59F") +
  labs(x='Time (min)', y=bquote('Mass ('*mu~ 'g/' ~ cm^-2*')'))+
  theme(axis.text=element_text(face = 'bold',size=15), axis.title=element_text(face = 'bold',size=15),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.y=element_text(face = 'bold',size=15)) 


