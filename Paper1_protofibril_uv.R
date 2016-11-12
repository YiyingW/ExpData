raw_data <- read.csv("data/protofibril_uv.csv")

ggplot(data=raw_data) +
  geom_point(mapping=aes(x=mL, y=mAU), color="#D55E00",size=2) +
  ggtitle("SEC elution profile") +
  theme(axis.text=element_text(face = 'bold',size=25), axis.title=element_text(face = 'bold',size=25),
        axis.title.y=element_text(face = 'bold',size=25),
        plot.title=element_text(face = 'bold',size=25)) +
  annotate("text", x=15, y=28, label="Aggregate", size=12, fontface='bold',color="#0072B2") +
  annotate("text", x=16, y=12, label="Monomer", size=12, fontface='bold',color="#0072B2")
  

