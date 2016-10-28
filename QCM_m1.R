raw_data <- read.csv("data/Paper1_elongation/m1.csv")
yellow <- c("#E69F00",  "#CC79A7")
ggplot(data = raw_data) +
  geom_point(mapping=aes(x=time, y=delta_mass, color=when), size=3) +
  geom_smooth(mapping=aes(x=time, y=delta_mass, color=when),method=lm, se=TRUE) +
  scale_color_manual(values=yellow)