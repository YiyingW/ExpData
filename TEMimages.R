library(png)

col_CONT <- "#F8766D"
col_RES <- "#A3A500"
col_PIC <- "#00BF7D"
col_OXR <- "#00B0F6"
col_PID <- "#E76BF3"


img <- readPNG("data/pid_structure.png")

h<-dim(img)[1]
w<-dim(img)[2]
png("out_pid_structure.png", width=w, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")    
rasterImage(img, usr[1], usr[3], usr[2], usr[4])
#rect(0.73,0.85,0.97,0.96, col='white', border=NA)
#add text
text(.8,.1, "piceid (PID)", cex=2, font=2, col=col_PID)

#close image
dev.off()
