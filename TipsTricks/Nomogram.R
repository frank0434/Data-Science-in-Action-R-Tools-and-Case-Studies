
# install.packages("Hmisc")
# install.packages("lattice")
# install.packages("Formula")
# install.packages("ggplot2")
# install.packages("foreign")
# install.packages("rms")

library(rms)
library(ggplot2)
setwd("D:\\bio-analysis\\immune prognosis\\redox-immune\\TCGA\\16 nomogram")          #???ù???Ŀ¼

rt=read.table("indepInput.txt",sep="\t",header=T,row.names=1,check.names=F)           #??ȡ?????ļ?

#???ݴ???
dd <- rms::datadist(rt)
options(datadist="dd")

#???ɺ???
f <- cph(Surv(futime, fustat) ~ Gender+Age+Histologicgrade+TNMstage+Riskscore, x=T, y=T, surv=T, data=rt, time.inc=1) #????R?????£?ԭʼ?ű?????.??????Ϊ????????ʾ?ı?��????Histologicgrade??TNMstage??Riskscore?????пո????????򱨴���
surv <- Survival(f)

#??��nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(3, x), function(x) surv(5, x)), 
    lp=F, funlabel=c("1-year survival", "3-year survival", "5-year survival"), 
    maxscale=100, 
    fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2,0.1,0.05))  

#nomogram???ӻ?
pdf(file="nomogram.pdf",height=6,width=10)
plot(nom)
dev.off()

#????cilibration curve
time=1
f <- cph(Surv(futime, fustat) ~ Gender+Age+Histologicgrade+TNMstage+Riskscore, x=T, y=T, surv=T, data=rt, time.inc=time) 
cal <- calibrate(f, cmethod='KM', method="boot", u=time, m=100, B=1000)  

time=3
f <- cph(Surv(futime, fustat) ~ Gender+Age+Histologicgrade+TNMstage+Riskscore, x=T, y=T, surv=T, data=rt, time.inc=time) 
cal3 <- calibrate(f,cmethod='KM', method="boot", u=time, m=100, B=1000) 

time=5
f <- cph(Surv(futime, fustat) ~ Gender+Age+Histologicgrade+TNMstage+Riskscore, x=T, y=T, surv=T, data=rt, time.inc=time) 
cal5 <- calibrate(f, cmethod='KM', method="boot", u=time, m=100, B=1000) 

# Draw graphs
plot(cal,lwd=2,lty=1,xlab=" Predicted OS probability",
     ylab=list("Actual OS probability"),xlim=c(0,1),ylim=c(0,1),
     errbar.col=c(rgb(0,0,255,maxColorValue=255)),col=c(rgb(0,0,255,maxColorValue=255)))
subset_cal3 <- as.matrix(cal3[,c("mean.predicted","KM","std.err")])
lines(subset_cal3,
      type="b",pch=16,lwd=2,lty=1,col=c(rgb(0,255,0,maxColorValue=255)))
segments(subset_cal3[1,1], subset_cal3[1,1] - subset_cal3[1,3],
         subset_cal3[1,1], subset_cal3[1,1] + subset_cal3[1,3],
         pch=16,lwd=2,lty=1,col=c(rgb(0,255,0,maxColorValue=255)))

lines(cal5[,c("mean.predicted","KM","std.err")],type="b",pch=16,lwd=2,lty=1,col=c(rgb(255,0,0,maxColorValue=255)))

legend("bottomright",inset=0.05,
       c("1-year", "3-year", "5-year"),
       x.intersp=0.1, y.intersp=1,
       lty= 1,lwd=2,col=c("blue","green","red"),
       bty = "n",# ?????߿򣬴˴?Ϊ??Ҫ?߿?
       seg.len=0.3,cex=0.8)



dev.off()


#ggplot2 draw the graph


subset_cal <- as.data.frame(cal[,c("mean.predicted","KM","std.err")])
subset_cal$year <- "1-year"

subset_cal3 <- as.data.frame(cal3[, c("mean.predicted","KM","std.err")])
subset_cal3$year <- "3-year"
subset_cal5 <- as.data.frame(cal5[, c("mean.predicted","KM","std.err")])
subset_cal5$year <- "5-year"
l <- list(subset_cal, subset_cal3, subset_cal5)
df <- do.call(rbind, l)

lineszie <- 1
textsize <- 12
col1=c(rgb(0,0,255,maxColorValue=255))
col3=c(rgb(0,255,0,maxColorValue=255))
col5=c(rgb(255,0,0,maxColorValue=255))
df$year <- as.factor(df$year)
p <- ggplot(df, aes(mean.predicted, KM, group = year)) +
    geom_point(aes(color = year), show.legend = FALSE,
               width = 0.1, position = position_dodge(width = 0.2)) +
    geom_line(aes(color = year),size = lineszie) +
    xlim(-0.05,1)+ 
    ylim(-0.05,1) +
    labs(x = " Predicted OS probability", y = "Actual OS probability")+
    geom_errorbar(aes(x = mean.predicted, y = KM,
                      ymin = KM - std.err, 
                      ymax = KM + std.err, 
                      group = year, color = year), size = lineszie, width = 0.015)+
    geom_abline(color = "grey80",size  = 1) +
    scale_color_manual(name = "", values = c(col1,col3,col5))+
    theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = "NA"),
          legend.position = c(0.6,0.2), text = element_text(size = textsize),
          legend.text = element_text(size = textsize, face = "bold"), legend.key.width =  unit(1.2, "cm"),
          axis.text = element_text(size = textsize))
p
ggsave("ActualVSPrediction.png", dpi = 300, height = 7, width = 7)
