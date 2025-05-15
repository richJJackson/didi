### espac4 - 5yfu Manuscript
setwd("/Volumes/RICHJ23/Projects/Cancer/Pancreas/ESPAC4_5YR_FU/Data")


library(survival)

data <- read.csv("panc_e4.csv")

data$s.ob <- Surv(data$stime,data$cen)

par(mfrow=c(1,3))

plot(survfit(s.ob~cut(data$PostOpCA199,c(-Inf,37,Inf)),data=data),col=c(2,4),
     lwd=4,cex.axis=1.3,cex.lab=1.4,xlab="Overall Survival",ylab="Time (Months)")
legend(100,0.9,c("<37",">37"),bty="n",col=c(2,4),lwd=4,cex=1.3)

plot(survfit(s.ob~cut(data$PostOpCA199,c(-Inf,90,Inf)),data=data),col=c(2,4),
     lwd=4,cex.axis=1.3,cex.lab=1.4,xlab="Overall Survival",ylab="Time (Months)")
legend(100,0.9,c("<90",">90"),bty="n",col=c(2,4),lwd=4,cex=1.3)

plot(survfit(s.ob~cut(data$PostOpCA199,c(-Inf,180,Inf)),data=data),col=c(2,4),
     lwd=4,cex.axis=1.3,cex.lab=1.4,xlab="Overall Survival",ylab="Time (Months)")
     legend(100,0.9,c("<180",">180"),bty="n",col=c(2,4),lwd=4,cex=1.3)


which(is.na(data$PostOpCA199))


st <-data$stime
cen <- data$cen
x <- log(data$PostOpCA199+1)

didi <- didiEst(st,cen,x,bw.x=0.6)
plot(didi)

