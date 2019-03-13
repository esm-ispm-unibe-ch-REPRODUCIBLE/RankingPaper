# if not already installed 
install.packages("fGarch")
library("fGarch")
source('~/Google Drive/_mydrive/R projects/Ranking/Functions Needed in Ranking.R')
pdf("Figures 2 to 4.pdf")

###############################################
#Producing Figure 2 ###########################
###############################################

par(mfrow=c(3,1))

plotNdensity.fun(c(10,1,2,3),c(3,3,3,3), maxYaxis=0.4, xaxisrange=c(-8,15),  cols=1:4)
title("Scenario 1")
legend(-8,0.4, c("P~N(10,3)","A~N(1,3)","B~N(2,3)","C~N(3,3)"), lty = rep(1,4),  col=1:4,lwd=2) 
plotNdensity.fun(c(10,1,1,1),c(3,1,3,5), maxYaxis=0.4, xaxisrange=c(-8,15),  cols=1:4)
abline(v=1)
legend(-8,0.4, c("P~N(10,3)","A~N(1,1)","B~N(1,3)","C~N(1,5)"), lty = rep(1,4),  col=1:4,lwd=2) 
title("Scenario 2")
plotNdensity.fun(c(10,1,1,1),c(3,3,3,3),xi=c(1,0.5,2,2.5), maxYaxis=0.4, xaxisrange=c(-8,15),  cols=1:4)
abline(v=1)
legend(-8,0.4, c("P~N(10,3)","A~N(1,3,0.5)","B~N(1,3,2)","C~N(1,3,2.5)"), lty = rep(1,4),  col=1:4,lwd=2) 
title("Scenario 3")


###############################################
#Producing Table 1 ###########################
###############################################

scen1=relativeranking.fun(c(10,1,2,3),c(3,3,3,3))
scen2=relativeranking.fun(mu=c(10, 1,1,1),sigma=c(3,1,3,5))
scen3=relativeranking.fun(mu=c(10, 1,1,1),sigma=c(3,3,3,3),xi=c(1,0.5,2,2.5))
scen1$Pscore=1-scen1$Pscore
scen2$Pscore=1-scen2$Pscore
scen3$Pscore=1-scen3$Pscore

Table1=round(matrix(unlist(c(scen1,scen2,scen3)),ncol=4,byrow=T)*100,1)
rownames(Table1)=rep(names(scen1),3)
colnames(Table1)=c("P","A","B","C")

sink("Table 1.txt")
cat("\n \n TABLE 1 \n \n")
print(Table1)
sink()

###############################################
#Producing Figure 3 ###########################
###############################################

par(mfcol=c(3,2))
plotNdensity.fun(c(3,1,1,1),c(1,1,1.03,1.05), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(-4,0.4, c("P~N(3,1)","A~N(1,1)","B~N(1,1)","C~N(1,1)"), lty=1,col=1:4,cex=0.8)  
title("Scenario i")
plotNdensity.fun(c(3,1,1,1),c(1,1,1.02,2), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(-4,0.4, c("P~N(3,1)","A~N(1,1)","B~N(1,1)","C~N(1,2)"),lty=1,col=1:4,cex=0.8)  
title("Scenario ii")
plotNdensity.fun(c(3,1,1,1),c(1,1,1.02,5), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(-4,0.4, c("P~N(3,1)","A~N(1,1)","B~N(1,1)","C~N(1,5)"), lty=1,col=1:4,cex=0.8)  
title("Scenario iii")
# --

plotNdensity.fun(c(2,1,1,1),c(1,1,1.02,2), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(3,0.4, c("P~N(2,1)","A~N(1,1)","B~N(1,1)","C~N(1,2)"), lty=1,col=1:4,cex=0.8) 
title("Scenario iv")
plotNdensity.fun(c(-2,1,1,1),c(1,1,1.02,2), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(3,0.4, c("P~N(-2,1)","A~N(1,1)","B~N(1,1)","C~N(1,2)"), lty=1,col=1:4,cex=0.8) 
title("Scenario v")
plotNdensity.fun(c(-3,1,1,1),c(1,1,1.02,2), maxYaxis=0.4, xaxisrange=c(-4,6),cols=1:4)
abline(v=1)
legend(3,0.4, c("P~N(-3,1)","A~N(1,1)","B~N(1,1)","C~N(1,2)"), lty=1,col=1:4,cex=0.8) 
title("Scenario vi")

###############################################
#Producing Table 2  ###########################
###############################################

a<-rbind(relativeranking.fun(c(3,1,1,1),c(1,1,1,1))$SUCRA,
relativeranking.fun(c(3,1,1,1),c(1,1,1,2))$SUCRA,
relativeranking.fun(c(3,1,1,1),c(1,1,1,5))$SUCRA,
relativeranking.fun(c(2,1,1,1),c(1,1,1,2))$SUCRA,
relativeranking.fun(c(-2,1,1,1),c(1,1,1,2))$SUCRA,
relativeranking.fun(c(-3,1,1,1),c(1,1,1,2))$SUCRA
)

Table2<-round(a,2)*100

b<-rbind(relativeranking.fun(c(3,1,1,1),c(1,1,1,1))$Pbest,
         relativeranking.fun(c(3,1,1,1),c(1,1,1,2))$Pbest,
         relativeranking.fun(c(3,1,1,1),c(1,1,1,5))$Pbest,
         relativeranking.fun(c(2,1,1,1),c(1,1,1,2))$Pbest,
         relativeranking.fun(c(-2,1,1,1),c(1,1,1,2))$Pbest,
         relativeranking.fun(c(-3,1,1,1),c(1,1,1,2))$Pbest
)
Table2pbest<-round(b,2)*100
sink("Table 2.txt")
cat("\n \n TABLE 2 \n \n")
print(Table2)

cat("\n \n TABLE 2 Pbest \n \n")
print(Table2pbest)
sink()


###############################################
#Producing Table 3  ###########################
###############################################
a<-rbind(relativeranking.fun(c(10,1,2,3),c(3,3,3,3),lowbest=T)$SUCRA,
         relativeranking.fun(c(10,1,2,3),c(3,10,3,3),lowbest=T)$SUCRA,
         relativeranking.fun(c(10,1,2,3),c(3,15,3,3),lowbest=T)$SUCRA,
         relativeranking.fun(c(10,1,2,3),c(3,20,3,3),lowbest=T)$SUCRA)
Table3<-round(a,2)
sink("Table 3.txt")
cat("\n \n TABLE 3 \n \n")
print(Table3)
sink()
#______________



###############################################
#Producing Figure 4 ###########################
###############################################

Sucras.App<-c()
Pbest.App<-c()
MeanRank.App<-c()
a<-c()
for (i in seq(1,10,0.5))
{a<-relativeranking.fun(c(-2,1,1.5,2),c(1,1,1,i))
Sucras.App<-rbind(Sucras.App,a$SUCRA)
Pbest.App<-rbind(Pbest.App,a$Pbest)
MeanRank.App<-rbind(MeanRank.App,a$MeanRank)
}

par(mfrow=c(2,1))
plot(c(1,10),c(0,1),type="n", xlab="SD in treatment C",ylab="SUCRA")
lines(seq(1,10,0.5),Sucras.App[,1], lty=1,col=1, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,2], lty=1,col=2, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,3], lty=1,col=3, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,4], lty=1,col=4, lwd=2)
abline(v=7.5)
legend(0.7,0.95, c("P","A","B","C" ), lty=1, col = 1:4, lwd=rep(2,4),cex=0.6) 

plot(c(1,10),c(0,1),type="n", xlab="SD in treatment C",ylab="P(best outcome)")
lines(seq(1,10,0.5),Pbest.App[,1], lty=1,col=1, lwd=2)
lines(seq(1,10,0.5),Pbest.App[,2], lty=1,col=2, lwd=2)
lines(seq(1,10,0.5),Pbest.App[,3], lty=1,col=3,lwd=2)
lines(seq(1,10,0.5),Pbest.App[,4], lty=1,col=4, lwd=2)
abline(v=2)
legend(0.7,0.95, c("P","A","B","C" ), lty=1, col = 1:4, lwd=rep(2,4),cex=0.6) 

dev.off()

rm(list=ls())
