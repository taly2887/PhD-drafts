a1<-read.csv("fNCS game.csv",header=T) 
agg1 <-aggregate(a1, by=list(a1$K,a1$E,a1$f,a1$state),FUN=mean, na.rm=TRUE)

a2<-read.csv("RawData_states.csv",header=T)
State_R<-aggregate(a2$R, by=list(a2$State, a2$Group), FUN=mean, na.rm=TRUE)
g15E<-State_R$x[1:8]
g16E<-State_R$x[9:16]
g15s<-c(1:8)
g15E<-cbind(g15s,g15E)
g16E<-cbind(g15s,g16E)
g15s<-rm

for (i in 1:15){
  g15E<-rbind(g15E,g15E)
  g16E<-rbind(g16E,g16E)
}

g16E<-as.data.frame(g16E)
g15E<-as.data.frame(g15E)

r<-nrow(agg1)
g16E<-g16E[1:r,]
g15E<-g15E[1:r,]
agg1<-cbind(agg1,g15E,g16E)

agg1$G15_msd <- (agg1$g16-agg1$g15E)^2
agg1$G16_msd <- (agg1$g16-agg1$g16E)^2
agg1$meanMSD=(agg1$G15_msd+agg1$G16_msd)/2

agg2 <-aggregate(agg1, by=list(agg1$K,agg1$E,agg1$f),FUN=mean, na.rm=TRUE)

#write.csv(agg2,"fNCS final summary.csv", row.names = FALSE)
write.csv(agg1,"fNCS states.csv", row.names = FALSE)

plot(agg2$E,agg2$meanMSD)
plot(agg2$f,agg2$meanMSD)

