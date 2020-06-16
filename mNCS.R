dat2<-read.csv("RawData_states.csv",header=T)

participants=100
trials=160
b<-setNames(data.frame(matrix(ncol=7, nrow=0)), c("state","g9","g15","g16","E","K","Miu"))

for (loop in 1:100){
  for (Miu in seq(from=12, to=12, by=0.05)){
    for (E in seq(from=55, to=55, by=0.05)){
      smp_sizeE <- floor(E/100 * nrow(dat2))
      E_ind <- sample(seq_len(nrow(dat2)), size = smp_sizeE)
      E_shuffled <- as.data.frame(dat2[E_ind,])
      Stay <- as.data.frame(dat2[-E_ind,])
      
      smp_sizeMiu <- floor(Miu/100 * nrow(dat2))
      Miu_ind <- sample(seq_len(nrow(Stay)), size = smp_sizeMiu)
      Miu_shuffled <- as.data.frame(dat2[Miu_ind,])
      Stay <- as.data.frame(Stay[-Miu_ind,])
      
      Stay$where<-"Stay"
      E_shuffled$where<-"E"
      Miu_shuffled$where<-"f"
      
      rand_vec_s1<-c()
      rand_vec_s2<-c()
      rand_vec_s3<-c()
      rand_vec_s4<-c()
      rand_vec_s5<-c()
      rand_vec_s6<-c()
      rand_vec_s7<-c()
      rand_vec_s8<-c()
      for (i in 1:nrow(E_shuffled)){
        rand_vec_s1[i] <- sample (c(2:8), size=1, replace=F)
        rand_vec_s2[i] <- sample (c(1,3:8), size=1, replace=F)
        rand_vec_s3[i] <- sample (c(1,2,4:8), size=1, replace=F)
        rand_vec_s4[i] <- sample (c(1:3,5:8), size=1, replace=F)
        rand_vec_s5[i] <- sample (c(1:4,6:8), size=1, replace=F)
        rand_vec_s6[i] <- sample (c(1:5,7,8), size=1, replace=F)
        rand_vec_s7[i] <- sample (c(1:6,8), size=1, replace=F)
        rand_vec_s8[i] <- sample (c(1:7), size=1, replace=F)
        
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 1),rand_vec_s1[i],0)
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 2),rand_vec_s2[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 3),rand_vec_s3[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 4),rand_vec_s4[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 5),rand_vec_s5[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 6),rand_vec_s6[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 7),rand_vec_s7[i],E_shuffled$State_Shuffled[i])
        E_shuffled$State_Shuffled[i] <- ifelse((E_shuffled$State[i] == 8),rand_vec_s8[i],E_shuffled$State_Shuffled[i])
      }
      
      rand_vec_s1<-c()
      rand_vec_s2<-c()
      
      for (i in 1:nrow(Miu_shuffled)){
        rand_vec_s1[i] <- sample (c(6:8,4), size=1, replace=F)
        rand_vec_s2[i] <- sample (c(1:3,5), size=1, replace=F)
        Miu_shuffled$State_Shuffled[i] <- rand_vec_s1[i]
        Miu_shuffled$State_Shuffled[i] <- ifelse((Miu_shuffled$State[i] < 4 | Miu_shuffled$State[i] ==5), rand_vec_s2[i], Miu_shuffled$State_Shuffled[i])
      }
      
      E_shuffled$State<-E_shuffled$State_Shuffled
      E_shuffled$State_Shuffled<-NULL
      Miu_shuffled$State<-Miu_shuffled$State_Shuffled
      Miu_shuffled$State_Shuffled<-NULL
      RawData<-rbind(Stay,E_shuffled,Miu_shuffled)
      RawData <- RawData[order(RawData$X),]
      
    # Kappa loop
    for (K in 3:3){
      row_count=0
      c=0
      
      for (j in 1:participants){
        vec1<-c(0)
        vec2<-c(0)
        vec3<-c(0)
        vec4<-c(0)
        vec5<-c(0)
        vec6<-c(0)
        vec7<-c(0)
        vec8<-c(0)
        roll1=0
        roll2=0
        roll3=0
        roll4=0
        roll5=0
        roll6=0
        roll7=0
        roll8=0
        
        for (i in 1:trials){
          
          sampley=sample(1:K,1)
          c=row_count+i
          
          # State 1
          if(RawData$State[c]==1){
            sampley2<-ifelse(length(vec1) < sampley,length(vec1),sampley)
            sam<-sample(vec1,size=sampley2,replace=F)
            RawData$PredS1[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS1[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS1[c])
            roll1=roll1+1
            vec1[roll1]<-RawData$a_payoff[c]
            
            if(roll1==1){RawData$PredS1[c]=0.5}
            
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #2
          if(RawData$State[c]==2){
            sampley2<-ifelse(length(vec2)<sampley,length(vec2),sampley)
            sam<-sample(vec2,size=sampley2,replace=F)
            RawData$PredS2[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS2[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS2[c])
             
            roll2=roll2+1
            vec2[roll2]<-RawData$a_payoff[c]
            
            if(roll2==1){RawData$PredS2[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #3
          if(RawData$State[c]==3){
            sampley2<-ifelse(length(vec3)<sampley,length(vec3),sampley)
            sam<-sample(vec3,size=sampley2,replace=F)
            RawData$PredS3[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS3[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS3[c])
            roll3=roll3+1
            vec3[roll3]<-RawData$a_payoff[c]
            
            if(roll3==1){RawData$PredS3[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #4
          if(RawData$State[c]==4){
            sampley2<-ifelse(length(vec4)<sampley,length(vec4),sampley)
            sam<-sample(vec4,size=sampley2,replace=F)
            RawData$PredS4[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS4[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS4[c])
            roll4=roll4+1
            vec4[roll4]<-RawData$a_payoff[c]
            
            if(roll4==1){RawData$PredS4[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #5
          if(RawData$State[c]==5){
            sampley2<-ifelse(length(vec5)<sampley,length(vec5),sampley)
            sam<-sample(vec5,size=sampley2,replace=F)
            RawData$PredS5[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS5[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS5[c])
            roll5=roll5+1
            vec5[roll5]<-RawData$a_payoff[c]
            
            if(roll5==1){RawData$PredS5[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #6
          if(RawData$State[c]==6){
            sampley2<-ifelse(length(vec6)<sampley,length(vec6),sampley)
            sam<-sample(vec6,size=sampley2,replace=F)
            RawData$PredS6[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS6[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS6[c])
            roll6=roll6+1
            vec6[roll6]<-RawData$a_payoff[c]
            
            if(roll6==1){RawData$PredS6[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS7[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #7
          if(RawData$State[c]==7){
            sampley2<-ifelse(length(vec7)<sampley,length(vec7),sampley)
            sam<-sample(vec7,size=sampley2,replace=F)
            RawData$PredS7[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS7[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS7[c])
            roll7=roll7+1
            vec7[roll7]<-RawData$a_payoff[c]
            
            if(roll7==1){RawData$PredS7[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS8[c]<-NA
          }
          
          #8
          if(RawData$State[c]==8){
            sampley2<-ifelse(length(vec8)<sampley,length(vec8),sampley)
            sam<-sample(vec8,size=sampley2,replace=F)
            RawData$PredS8[c]<-ifelse(sum(sam)>0,1,0)
            RawData$PredS8[c]<-ifelse(sum(sam)==0,0.5,RawData$PredS8[c])
            roll8=roll8+1
            vec8[roll8]<-RawData$a_payoff[c]
            
            if(roll8==1){RawData$PredS8[c]=0.5}
            
            RawData$PredS1[c]<-NA
            RawData$PredS2[c]<-NA
            RawData$PredS3[c]<-NA
            RawData$PredS4[c]<-NA
            RawData$PredS5[c]<-NA
            RawData$PredS6[c]<-NA
            RawData$PredS7[c]<-NA
          }
        } #end of for loop for trials
        
        c=row_count+trials
        row_count=row_count+trials
        
      }#end of for loop for participants
      
      g9data<-RawData[RawData$Group==9,]
      g9temp<-as.data.frame(colMeans(g9data[,13:20], na.rm=TRUE))
      g15data<-RawData[RawData$Group==15,]
      g15temp<-as.data.frame(colMeans(g15data[,13:20], na.rm=TRUE))
      g16data<-RawData[RawData$Group==16,]
      g16temp<-as.data.frame(colMeans(g16data[,13:20], na.rm=TRUE))
      g16temp[,order(colnames(g16temp))]
      
      a<-cbind(g9temp,g15temp,g16temp)
      a <- a[order(row.names(a)), ]
      a<-cbind(c(1:8),a)
      colnames(a)<-c('state','g9','g15','g16')
      a$E<-E
      a$K<-K
      a$Miu<-Miu
      b<-rbind(b,a)
    }#end of K loop
      Sys.time()
      print(E)
    }#end of E loop
    Sys.time()
    print(Miu)
  } #end of Miu loop
  Sys.time()
  print(loop)
} #end of loop

write.csv(b,"mNCS1.csv", row.names = FALSE)
library(beepr)
beep(sound=2)
