library(ggplot2)

source("visualization_functions.r")
source("sleep_data.r") # contains timesList

# ---------------------------- prepare data for plotting -------------------------------- #

# check if all days are odd numbers (a day starts with wake up and ends with wake up on the day after)
e<-any(odd(unlist(lapply(FUN=length,X=timesList)))=="no")==TRUE
if(e==TRUE) paste("Day",which(odd(unlist(lapply(FUN=length,X=timesList)))=="no"),"is an even number!")
stopifnot(any(odd(unlist(lapply(FUN=length,X=timesList)))=="no")==FALSE)
     
# bind together the data frame
dat <- data.frame(time=unlist(lapply(FUN=polygonTime,X=timesList)),
                  day=unlist(mapply(FUN=polygonDay,time=timesList,x=1:length(timesList))),
                  sleep=unlist(lapply(FUN=polygonSleep,X=timesList)))
dat$id <- rep(1:(nrow(dat)/4),each=4)


# ------------------------------ plot --------------------------------------------- #

#pdf("sleep.pdf",width=8,height=5)
gp <- ggplot(data=dat,aes(x=day,y=time,fill=sleep,group=id))
datText <-
    data.frame(x=c(59,rep(NA,7143)),y=c(5.5,rep(NA,7143)),label="Sleeping in the same bed",id=99999,sleep=c("black",rep(NA,7143)))
datP <- data.frame(x=c(59,59,77,77),y=c(4.75,32,32,4.75),id=999999)
gp2 <-
    gp+geom_polygon(data=datP,aes(x=x,y=y),fill="lightgrey")+geom_polygon()+
        geom_path(color="white",size=.02)+
        scale_y_continuous(breaks=seq(5,31,by=2),
                                     labels=c(seq(5,23,by=2),seq(1,7,by=2)),
                                     "Hour")+
                  scale_x_continuous(breaks=c(1.5,seq(5.5,length(timesList)+.5,by=5)),labels=c(1,seq(5,length(timesList),by=5)),"Day")+theme_bw()+#geom_segment(aes(x=57,xend=57,y=5.5,yend=31),size=.6,linetype=2,color="white")+
                   theme(legend.title=element_blank())+coord_cartesian(xlim=c(0,78),ylim=c(4.5,32.5))+#geom_text(data=datText,aes(x=x,y=y),label=datText$label,size=5)
                                        annotate(geom="text",x=68.5,y=31.3,size=3,label="Co-sleeping")

#ggsave("sleep.pdf",width=6,height=3.5)

#dev.off()

# -------------------- plot ratio of sleep to total time --------- #
durSleep<-sapply(FUN=getSleep,X=1:length(timesList),data.list=timesList)
durAwake <-sapply(FUN=getSleep,X=1:length(timesList),data.list=timesList,sleep="awake")
dat2 <-data.frame(duration=c(durSleep,durAwake),
                  Proportion=c(durSleep,durAwake)/rep(durAwake+durSleep,2),
           sleep=rep(c("asleep","awake"),each=length(durSleep)),
           day=rep(1:length(durSleep),2))
gp2 <- ggplot(data=dat2,aes(x=day,y=Proportion,fill=sleep))

#pdf("relativeduration.pdf",width=8,height=5)
gp2+geom_bar(stat="identity")+
 # geom_hline(yintercept=mean(dat2[which(dat2$sleep=="asleep"),]$Proportion),linetype=2)+
  stat_smooth(data=dat2[which(dat2$sleep=="awake"),],se=FALSE,linetype=2,color="white",
              size=1)
#dev.off()

# ---------------------- plot ratio of daytime sleeps -------------- "
durSleepb<-do.call(args=lapply(FUN=getSleep,X=1:length(timesList),data.list=timesList,daytime=TRUE),"rbind")
durAwakeb<-do.call(args=lapply(FUN=getSleep,X=1:length(timesList),sleep="awake",data.list=timesList,daytime=TRUE),"rbind")
dat2b<-rbind(durSleepb,durAwakeb)
dailySum <-aggregate(data=dat2b,amount~day+daynight,sum)
dat2b$Proportion <- sapply(X=1:nrow(dat2b),FUN=function(x) dat2b$amount[x]/dailySum[which(dailySum[,1]==dat2b$day[x]&dailySum[,2]==dat2b$daynight[x]),3])

gp2b <- ggplot(data=dat2b,aes(x=day,y=Proportion,fill=status))

#pdf("relativedurationDayNight.pdf",width=12,height=5)
gp2b+geom_bar(stat="identity")+facet_wrap(~daynight)+
  # geom_hline(yintercept=mean(dat2[which(dat2$sleep=="asleep"),]$Proportion),linetype=2)+
  stat_smooth(data=dat2b[dat2b$status=="awake",],se=FALSE,linetype=2,color="white",
              size=1)
#dev.off()


# ----------------- plot daily number of asleep actions required --------------- #
dat3 <- data.frame(actions=floor(unlist(lapply(timesList,length))/2),
                   day=1:length(timesList),
                   lengthofday=aggregate(data=dat2,duration~day,FUN=sum)[,2])

#pdf("requiredactions.pdf",width=8,height=5)
gp3 <- ggplot(data=dat3,aes(x=day,y=actions*24/lengthofday))
gp3+geom_bar(stat="identity")+geom_smooth(se=FALSE,linetype=2,size=2,color="yellow")+scale_y_continuous("number of standardized daily asleep actions")
#dev.off

# ---------------------------- efficiency of asleep actions ------------------------- #
dat4 <-data.frame(efficiency=durSleep/dat3$actions*24/dat3$lengthofday*60,
                  day=1:length(timesList))

#pdf("efficiency.pdf",width=8,height=5)
gp4 <- ggplot(data=dat4,aes(x=day,y=efficiency))
gp4+geom_bar(stat="identity")+geom_smooth(se=FALSE,linetype=2,size=2,color="yellow")+scale_y_continuous("minutes of sleep per asleep action")
#dev.off()
