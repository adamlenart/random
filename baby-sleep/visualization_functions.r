library(ggplot2)
# --------------------------- transform data ------------------------ #

# the time vector is recorded as 5.2 for 5:20 for convenience but 5:20 is 5.33 hours numerically,
# and I would also like the hours after midnight to be stacked on the top of the earlier hours such as
# 1 am should be 25
transformTime <- function(x) {
  x1<- floor(x)
  x2<- x%%1
  x<- x1+x2*10/6
  minindex <- min(which(x>10))
  index<- which(x<10)[which(x<10)>min(which(x>10))]
  x[index] <- x[index]+24
  return(x)
}

# ---------------- data handling for geom_polygon ------------------- #

# create the 4 corners on the y-axis
polygonTime <- function(x) {
  x<- transformTime(x)
  timeLs <- list()
  for(i in 1:(length(x)-1)) {
    timeLs[[i]] <-c(x[i],x[i+1],x[i+1],x[i])
  }
  unlist(timeLs)
}

# create the 4 corners on the x-axis
polygonDay <- function(x,time)  rep(c(x,x,x+1,x+1),length(time)-1)

# create factor (whether baby is asleep or not)
polygonSleep <- function(time) c(rep(c(rep("awake",4),rep("asleep",4)),length(time)/2))


# ----------------------------- calculate ----------------------- #

odd <- function(x) ifelse(x%%2==1,yes="yes",no="no")

getSleep <- function(day,data.list,sleep="asleep",daytime=FALSE) {
  times<- data.list[[day]]
  minindex <- min(which(times>10))
  index<- which(times<10)[which(times<10)>min(which(times>10))]
  times[index] <- times[index]+24
  intervals <- diff(times)
  asleep<-odd(1:length(intervals))=="no"
  if(daytime==FALSE) {
    total<-switch(sleep,"asleep"=intervals[asleep],
                  "awake" =intervals[asleep==FALSE]) 
    return(sum(total))          
  }
  if(daytime==TRUE) {
    daynightIndex<- ifelse(times>=18,yes="night",no="day")
    total<-switch(sleep,"asleep"=intervals[asleep],
                  "awake" =intervals[asleep==FALSE]) 
    dayTotal <- switch(sleep,"asleep"=total[(daynightIndex[-length(daynightIndex)]=="day")[which(asleep==TRUE)]],
                       "awake"=total[(daynightIndex[-length(daynightIndex)]=="day")[which(asleep==FALSE)]])
    nightTotal <- switch(sleep,"asleep"=total[(daynightIndex[-length(daynightIndex)]=="night")[which(asleep==TRUE)]],
                         "awake"=total[(daynightIndex[-length(daynightIndex)]=="night")[which(asleep==FALSE)]])
    return(data.frame(amount=c(sum(dayTotal),sum(nightTotal)),
                      daynight=c("day","night"),
                      status=sleep,
                      day=day))
  }
}

# get duration of sleep only during the night
getNightSleep <- function(day,data.list) {
  times<- data.list[[day]]
  # not exactly sure of the rationale of indexing as such, I wrote this code about 3 years ago and
  # was negligent (and sleep-deprived!) to annotate it properly
  minindex <- min(which(times>10))
  index<- which(times<10)[which(times<10)>min(which(times>10))]
  times[index] <- times[index]+24
  intervals <- diff(times)
  asleep<-odd(1:length(intervals))=="no"
  # define night by sleeping times starting after 6.30 pm
  night <- which(times>18.5)
  if(any(times>30.5)) {
    if(sum(times>30.5)==1) {
      times[times>30.5] <- 30.5
    } else {
      times[min(which(times>30.5))] <- 30.5
      times[1:min(which(times>30.5))]
    }
  }
  
  nightS <- ifelse(asleep[night[1]]==TRUE,
                   yes=sum(diff(c(18,times[night]))[seq(2,length(night),by=2)]),
                   no=sum(diff(c(18,times[night]))[seq(1,length(night),by=2)])) 
  return(nightS)
}


# extract wake ups during the night
getNightWakeUp <- function(day,data.list) {
  times<- data.list[[day]]
  minindex <- min(which(times>10))
  index<- which(times<10)[which(times<10)>min(which(times>10))]
  times[index] <- times[index]+24
  night <- which(times>18.5)
  return(ifelse(odd(length(night))=="yes",yes=ceiling(length(night)/2),                                            no=length(night)/2))
}