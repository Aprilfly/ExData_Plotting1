plot3<-function()
{
  ##set the working directory
  setwd("E:/Cousera-Data Science/Exploratory Data Analysis/exdata-data-household_power_consumption")
  ##read the corresponding data, which is that on 1/2/2007 and 2/2/2007
  expecteddata<-read.table("E:/Cousera-Data Science/Exploratory Data Analysis/exdata-data-household_power_consumption/household_power_consumption.txt",skip=66637,nrow=2880)
  ##read the head of the data
  head<-read.table("E:/Cousera-Data Science/Exploratory Data Analysis/exdata-data-household_power_consumption/household_power_consumption.txt",nrow=1)
  ##conbine the row of head and rest of data together
  d<-rbind(head,expecteddata)
  ##convert the data frame d to character mode
  d1<-as.vector(d$V1,mode="character")
  ##split the character data by ";"
  d2<-strsplit(d1,";")
  ##the function sm1() is to get the Sub_metering_1
  sm1<-function(d2)
  {
    for(i in 2:2881)
    {
      sm1<-c(sm1,as.vector(d2[[i]][7],mode="numeric"))
    }
    sm1
  }
  ##the function sm2() is to get the Sub_metering_2
  sm2<-function(d2)
  {
    for(i in 2:2881)
    {
      sm2<-c(sm2,as.vector(d2[[i]][8],mode="numeric"))
    }
    sm2
  }
  ##the function sm3() is to get the Sub_metering_3
  sm3<-function(d2)
  {
    for(i in 2:2881)
    {
      sm3<-c(sm3,as.vector(d2[[i]][9],mode="numeric"))
    }
    sm3
  }
  ##get the data of sub_metering_1 and store it to variable d13
  d13<-sm1(d2)
  ##get the data of sub_metering_2 and store it to variable d14
  d14<-sm2(d2)
  ##get the data of sub_metering_3 and store it to variable d15
  d15<-sm3(d2)
  d6<-c(1:2880)
  ##set the canvas of plot3 to 480 pixel to 480 pixel
  png(filename="plot3.png",width=480,height=480)
  ##draw the plot3 according to the requirement 
  plot(d6,d14[2:2881],pch=".",xlab="",ylab="Energy sub metering",xaxt="n",col="red",ylim=range(0,38))
  lines(d6,d14[2:2881],col="red")
  par(new=TRUE)
  plot(d6,d13[2:2881],pch=".",xlab="",ylab="Energy sub metering",xaxt="n",col="black",ylim=range(0,38))
  lines(d6,d13[2:2881],col="black")
  par(new=TRUE)
  plot(d6,d15[2:2881],pch=".",xlab="",ylab="Energy sub metering",xaxt="n",col="blue",ylim=range(0,38))
  lines(d6,d15[2:2881],col="blue")
  
  axis(1,at=2,labels="Thur")
  axis(1,at=1442,labels="Fri")
  axis(1,at=2881,labels="Sat")
  
  legend("topright",pch="-",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}