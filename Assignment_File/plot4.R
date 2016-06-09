plot4<-function(){
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
  
  ##set the canvas of plot4 to 480 pixel to 480 pixel
  png(filename="plot4.png",width=480,height=480)
  ##set the arrangement of plot4 to (2,2)
  par(mfrow=c(2,2))
  
  
  ##draw the 1st plot of plot4
  ##the function of GAP() is to obtain the data in the column of "Global Active Power"
  GAP<-function(d2)
  {
    for(i in 2:2881)
    {
      GAP<-c(GAP,as.vector(d2[[i]][3],mode="numeric"))
    }
    GAP
  }
  ##get the data of "Global Active Power" and store it into d3
  d3<-GAP(d2)
  d4<-as.vector(d3,mode="character")
  d4<-as.vector(d4,mode="numeric")
  d5<-c(1:2880)
  plot(d5,d4[2:2881],pch=".",xlab="",ylab="Global Active Power (kilowatts)",xaxt="n")
  axis(1,at=2,labels="Thur")
  axis(1,at=1442,labels="Fri")
  axis(1,at=2881,labels="Sat") 
  lines(d5,d4[2:2881])
  
  ##draw the 2nd plot of plot4
  ##Get the information of Voltage first and store it into d10
  Volt<-function(d2)
  {
    for(i in 2:2881)
    {
      Volt<-c(Volt,as.vector(d2[[i]][5],mode="numeric"))
    }
    Volt
  }
  d10<-Volt(d2)
  ##draw the 2nd plot of plot4 according to the requirement
  plot(d6,d10[2:2881],pch=".",xlab="datetime",ylab="Voltage",xaxt="n")
  lines(d6,d10[2:2881])
  axis(1,at=2,labels="Thur")
  axis(1,at=1442,labels="Fri")
  axis(1,at=2881,labels="Sat")
  
  ##draw the 3rd plot of plot4
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
  d13<-sm1(d2)
  d14<-sm2(d2)
  d15<-sm3(d2)
  d6<-c(1:2880)
  ##draw the 3rd plot of plot4 according to the requirement
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
  
  ##draw the 4th plot of plot4
  ##function GRP() is to get the data of Global reactive power
  GRP<-function(d2)
  {
    for(i in 2:2881)
    {
      GRP<-c(GRP,as.vector(d2[[i]][4],mode="numeric"))
    }
    GRP
  }
  d11<-GRP(d2)
  ##draw the 4th plot of plot4 according to the requirement
  plot(d6,d11[2:2881],pch=".",xlab="datetime",ylab="Global_reactive_power",xaxt="n")
  lines(d6,d11[2:2881])
  axis(1,at=2,labels="Thur")
  axis(1,at=1442,labels="Fri")
  axis(1,at=2881,labels="Sat")
  ##off the device
  dev.off()
  
}