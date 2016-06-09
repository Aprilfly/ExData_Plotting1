plot2<-function(){
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
  
  png(filename="plot2.png",width=480,height=480)
  d5<-c(1:2880)
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
  ##convert the list type data to numeric data
  d4<-as.vector(d3,mode="character")
  d4<-as.vector(d4,mode="numeric")
  ##draw the 2nd plot according to the requirement
  plot(d5,d4[2:2881],pch=".",xlab="",ylab="Global Active Power (kilowatts)",xaxt="n")
  axis(1,at=2,labels="Thur")
  axis(1,at=1442,labels="Fri")
  axis(1,at=2881,labels="Sat")
  lines(d5,d4[2:2881])
  dev.off()
}