# R-sample-code
library(dplyr) 
library(zoo) #allows for converting from numeric back to date
stores <- read.csv("~/GradSchool/walmart-recruiting-store-sales-forecasting/stores.csv") 
features <- read.csv("~/GradSchool/walmart-recruiting-store-sales-forecasting/features.csv")
train <- read.csv("~/GradSchool/walmart-recruiting-store-sales-forecasting/train.csv")            #Importing the datasets to R from computer

train$Returns <- lapply(train$Weekly_Sales,function(sales){
       ifelse(sales < 0,sales,0)
   })                                                                               #Creating A returns variable for when sales are negative
train$Weekly_Sales <- lapply(train$Weekly_Sales,function(sales){
       ifelse(sales > 0,sales,0)
   })                                                                                #Changing negative sales to be zero 

train$Week<-numeric(421570)   
train$Week<-((as.numeric(as.Date(train$Date)))-14638)/7 #Week number variable created for easier indexing

m<-numeric(6435)

for(i in 1:45){
for(j in 1:143){
m[((i-1)*143)+j]<-train[train$Store == i & train$Week == j,]$Store                   
}
}                                                                                   #Getting a vector with store numbers

Store<-m

Walmart<-as.data.frame(Store)                                                       #Creating a new data frame with only total weekly sales

Week<-c(1:143)
Walmart$Week<-as.numeric(replicate(45, Week))                                       #Creating Week number to allow for indexing

Walmart$Date<-as.Date((Walmart$Week*7)+14638)

train$Weekly_Sales<-as.numeric(train$Weekly_Sales)                                  
for(i in 1:45){
for(j in 1:143){
m[((i-1)*143)+j]<-sum(train[train$Store == i & train$Week == j,]$Weekly_Sales)
}
}                                                                                    #Adding all department sales together to get total Weekly Sales
Walmart$WeeklySales<-m

train$Returns<-as.numeric(train$Returns)  #Total Weekly Returns
for(i in 1:45){
  for(j in 1:143){
    m[((i-1)*143)+j]<-sum(train[train$Store == i & train$Week == j,]$Returns)
}
}                                                                                       #Adding all department returns together to get toal weekly returns
Walmart$Returns<-m

n<-list()
for(i in 1:143){
n[i]<-train$IsHoliday[i] 
}
Walmart$IsHoliday<-c(replicate(45,n))

Walmart <- full_join(Walmart,stores,by=c("Store"))                                  #adding type variables from store datas set

Walmart$IsHoliday<-as.character(Walmart$IsHoliday)                                  #Turning a list into a csv isn't as easy as turning the variable into a character

write.csv(Walmart, file = "Walmart.csv", row.names = FALSE)                         #Will join data with features dataset in excel


features$Week<-numeric(8190)
features$Week<-((as.numeric(as.Date(features$Date)))-14638)/7
m<-numeric(8190)


Walmart <- read.csv("~/GradSchool/walmart-recruiting-store-sales-forecasting/Walmart.csv")

Walmart$Date<-as.Date((Walmart$Week*7)+14638)

Walmart[Walmart$Week == 1,]$Returns<-0           #returns from 1st weeks sales don't have a value to be put with

Walmart$Net_Sales<-numeric(6435)
for(i in 1:6435){
Walmart$Net_Sales[i]<-Walmart$WeeklySales[i]+Walmart$Returns[i+1] 
}                                                         #Returns are assumed to be from previous weeks sales, so net sales is current week sales added to next weeks returns
Walmart$Net_Sales[6435]<-760281.4



Walmart$MarkDown1<-replace(Walmart$MarkDown1, is.na(Walmart$MarkDown1),0)
Walmart$MarkDown2<-replace(Walmart$MarkDown2, is.na(Walmart$MarkDown2),0)
Walmart$MarkDown3<-replace(Walmart$MarkDown3, is.na(Walmart$MarkDown3),0)
Walmart$MarkDown4<-replace(Walmart$MarkDown4, is.na(Walmart$MarkDown4),0)
Walmart$MarkDown5<-replace(Walmart$MarkDown5, is.na(Walmart$MarkDown5),0) #Changing all the values in Markdowns of NA to 0

Walmart$IsHoliday<-as.numeric(Walmart$IsHoliday)

Walmart<-Walmart[,c(1,2,3,4,5,18,6,9,10,11,12,13,14,15,16,17,7,8)] #Reorganizing the data
for(i in 1:6435){
  Walmart$TypeB[i]<-ifelse(Walmart$Type[i] =="B",1,0) #creating an indicator variable for Type B
  }

for(i in 1:6435){
  Walmart$TypeC[i]<-ifelse(Walmart$Type[i] =="C",1,0)#creating a indicator variable for Type C
}

write.csv(Walmart, file = "Walmart.csv", row.names = FALSE)





