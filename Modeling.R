df<-read.table(choose.files(),header = T,sep = ",")
#clean data
df<-df[(df$Satisfaction=="1" | 
          df$Satisfaction=="1.5" |
          df$Satisfaction=="2" |
          df$Satisfaction=="2.5" |
          df$Satisfaction=="3" |
          df$Satisfaction=="3.5" |
          df$Satisfaction=="4" |
          df$Satisfaction=="4.5" |
          df$Satisfaction=="5" ),]
newCol<-colnames(df)
newCol<-gsub("\\.", "", newCol)
colnames(df)<-newCol
a <- sub("No","0",df$Flightcancelled)
b <- sub("Yes","1",a)
df$Flightcancelled <- b
df$Flightcancelled<-as.numeric(df$Flightcancelled)
df$Satisfaction<- as.numeric(as.character(df$Satisfaction))

#average satisfaction per airline
CompOverallSat<-aggregate(df[, 1], list(df$AirlineName), mean)
CompOverallSat<-data.frame(CompOverallSat)
colnames(CompOverallSat) <- c("Airline", "AverageCustRating")
library(ggplot2)
plot1<-ggplot(CompOverallSat, aes(x=Airline, y=AverageCustRating)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot1

#Total Count of Travelers by Gender
countvar<-data.frame(table(df$Gender))
colnames(countvar) <- c("Gender", "NoOfTravelers")
countvar1<-aggregate(df[, 1], list(df$Gender), mean)
colnames(countvar1) <- c("Gender", "AverageSatisfaction")
countvar<-merge(x = countvar, y = countvar1, by = "Gender", all = TRUE)
plot2<-ggplot(countvar, aes(x=Gender, y=AverageSatisfaction)) + geom_text(aes(label=NoOfTravelers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="lightseagreen") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Gender wise Customers") + theme(plot.title= element_text(hjust=0.5)) 
plot2

#Total Number of Flights by various Age groups
countvar<-data.frame(table(df$Age))
colnames(countvar) <- c("Age", "CountOfFlights")
plot3<-ggplot(countvar, aes(x=Age, y=CountOfFlights)) + geom_text(aes(label=NoOfTravelers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="lightseagreen") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Gender wise Customers") + theme(plot.title= element_text(hjust=0.5)) 
plot3



#map
us <- map_data("state")
dfSoutheast<-df[df$AirlineCode=="US",]
GenderData <- data.frame(table(dfSoutheast$OriginState))
colnames(GenderData) <- c("stateName", "NoOfTravelers")
GenderData$State<-tolower(GenderData$State)
GenderData <- GenderData[-39,]
GenderData <- GenderData[-45,]
map<-ggplot(GenderData,aes(map_id=stateName))+geom_map(map=us, aes(fill=NoOfTravelers))+expand_limits(x = us$long, y = us$lat)+coord_map() + ggtitle("No.Of Travelers per state")+geom_text(aes(x=GenderData$lon, y=GenderData$lat, label=GenderData$state.abb), size=2)+scale_fill_gradient(low = "white", high = "blue", guide = "colorbar")
map
#duration of flight



#linear model
LM1<-lm(Satisfaction~AirlineStatus+Age+Gender+PriceSensitivity+YearofFirstFlight+NoofFlightspa+XofFlightwithotherAirlines+TypeofTravel+NoofotherLoyaltyCards+ShoppingAmountatAirport+EatingandDrinkingatAirport+Class+DayofMonth+ScheduledDepartureHour++Flightcancelled+DepartureDelayinMinutes+ArrivalDelayinMinutes+Flighttimeinminutes+FlightDistance+ArrivalDelaygreater5Mins,data=df)
summary(LM1)
LM2<-lm(Satisfaction~AirlineStatus+Age+Gender+ShoppingAmountatAirport+PriceSensitivity+YearofFirstFlight+NoofFlightspa+TypeofTravel+Class+ScheduledDepartureHour+ArrivalDelaygreater5Mins,data=df)
summary(LM2)
#single regression model
LM5<-lm(Satisfaction~NoofFlightspa,data=df)
summary(LM5)
plot(Satisfaction~NoofFlightspa,xlab="NoofFlightspa",ylab="Satisfaction",data=df)
abline(LM5)
#NoofFlightspa=0.05671105
LM5<-lm(Satisfaction~TypeofTravel,data=df)
summary(LM5)
plot(Satisfaction~TypeofTravel,xlab="TypeofTravel",ylab="Satisfaction",data=df)
#TypeofTravel=0.3350338
LM5<-lm(Satisfaction~ShoppingAmountatAirport,data=df)
summary(LM5)
plot(Satisfaction~ShoppingAmountatAirport,xlab="ShoppingAmountatAirport",ylab="Satisfaction",data=df)
#ShoppingAmountatAirport=0.0002999279 
LM5<-lm(Satisfaction~Class,data=df)
summary(LM5)
plot(Satisfaction~Class,xlab="Class",ylab="Satisfaction",data=df)
#Class=0.002526544
LM5<-lm(Satisfaction~ScheduledDepartureHour,data=df)
summary(LM5)
plot(Satisfaction~ScheduledDepartureHour,xlab="ScheduledDepartureHour",ylab="Satisfaction",data=df)
#ScheduledDepartureHour=-6.981177e-06
LM5<-lm(Satisfaction~ArrivalDelaygreater5Mins,data=df)
plot(Satisfaction~ArrivalDelaygreater5Mins,xlab="ArrivalDelaygreater5Mins",ylab="Satisfaction",data=df)
summary(LM5)
#ArrivalDelaygreater5Mins=0.02528861
#Linear Model with Airline Status as predictor
LMAirlineStatus<-lm(Satisfaction~AirlineStatus,data=df)
summary(LMAirlineStatus)
plot(Satisfaction~AirlineStatus,xlab="AirlineStatus",ylab="Satisfaction",data=df)
#AirlineStatus=0.1184333 
#Linear Model with Age as predictor
LMAge<-lm(Satisfaction~Age,data=df)
summary(LMAge)
plot(Satisfaction~Age,xlab="Age",ylab="Satisfaction",data=df)
abline(LMAge)
#Age=0.0492023
#Linear Model with Gender as predictor
LMGender<-lm(Satisfaction~Gender,data=df)
summary(LMGender)
plot(Satisfaction~Gender,xlab="Gender",ylab="Satisfaction",data=df)
abline(LMGender)
#Gender=0.01760919 
#Linear Model with Price Sensitivity as predictor
LMPriceSensitivity<-lm(Satisfaction~PriceSensitivity,data=df)
summary(LMPriceSensitivity)
plot(Satisfaction~PriceSensitivity,xlab="PriceSensitivity",ylab="Satisfaction",data=df)
abline(LMPriceSensitivity)
#PriceSensitivity=0.007641272 
#Linear Model with Airline Status Year of first flight as predictor
LMFirstFlight<-lm(Satisfaction~YearofFirstFlight,data=df)
summary(LMFirstFlight)
plot(Satisfaction~YearofFirstFlight,xlab="YearofFirstFlight",ylab="Satisfaction",data=df)
abline(LMFirstFlight)
#YearofFirstFlight=5.270168e-05 

#Association rules mining
createBuckets<- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

vBuckets<-replicate(length(df$Satisfaction),"Median")
vBuckets[df$Satisfaction>3]<-"High"
vBuckets[df$Satisfaction<3]<-"Low"

Satisfaction<-as.factor(vBuckets)

age<-createBuckets(df$Age)
pricesensitive<-createBuckets(df$PriceSensitivity)
yearoffirstflight<-createBuckets(df$YearofFirstFlight)
noofflightspa<-createBuckets(df$NoofFlightspa)
shoppingamount<-createBuckets(df$ShoppingAmountatAirport)
scheduleddeparturehour<-createBuckets(df$ScheduledDepartureHour)

library(arules)
library(arulesViz)

ruleDFSE<- data.frame(Satisfaction,age,df$Gender,pricesensitive,yearoffirstflight,noofflightspa,df$TypeofTravel,shoppingamount,df$Class,scheduleddeparturehour,df$ArrivalDelaygreater5Mins)

hotelSurveyruleDFSE<-as(ruleDFSE,"transactions")

rulesetsoutheastH<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.8),appearance = list(default="lhs", rhs=("Satisfaction=High")))
inspect(rulesetsoutheastH)
plot(rulesetsoutheastH)

goodrulesH<-sort(rulesetsoutheastH,by="lift")[1:5]
plot(goodrulesH, method = "graph", engine = "htmlwidget")
inspect(goodrulesH)

rulesetsoutheastL<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.5),appearance = list(default="lhs", rhs=("Satisfaction=Low")))
inspect(rulesetsoutheastL)
plot(rulesetsoutheastL)

goodrules1<-sort(rulesetsoutheastL,by="lift")[1:5]
inspect(goodrules1)

plot(goodrules1, method = "graph", engine = "htmlwidget")

#SVM
df$happy<-df$Satisfaction
df$happy[df$happy>=4]<-"happy"
df$happy[df$happy<4]<-"unhappy"
df1<-data.frame(df$happy,df$AirlineStatus,df$Age,df$Gender,df$PriceSensitivity,df$YearofFirstFlight,df$NoofFlightspa,df$TypeofTravel,df$ShoppingAmountatAirport,df$Class,df$ScheduledDepartureHour,df$ArrivalDelaygreater5Mins)
cutPoint2_3 <- floor(2 * dim(df1)[1]/3)
randIndex <- sample(1:dim(df1)[1])
trainData <- df1[randIndex[1:cutPoint2_3],]
testData <- df1[randIndex[(cutPoint2_3+1):dim(df1)[1]],]
happy<-testData$df.happy
table(happy)
svmOutput <- ksvm(df.happy ~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=250,cross=3, prob.model=TRUE)

svmPred <- predict(svmOutput, testData, type = "votes")
compTable <- data.frame(testData$df.happy,svmPred[2,])
table(compTable)
#confusion matrix
ctable <- as.table(matrix(c(1388,291,474,1040), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),conf.level = 0, margin = 1, main = "Confusion Matrix")

#Scatter plot of Satisfaction and Airline status
a <- sub("Blue","1",df$AirlineStatus)
b <- sub("Silver","2",a)
c<-sub("Gold","3",b)
d<-sub("Platinum","4",c)
df$AirlineStatus<-d
df$AirlineStatus<-as.numeric(df$AirlineStatus)
df$AirlineStatus<-jitter(df$AirlineStatus)
df$Satisfaction<-jitter(df$Satisfaction)
g<-ggplot(df,aes(x=AirlineStatus,y=Satisfaction))+geom_point()
g
#Bar chart for average customer satisfaction and type of travel


#Scatter plot of satisfaction and type of travel
a <- sub("Personal Travel","1",df$TypeofTravel)
b <- sub("Mileage tickets","2",a)
c<-sub("Business travel","3",b)
df$TypeofTravel<-c
df$TypeofTravel<-as.numeric(df$TypeofTravel)
df$TypeofTravel<-jitter(df$TypeofTravel)
g1<-ggplot(df,aes(x=TypeofTravel,y=Satisfaction))+geom_point()
g1

#Bar chart of average customer satisfaction by gender

#Average customer satisfaction for men that do and don¡¯t shop at airport
dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),]
dffemale<-dfSoutheast[dfSoutheast$Gender=="Male",]
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport>0]<-"shopping"
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport==0]<-"not shopping"

delay<-aggregate(dffemale[, 1], list(dffemale$ShoppingAmountatAirport), mean)
plot<-ggplot(delay, aes(x=X, y=Avaragesatisfaction)) + geom_bar(stat="identity",colour="white",fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Satisfaction In Man")
plot

#Average customer satisfaction for women that do and don¡¯t shop at airport
dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),]
dffemale<-dfSoutheast[dfSoutheast$Gender=="Female",]
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport>0]<-"shopping"
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport==0]<-"not shopping"

delay<-aggregate(dffemale[, 1], list(dffemale$ShoppingAmountatAirport), mean)
plot<-ggplot(delay, aes(x=X, y=Avaragesatisfaction)) + geom_bar(stat="identity",colour="white",fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Satisfaction In woman")
plot

#Average arrival delay per state
dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),]
delay<-aggregate(dfSoutheast[, 23], list(dfSoutheast$OriginState), mean)
colnames(delay) <- c("stateName", "ArrivalDelayinMinutes")
plot<-ggplot(delay, aes(x=stateName, y=ArrivalDelayinMinutes)) + geom_bar(stat="identity",colour="white",fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Arrival Delay Per State")
plot

#Average depature delay per state
dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),]
delay<-aggregate(dfSoutheast[, 24], list(dfSoutheast$OriginState), mean)
colnames(delay) <- c("stateName", "ArrivalDelayinMinutes")
plot<-ggplot(delay, aes(x=stateName, y=ArrivalDelayinMinutes)) + geom_bar(stat="identity",colour="white",fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Departure Delay Per State")
plot

#Average customer rating by age group
AgeSat<-aggregate(df2[, 1], list(df2$Age), mean)

AgeSat<-data.frame(AgeSat)

colnames(AgeSat) <- c("Age", "AverageCustRating")
AgeSat<-merge(x = AgeSat, y = countvar, by = "Age", all = TRUE)
plot2<-ggplot(AgeSat, aes(x=Age, y=AverageCustRating, label=CountOfFlights)) + geom_text(aes(label=CountOfFlights), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="blue") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Age wise average Customer Satisfaction") + theme(plot.title= element_text(hjust=0.5)) 


