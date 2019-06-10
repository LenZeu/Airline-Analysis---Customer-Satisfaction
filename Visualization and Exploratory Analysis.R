df<-read.table(choose.files(),header = T,sep = ",")
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

us <- map_data("state")
dfSoutheast<-df[df$AirlineCode=="US",]
str(dfSoutheast)

install.packages("mice")
library(VIM)

dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),]
dffemale<-dfSoutheast[dfSoutheast$Gender=="Female",]
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport>0]<-"shopping"
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport==0]<-"not shopping"

delay<-aggregate(dffemale[, 1], list(dffemale$ShoppingAmountatAirport), mean)




delay<-aggregate(dfSoutheast[, 23], list(dfSoutheast$OriginState), mean)


colnames(delay)<-c("X","Avaragesatisfaction")
plot<-ggplot(delay, aes(x=X, y=Avaragesatisfaction)) + geom_bar(stat="identity",colour="white",fill="blue") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Satisfaction In Man")
plot

delay<-aggregate(dfSoutheast[, "ArrivalDelayinMinutes"], list(dfSoutheast$OriginState), mean)



mergedf<-merge(dfSoutheast$OriginState)


GenderData <- data.frame(table(dfSoutheast$OriginState))
colnames(GenderData) <- c("stateName", "NoOfTravelers")
str(GenderData)
View(GenderData)
GenderData$State<-tolower(GenderData$State)

GenderData <- GenderData[-39,]
GenderData <- GenderData[-45,]

GenderData$state.abb<-statemap$state.abb
GenderData$stateName<-tolower(GenderData$stateName)
statemap <- data.frame(state.abb)
statemap$Lon <- state.center$x
statemap$Lat <- state.center$y
GenderData$lon<-statemap$Lon
GenderData$lat<-statemap$Lat

map<-ggplot(GenderData,aes(map_id=stateName))+geom_map(map=us, aes(fill=NoOfTravelers))+expand_limits(x = us$long, y = us$lat)+coord_map() + ggtitle("No.Of Travelers per state")+geom_text(aes(x=GenderData$lon, y=GenderData$lat, label=GenderData$state.abb), size=2)+scale_fill_gradient(low = "white", high = "blue", guide = "colorbar")
map


statemap <- data.frame(state.abb)
statemap$Lon <- state.center$x
statemap$Lat <- state.center$y
str(statemap)
GenderData$StateAbb <- state.abb[match(GenderData$State, state.name)]
View(GenderData)

GenderData$Lon <- state.center$x
GenderData$Lat <- state.center$y

View(GenderData)