suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Research in",i))
#dev.off()
}
rm(list = ls())
clc
clearPushBack()
library(ggplot2)
library(plyr)
library(rworldmap)
library(RColorBrewer)
cwur    <- read.csv(file="world-university-rankings/cwurData.csv", header=TRUE, sep=",")
exp     <- read.csv(file="world-university-rankings/education_expenditure_supplementary_data.csv", header=TRUE, sep=",")
att     <- read.csv(file="world-university-rankings/educational_attainment_supplementary_data.csv", header=TRUE, sep=",")
country <- read.csv(file="world-university-rankings/school_and_country_table.csv", header=TRUE, sep=",")
sh      <- read.csv(file="world-university-rankings/shanghaiData.csv", header=TRUE, sep=",")
times   <- read.csv(file="world-university-rankings/timesData.csv", header=TRUE, sep=",")
nyear<-unique(times$year)
times$num_students<-as.numeric(times$num_students)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Research in",i))
#dev.off()
}
nyear<-unique(times$year)
times$num_students<-as.numeric(times$num_students)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth', numCats=9,mapTitle=paste("Research in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'PuRd') #RdPu PuBuGn
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Research in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'PuBuGn') #RdPu PuBuGn PuRd
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Research in",i))
#dev.off()
}
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(num_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Number of students in",i))
#dev.off()
}
times$international_students<-as.numeric(times$international_students) #international_students
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Number of students in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'red') #RdPu PuBuGn PuRd
colourPalette <- brewer.pal(9,'YlGnBu') #RdPu PuBuGn PuRd YlGnBu
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Number of students in",i))
#dev.off()
}
View(times)
View(times)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
View(times)
View(cwur)
View(cwur)
View(cwur)
View(times)
View(times)
View(times)
View(times)
View(exp)
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)
library(ggplot2)
library(plyr)
library(dplyr)
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)
View(cwurCount)
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
View(times)
View(times)
View(cwur)
View(cwur)
View(cwur)
View(times)
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
library(ggplot2)
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
rm
rm(list = ls())
library(ggplot2)
library(compare)
library(plyr)
library(dplyr)
library(rworldmap)
library(RColorBrewer)
cwur    <- read.csv(file="world-university-rankings/cwurData.csv", header=TRUE, sep=",")
times   <- read.csv(file="world-university-rankings/timesData.csv", header=TRUE, sep=",")
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
colourPalette <- brewer.pal(9,'YlGnBu') #RdPu PuBuGn PuRd YlGnBu
nyear<-unique(times$year)
times$international_students<-as.numeric(times$international_students) #international_students
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'PuBuGn') #RdPu PuBuGn PuRd YlGnBu
nyear<-unique(times$year)
times$international_students<-as.numeric(times$international_students) #international_students
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'RdPu') #RdPu PuBuGn PuRd YlGnBu
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(international_students))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
colourPalette <- brewer.pal(9,'YlGnBu') #RdPu PuBuGn PuRd YlGnBu
View(cwurCount)
View(cwur)
View(times)
View(times)
View(times)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(researches))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
View(times)
View(times)
View(times)
View(times)
View(times)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(research))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Percentage of international students in",i))
#dev.off()
}
View(times)
View(times)
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(research))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste("Researches in",i))
#dev.off()
}
for(i in nyear) {
suni<-subset(times,year==i)
mm<-ddply(suni,.(country),summarize,value=mean(teaching))
#mapDevice('x11')
gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth',colourPalette=colourPalette, numCats=9,mapTitle=paste(" in",i))
#dev.off()
}
rm(list = ls())
cwur    <- read.csv(file="world-university-rankings/cwurData.csv", header=TRUE, sep=",")
times   <- read.csv(file="world-university-rankings/timesData.csv", header=TRUE, sep=",")
library(ggplot2)
library(plyr)
library(dplyr)
library(rworldmap)
library(RColorBrewer)
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_col(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_col() +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_col() +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(-country, -count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(-country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="top") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=-reorder(country, -count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1
p1 <- ggplot(cwurCount,
aes(x=-reorder(-country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(-x=reorder(-country, count), y=count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=-country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_col(position = "stack") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_bar(position = "nonw") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_bar(position = "none") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_bar(position = "none") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_bar(position = "none") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="-Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=-count, fill=country)) +
geom_bar(stat = "identity") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="-Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat = "identity") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="-Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat = "identity") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="-vdfwvfvwCount",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat = "identity") +
coord_flip() +
theme(legend.position="bottom") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, -count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x=reorder(country, count), y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
p1
p1 <- ggplot(cwurCount,
aes(x= country, y=count, fill=country)) +
geom_bar(stat="identity") +
coord_flip() +
theme(legend.position="none") +
labs(x="Count",y="Country") +
ggtitle("Countries by Number of Ranked Universities")
