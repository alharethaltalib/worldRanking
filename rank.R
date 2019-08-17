
library(ggplot2)
library(compare)
library(plyr)
library(dplyr)
library(rworldmap)
library(RColorBrewer)



cwur    <- read.csv(file="world-university-rankings/cwurData.csv", header=TRUE, sep=",")
exp     <- read.csv(file="world-university-rankings/education_expenditure_supplementary_data.csv", header=TRUE, sep=",")
att     <- read.csv(file="world-university-rankings/educational_attainment_supplementary_data.csv", header=TRUE, sep=",")
country <- read.csv(file="world-university-rankings/school_and_country_table.csv", header=TRUE, sep=",")
sh      <- read.csv(file="world-university-rankings/shanghaiData.csv", header=TRUE, sep=",")
times   <- read.csv(file="world-university-rankings/timesData.csv", header=TRUE, sep=",")

#table
cwurCount <- cwur %>% group_by(country) %>% summarise(count=n()) %>% arrange(count) %>% tail(23)

p1 <- ggplot(cwurCount, 
             aes(x=reorder(country, -count), y=count, fill=country)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(x="Count",y="Country") +
  ggtitle("Countries by Number of Ranked Universities")



#mapS
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


rm(list = ls())
#detach("package:plyr",unload = TRUE)
