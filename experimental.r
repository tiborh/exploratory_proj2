source("./functions.r")

NEI <- save.load.full.data()

## 1
years <- c(1999,2002,2005,2008)
plot(years,tapply(NEI$Emissions,NEI$year,sum)/1e6,
     ylab="Total PM25 Emissions, millions of tons",type="b",xlab="Year",xaxt="n")
axis(1,at=years)

## 1b
library(dplyr)
neibyyear<- group_by(NEI, year) %>% summarise(totem = sum(Emissions))

for(i in 1:length(neibyyear)){ 
    plot(neibyyear$year, neibyyear$totem, type="n", main='Total Emissions US by Year') 
    lines(neibyyear$year, neibyyear$totem) 
}

## 1c
totalEmission <- tapply(NEI$Emissions,NEI$year,sum)
barplot(totalEmission,xlab="year",ylab="PM2.5 emitted (tons)",
        main="Total PM2.5 Emission in the US\n(1999-2008)")

## 2
plot(years,with(subset(NEI,fips == 24510),tapply(Emissions,year,sum)),
  ylab="Total PM25 Emissions, tons",type="b",xlab="Year",xaxt="n",main="Total Emissions in Baltimore City")
axis(1,at=years)

## 2b
neibaltbytype<- subset(NEI, fips == 24510) %>% group_by(type, year) %>% summarise(TotalEmissions = sum(Emissions))

ggplot(data=neibaltbytype, aes(x=year,y=TotalEmissions,group=type, colour=type)) + 
    geom_line() + 
    geom_point( size=4, shape=21, fill="white")

## 2c

totalEmission <- NEI %>% 
        filter(fips == "24510") %>%
        group_by(year) %>% 
        summarise(Emissions=sum(Emissions))
with(totalEmission,{
        barplot(Emissions,names.arg=year,xlab="year",ylab="PM2.5 emitted (tons)",
                main="Total PM2.5 Emission in Baltimore City, Maryland\n(1999-2008)")
})


## 3
library(ggplot2)
library(plyr)

typeyear <- ddply(subset(NEI,fips == 24510), c("type","year"), summarize, emissions=sum(Emissions))
typeyear$type <- factor(typeyear$type,c("NONPOINT","POINT","NON-ROAD","ON-ROAD"))

qplot(year,emissions,data=typeyear,facets=.~type, geom="line") +
  scale_x_continuous(breaks=c(years)) +
  scale_y_continuous(name="Total PM25 emissions, tons") +
  theme_bw()

qplot(year,emissions,data=typeyear,colour=type, geom="line") +
  scale_x_continuous(breaks=c(years)) +
  scale_y_continuous(name="Total PM25 emissions, tons") +
  theme_bw()

## 3a

Baltimore <- subset(NEI, fips == "24510")

PM25_ByYear_type <- ddply(Baltimore, .(year, type), function(x) sum(x$Emissions))
colnames(PM25_ByYear_type)[3] <- "Emissions"

qplot(year, Emissions, data=PM25_ByYear_type, color=type, geom="line") +
  ggtitle(expression( PM[2.5] ~ "Emissions by Source Type and Year" ~ " in Baltimore City")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))


## 4
SCC <- save.load.full.data("s")

coalcodes <- SCC[grep("Coal",SCC$SCC.Level.Three),'SCC'] # identify coal related SCC numbers
d <- ddply(subset(NEI,NEI$SCC %in% coalcodes),c("year"), summarize, emissions=sum(Emissions))

qplot(year,emissions,data=d,geom="line") +
  scale_x_continuous(breaks=c(years)) +
  scale_y_continuous(name="Total PM25 emissions, tons") +
  theme_bw()

## 4a

Coal_Comb_SCC <- subset(SCC, EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal",
                                              "Fuel Comb - Electric Generation - Coal",
                                              "Fuel Comb - Industrial Boilers, ICEs - Coal"))

Coal_Comb_SCC1 <- subset(SCC, grepl("Comb", Short.Name) & grepl("Coal", Short.Name))

Coal_Comb_SCC_join <- union(Coal_Comb_SCC$SCC, Coal_Comb_SCC1$SCC)

Coal_Comb <- subset(NEI, SCC %in% Coal_Comb_SCC_join)

coal_Comb_PM25_ByYear <- ddply(Coal_Comb, .(year, type), function(x) sum(x$Emissions))
colnames(coal_Comb_PM25_ByYear)[3] <- "Emissions"

qplot(year, Emissions, data=coal_Comb_PM25_ByYear, color=type, geom="line") +
stat_summary(fun.y = "sum", fun.ymin = "sum", fun.ymax = "sum", 
            color = "black", aes(shape="total"), geom="line") +
geom_line(aes(size="total", shape = NA)) +
ggtitle(expression("Coal Combustion" ~ PM[2.5] ~ "Emissions by Source Type and Year")) +
xlab("Year") + ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))


## 5

vehcodes <- SCC[grep("Vehicle",SCC$SCC.Level.Three),'SCC'] # identify motor vehicle related SCC numbers
d <- ddply(subset(NEI,fips == 24510 & NEI$SCC %in% vehcodes),c("year"), summarize, emissions=sum(Emissions))

qplot(year,emissions,data=d,geom="line") +
  scale_x_continuous(breaks=c(years)) +
  scale_y_continuous(name="Total PM25 emissions, tons") +
  theme_bw()

## 5a

Baltimore <- subset(NEI, fips == "24510" & type=="ON-ROAD")

Baltimore_PM25_ByYear <- ddply(Baltimore, .(year), function(x) sum(x$Emissions))
colnames(Baltimore_PM25_ByYear)[2] <- "Emissions"

qplot(year, Emissions, data=Baltimore_PM25_ByYear, geom="line") +
  ggtitle(expression(PM[2.5] ~ "Motor Vehicle Emissions by Year in Baltimore")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))


## 6
vehcodes <- SCC[grep("Vehicle",SCC$SCC.Level.Three),'SCC'] # identify motor vehicle related SCC numbers
d <- ddply(subset(NEI,(fips == "24510" | fips == "06037") & NEI$SCC %in% vehcodes),c("year","fips"), summarize, emissions=sum(Emissions))
d$fips = factor(d$fips,labels=c("Los Angeles County, CA","Baltimore City, MD"))

qplot(year,emissions,data=d,facets=.~fips,geom="line") +
  scale_x_continuous(breaks=c(years)) +
  scale_y_continuous(name="Total PM25 emissions, tons") +
  theme_bw()

