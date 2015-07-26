by.year <- function(inFrame)
    {
        inFrame.by.year <- group_by(inFrame,year)
        sum.inFrame <- summarise(inFrame.by.year,sum(Emissions))
        names(sum.inFrame) = c("year","emissions")
        return(sum.inFrame)
    }

get.pm25 <- function()
    {
        source("./functions.r")
        pm25 <- save.load.full.data()
        if (length(pm25) == 0)
            {
                get.data.files.ready()
                pm25 <- save.load.full.data()
            }
        pm25.baltimore <- filter(pm25,fips == "24510")
        
        return(pm25.baltimore)
    }

get.clean.sums <- function()
    {
        library(dplyr)
        pm25 <- get.pm25()
        ## type levels: "NONPOINT" "NON-ROAD" "ON-ROAD"  "POINT"
        nonp <- filter(pm25,type=="NONPOINT")
        nonr <- filter(pm25,type=="NON-ROAD")
        onr <-  filter(pm25,type=="ON-ROAD")
        pnt <-  filter(pm25,type=="POINT")
        rm(pm25)

        ## get sums tables
        sum.nonp <- by.year(nonp)
        sum.nonr <- by.year(nonr)
        sum.onr <- by.year(onr)
        sum.pnt <- by.year(pnt)
        rm(list=c("nonp","nonr","onr","pnt"))

        ## create type sums table
        type.sums <- data.frame(sum.onr$year)

        ## fill up type sums table
        type.sums$nonp <- sum.nonp$emissions
        type.sums$pnt <- sum.pnt$emissions
        type.sums$nonr <- sum.nonr$emissions
        type.sums$onr <- sum.onr$emissions
        names(type.sums) = c("year","nonPoint","point","nonRoad","onRoad")

        ## tidy up the data
        library(tidyr)
        clean.sums <- type.sums %>% gather(types,sums,onRoad:nonPoint)
        clean.sums$types = factor(clean.sums$types)
        
        return(clean.sums)
    }

draw.plot.3 <- function(clean.sums,to.png=T) {
    library(ggplot2)
    if (to.png==T)
        png("plot3.png")
    g <- ggplot(clean.sums,aes(year,sums,colour=types,group=types))
    p <- g + geom_point() + geom_line()
    p <- p + labs(title="Sum of PM2.5 Emissions per Year by Type\nIn Baltimore",
                  y="Sum of Emissions",x="Year")
    print(p)
    if (to.png==T)
        dev.off()
}

draw.plot.3.basic <- function(clean.sums,to.png=T)
    {
        if (to.png==T)
            png("plot3basic.png")
        with(clean.sums,plot(year,sums,
                             main="Sum of PM2.5 Emissions per Year by Type\nIn Baltimore",
                             ylab="Sum of Emissions",
                             xlab="Year",xaxt = "n", type="n"))
        axis(1,at=clean.sums$year[1:4],labels=clean.sums$year[1:4])
        with(subset(clean.sums,types=="nonRoad"),lines(year,sums,col="blue"))
        with(subset(clean.sums,types=="onRoad"),lines(year,sums,col="red"))
        with(subset(clean.sums,types=="nonPoint"),lines(year,sums,col="green"))
        with(subset(clean.sums,types=="point"),lines(year,sums,col="black"))
        legend("topright",lwd=c(1,1,1,1),col=c("green","black","blue","red"),
               legend = c("NONPOINT", "POINT", "NON-ROAD", "ON-ROAD"))
        if (to.png==T)
            dev.off()
    }

clean.sums <- get.clean.sums()
draw.plot.3.basic(clean.sums)
draw.plot.3(clean.sums)
