get.pm25.baltimore <- function()
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

get.mv.scc <- function()
    {
        scc <- save.load.full.data("s")
        motorvehicles <- grep("Mobile - On-Road",levels(scc$EI.Sector),value=T)
        motorvehicle.scc <- filter(scc,EI.Sector %in% motorvehicles)
        motorvehicle.scc$SCC = factor(motorvehicle.scc$SCC)
        mv.scc <- levels(motorvehicle.scc$SCC)

        return(mv.scc)
    }

get.sums <- function(pm25.baltimore,mv.scc)
    {
        library(dplyr)
        pm25.baltimore.mv <- filter(pm25.baltimore,SCC %in% mv.scc)
        # 1119 observations

        mv.baltimore.by.yr <- group_by(pm25.baltimore.mv,year)
        sum.baltimore.mv <- summarise(mv.baltimore.by.yr,sum(Emissions))
        names(sum.baltimore.mv) = c("year","emissions")

        return(sum.baltimore.mv)
    }

draw.plot.5 <- function(sum.baltimore.mv,to.png=T) {
    if (to.png==T)
        png("plot5.png")
    plot(sum.baltimore.mv$year,sum.baltimore.mv$emissions,
         main="Sum of Motor Vehicle Related PM2.5 Emissions per Year\nin Baltimore",
         ylab="Sum of Emissions",
         xlab="Year",xaxt = "n", type="n")
    axis(1,at=sum.baltimore.mv$year,labels=sum.baltimore.mv$year)
    lines(sum.baltimore.mv$year,sum.baltimore.mv$emissions)
    if (to.png==T)
        dev.off()
}

pm25.baltimore <- get.pm25.baltimore()
mv.scc <- get.mv.scc()
sum.bal.mv <- get.sums(pm25.baltimore,mv.scc)
draw.plot.5(sum.bal.mv)
