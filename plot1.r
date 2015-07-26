get.sums <- function()
    {
        source("./functions.r")
        pm25 <- save.load.full.data()
        if (length(pm25) == 0)
            {
                get.data.files.ready()
                pm25 <- save.load.full.data()
            }
        sum.pm25.emi <- make.year.table(pm25)
        return(sum.pm25.emi)
    }

draw.plot.1 <- function(sum.pm25.emi,to.png=T) {
    if (to.png==T)
        png("plot1.png")
    plot(sum.pm25.emi$year,sum.pm25.emi$emissions,
         main="Sum of PM2.5 Emissions per Year",
         ylab="Sum of Emissions",
         xlab="Year",xaxt = "n", type="n")
    axis(1,at=sum.pm25.emi$year,labels=sum.pm25.emi$year)
    lines(sum.pm25.emi$year,sum.pm25.emi$emissions)
    if (to.png==T)
        dev.off()
}

sum.pm25.emi <- get.sums()
draw.plot.1(sum.pm25.emi)
