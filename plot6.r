source("./functions2.r")

draw.plot.6 <- function(sum.baltimore.mv,sum.la.mv,to.png=T) {
    if (to.png==T)
        png("plot6.png")
    par(mfrow=c(2,1))
    plot(sum.baltimore.mv$year,sum.baltimore.mv$emissions,
         main="Sum of Motor Vehicle Related PM2.5 Emissions per Year\nin Baltimore",
         ylab="Sum of Emissions",
         xlab="Year",xaxt = "n", type="n")
    axis(1,at=sum.baltimore.mv$year,labels=sum.baltimore.mv$year)
    lines(sum.baltimore.mv$year,sum.baltimore.mv$emissions)
    plot(sum.la.mv$year,sum.la.mv$emissions,
         main="Sum of Motor Vehicle Related PM2.5 Emissions per Year\nin Los Angeles County",
         ylab="Sum of Emissions",
         xlab="Year",xaxt = "n", type="n")
    axis(1,at=sum.la.mv$year,labels=sum.la.mv$year)
    lines(sum.la.mv$year,sum.la.mv$emissions,col="blue")
    if (to.png==T)
        dev.off()
}

pm25 <- get.pm25()
pm25.baltimore <- filter(pm25,fips == "24510")
pm25.la <- filter(pm25,fips == "06037")
rm(pm25)
mv.scc <- get.mv.scc()
sum.bal.mv <- get.sums(pm25.baltimore,mv.scc)
sum.la.mv <- get.sums(pm25.la,mv.scc)
draw.plot.6(sum.bal.mv,sum.la.mv)

