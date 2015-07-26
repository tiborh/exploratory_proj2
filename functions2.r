get.pm25 <- function()
    {
        source("./functions.r")
        pm25 <- save.load.full.data()
        if (length(pm25) == 0)
            {
                get.data.files.ready()
                pm25 <- save.load.full.data()
            }
        return(pm25)
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

get.sums <- function(pm25.city,mv.scc)
    {
        library(dplyr)
        pm25.city.mv <- filter(pm25.city,SCC %in% mv.scc)

        mv.city.by.yr <- group_by(pm25.city.mv,year)
        sum.city.mv <- summarise(mv.city.by.yr,sum(Emissions))
        names(sum.city.mv) = c("year","emissions")

        return(sum.city.mv)
    }
