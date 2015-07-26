get.selected.data <- function(type)
    {
        if (type=="p")
            the.data <- save.load.full.data()
        else if (type=="s")
            the.data <- save.load.full.data(type)
        else
            return(NULL)
        
        return(the.data)
    }

get.dat <- function(type="p")
    {
        source("./functions.r")
        the.data <- get.selected.data(type)
        if (length(the.data) == 0)
            {
                get.data.files.ready()
                the.data <- get.selected.data(type)
            }

        return(the.data)
    }

get.carbon.scc <- function(scc)
    {
        carbon.dat <- data.frame()
        carbon.scc.dat <- data.frame()
        carbon.scc.dat$scc <- vector("character")

        ## everything is collected which has "Coal" or "coal" in its name.
        ## this category is too broad, later will need to be refined
        ## to make it "coal _combustion_ related"
        for(a.row in scc)
            {
                the.index <- grep("[Cc]oal",a.row)
                if (class(the.index) == "integer")
                    {
                        coal.expr <- grep("[Cc]oal",a.row,value=T)
                        if (length(the.index) != 0)
                            {
                                carbon.dat <- rbind(carbon.dat,scc[the.index,])
                                carbon.scc.dat <- rbind(carbon.scc.dat,data.frame(scc$SCC[the.index]))
                            }
                    }
            }

        names(carbon.scc.dat) = c("scc")
        carbon.scc.dat$scc = factor(carbon.scc.dat$scc)
        carbon.related.scc <- levels(carbon.scc.dat$scc)

        return(carbon.related.scc)
    }

get.sum.carbon <- function(pm25,carbon.related.scc)
    {
        library(dplyr)
        carbon.related.dat <- dplyr::filter(pm25,SCC %in% carbon.related.scc)
        
        carbon.by.yr <- group_by(carbon.related.dat,year)
        sum.carbon <- summarise(carbon.by.yr,sum(Emissions))
        names(sum.carbon) = c("year","emissions")
        return(sum.carbon)
    }

draw.plot.4 <- function(sum.carbon,to.png=T) {
    if (to.png==T)
        png("plot4.png")
    plot(sum.carbon$year,sum.carbon$emissions,
         main="Sum of Coal Related PM2.5 Emissions per Year",
         ylab="Sum of Emissions",
         xlab="Year",xaxt = "n", type="n")
    axis(1,at=sum.carbon$year,labels=sum.carbon$year)
    lines(sum.carbon$year,sum.carbon$emissions)
    if (to.png==T)
        dev.off()
}

pm25 <- get.dat()
scc <- get.dat("s")
carbon.scc <- get.carbon.scc(scc)
sum.carbon <- get.sum.carbon(pm25,carbon.scc)
draw.plot.4(sum.carbon)
