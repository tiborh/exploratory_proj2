unzip.files <- function(filePath,dat.file1,dat.file2,dat.dir=file.path("./data"))
{
    saved.wd <- getwd()
    if(!dir.exists(dat.dir))
        dir.create(dat.dir)
    setwd(dat.dir)
    if (!file.exists(dat.file1))
        unzip(filePath,dat.file1)
    if (!file.exists(dat.file2))
        unzip(filePath,dat.file2)
    setwd(saved.wd)
}

prepare.zfile <- function(fileUrl,dat.file1,dat.file2,dat.dir=file.path("./data"))
    {
        saved.wd <- getwd()
        if(!dir.exists(dat.dir))
            dir.create(dat.dir)
        setwd(dat.dir)
        temp <- tempfile()
        download.file(fileUrl,temp,method="curl")
        if (!file.exists(dat.file1))
            unzip(temp,dat.file1)
        if (!file.exists(dat.file2))
            unzip(temp,dat.file2)
        unlink(temp)
        setwd(saved.wd)
    }

ready.the.files <- function(dat.file1,dat.file2,dat.dir=file.path("./data"))
    {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        file.dest <- file.path("NEI_data.zip")

        if(!file.exists(file.path(paste0(dat.dir,"/",file.dest))))
            prepare.zfile(fileUrl,dat.file1,dat.file2)
        else
            unzip.files(file.dest,dat.file1,dat.file2)
    }

get.data <- function(dat.file,dat.dir=file.path("./data"))
    {
        readRDS(file.path(paste0(dat.dir,"/",dat.file)))
    }

save.load.full.data <- function(whichdata="p",loadsave="l",datavar=NULL)
    {
        dat.dir <- file.path("./data")
        pm25.fn = "pm25.dat"
        scc.fn = "scc.dat"
        pm25.saved <- file.path(paste0(dat.dir,"/",pm25.fn))
        scc.saved <- file.path(paste0(dat.dir,"/",scc.fn))
        if (loadsave=="l")
            {
                if (whichdata=="p")
                    {
                        if(file.exists(pm25.saved))
                            {
                                load(pm25.saved)
                                return(datavar)
                            }
                        else
                            {
                                print(paste("file does not exist:",pm25.saved))
                                return(NULL)
                            }
                    }
                else if (whichdata=="s")
                    {
                        if(file.exists(scc.saved))
                            {
                                load(scc.saved)
                                return(datavar)
                            }
                        else
                            {
                                print(paste("file does not exist:",scc.saved))
                                return(NULL)
                            }                        
                    }
                else
                    {
                        print(paste("Invalid load option:",whichdata))
                        return(NULL)
                    }
            }
        else if (loadsave=="s")
            {
                if (whichdata=="p")
                    {
                        save(datavar,file=pm25.saved)
                        return(pm25.saved);
                    }
                else if (whichdata=="s")
                    {
                        save(datavar,file=scc.saved)
                        return(scc.saved)
                    }
                else
                    {
                        print(paste("Invalid save option:",whichdata))
                        return(NULL)
                    }
            }
        else
            {
                print(paste("Invalid loadsave option:",loadsave))
                return(NULL)
            }
    }

clean.pm25 <- function(pm25)
    {
        ## clean and examine:
        pm25$fips = factor(pm25$fips)           # 3263
        pm25$SCC = factor(pm25$SCC)             # 5386
        pm25$Pollutant = factor(pm25$Pollutant) # 1
        pm25$type = factor(pm25$type)           # 4
        ## levels of type: "NONPOINT" "NON-ROAD" "ON-ROAD"  "POINT"
        pm25$fyr = factor(pm25$year)            # 4
        ## levels of fyr:  1999 2002 2005 2008
        ## sum(is.na(pm25$Emissions))              # 0

        return(pm25)
    }

get.data.files.ready <- function()
    {
        ## Getting the data:
        pm25.file <- file.path("summarySCC_PM25.rds")
        scc.file <- file.path("Source_Classification_Code.rds")
        dat.dir <- file.path("./data")

        if(!file.exists(paste0(dat.dir,"/",pm25.file)))
           ready.the.files(pm25.file,scc.file)
        pm25 <- get.data(pm25.file)
        pm25 <- clean.pm25(pm25)
        scc <- get.data(scc.file)

        save.load.full.data("p","s",pm25)
        save.load.full.data("s","s",scc)
    }

make.year.table <- function(indat)
    {
        library(dplyr)
        indat.by.yr <- group_by(indat,year)
        sum.dat <- summarise(indat.by.yr,sum(Emissions))
        names(sum.dat) = c("year","emissions")
        return(sum.dat)
    }
