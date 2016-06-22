library(codetools)
library(plyr)
library(stringr)
library(ggplot2)
  
plot_styles <- new.env()
plot_styles$class_colors <- c("LMGTE Am"="Orange","LMGTE Pro"="Green",LMP2="Blue",LMP1="Red","CDNT"="gray28")
plot_styles$color_by_class <- scale_colour_manual(name="Class",values=plot_styles$class_colors)
plot_styles$fill_by_class <- scale_fill_manual(name="Class",values=plot_styles$class_colors)
plot_styles$scale_x_hour <- scale_x_continuous(breaks=0:24,minor_breaks=seq(0,24,1/6))

WEC.Analysis <- function(year, race, analyze_events=FALSE) {
  Prefix <- paste("./Data/", year, "_", race, sep='')
  
  Classification <- read.csv(paste(Prefix, "/Classification.csv", sep=""), sep=";")
  Analysis <- read.csv(paste(Prefix, "/Analysis.csv", sep=""), sep=";")
  Grid.Table <- read.csv(paste(Prefix, "/Grid.csv", sep=""), header=FALSE)
  
  ### Fill in Grid
  
  fill_grid <- function(Grid.Table) {
    # Grid.Table is the "Starting Grid" document parsed by Tabula using --area 200,240,810,370
    n_grid <- length(Grid.Table$V1)
    Grid.Left <- data.frame(Pos=integer(n_grid), Car.Number=integer(n_grid))
    Grid.Right <- data.frame(Pos=integer(n_grid), Car.Number=integer(n_grid))
    
    Grid.Left$Pos <- Grid.Table$V2
    Grid.Left$Car.Number <- Grid.Table$V1
    Grid.Right$Pos <- Grid.Table$V4
    Grid.Right$Car.Number <- Grid.Table$V5
    
    Grid <- rbind(Grid.Left, Grid.Right)
    Grid <- na.omit(Grid[order(Grid$Pos),])
    Grid$Car.Number <- as.factor(Grid$Car.Number)
    row.names(Grid) <- Grid$Pos
    Grid
  }
  
  Grid <- fill_grid(Grid.Table)
  
  ### Fill in Positions
  
  Positions <- merge(
    subset(Classification,select=c("NUMBER","POSITION")), Grid,
    by.x=c("NUMBER"),by.y=c("Car.Number"))
  names(Positions) <- c("Car.Number","Finishing","Grid")
  
  ### Fill in Analysis
  
  parse_timing <- function(timing) {
    if (str_count(timing, ":") == 2) {
      parts <- str_match(timing, "(\\d+)\\:(\\d+)\\:([\\d\\.]+)")
      as.numeric(parts[2]) * 3600 + as.numeric(parts[3]) * 60 + as.numeric(parts[4])
    } else if (str_count(timing, ":") == 1) {
      parts <- str_match(timing, "(\\d+)\\:([\\d\\.]+)")
      as.numeric(parts[2]) * 60 + as.numeric(parts[3])
    } else if (timing == "") {
      NA  
    } else {
      as.numeric(timing)
    }
  }
  
  apply_parse_timing <- function(col) mapply(function(f) parse_timing(as.character(f)), col)
  
  fill_analysis <- function(Analysis, Classification, Grid) {
    Analysis$NUMBER <- as.factor(Analysis$NUMBER)
    Analysis$DRIVER_NUMBER <- as.factor(Analysis$DRIVER_NUMBER)
    
    Analysis$Hour <- apply_parse_timing(Analysis$HOUR)
    
    Analysis$S1.Time <- apply_parse_timing(Analysis$S1)
    Analysis$S2.Time <- apply_parse_timing(Analysis$S2)
    Analysis$S3.Time <- apply_parse_timing(Analysis$S3)
    Analysis$Lap.Time <- apply_parse_timing(Analysis$LAP_TIME)
    Analysis$Pit.Time <- apply_parse_timing(Analysis$PIT_TIME)
    Analysis$Enter.Pit <- Analysis$CROSSING_FINISH_LINE_IN_PIT == "B"
    
    # Time since race start
    Analysis$Time <- apply_parse_timing(Analysis$ELAPSED)
    
    Car.Numbers <- sort(unique(Classification$NUMBER))
    Laps <- max(Analysis$LAP_NUMBER)
    Analysis
  }
  
  Analysis <- fill_analysis(Analysis=Analysis, Classification=Classification, Grid=Grid)
  
  ### Fill in Events
  
  fill_events <- function(Analysis, Classification, Grid) {
    Make.Events <- function(n) {
      data.frame(
        Car.Number=integer(n),
        Driver.Number=integer(n), 
        Time=numeric(n),
        Lap=integer(n),
        Lap.Time=numeric(n),
        Sector.Time=numeric(n), # sector time of last finished sector
        Sector=integer(n), # sector just finished at event: 0 (new lap), 1, 2
        Enter.Pit=logical(n), # lap finished with pit
        Pit.Time=numeric(n), # time spent in pit at start of lap
        Key=integer(n) # for sorting: 10*Lap + 3*Sector
      )
    }
    
    Sector.Weights <- c(mean(Analysis$S1.Time, na.rm=TRUE), mean(Analysis$S2.Time, na.rm=TRUE), mean(Analysis$S3.Time, na.rm=TRUE)) / mean(Analysis$Lap.Time, na.rm=TRUE)
    Sector.Keys <- 10*c(0, Sector.Weights[1], Sector.Weights[1]+Sector.Weights[2])
    
    NRow <- nrow(Analysis)
    Events.Lap <- Make.Events(NRow)
    Events.Lap$Car.Number <- Analysis$NUMBER
    Events.Lap$Driver.Number <- Analysis$DRIVER_NUMBER
    Events.Lap$Time <- Analysis$Time
    
    Events.Lap$Lap <- Analysis$LAP_NUMBER
    Events.Lap$Enter.Pit <- Analysis$Enter.Pit
    Events.Lap$Pit.Time <- Analysis$Pit.Time
  
    # Sector events
    Events.S1 <- Events.Lap
    Events.S1$Lap <- Events.S1$Lap - 1L
    Events.S1$Enter.Pit <- F
    Events.S1$Pit.Time <- NA
    Events.S1$Lap.Time <- NA
    Events.S2 <- Events.S1
    Events.S1$Sector <- 1L
    Events.S2$Sector <- 2L
    
    Events.Lap$Sector.Time <- apply_parse_timing(Analysis$S3)
    Events.Lap$Lap.Time <- Analysis$Lap.Time
    
    Events.S1$Sector.Time <- apply_parse_timing(Analysis$S1)
    Events.S1$Time <- Analysis$Time - Analysis$S2.Time - Analysis$S3.Time
    
    Events.S2$Sector.Time <- apply_parse_timing(Analysis$S2)
    Events.S2$Time <- Analysis$Time - Analysis$S3.Time
    
    compute_key <- function(Events) 10*Events$Lap + Sector.Keys[Events$Sector+1]
    Events.S1$Key <- compute_key(Events.S1)
    Events.S2$Key <- compute_key(Events.S2)
    Events.Lap$Key <- compute_key(Events.Lap)
    
    # Events for race start
    Analysis.L0 <- subset(Analysis, LAP_NUMBER==1)
    Events.L0 <- Make.Events(nrow(Analysis.L0))
    
    Events.L0$Lap <- 0L
    Events.L0$Car.Number <- Analysis.L0$NUMBER
    Events.L0$Driver.Number <- Analysis.L0$DRIVER_NUMBER
    Events.L0$Time <- round(Analysis.L0$Time - Analysis.L0$Lap.Time, digits=6) # they should all equal to zero
    Events.L0$Pit.Time <- NA
    Events.L0$Key <- 0
    Events.L0$Lap.Time <- NA
    Events.L0$Sector.Time <- NA
    
    # Events for race finish
    Events.Finish <- Make.Events(nrow(Classification))
    Events.Finish$Car.Number <- Classification$NUMBER
    Events.Finish$Lap.Time <- NA
    Events.Finish$Sector.Time <- NA
    Events.Finish$Time <- max(Events.Lap$Time)
    Events.Finish$Lap <- Classification$LAPS
    Events.Finish$Key <- compute_key(Events.Finish)
    Events.Finish$Pit.Time <- NA
    
    Events <- rbind(Events.Lap, Events.S1, Events.S2, Events.L0, Events.Finish)
    Events <- Events[order(Events$Time),]
    Events
  }
  
  Car.Info <- Classification[order(Classification$NUMBER),]
  row.names(Car.Info) <- Car.Info$NUMBER
  get_car <- function(n) { Car.Info[as.character(n),] }
  
  # Fill in info for Class, Vehicle and Team by Car.Number
  fill_info <- function(DF) {
    DF$Class <- mapply(DF$Car.Number, FUN=function(n) get_car(n)$CLASS)
    DF$Vehicle <- mapply(DF$Car.Number, FUN=function(n) get_car(n)$VEHICLE)
    DF$Team <- mapply(DF$Car.Number, FUN=function(n) get_car(n)$TEAM)
    DF$Description <- mapply(DF$Car.Number, FUN=function(n) { c <- get_car(n); sprintf("%d %s: %s [%s]", n, c$TEAM, c$VEHICLE, c$CLASS) })
    DF
  }
  
  Events <- NA
  if (analyze_events) {
    Events <- fill_events(Analysis=Analysis, Classification=Classification, Grid=Grid)
    Events <- fill_info(Events)
  }
  
  Positions <- fill_info(Positions)
    
  make_plots <- function() {
    
    lap_plt <- NA
    if (analyze_events) {
      lap_plt <- qplot(Time/3600, Key/10, data=Events, geom="line", group=Car.Number, colour=Class)
      lap_plt <- lap_plt + xlab("Hour") + ylab("Lap")
      lap_plt <- lap_plt + plot_styles$scale_x_hour + scale_y_continuous(breaks=seq(0,400,50),minor_breaks=seq(0,400,5))
      lap_plt <- lap_plt + plot_styles$color_by_class + geom_line(size=0)
    }
    
    finishing_plt <- qplot(Grid, Finishing, data=Positions, colour=Class, label=Car.Number) + xlab("Grid Position") + ylab("Finishing Position")
    finishing_plt <- finishing_plt + geom_label(aes(fill=Class), data=Positions, colour="white", fontface="bold", label.size=0)
    finishing_plt <- finishing_plt + plot_styles$color_by_class + plot_styles$fill_by_class
    finishing_plt <- finishing_plt + scale_x_continuous(breaks=1:100, minor_breaks=1:100) + scale_y_continuous(breaks=1:100, minor_breaks=1:100)
    
    #plot(subset(Analysis,is.na(Pit.Time & Lap.Time < 600), select=c("Lap.Time","S1.Time","S2.Time","S3.Time")))
  
    #hist(subset(Analysis,is.na(Pit.Time) & Lap.Time < 600)$Lap.Time, breaks=seq(100,600,5))
    
    e <- new.env()
    e$laps <- lap_plt
    e$finishing <- finishing_plt
    e
  }
  
  e <- new.env()
  e$Analysis <- Analysis
  e$Classification <- Classification
  e$Grid <- Grid 
  e$Positions <- Positions
  e$Events <- Events 
  e$Car.Info <- Car.Info
  e$make_plots <- make_plots
  e$fill_info <- fill_info
  
  e
}