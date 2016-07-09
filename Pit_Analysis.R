library(ggplot2)
library(data.table)

Analyze.Pitstops <- function(Race) {
  Pit.Analysis <- as.data.table(Race$Analysis)
  Pit.Analysis <- merge(as.data.table(Race$Classification)[, NUMBER := as.factor(NUMBER)], Pit.Analysis, by="NUMBER", suffixes=c(".C", ""))
  
  setnames(Pit.Analysis,
           c("NUMBER", "DRIVER_NUMBER", "TEAM", "VEHICLE", "TYRES", "CLASS", "GROUP", "DRIVER_NAME", "LAP_NUMBER"),
           c("Car.Number", "Driver.Number", "Team", "Vehicle", "Tyres", "Class", "Group", "Driver.Name", "Lap"))
  
  Pit.Analysis <- Pit.Analysis[,.(Car.Number, Driver.Number, Team, Vehicle, Tyres, Class, Group, Lap, Lap.Time, Time, Pit.Time)]
  Pit.Analysis[, `:=`(Pit = !is.na(Pit.Time) & Pit.Time > 1e-2, Time = Time - Lap.Time)][,Roll.Time := Time]
  Pit.Analysis <- Pit.Analysis[(Pit)]
  
  setkey(Pit.Analysis, Car.Number, Roll.Time)
  
  Pit.Analysis[, Driver.Number := as.integer(Driver.Number)]
  Pit.Analysis[, Driver.Change := c(FALSE, diff(Driver.Number) != 0), by=Car.Number]
  Pit.Analysis[, `:=`(
    Stint = cumsum(Pit),
    Stint.Length = c(0, diff(Lap)),
    Changes = 1+cumsum(Driver.Change),
    Total.Pit.Time = cumsum(ifelse(Pit, Pit.Time, 0))
  ), by=Car.Number]
  Pit.Analysis[,`:=`(Lap.Time = NULL, Pit = NULL)]
  
  Pit.Summary <- Pit.Analysis[,.(
    Team=Team[1], Class=Class[1], Laps=max(Lap), 
    Stints=max(Stint), Changes=max(Changes), Total.Pit.Time=max(Total.Pit.Time)
  ), keyby=Car.Number]
  
  Total.Times <- as.data.table(Race$Classification)[
    ,.(Car.Number=as.factor(NUMBER),
       Fastest.Lap = Rcpp_apply_parse_timing(gsub("'", ":", Race$Classification$FL_TIME)),
       Total.Time = Rcpp_apply_parse_timing(gsub("'", ":", Race$Classification$TOTAL_TIME)),
       Classified=STATUS == "Classified", Position=POSITION)
  ]
  Pit.Summary <- Pit.Summary[Total.Times][, Race.Time := Total.Time - Total.Pit.Time]
  
  Pit.Summary[, `:=`(
    Laps.Stint = Laps/Stints,
    Laps.Driver = Laps/Changes,
    Pit.Time.Lap = Total.Pit.Time/Laps,
    Pit.Time.Ratio = Total.Pit.Time/Race.Time,
    Avg.Lap = Race.Time/Laps
  )]
  #View(Pit.Analysis)
  #View(Pit.Summary)
  list(Pit.Analysis=Pit.Analysis, Pit.Summary=Pit.Summary)
}
