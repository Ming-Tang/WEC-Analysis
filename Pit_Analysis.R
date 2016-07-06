Race <- Le_Mans.2016
Pit.Analysis <- as.data.table(Race$Analysis)[,.(NUMBER, DRIVER_NUMBER, TEAM, CLASS, DRIVER_NAME, LAP_NUMBER, Lap.Time, Time, Pit.Time)]
setnames(Pit.Analysis,
         c("NUMBER", "DRIVER_NUMBER", "TEAM", "CLASS", "DRIVER_NAME", "LAP_NUMBER"),
         c("Car.Number", "Driver.Number", "Team", "Class", "Driver.Name", "Lap"))
Pit.Analysis[, `:=`(Pit = !is.na(Pit.Time) & Pit.Time > 1e-2, Time = Time - Lap.Time)]
Pit.Analysis <- Pit.Analysis[(Pit)]
setkey(Pit.Analysis, Car.Number, Time)
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
  Stint=max(Stint), Changes=max(Changes), Total.Pit.Time=max(Total.Pit.Time)
), keyby=Car.Number]
Pit.Summary[, `:=`(
  Laps.Stint=Laps/Stint,
  Laps.Driver=Laps/Changes,
  Pit.Time.Lap = Total.Pit.Time/Laps
)]
View(Pit.Analysis)
View(Pit.Summary)

ggplot(Pit.Analysis, aes(x=Time/3600,y=Car.Number,col=Class,label=Lap)) +
  geom_errorbarh(aes(height=ifelse(Driver.Change,0.7,0.2),xmin=Time/3600,xmax=(Time+Pit.Time)/3600, yend=Car.Number)) +
  plot_styles$color_by_class + xlab("Hour") + scale_x_continuous(breaks=0:24, minor_breaks=seq(0, 24, 1/6))