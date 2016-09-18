Race <- Mexico.2016
P <- Analyze.Pitstops(Race)
Pit.Analysis <- P$Pit.Analysis
LC <- lap_chart_by_time(Race, seq(0, 6*3600, 20))
LC.DT <- DT_from_lap_chart(LC)
setnames(LC.DT, "X", "Time")
setkey(LC.DT, Car.Number, Time)
setkey(Pit.Analysis, Car.Number, Roll.Time)

Events.DT <- as.data.table(Race$Events)
setkey(Events.DT, Car.Number, Time)

#View(LC.DT)
last_pit <- function(car, t) P$Pit.Analysis[.(car, t), roll=TRUE]
in_pit <- function(car, t) { p <- last_pit(car, t); !is.na(p$Time) & p$Time <= t & t <= p$Time + p$Pit.Time }
LC.DT[,In.Pit := in_pit(Car.Number, Time), by=Car.Number]
qplot(Time,Pos,data=LC.DT, group=Car.Number, geom="line", size=In.Pit) +
  scale_size_discrete(range=c(0.1, 2))

LC.DT[,Hour:=Time/3600][,Focus:=Car.Number %in% c("1","2","5","6","7","8")]
qplot(Hour,Pos,data=LC.DT, group=Car.Number, geom="path",
      alpha=Focus, size=In.Pit, col=Car.Number, group=Car.Number,
      lineend="round", linejoin="round") +
  #geom_point(aes(Hour, Pos, col=Car.Number, group=Car.Number, alpha=Focus), 
  #           data=subset(LC.DT, In.Pit), size=1.5) +
  scale_size_discrete(c(0, 2)) + scale_alpha_discrete(c(0, 0.5)) +
  coord_cartesian(ylim=c(0, 65), xlim=c(0, 6), expand=FALSE) +
  scale_y_continuous(breaks=1:100) +
  scale_x_continuous(breaks=seq(0, 24, 1), minor=seq(0, 24, 1/6)) +
  scale_size_discrete(range=c(0.6, 2.2)) +
  scale_alpha_discrete(range=c(0.3, 1))