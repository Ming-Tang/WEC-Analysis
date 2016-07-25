Race <- Le_Mans.2016
LC <- lap_chart(Race)
DT <- DT_from_lap_chart(LC)
DT <- Race$fill_info(DT)
Finishes <- DT[,
  .(Car.Number, Pos=Pos[.N], Lap=X[.N],
    Desc=sprintf("%s - [%s] %s", Car.Number, Class, Team)),
  by=Car.Number]

print(
  qplot(X, Pos, data=subset(DT, !is.na(Car.Number)),
        group=Car.Number, col=Car.Number, geom="path") +
    xlab("Lap") + coord_cartesian(expand=FALSE) +
    scale_x_continuous(breaks=seq(0,400,5), minor_breaks=seq(0,400,1)) + 
    geom_text(aes(x=Lap, y=Pos, label=Desc), data=Finishes, inherit.aes=FALSE, hjust=0, nudge_x=1)
)