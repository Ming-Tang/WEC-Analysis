library(ggplot2)
library(data.table)
#Le_Mans.2016 <- WEC.Analysis(2016, "Le_Mans", TRUE)
#Spa.2016 <- WEC.Analysis(2016, "Spa", TRUE)
#Silverstone.2016 <- WEC.Analysis(2016, "Silverstone", TRUE)

gap_evolution_graph <- function(Race, car_number_subj, car_number_obj, ylim=NULL) {
  
Drivers <- Race$Drivers
Events <- Race$Events
#car_number_subj <- 2
#car_number_obj <- 5

car_subj <- data.table(subset(Events, Car.Number==car_number_subj))[,.(Lap,Sector,Key,Time,Enter.Pit,Pit.Time,Driver.Name,Driver.Number)][1:.N-1]
car_obj <- data.table(subset(Events, Car.Number==car_number_obj))[,.(Lap,Sector,Key,Time,Enter.Pit,Pit.Time,Driver.Name,Driver.Number)][1:.N-1]
drivers_subj <- Drivers[[as.character(car_number_subj)]]
drivers_obj <- Drivers[[as.character(car_number_obj)]]

car_subj[,Driver.Change := c(0, diff(as.integer(Driver.Number))) != 0]
car_obj[,Driver.Change := c(0, diff(as.integer(Driver.Number))) != 0]

Joined <- merge(car_obj,car_subj, by=c("Lap","Sector"), all=TRUE)
Joined[,Gap := Time.y - Time.x]
Joined[,DGap := c(0, diff(Gap))]
Joined[,Cross := sign(Gap + c(diff(Gap), 0)) != sign(Gap)]

geom_pitstop <- function(data, ...) {
  geom_segment(aes(x=Lap, xend=Lap, y=0, yend=Pit.Time), ..., show.legend=FALSE, data=subset(data, !is.na(Pit.Time)))
}

geom_drivers <- function(data, ...) {
  data_subset <- subset(data, Driver.Change|(Lap==0&Sector==0))
  geom_text(aes(label=Driver.Name, x=Lap), data=data_subset, ...)
}

plt <- ggplot(Joined, aes(x=Key.x/10, y=Gap)) +
  labs(title=sprintf("Gap Evolution between Car %d and Car %d", car_number_subj, car_number_obj)) + xlab("Lap") + ylab("Gap (s)") +
  coord_cartesian(xlim=c(0,max(Joined$Lap)), ylim=ylim) +
  scale_x_continuous(breaks=seq(0, max(Joined$Lap), 10), minor_breaks=1:max(Joined$Lap)) +
  scale_y_continuous(breaks=seq(-400, 400, 10), minor_breaks=seq(-30,30,1))

plt <- plt +
  geom_line(aes(x=Key.x/10, y=DGap), colour="green", alpha=0.4, show.legend=FALSE) +
  geom_hline(yintercept=0, size=0.5) +
  geom_pitstop(car_subj, col="orange", alpha=0.7, size=1.5) +
  geom_pitstop(car_obj, col="blue", alpha=0.5, size=1) +
  geom_line(size=1) + geom_point(aes(x=Key.x/10, y=0), size=1.8, col="red", data=subset(Joined,Cross))

plt <- plt +
  geom_drivers(car_subj, y=100, col="orange", hjust=0, vjust=1, fontface="bold", angle=45) +
  geom_drivers(car_obj, y=-100, col="blue", hjust=0, vjust=0, fontface="bold", angle=-45)

plt

}