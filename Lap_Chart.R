library(data.table)
library(profvis)

#profvis({

Race <- Le_Mans.2016
Grid <- Race$Grid
Events <- as.data.table(Race$Events)
setkey(Events, Lap, Time)

#sectors <- c(0L)
sectors <- c(0L,1L,2L)

n_sectors <- length(sectors)
n_laps <- max(Events$Lap)
n_cars <- length(levels(Events$Car.Number))
Lap.Chart <- matrix(NA, nrow=1L + n_sectors * (n_laps+1), ncol=n_cars)
rownames(Lap.Chart) <- c("Grid", sapply(1L:(n_laps+1), function(i) paste(i, sectors, sep=".")))
colnames(Lap.Chart) <- 1L:n_cars

get_index <- function(lap, sector) 2 + (lap - 1) * n_sectors + sector
get_lap_from_index <- function(index) 1 + (index - 2) %/% n_sectors
get_sector_from_index <- function(index) if (index == 1) 1 else index - get_index(get_lap_from_index(index), 0)
get_fractional_lap <- function(index) pmax(0, (index - 2) / n_sectors + 1)

Lap.Chart[1,] <- as.character(Grid$Car.Number)

for (lap in 1L:n_laps) {
  for (sector in sectors) {
    car_numbers_0 <- Events[J(lap)][Sector==sector, as.character(Car.Number)]
    car_numbers <- unique(car_numbers_0, fromLast=TRUE)
    #stopifnot(length(unique(car_numbers)) == length(car_numbers))
    if (length(car_numbers) != length(car_numbers_0)) {
      print(sprintf("lap=%d sector=%d", lap, sector))
      print("*** car_numbers_0: ")
      print(car_numbers_0)
      print("*** unique(car_numbers_0, fromLast=TRUE): ")
      print(car_numbers)
      print("*** times:")
      print(Events[Lap==lap & Sector==sector, Time])
      print("")
    }
    #times <- Events[Lap==lap & Sector==sector, Time]
    #stopifnot(all.equal(sort(times), times))
    #stopifnot(n_cars - length(car_numbers) >= 0)
    Lap.Chart[get_index(lap, sector),] <- c(car_numbers, rep(NA, n_cars - length(car_numbers)))
  }
}
View(t(Lap.Chart))

#})

DT <- data.table(Index=integer(n), Pos=integer(n), Car.Number=character(n))
i <- 1L
xs <- 1L:dim(Lap.Chart)[1L]
ys <- 1L:dim(Lap.Chart)[2L]
for (x in xs) {
  for (y in ys) {
    set(DT, i, 1L, x)
    set(DT, i, 2L, y)
    set(DT, i, 3L, as.character(Lap.Chart[x, y]))
    i <- i + 1L
 }
}

qplot(get_fractional_lap(Index), Pos, data=subset(DT, !is.na(Car.Number)), group=Car.Number, colour=Car.Number, geom="line") +
  scale_x_continuous(breaks=seq(0,500,1), minor_breaks = seq(0, 500, 1/n_sectors)) +
  scale_y_continuous(breaks=1:100, minor_breaks = 1:100) + xlab("Lap")
