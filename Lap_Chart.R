library(data.table)
library(profvis)

lap_chart <- function(Race, per_sector=FALSE) {
  Grid <- Race$Grid
  Events <- as.data.table(Race$Events)
  setkey(Events, Time)
  setkey(Events, Lap, Sector)
  
  sectors <- if(per_sector) c(0L,1L,2L) else c(0L)
  
  n_sectors <- length(sectors)
  n_laps <- max(Events$Lap)
  n_cars <- length(levels(Events$Car.Number))
  Lap.Chart <- matrix(NA_character_, nrow=n_sectors * (n_laps+1), ncol=n_cars)
  rnames <- sapply(0L:n_laps, function(i) paste(i, sectors, sep="."))
  rnames[1] <- "Grid"
  rownames(Lap.Chart) <- rnames
  colnames(Lap.Chart) <- 1L:n_cars
  
  get_index <- function(lap, sector) lap * n_sectors + sector + 1
  get_lap_from_index <- function(index) (index - 1) %/% n_sectors
  get_sector_from_index <- function(index) (index - 1) %% n_sectors
  get_fractional_lap <- function(index) (index - 1) / n_sectors
  
  for (lap in 0L:n_laps) {
    for (sector in sectors) {
      car_numbers_0 <- Events[.(lap, sector), as.character(Car.Number)]
      car_numbers <- unique(car_numbers_0, fromLast=TRUE)
      #stopifnot(length(unique(car_numbers)) == length(car_numbers))
      if (FALSE && length(car_numbers) != length(car_numbers_0)) {
        print(sprintf("lap=%d sector=%d", lap, sector))
        print("*** car_numbers_0: ")
        print(car_numbers_0)
        print("*** unique(car_numbers_0, fromLast=TRUE): ")
        print(car_numbers)
        print("*** times:")
        print(Events[Lap==lap & Sector==sector, Time])
        print("")
      }
      if (FALSE) {
        times <- Events[.(lap, sector), Time]
        stopifnot(all.equal(sort(times), times))
        stopifnot(n_cars - length(car_numbers) >= 0)
      }
      Lap.Chart[get_index(lap, sector),] <- c(car_numbers, rep(NA, n_cars - length(car_numbers)))
    }
  }
  Lap.Chart[1,] <- as.character(Grid$Car.Number)
  
  list(Events=Events, Lap.Chart=Lap.Chart, xlab="Lap", xfunc=get_fractional_lap, n_sectors=n_sectors, scales=list(
    scale_x_continuous(breaks=seq(0,500,1), minor_breaks=seq(0, 500, 1/n_sectors)),
    scale_y_continuous(breaks=1:100, minor_breaks=1:100),
    xlab("Lap")
  ))
}

lap_chart_by_time <- function(Race, times) {
  times <- unique(sort(times))
  Grid <- as.data.table(Race$Grid)[,Car.Number := as.character(Car.Number)]
  setkey(Grid, Car.Number)
  Events.Time <- as.data.table(Race$Events)[,.(Car.Number, Time, Lap, Sector)]
  Events.Time[,Roll.Time := Time][
    # Used to tie-break (lap, sector) combinations. Earlier time for same (lap, sector) have higher value, or at race start, grid position determines value
    ,New.Key := ifelse(Lap + Sector == 0, -Grid[.(as.character(Car.Number))]$Pos, Lap*10 + Sector + 1 - Time / max(Time))
  ]
  setkey(Events.Time, Time)
  setkey(Events.Time, Car.Number, Roll.Time)
  setkey(Grid, Pos)
  
  #sectors <- c(0L)
  sectors <- c(0L,1L,2L)
  
  cars <- as.character(levels(Events.Time$Car.Number))
  n_sectors <- length(sectors)
  n_laps <- max(Race$Events$Lap)
  n_cars <- length(cars)
  
  standings_at <- function(time) {
    Events.Time[.(cars, Roll.Time=time), .SD, roll=+Inf][order(-New.Key)]
  }
  
  Lap.Chart <- matrix(NA_character_, nrow=length(times) + 2, ncol=n_cars)
  rownames(Lap.Chart) <- c("Grid", sapply(times, function(t) {
    sprintf("%02d:%02d", floor(t %/% 3600), floor((t %% 3600) / 60))
  }), "Finish")
  colnames(Lap.Chart) <- 1:n_cars
  
  Lap.Chart[1,] <- as.character(Grid$Car.Number)
  i <- 2
  for (time in c(times, Inf)) {
    car_numbers_0 <- as.character(standings_at(time)$Car.Number)
    car_numbers <- car_numbers_0
    car_numbers <- unique(car_numbers_0) #, fromLast=TRUE)
    #print(dim(car_numbers)[1])
    #stopifnot(length(unique(car_numbers_0)) == length(car_numbers_0))
    stopifnot(length(car_numbers) == n_cars)
    Lap.Chart[i,] <- c(car_numbers, rep(NA, n_cars - length(car_numbers)))
    i <- i + 1
  }
  
  list(Events=Events.Time, Lap.Chart=Lap.Chart, xlab="Time", xfunc=function(i) {
    ifelse(i == 1, 0.0, ifelse(i - 1 >= length(times), times[length(times)], times[i]))
  }, n_sectors=n_sectors, scales=list(xlab("Time"), scale_x_continuous(minor_breaks=times)))
}

DT_from_lap_chart <- function(LC) {
  Lap.Chart <- LC$Lap.Chart
  xfunc <- LC$xfunc
  n <- length(Lap.Chart)
  DT <- data.table(Index=integer(n), Pos=integer(n), Car.Number=character(n), Label=character(n), X=numeric(n))
  i <- 1L
  xs <- 1L:dim(Lap.Chart)[1L]
  ys <- 1L:dim(Lap.Chart)[2L]
  rn <- rownames(Lap.Chart)
  for (x in xs) {
    for (y in ys) {
      if (is.na(Lap.Chart[x, y])) next
      set(DT, i, 1L, x)
      set(DT, i, 2L, y)
      set(DT, i, 3L, Lap.Chart[x, y])
      set(DT, i, 4L, rn[x])
      set(DT, i, 5L, xfunc(x))
      i <- i + 1L
    }
  }
  DT
}

plot_lap_chart <- function(LC) {
  DT <- DT_from_lap_chart(LC)
  n_sectors <- DT$n_sectors
  qplot(X, Pos, data=subset(DT, !is.na(Car.Number)),
        group=Car.Number, col=Car.Number, geom="line") + LC$scales
}
