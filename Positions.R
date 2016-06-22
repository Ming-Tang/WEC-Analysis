library(data.table)
Race.Analysis <- Spa.2016
Grid <- Race.Analysis$Grid
Events <- Race.Analysis$Events
Classification <- Race.Analysis$Classification

profvis({
n_grid <- length(Grid$Car.Number)
Positions <- data.table(Car.Number=Grid$Car.Number, Lap=integer(n_grid), Sector=integer(n_grid), Time=numeric(n_grid))
rownames(Positions) <- rownames(Grid)

n_est = dim(Events)[1]*2L
Overtakes <- data.table(Car.Number=integer(n_est), Position=integer(n_est), 
                        Lap=integer(n_est), Sector=integer(n_est), Time=numeric(n_est))
overtake_count <- 1L

add_overtake <- function(Car.Number, Position, Lap, Sector, Time) {
  set(Overtakes, overtake_count, 1L, Car.Number)
  set(Overtakes, overtake_count, 2L, Position)
  set(Overtakes, overtake_count, 3L, Lap)
  set(Overtakes, overtake_count, 4L, Sector)
  set(Overtakes, overtake_count, 5L, Time)
  overtake_count <<- overtake_count + 1L
}

# TODO convert Positions to separate vectors

for (i in 1L:dim(Grid)[1]) {
  pos <- Grid[i,];
  add_overtake(Car.Number=pos$Car.Number, Position=i, Lap=0L, Sector=0L, Time=0)
}

for (idx in 1L:dim(Events)[1]) {
  evt <- Events[idx,]
  if (is.na(evt$Time)) next
  if (evt$Time < 1e-6) next # Race start event
  #if (evt$Time >= 86672.78) break
  #if (idx > 2500L) break
  
  # Update and write back position
  pos_idx <- Positions[, .I[Car.Number==evt$Car.Number]]
  # set(Positions, idx_pos, 1L, evt$Car.Number)
  set(Positions, pos_idx, 2L, evt$Lap)
  set(Positions, pos_idx, 3L, evt$Sector)
  set(Positions, pos_idx, 4L, evt$Time)
  pos <- Positions[pos_idx]
  #print(sprintf("%5d %f: Car %2s (P%2d) crosses Lap %d Sector %d", idx, evt$Time,
  #              as.character(evt$Car.Number), pos_idx, evt$Lap, evt$Sector))
  
  
  key <- 10*pos$Lap + pos$Sector
  gained <- 0L
  
  # Determine position switches
  if (pos_idx-1 > 0L) {
    for (pos_idx1 in seq(pos_idx-1L, 0L, -1L)) {
      if (pos_idx1 > 0L) {
        pos1 <- Positions[pos_idx1]
        key1 <- 10*pos1$Lap + pos1$Sector
        
        if (key > key1 | (key == key1 & pos$Time <= pos1$Time)) {
          print(sprintf("%5d %f:  -> Car %2s is overtaken by Car %2s : P%2d -> P%2d", idx, evt$Time,
                        as.character(pos1$Car.Number), as.character(pos$Car.Number), pos_idx1, pos_idx1+1))
          # position drop
          add_overtake(Car.Number=pos1$Car.Number, Position=pos_idx+1L, Lap=pos1$Lap, Sector=pos1$Sector, Time=evt$Time)
          gained <- gained + 1L
          
          # Perform swap
          pos.1 <- Positions[pos_idx1+1L,] 
          pos.2 <- Positions[pos_idx1,]
          #Positions[pos_idx1,] <- pos.1
          #Positions[pos_idx1+1L,] <- pos.2
          
          set(Positions, pos_idx1, 1L, pos.1$Car.Number)
          set(Positions, pos_idx1, 2L, pos.1$Lap)
          set(Positions, pos_idx1, 3L, pos.1$Sector)
          set(Positions, pos_idx1, 4L, pos.1$Time)
          
          set(Positions, pos_idx1+1L, 1L, pos.2$Car.Number)
          set(Positions, pos_idx1+1L, 2L, pos.2$Lap)
          set(Positions, pos_idx1+1L, 3L, pos.2$Sector)
          set(Positions, pos_idx1+1L, 4L, pos.2$Time)
          
          next
        }
      }
      if (gained > 0L) {
        print(sprintf("%5d %f: Car %2s : P%2d -> P%2d (gained %d)", idx, evt$Time,
                      as.character(pos$Car.Number), pos_idx, pos_idx1+1L, gained))
        # position gain
        add_overtake(Car.Number=pos$Car.Number, Position=pos_idx1+1L, Lap=pos$Lap, Sector=pos$Sector, Time=evt$Time)
        break
      }
    }
  }
  if (gained == 0L) {
    add_overtake(Car.Number=pos$Car.Number, Position=pos_idx, Lap=pos$Lap, Sector=pos$Sector, Time=pos$Time)
  }
}

Overtakes <- head(Overtakes, overtake_count)
#Overtakes$Car.Number <- factor(Overtakes$Car.Number, levels=1:length(levels(Grid$Car.Number)), labels=levels(Grid$Car.Number))

#for (i in 1:dim(Classification)[1]) {
#  pos <- Classification[i,];
#  Overtakes[overtake_count,] <- c(
#    Car.Number=as.character(pos$NUMBER), Position=pos$POSITION, Lap=pos$LAPS, Sector=0, Time=Inf)
#  overtake_count <- overtake_count + 1
#}

Overtakes <- Race.Analysis$fill_info(Overtakes)
})

View(Positions)
View(Overtakes)
qplot(Time/3600, Position, group=Car.Number, data=Overtakes,
      colour=Class, geom="path") + xlab("Hour") + plot_styles$scale_x_hour + plot_styles$color_by_class
