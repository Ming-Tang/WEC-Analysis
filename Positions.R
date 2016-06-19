library(data.table)
Grid <- Spa.2016$Grid
Events <- Spa.2016$Events
Classification <- Spa.2016$Classification

n_grid <- length(Grid$Car.Number)
Positions <- data.frame(Car.Number=Grid$Car.Number, Lap=integer(n_grid), Sector=integer(n_grid), Time=numeric(n_grid))
rownames(Positions) <- rownames(Grid)

n_est = dim(Events)[1]
Overtakes <- data.table(Car.Number=integer(n_est), Position=integer(n_est), 
                        Lap=integer(n_est), Sector=integer(n_est), Time=numeric(n_est))
overtake_count <- 1

for (i in 1:dim(Grid)[1]) {
  pos <- Grid[i,];
  Overtakes[overtake_count,] <- c(
    Car.Number=pos$Car.Number, Position=i, Lap=0, Sector=0, Time=0)
  overtake_count <- overtake_count + 1
}

# TODO copy Grid to Overtakes

for (idx in 1:min(Inf, dim(Events)[1])) {
  evt <- Events[idx,]
  if (is.na(evt$Time)) next
  if (evt$Time < 1e-6) next # Race start event
  #if (evt$Time >= 86672.78) break
  #if (idx > 1000) break
  
  # Update and write back position
  pos <- subset(Positions, Car.Number==evt$Car.Number)[1,]
  pos$Lap <- evt$Lap
  pos$Sector <- evt$Sector
  pos$Time <- evt$Time
  Positions[rownames(pos),] <- pos
  #print(pos)
  pos_idx <- as.integer(rownames(pos))
  #print(sprintf("%5d %f: Car %2s (P%2d) crosses Lap %d Sector %d", idx, evt$Time,
  #              as.character(evt$Car.Number), pos_idx, evt$Lap, evt$Sector))
  
  key <- 10*pos$Lap + pos$Sector
  gained <- 0
  
  # Determine position switches
  if (pos_idx-1 > 0) {
    for (pos_idx1 in seq(pos_idx-1, 0, -1)) {
      if (pos_idx1 > 0) {
        pos1 <- Positions[pos_idx1,]
        key1 <- 10*pos1$Lap + pos1$Sector
        
        if (key > key1 | (key == key1 & pos$Time <= pos1$Time)) {
          print(sprintf("%5d %f: -- Car %2s is overtaken by Car %2s : P%2d -> P%2d", idx, evt$Time,
                        as.character(pos1$Car.Number), as.character(pos$Car.Number), pos_idx1, pos_idx1+1))
          # position drop
          Overtakes[overtake_count,] <- c(
            Car.Number=pos1$Car.Number, Position=pos_idx+1, Lap=pos1$Lap, Sector=pos1$Sector, Time=evt$Time)
          overtake_count <- overtake_count + 1
          
          gained <- gained + 1
          
          # Perform swap
          pos.1 <- Positions[pos_idx1+1,] 
          pos.2 <- Positions[pos_idx1,]
          Positions[pos_idx1,] <- pos.1
          Positions[pos_idx1+1,] <- pos.2
          next
        }
      }
      if (gained > 0) {
        print(sprintf("%5d %f: Car %2s : P%2d -> P%2d (gained %d)", idx, evt$Time,
                      as.character(pos$Car.Number), pos_idx, pos_idx1+1, gained))
        # position gain
        Overtakes[overtake_count,] <- c(
          Car.Number=pos$Car.Number, Position=pos_idx1+1, Lap=pos$Lap, Sector=pos$Sector, Time=evt$Time)
        overtake_count <- overtake_count + 1
        break
      }
    }
  }
  if (gained == 0) {
    Overtakes[overtake_count,] <- c(
      Car.Number=pos$Car.Number, Position=pos_idx, Lap=pos$Lap, Sector=pos$Sector, Time=pos$Time)
    overtake_count <- overtake_count + 1
  }
}

Overtakes$Car.Number <- factor(Overtakes$Car.Number, levels=1:length(levels(Grid$Car.Number)), labels=levels(Grid$Car.Number))

#for (i in 1:dim(Classification)[1]) {
#  pos <- Classification[i,];
#  Overtakes[overtake_count,] <- c(
#    Car.Number=as.character(pos$NUMBER), Position=pos$POSITION, Lap=pos$LAPS, Sector=0, Time=Inf)
#  overtake_count <- overtake_count + 1
#}

Overtakes <- Spa.2016$fill_info(Overtakes)

View(Positions)
View(Overtakes)
qplot(Time/3600, Position, group=Car.Number, data=Overtakes,
      colour=Class, geom="path") + xlab("Hour") + plot_styles$scale_x_hour + plot_styles$color_by_class
