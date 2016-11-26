# When a new race is added, delete WEC_Races.Rdata to invalidate

race_list <- c(
  "Le_Mans_2015",
  "Silverstone_2016",
  "Spa_2016",
  "Le_Mans_2016",
  "Nurburgring_2016",
  "Mexico_2016",
  "COTA_2016",
  "Fuji_2016",
  "Shanghai_2016",
  "Bahrain_2016"
)

load_race <- function(year, race) {
  #print(paste("load_race: ", year, race, " - START"))
  res <- WEC.Analysis(year, race, TRUE)
  #print(paste("load_race: ", year, race, " - FINISH"))
  res
}

if (!file.exists("WEC_Races.Rdata")) {
  #print("WEC_races.Rdata doesn't exist")
  delayedAssign("Le_Mans_2015", load_race(2015, "Le_Mans"))
  delayedAssign("Silverstone_2016", load_race(2016, "Silverstone"))
  delayedAssign("Spa_2016", load_race(2016, "Spa"))
  delayedAssign("Le_Mans_2016", load_race(2016, "Le_Mans"))
  delayedAssign("Nurburgring_2016", load_race(2016, "Nurburgring"))
  delayedAssign("Mexico_2016", load_race(2016, "Mexico"))
  delayedAssign("COTA_2016", load_race(2016, "COTA"))
  delayedAssign("Fuji_2016", load_race(2016, "Fuji"))
  delayedAssign("Shanghai_2016", load_race(2016, "Shanghai"))
  delayedAssign("Bahrain_2016", load_race(2016, "Bahrain"))
  save(list=race_list, file="WEC_Races.Rdata")
} else {
  #print("WEC_races.Rdata exists")
  load("WEC_Races.Rdata")  
}


delayedAssign("choices", {
  choices <- as.list(race_list)
  names(choices) <- gsub("_", " ", race_list)
  choices
})

retrieve_race <- function(name) { get(name) }