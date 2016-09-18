# When a new race is added, delete WEC_Races.Rdata to invalidate

race_list <- c(
  "Le_Mans_2015",
  "Silverstone_2016",
  "Spa_2016",
  "Le_Mans_2016",
  "Nurburgring_2016",
  "Mexico_2016",
  "COTA_2016"
)

load_race <- function(year, race) WEC.Analysis(year, race, TRUE)

if (!file.exists("WEC_Races.Rdata")) {
  delayedAssign("Le_Mans_2015", load_race(2015, "Le_Mans"))
  delayedAssign("Silverstone_2016", load_race(2016, "Silverstone"))
  delayedAssign("Spa_2016", load_race(2016, "Spa"))
  delayedAssign("Le_Mans_2016", load_race(2016, "Le_Mans"))
  delayedAssign("Nurburgring_2016", load_race(2016, "Nurburgring"))
  delayedAssign("Mexico_2016", load_race(2016, "Mexico"))
  delayedAssign("COTA_2016", load_race(2016, "COTA"))
  
  save(list=race_list, file="WEC_Races.Rdata")
} else {
  load("WEC_Races.Rdata")  
}


delayedAssign("choices", {
  choices <- as.list(race_list)
  names(choices) <- gsub("_", " ", race_list)
  choices
})

retrieve_race <- function(name) { get(name) }