analyze_events <- TRUE
delayedAssign("Le_Mans_2015", WEC.Analysis(2015, "Le_Mans", analyze_events))
delayedAssign("Silverstone_2016", WEC.Analysis(2016, "Silverstone", analyze_events))
delayedAssign("Spa_2016", WEC.Analysis(2016, "Spa", analyze_events))
delayedAssign("Le_Mans_2016", WEC.Analysis(2016, "Le_Mans", analyze_events))

race_list <- c(
  "Le_Mans_2015",
  "Silverstone_2016",
  "Spa_2016",
  "Le_Mans_2016"
)

delayedAssign("choices", {
  choices <- as.list(race_list)
  names(choices) <- gsub("_", " ", race_list)
  choices
})

retrieve_race <- function(name) { get(name) }