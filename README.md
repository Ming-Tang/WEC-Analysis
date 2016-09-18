WEC-Analysis
============

R scripts to analyze and visualize data from World Endurance Championship races.

# Race Data

The race data is placed in the `Data/` directory. Each race is in its own directory of `Data/<race name>`.

Each race directory should contain the following files:
 - `Analysis.csv`: Chronological analysis
 - `Classification.csv`: Race classification
 - `Grid.csv`: Starting grid, scan of `Grid.pdf`

The starting grid is converted from the PDF file `Grid.pdf` using [Tabula](http://tabula.technology).
The command line automation is provided by [tabula-java](https://github.com/tabulapdf/tabula-java), and it is needed for the Python script.

# `download.py`
Downloads data provided by [fiawec.alkamelsystems.com](http://fiawec.alkamelsystems.com).

Examples:

```
$ python download.py -s 2016 -e 'LE MANS' -p Data/2016_Le_Mans
$ python download.py -s 2016 -e 'CIRCUIT OF THE AMERICAS' -p Data/2016_COTA
```

# `WEC_Analysis.R`
R script to perform analysis on the downloaded data. It provides:

 - `Grid` and `Classification`
 - `Positions`: Grid vs finishing positions
 - `Analysis`: The chronological analysis data
 - `Events`: Events for lap and sector arrivals

# `Races.Rmd`
Maintains a list of WEC races.

# `Report_Interactive.Rmd`
Interactive app for exploring lap time distribution and finishing positions of WEC races.
Requires [Shiny](http://shiny.rstudio.com) to run.

# `Lap_Charts.Rmd`
Code to generate lap charts for each race.

## Screenshot
![](screenshot.png)
