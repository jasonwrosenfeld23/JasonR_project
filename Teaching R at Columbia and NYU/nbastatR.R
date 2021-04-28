#install.packages("devtools")
library(devtools)

#Run the next line once to install the nbastatR package
#devtools::install_github("abresler/nbastatR")

#Call the package
library(nbastatR)

#Gather a table of all NBA players in the database
assign_nba_players()

#Download Basketball Reference Player Season Tables
bref_players_stats(seasons = 2018:2021, tables = c("advanced", "totals"))

#Download NBA Game Logs
game_logs(seasons = 2018:2021, result_types = c("team", "player"))

#Download NBA Team Salaries
salary = nba_insider_salaries()

#For a full listing of all the functions in the nbastatR package, see:
#http://asbcllc.com/nbastatR/reference/index.html

injuries =  bref_injuries()

################################
#Run the next line once to install the tidyverse package, which includes dplyr
#install.packages("tidyverse")

#Call the package
library(tidyverse)

#Choose specific columns from a dataset using select()
players = df_nba_player_dict %>% #call the dataset and add your first pipe (%>%) so that R keeps reading
  select(isActive, namePlayer, idPlayer, yearSeasonFirst, countSeasons) #Use the select() command to choose the field(s)

#Create a filtered data frame
active_players <- players %>%
  filter(isActive == "TRUE") #this is filtering only rows where isActive is TRUE

#You can alternatively stack pipes on top of each other
active_players_clone = df_nba_player_dict %>% 
  select(isActive, namePlayer, idPlayer, yearSeasonFirst, countSeasons) %>%
  filter(isActive == "TRUE")

#We can add more filters by separating them by commas ("filter by this AND this")
active_players_2000s = players %>%
  filter(isActive == "TRUE", yearSeasonFirst >= 2001, yearSeasonFirst <= 2010) 

#We can "filter by this OR this" by using the vertical line above backslash (|)
active_players_2000s_warped = players %>%
  filter(isActive == "TRUE" | yearSeasonFirst >= 2001, yearSeasonFirst <= 2010) 

#Let's switch to the BREF Advanced data to see how Advanced Stats were distributed
#among positions in 2019 using group_by() and summarize()
Adv2019 = dataBREFPlayerAdvanced %>%
  filter(minutes >= 500) %>%
  group_by(slugPosition) %>% #we are "grouping" all of the players at each position together
  summarize(
    MEAN_minutes = mean(minutes), MEAN_PER = mean(ratioPER), ws = mean(ratioWS), 
    OWS = mean(ratioOWS), dws = mean(ratioDWS), 
    BPM = mean(ratioBPM), players=n()) %>% #summarize() allows us to create summary data
  filter(players > 10)