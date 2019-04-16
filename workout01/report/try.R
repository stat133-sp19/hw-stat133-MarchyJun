library(dplyr)
# import data
shot_data <- read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/shots-data.csv')

boolen_2pt <- shot_data$shot_type == '2PT Field Goal'
boolen_3pt <- shot_data$shot_type == '3PT Field Goal'

ESP_2PT <- 
data.frame(
arrange(
summarise(
  group_by(shot_data[boolen_2pt,], name),
  total = length(shot_made_flag),
  made = sum(shot_made_flag == 'shot_yes'),
  perc_made_2PT = made/total),
  desc(perc_made_2PT)
))

ESP_3PT <- 
data.frame(
arrange(
summarise(
  group_by(shot_data[boolen_3pt,], name),
  total = length(shot_made_flag),
  made = sum(shot_made_flag == 'shot_yes'),
  perc_made_3PT = made/total),
  desc(perc_made_3PT)
))

ESP_total <- 
data.frame(
arrange(
summarise(
  group_by(shot_data, name),
  total = length(shot_made_flag),
  made = sum(shot_made_flag == 'shot_yes'),
  perc_made_total = made/total),
  desc(perc_made_total)
))
