rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(rvest)

#read data from file
df = read_csv('BAIS6060_ProjectData.csv')

#data cleaning
{
#factorized data
factor(df$type)
factor(df$pitch_type)
factor(df$hit_location)
factor(df$bb_type)
factor(df$balls)
factor(df$strikes)
factor(df$events)
factor(df$description)
factor(df$zone)
factor(df$stand)
factor(df$p_throws)
factor(df$home_team)
factor(df$away_team)
factor(df$outs_when_up)
factor(df$inning)
factor(df$inning_topbot)
factor(df$babip_value)
factor(df$iso_value)
factor(df$woba_denom)
factor(df$woba_value)
factor(df$launch_speed_angle)
factor(df$pitch_name)
factor(df$if_fielding_alignment)
factor(df$of_fielding_alignment)

#Single valued (not important)
df$game_year = NULL
df$game_type = NULL

#Duplicated data
#df$pitcher = NULL
#df = rename(df, pitcher = player_name)
df$batter = NULL

#No data for year
df$spin_dir = NULL
df$spin_rate_deprecated = NULL
df$break_angle_deprecated = NULL
df$break_length_deprecated = NULL
df$tfs_deprecated = NULL
df$tfs_zulu_deprecated = NULL
df$umpire = NULL
df$sv_id = NULL
}

#create player look-up table
{
#Read player listing from MLB site
urlPlayers = "https://www.mlb.com/players"
pg <- read_html(urlPlayers)

#extract relevant HTML lines
playerLinks = html_nodes(pg, "a.p-related-links__link")

#get listing of player names
playerNames = html_text(playerLinks)
playerLUT = data.frame(playerNames)

#string to parse
playerAttr = html_attr(playerLinks, "href")

#for each string extract the playerID and append to playerLUT
a = strsplit(playerAttr, "")
for (n in 1:length(a)) {
  b = unlist(a[n])
  mynameT = grepl("[0-9]", b)
  cc = b[mynameT]
  d = paste(cc, collapse = "")
  playerLUT$playerID[n] = d
}
}

#playerStats
#   pulls statcast stats for a player
#input:
#   playerName - the name of the player
#output:
#   all 2020 batting stats for the player
playerStats <- function(playerName) {
  
  #get the corresponding playerID
  player = (playerLUT$playerNames == playerName)
  playerID = playerLUT$playerID[player]
  
  #if name not found, error out
  if(length(playerID) == 0) {
    err = sprintf("Name \"%s\" was not found. Ensure spelling and capitalization are correct", playerName)
    stop(err)
  }
  
  #put together URL for download
  url1 = "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2020%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&batters_lookup%5B%5D="
  url2 = "&hfFlag=&hfBBT=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&type=details"
  urlDownload = paste(url1, playerID, url2, sep="")
  
  #download data
  download.file(urlDownload, "data.csv")
  data <- readr::read_csv("data.csv")
  
  return(data)
}