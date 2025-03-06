#==================================================Notes============================================
#->This is the script file to clean the data present in atp_matches, atp_players and atp_rankings from the package deuce
#
#->Consider keeping only Grand Slam data (or Grand Slams and Masters) if the data set ends up being too large
#->NB: We need to figure out how to create the response variable y_ijt. We take time to be the numbers 1,2,3,... since the data is in order of occurrence
#
#Consider removing the unnecessary columns and remove retired matches
# What has been cleaned:
# 1. Matches from 2000 onwards are considered  
# 2. Only Masters and Grand Slam tournaments are considered (consider removing the ATP 250 500 if dataset is too large)
# 3. Remove all players who played less that 10 matches. To obtain these separately, simply alter lines 47 to 49 and change the name of the saved data set at the end of this script file. The model presented in paper 17 distinguishes between these two by considering the probability of winning a set. If we end up only considering Grand Slams (due to size limitations), then we do not need to go into the probability of winning a set. 
# 4. Only matches which were played in full (not retired due to injury) was considered
# 5. We unified the spelling of US open (this was spelled in two different ways)
# 6. We removed the Australian Open Qualifiers (97) as this is the same tournament as Australian Open (just the qualifiers which were held in Doha Qatar. More info: https://en.wikipedia.org/wiki/2021_Australian_Open_–_Men%27s_singles_qualifying)
# 7. It is important to note that we only have till 16th Jan 2023 (incomplete year). Should we keep this? I think we should, as it shouldn't affect estimation. Just keep it in mind when when discussing and showing summary statistics. Could be used as test set... or take 2023 of Sackmann and merge it to this data set.
# 8. Removed the surface "carpet" since this is not often played
# 9. Remove those matches for which any player has missing ranks
# 10. Added the transformed rankings variables and difference in rankings 
# 11. Removed those matches which do not have any information on the break points (since we are considering these in our model)
# 12. We changed the ID of the players to consist of 1,...,K
# 13. We adopted the convention that player i in the model described in the paper is taken to be the player with the lowest ID number. 
# 14. We removed players whose latest year played is <=2020. The idea is to remove retired players from the model. However, I have some objection to this. First of all, the data set has been reduced significantly to 9294 observations for combined Grand Slam and Masters, which leaves us with very little matches when we split Grand Slam and Masters tournaments. Secondly, keeping retired players gives us a one up on the ATP rankings, since retired players are automatically removed. But wouldn't it be interesting to compare active players with retired players? Wouldn't it be an interesting result if, say, we get that the ability of one particular player is worse than a retired one? This already gives us a lot more information which cannot be extrapolated from the official ATP rankings. And if we are uninterested in retired players, we can just discard their estimated abilities, as I do not believe there would be any major changes in the estimates if we simply remove a player.
# 15. I have edited the formula for the transformed round depending on draw size of the tournament
# 16. I have appended test sets for 2023 and 2024 from sackmann
#===================================================================================================


#importing packages
rm(list=ls())
library(deuce)
library(dplyr)

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Data Cleaning and Generation/deuce data")

#loading data
data("atp_matches")
data("atp_players")
data("atp_tournaments")
data("atp_rankings")

#Finding the row indices for the first and last match of 2000 and 2022, respectively
n_2000 <- min(which(atp_matches$year==2000))
n_2022 <- max(which(atp_matches$year == 2022))

atp_matches_2000 = atp_matches[n_2000:n_2022,] #taking matches from 2000 to 2022 for training set 

#appending 2023 and 2024 of sackmann
sackmann_2023 <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Data Cleaning and Generation/deuce data/atp_matches_2023.csv")
sackmann_2024 <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/Data Cleaning and Generation/deuce data/atp_matches_2024.csv")

#changing the names of the levels to match 
sackmann_2023$tourney_level[which(sackmann_2023$tourney_level=="G")] <- "Grand Slams"
sackmann_2023$tourney_level[which(sackmann_2023$tourney_level=="M")] <- "Masters"

#fixing tournament names and taking subset
sackmann_2023$tourney_name[which(sackmann_2023$tourney_name == "Us Open")] <- "US Open"
colnames(sackmann_2023)[6] <- "tourney_start_date"
sackmann_2023 <- sackmann_2023[which(sackmann_2023$tourney_level == "Grand Slams" | sackmann_2023$tourney_level == "Masters"),]
sackmann_2023$year <- 2023

#match the data types
sackmann_2023$draw_size <- as.character(sackmann_2023$draw_size)
sackmann_2023$winner_seed <- as.character(sackmann_2023$winner_seed)
sackmann_2023$loser_seed <- as.character(sackmann_2023$loser_seed)
sackmann_2023$tourney_start_date <- as.Date(sackmann_2023$tourney_start_date)

#Repeating for 2024
#changing the names of the levels to match 
sackmann_2024$tourney_level[which(sackmann_2024$tourney_level=="G")] <- "Grand Slams"
sackmann_2024$tourney_level[which(sackmann_2024$tourney_level=="M")] <- "Masters"

#fixing tournament names and taking subset
sackmann_2024$tourney_name[which(sackmann_2024$tourney_name == "Us Open")] <- "US Open"
colnames(sackmann_2024)[6] <- "tourney_start_date"
sackmann_2024 <- sackmann_2024[which(sackmann_2024$tourney_level == "Grand Slams" | sackmann_2024$tourney_level == "Masters"),]
sackmann_2024$year <- 2024

#match the data types
sackmann_2024$draw_size <- as.character(sackmann_2024$draw_size)
sackmann_2024$winner_seed <- as.character(sackmann_2024$winner_seed)
sackmann_2024$loser_seed <- as.character(sackmann_2024$loser_seed)
sackmann_2024$tourney_start_date <- as.Date(sackmann_2024$tourney_start_date)

#combining deuce data and sackmann (remove this line to get the training set)
#atp_matches_2000 <- bind_rows(atp_matches_2000,sackmann_2023,sackmann_2024) #combining to get the training and test set (together)

#checking the different tournament levels
summary(atp_matches_2000$tourney_level) 


#select one of the three to generate the total, Grand Slam or Masters data sets - we remove Challenger, Davis Cup, Finals and Futures, as these are not the highest level of professional tennis/not rated. 

atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$tourney_level %in% c("Grand Slams","Masters")),] 
#atp_matches_2000 = atp_matches_2000[which((atp_matches_2000$tourney_level %in% c("Grand Slams")) & atp_matches_2000$best_of == 5),] 
#atp_matches_2000 = atp_matches_2000[which((atp_matches_2000$tourney_level %in% c("Masters")) & atp_matches_2000$best_of == 3),] 


#checking the types of surfaces
atp_matches_2000$surface <- factor(atp_matches_2000$surface)
summary(atp_matches_2000$surface) #remove carpet surface

atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$surface %in% c("Clay","Grass","Hard")),] #removing the carpet surface as this is seldom played

# Select one of these three options to get the surface-specific data sets 

#########################################################################################################################
#atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$surface %in% c("Hard")),] #Selecting only hard

#atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$best_of == 5),] #selecting only best of 5

#atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$year %in% c(2018,2019,2020,2021,2022,2023,2024)),]
#########################################################################################################################

#refactoring to remove unused levels
atp_matches_2000$surface <- factor(atp_matches_2000$surface) 

#checks
summary(atp_matches_2000$surface) #remove carpet surface

#remove the matches which were ended due to retirement (injury or other)
summary(atp_matches_2000$Retirement) #counts the number of matches which were not finished
atp_matches_2000 = atp_matches_2000[which(atp_matches_2000$Retirement == FALSE | is.na(atp_matches_2000$Retirement)),] #selects only those matches with matches which were completed in full
atp_matches_2000$Retirement = FALSE

#checks
summary(atp_matches_2000$Retirement)



#####Remove players whose latest match was played before 2021 (this is to remove players who have not competed/retired)####

#calculate the table of names and min/max year played (copied from summary_stats_deuce.R)

get_min_max <- data.frame(player = c(atp_matches_2000$winner_name,atp_matches_2000$loser_loser),year_played = c(atp_matches_2000$year,atp_matches_2000$year)) #we repeat year since the year that the winner and loser played the match are the same. The idea is to get the list of all players (whether winner or loser) together with the year they played their first and last match

#getting the minimum year the player has played
min_year_by_player <- by(get_min_max$year_played, get_min_max$player, min)

#constructing the required dataframe with player as column and min year played
min_year_df <- data.frame(
  player = names(min_year_by_player),
  min_year = as.vector(unlist(min_year_by_player))
)
#getting the max year in which a player has played
max_year_by_player <- by(get_min_max$year_played, get_min_max$player, max)

#constructing the required dataframe with player as column and max year played
max_year_df <- data.frame(
  player = names(max_year_by_player),
  max_year = as.vector(unlist(max_year_by_player))
)
#construction the final data frame
min_max_by_player <- data.frame(player = max_year_df$player, min_year <- min_year_df$min_year, max_year <- max_year_df$max_year)
colnames(min_max_by_player) <- c("player_name", "min_year","max_year")

#adding the number of years a player was active
min_max_by_player$years_played <- min_max_by_player$max_year-min_max_by_player$min_year+1

#find those players who have their last match played at 2020 or less
player_names_to_remove_retirement <- min_max_by_player$player_name[which(min_max_by_player$max_year<=2020)]
player_names_to_remove_no_years_played <- min_max_by_player$player_name[which(min_max_by_player$years_played<=2 & (min_max_by_player$max_year <= 2020))]

#choose only those matches for players which are not in the list player_names_to_remove. 
atp_matches_2000 <- atp_matches_2000[which(!(atp_matches_2000$winner_name %in% player_names_to_remove_retirement) | !(atp_matches_2000$loser_name %in% player_names_to_remove_retirement)),] #used to remove matches in which both players retired



#################################################################################################################


#Remove those matches for which there is no winner rank or no loser rank
atp_matches_2000 = atp_matches_2000[which(!is.na(atp_matches_2000$winner_rank) & !is.na(atp_matches_2000$loser_rank)),]

#remove those matches which do not have any information on break points
length(which(is.na(atp_matches_2000$w_bpFaced) | is.na(atp_matches_2000$w_bpSaved))) #finds number of matches for which this data is missing

atp_matches_2000 <- atp_matches_2000[which(!is.na(atp_matches_2000$w_bpFaced) & !is.na(atp_matches_2000$w_bpSaved)),] #removes these matches

#remove those matches for which there is no player height
atp_matches_2000 <- atp_matches_2000[which(!is.na(atp_matches_2000$winner_ht) & !is.na(atp_matches_2000$loser_ht)),]

#Ensuring that all tournaments have the same spelling
atp_matches_2000$tourney_name <- factor(atp_matches_2000$tourney_name)

summary(atp_matches_2000$tourney_name) #2 DIFFERENT SPELLINGS FOR US OPEN!!!

atp_matches_2000[which(atp_matches_2000$tourney_name == "Us Open"),2]="US Open" #changing to the correct spelling

#checks
summary(atp_matches_2000$tourney_name)

#removing the Aus Open Qualies tournament
atp_matches_2000<- atp_matches_2000[which(atp_matches_2000$tourney_name != "Doha Aus Open Qualies"),]

#checks
summary(atp_matches_2000$tourney_name)

atp_matches_2000$tourney_name <- factor(atp_matches_2000$tourney_name) #re-leveling the factor to remove the ones which were removed

#checking that all players have played a particular quantity of matches, say 10
min_games <- 10
players_who_played = c(atp_matches_2000$winner_id,atp_matches_2000$loser_id) #appending these together will give us a vector of all the names of players and the number of times the player appears is the number of games he has played (either winner or loser)
game_count = as.data.frame(table(players_who_played)) #tallies up the number of games played
game_count = game_count[which(game_count$Freq >= min_games),] #chooses the threshold of min games played

#View(game_count)

#The line below chooses those rows which only have players with over 10 matches played. If one of the players has less than 10 matches, the game is removed. Note that removing these players affects the number of matches of the selected players, so we may still end up with some players which have less than 10 matches
atp_matches_2000 = atp_matches_2000[which((atp_matches_2000$winner_id %in% game_count$players_who_played) & (atp_matches_2000$loser_id %in% game_count$players_who_played)),] 


unique_player_ID = unique(c(atp_matches_2000$winner_id,atp_matches_2000$loser_id)) #this gives us the list of player IDs which are present in the data set, so that we can remove unnecessary players from the atp_rankings and atp_players datasets

player_row_indices_to_be_kept_1 = which(atp_players$player_id %in% unique_player_ID) #finds those atp players in atp_players which are in unique player ID

player_row_indices_to_be_kept_2 = which(atp_rankings$player_id %in% unique_player_ID) #finds those atp players in atp_rankings which are in unique player ID

atp_players_2000 = atp_players[player_row_indices_to_be_kept_1,] #keeping only the relevant players which appear in matches post 2000
atp_rankings_2000 = atp_rankings[player_row_indices_to_be_kept_2,] #keeping only the relevant rankings of players which appear in matches post 2000

#Change the player IDs in the atp_players_2000 to be 1,...,number of players
convert_ID = base::subset(atp_players_2000,select = c(player_id,first_name,last_name)) #take subset for the required lookup table
convert_ID = sort_by.data.frame(convert_ID,convert_ID$last_name) #sort by last name (indexing will be done by last name)
convert_ID$new_id = 1:nrow(convert_ID) #give new index based on alphabetical order of last name
convert_ID = convert_ID[,c(4,1,2,3)] #reorder the columns for clarity
colnames(convert_ID) = c("new_id","old_id","first_name","last_name") #rename the columns for clarity
convert_ID$full_name = paste(convert_ID$first_name, convert_ID$last_name,sep = " ")

atp_players_2000_changed_id <- atp_players_2000 #make a copy of players data set
atp_players_2000_changed_id$player_id <- convert_ID$new_id[match(unlist(atp_players_2000_changed_id$player_id),convert_ID$old_id)] #this code changes the old ID in the players data set to the new ID

atp_matches_2000_changed_id <- atp_matches_2000 #make a copy
atp_matches_2000_changed_id$winner_id <- convert_ID$new_id[match(unlist(atp_matches_2000_changed_id$winner_id),convert_ID$old_id)] #changes the IDs of the winner_id column to the new IDs

atp_matches_2000_changed_id$loser_id <- convert_ID$new_id[match(unlist(atp_matches_2000_changed_id$loser_id),convert_ID$old_id)] #changes the IDs of the loser_id column to the new IDs

#repeats the above for the rankings data set
atp_rankings_2000_changed_id <- atp_rankings_2000
atp_rankings_2000_changed_id$player_id <- convert_ID$new_id[match(unlist(atp_rankings_2000_changed_id$player_id),convert_ID$old_id)]

#NB: There is an inconsistency for player ID 601 Joao Sousa and ID 602 Joao Souza. Player ID 602 is spelled incorrectly. The code below rectifies this issue
souza_id<- convert_ID$new_id[which(convert_ID$full_name == "Joao Souza")]
atp_matches_2000_changed_id$winner_name[which(atp_matches_2000_changed_id$winner_id == souza_id)] = "Joao Souza"
atp_matches_2000_changed_id$loser_name[which(atp_matches_2000_changed_id$loser_id == souza_id)] = "Joao Souza"


#adds the time variable t=1,...,nrow(data)
atp_matches_2000_changed_id$Time = 1:nrow(atp_matches_2000_changed_id) 
response_data = base::subset(atp_matches_2000_changed_id, select = c(Time, winner_id, winner_name, loser_id, loser_name))

#Adding the response variable y
y <- mat.or.vec(nrow(response_data),1)
for (i in 1:nrow(response_data)){
  if (response_data$winner_id[i] < response_data$loser_id[i]){
    y[i] = 1
  }else {
    y[i] = 0
  }
}
response_data$y = y #add y to the response data set and the atp_matches dataset
atp_matches_2000_changed_id$y = y

######### Calculating the predictors for M3 ###########

#adding transformed ranking points for winner

#if draw size = 128 or 96, take value 8. 56,48-> take value 6 (5 total rounds and winning final =>6) and 64 takes value 7
for (i in 1:nrow(atp_matches_2000_changed_id)){
  if(atp_matches_2000_changed_id$draw_size[i]=="128" | atp_matches_2000_changed_id$draw_size[i]=="96"){
    atp_matches_2000_changed_id$transformed_winner_rank[i] <- 8 - log2(atp_matches_2000_changed_id$winner_rank[i])

   #adding transformed ranking points for loser
   atp_matches_2000_changed_id$transformed_loser_rank[i] <- 8 - log2(atp_matches_2000_changed_id$loser_rank[i])
  }  
  
  if(atp_matches_2000_changed_id$draw_size[i]=="56" | atp_matches_2000_changed_id$draw_size[i]=="48" | atp_matches_2000_changed_id$draw_size[i]=="64"){
    atp_matches_2000_changed_id$transformed_winner_rank[i] <- 7 - log2(atp_matches_2000_changed_id$winner_rank[i])
    
    #adding transformed ranking points for loser
    atp_matches_2000_changed_id$transformed_loser_rank[i] <- 7 - log2(atp_matches_2000_changed_id$loser_rank[i])
  }
}

#empty vector for the difference
diff_transformed_rank <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

#checks whether the index of the winner is < loser. If so take i to be winner and j to be loser and calculate respective difference in transformed ranking. Swap otherwise.
for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i]<atp_matches_2000_changed_id$loser_id[i]){
    diff_transformed_rank[i] = atp_matches_2000_changed_id$transformed_winner_rank[i] - atp_matches_2000_changed_id$transformed_loser_rank[i]
  }else{
    diff_transformed_rank[i] = atp_matches_2000_changed_id$transformed_loser_rank[i] - atp_matches_2000_changed_id$transformed_winner_rank[i]
  }
}



#checking logic:
atp_matches_2000_changed_id$winner_id<atp_matches_2000_changed_id$loser_id #compare to the calculated scores

#Relabelling to get the transformed winner score of i and j (instead of winner and loser)
transformed_rank_i <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
transformed_rank_j <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i]<atp_matches_2000_changed_id$loser_id[i]){
    transformed_rank_i[i] = atp_matches_2000_changed_id$transformed_winner_rank[i]
    transformed_rank_j[i] = atp_matches_2000_changed_id$transformed_loser_rank[i]
  }else{
    transformed_rank_i[i] = atp_matches_2000_changed_id$transformed_loser_rank[i]
    transformed_rank_j[i] = atp_matches_2000_changed_id$transformed_winner_rank[i]
  }
}

#Add variables player_i_id, player_i_name, player_j_id and player_j_name for ease of reference
player_i_id <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
player_i_name <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
player_j_id <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
player_j_name <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i]<atp_matches_2000_changed_id$loser_id[i]){
    player_i_id[i] <- atp_matches_2000_changed_id$winner_id[i]
    player_i_name[i] <- atp_matches_2000_changed_id$winner_name[i]
    
    player_j_id[i] <- atp_matches_2000_changed_id$loser_id[i]
    player_j_name[i] <- atp_matches_2000_changed_id$loser_name[i]
  }else{
    player_i_id[i] <- atp_matches_2000_changed_id$loser_id[i]
    player_i_name[i] <- atp_matches_2000_changed_id$loser_name[i]
    
    player_j_id[i] <- atp_matches_2000_changed_id$winner_id[i]
    player_j_name[i] <- atp_matches_2000_changed_id$winner_name[i]
    
  }
}
#adding variables up till now to data set 
atp_matches_2000_changed_id$player_i_id <- player_i_id
atp_matches_2000_changed_id$player_i_name <- player_i_name
atp_matches_2000_changed_id$player_j_id <- player_j_id
atp_matches_2000_changed_id$player_j_name <- player_j_name

atp_matches_2000_changed_id$transformed_rank_i <- transformed_rank_i
atp_matches_2000_changed_id$transformed_rank_j <- transformed_rank_j
atp_matches_2000_changed_id$diff_transformed_rank <- diff_transformed_rank


##### Calculate difference in age ######

#defining empty vectors
diff_age <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
age_i <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
age_j <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

#Calculating the ages for players i_t and j_t
for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i] < atp_matches_2000_changed_id$loser_id[i]){
    age_i[i] <- atp_matches_2000_changed_id$winner_age[i]
    age_j[i] <- atp_matches_2000_changed_id$loser_age[i]
    diff_age[i] <- atp_matches_2000_changed_id$winner_age[i] - atp_matches_2000_changed_id$loser_age[i]
  }
  else{
    age_i[i] <- atp_matches_2000_changed_id$loser_age[i]
    age_j[i] <- atp_matches_2000_changed_id$winner_age[i]
    diff_age[i] <- atp_matches_2000_changed_id$loser_age[i] - atp_matches_2000_changed_id$winner_age[i]
  }
}
atp_matches_2000_changed_id$age_i <- age_i
atp_matches_2000_changed_id$age_j <- age_j
atp_matches_2000_changed_id$diff_age <- diff_age

#### Calculating difference in height ####

#Empty vectors
diff_height <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
height_i <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
height_j <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

#Calculating the heights for players i_t and j_t
for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i] < atp_matches_2000_changed_id$loser_id[i]){
    height_i[i] <- atp_matches_2000_changed_id$winner_ht[i]
    height_j[i] <- atp_matches_2000_changed_id$loser_ht[i]
    diff_height[i] <- atp_matches_2000_changed_id$winner_ht[i] - atp_matches_2000_changed_id$loser_ht[i]
  }
  else{
    height_i[i] <- atp_matches_2000_changed_id$loser_ht[i]
    height_j[i] <- atp_matches_2000_changed_id$winner_ht[i]
    diff_height[i] <- atp_matches_2000_changed_id$loser_ht[i] - atp_matches_2000_changed_id$winner_ht[i]
  }
}
atp_matches_2000_changed_id$height_i <- height_i
atp_matches_2000_changed_id$height_j <- height_j
atp_matches_2000_changed_id$diff_height <- diff_height

##### Calculating the difference in ratios of break points faced against break points saved #####

#calculating the ratios for winners and losers
winner_bp_saved_ratio <- atp_matches_2000_changed_id$w_bpSaved/atp_matches_2000_changed_id$w_bpFaced 
loser_bp_saved_ratio <- atp_matches_2000_changed_id$l_bpSaved/atp_matches_2000_changed_id$l_bpFaced 

#0/0 case is defined as 1.5, i.e we inflate this since having no break points faced is extremely good.
winner_bp_saved_ratio[which(is.na(winner_bp_saved_ratio))] <- 1.5 
loser_bp_saved_ratio[which(is.na(loser_bp_saved_ratio))] <- 1.5

#Appending to the data set
atp_matches_2000_changed_id$w_bp_saved_ratio <- winner_bp_saved_ratio
atp_matches_2000_changed_id$l_bp_saved_ratio <- loser_bp_saved_ratio

#finding the ratios of i_t and j_t and difference
bp_saved_ratio_i <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
bp_saved_ratio_j <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
diff_bp_saved_ratio <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

#calculating the difference in ratios according the convention of i and j
for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i] < atp_matches_2000_changed_id$loser_id[i]){
    bp_saved_ratio_i[i] <- atp_matches_2000_changed_id$w_bp_saved_ratio[i]
    bp_saved_ratio_j[i] <- atp_matches_2000_changed_id$l_bp_saved_ratio[i]
    diff_bp_saved_ratio[i] <- atp_matches_2000_changed_id$w_bp_saved_ratio[i] - atp_matches_2000_changed_id$l_bp_saved_ratio[i]
  }
  else{
    bp_saved_ratio_i[i] <- atp_matches_2000_changed_id$l_bp_saved_ratio[i]
    bp_saved_ratio_j[i] <- atp_matches_2000_changed_id$w_bp_saved_ratio[i]
    diff_bp_saved_ratio[i] <- atp_matches_2000_changed_id$l_bp_saved_ratio[i] - atp_matches_2000_changed_id$w_bp_saved_ratio[i]
  }
}
atp_matches_2000_changed_id$bp_saved_ratio_i <- bp_saved_ratio_i
atp_matches_2000_changed_id$bp_saved_ratio_j <- bp_saved_ratio_j
atp_matches_2000_changed_id$diff_bp_saved_ratio <- diff_bp_saved_ratio


#### Calculating difference in break points won (not used) ####
w_bp_chances <- atp_matches_2000_changed_id$l_bpFaced #the break point chances of one player is the break points faced of the opponent
l_bp_chances <- atp_matches_2000_changed_id$w_bpFaced #similarly but for the loser

w_bp_lost <- atp_matches_2000_changed_id$l_bpSaved #The bp lost by a player are those which were saved by the opponent
l_bp_lost <- atp_matches_2000_changed_id$w_bpSaved

w_bp_won <- w_bp_chances - w_bp_lost
l_bp_won <- l_bp_chances - l_bp_lost


bp_won_i <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
bp_won_j <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)
diff_bp_won <- mat.or.vec(nrow(atp_matches_2000_changed_id),1)

for (i in 1:nrow(atp_matches_2000_changed_id)){
  if (atp_matches_2000_changed_id$winner_id[i] < atp_matches_2000_changed_id$loser_id[i]){
    bp_won_i[i] <- w_bp_won[i]
    bp_won_j[i] <- l_bp_won[i]
    diff_bp_won[i] <- w_bp_won[i] - l_bp_won[i]
  }
  else{
    bp_won_i[i] <- l_bp_won[i]
    bp_won_j[i] <- w_bp_won[i]
    diff_bp_won[i] <- l_bp_won[i] - w_bp_won[i]
  }
}

atp_matches_2000_changed_id$w_bp_chances <- w_bp_chances
atp_matches_2000_changed_id$l_bp_chances <- l_bp_chances
atp_matches_2000_changed_id$w_bp_lost <- w_bp_lost
atp_matches_2000_changed_id$l_bp_lost <- l_bp_lost
atp_matches_2000_changed_id$w_bp_won <- w_bp_won
atp_matches_2000_changed_id$l_bp_won <- l_bp_won
atp_matches_2000_changed_id$bp_won_i <- bp_won_i
atp_matches_2000_changed_id$bp_won_j <- bp_won_j
atp_matches_2000_changed_id$diff_bp_won <- diff_bp_won

#### Creating Surface Indicator variables ####

I_grass <- mat.or.vec(nr = nrow(atp_matches_2000_changed_id),nc=1)
I_clay <- mat.or.vec(nr = nrow(atp_matches_2000_changed_id),nc=1)
I_hard <- mat.or.vec(nr = nrow(atp_matches_2000_changed_id),nc=1)

I_grass[which(atp_matches_2000_changed_id$surface == "Grass")] = 1
I_clay[which(atp_matches_2000_changed_id$surface == "Clay")] = 1
I_hard[which(atp_matches_2000_changed_id$surface == "Hard")] = 1

atp_matches_2000_changed_id$I_grass <- I_grass
atp_matches_2000_changed_id$I_clay <- I_clay
atp_matches_2000_changed_id$I_hard <- I_hard

#removing unnecessary variables from the data
atp_matches_2000_changed_id <- subset(atp_matches_2000_changed_id, select = -c(winner_seed,winner_entry,loser_seed,loser_entry,Retirement,WTB1,LTB1,WTB2,LTB2,WTB3,LTB3,WTB4,LTB4,WTB5,LTB5))

#checking correlations of the predictors
cor(subset(atp_matches_2000_changed_id,select = c(diff_transformed_rank,diff_age,diff_height,diff_bp_won)))

#clearing memory
remove(atp_matches) #removing the original data from memory
remove(atp_players)
remove(atp_rankings)
remove(atp_matches_2000)
remove(atp_rankings_2000)
remove(atp_players_2000)


#Saving the data sets (change name depending on what surface, tour and period being considered)
#write.csv2(atp_matches_2000_changed_id,file = "atp_matches_2000_train_test_MS_hard_2.csv")
#write.csv2(atp_players_2000_changed_id,file = "atp_players_2000_train_test_MS_hard_2.csv")
#write.csv2(atp_rankings_2000_changed_id,file = "atp_rankings_2000_train.csv")


