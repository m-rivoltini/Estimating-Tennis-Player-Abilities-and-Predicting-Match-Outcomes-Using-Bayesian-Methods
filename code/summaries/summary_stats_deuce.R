#This script file aims to obtain some summary statistics for the cleaned Grand Slam and Masters data sets (combined surfaces)

#clearing environment
rm(list=ls()) 

#setting working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/Uni Docs/Dissertation/R code/summaries") #setting working directory

#loading packages
library(deuce)
library(lessR)
library(ggplot2)
library(dplyr)
library(data.table)
library(xtable)

#loading the data sets (either Grand Slam or Masters data sets with all surfaces)
atp_matches_2000 <- read.csv2(file = "atp_matches_2000_train_MS_allsurf.csv",header = TRUE)

#changing from char to factor
atp_matches_2000$tourney_name = factor(atp_matches_2000$tourney_name) 

# Calculate percentages for pie charts
tourneys_data <- atp_matches_2000 %>%
  count(tourney_name) %>%
  mutate(perc = n / sum(n) * 100)  

#plotting a pie chart
ggplot(tourneys_data, aes(x = "", y = n, fill = tourneys_data$tourney_name)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +  # Place the legend on the right
  geom_text(aes(label = paste0(round(perc, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 2.5) +  # Add percentage labels
  labs(fill = "Tournament Name") +  # Legend title
  ggtitle("Match Distribution per Tournament for Masters Tour")

colnames(tourneys_data) <- c("Tournament Name","# of Matches","% Composition")

#Idea is to get a table with the win counts and loss counts
winning_player_counts <- as.data.frame(table(atp_matches_2000$winner_name)) #getting a table of players and the number of times they won
colnames(winning_player_counts) <- c("player_name","wins") #changing column names
top_players <- winning_player_counts[order(winning_player_counts$wins, decreasing = TRUE),] #ordering the data frame according to the players with the highest number of wins

#repeating the same for losing players
losing_player_counts <- as.data.frame(table(atp_matches_2000$loser_name)) 
colnames(losing_player_counts) <- c("player_name","losses")
worst_players <- losing_player_counts[order(losing_player_counts$losses, decreasing = TRUE),]


#constructing final data frame with total wins, losses and total matches played
total_wins_losses <- base::merge(winning_player_counts,losing_player_counts,by = "player_name",all = TRUE)
total_wins_losses[is.na(total_wins_losses)] <- 0

#getting total matches and win percentages
total_wins_losses$total_matches_played <- total_wins_losses$wins + total_wins_losses$losses
total_wins_losses$wins_percentage <- round(total_wins_losses$wins/total_wins_losses$total_matches_played*100,2)
total_wins_losses <- total_wins_losses[order(total_wins_losses$wins_percentage,decreasing = TRUE),]

#saving the total number of wins and losses for each player
write.csv2(total_wins_losses,file = "total_wins_losses_MS_allsurf.csv")

#Idea of the following is to plot the number of wins per year for the top players
wins_by_year = subset(atp_matches_2000,select = c("winner_name","year")) #takes a subset of the original data frame
top_players_year <-as.data.frame.matrix(table(wins_by_year)) #generates a data frame consisting of the number of wins for each year by player
top_players_year <- top_players_year[order(rowSums(top_players_year),decreasing = TRUE),] #orders according to the number of wins 

#repeating for losses
losses_by_year = subset(atp_matches_2000,select = c("loser_name","year")) 
worst_players_year <-as.data.frame.matrix(table(losses_by_year))
worst_players_year <- worst_players_year[order(rowSums(worst_players_year),decreasing = TRUE),]

#manipulating the data to plot
top_players_year_copy <- top_players_year
top_players_year <- as.data.frame.matrix(t(top_players_year))
top_players_year$year <- 2002:2022 #some matches were removed from the beginning of the period due to the cleaning procedures
rownames(top_players_year)=1:nrow(top_players_year) #obtaining the time series for wins 

#plotting the time series
plot(top_players_year$year,top_players_year[,1],type = "l",col=1,ylim = c(0,50), ylab = "Number of Wins", xlab = "Year", main = "Number of Wins for \nTop 5 Players (Masters Tour)")
lines(top_players_year$year,top_players_year[,2],type = "l",col=2)
lines(top_players_year$year,top_players_year[,3],type = "l",col=3)
lines(top_players_year$year,top_players_year[,4],type = "l",col=4)
lines(top_players_year$year,top_players_year[,5],type = "l",col=5)
legend("topright",legend = colnames(top_players_year)[1:5],col = c(1,2,3,4,5),lty=c(1,1,1,1),cex = 0.4)

#checking the distribution of number of matches per year.
n_matches_per_year <- mat.or.vec(24,1)

for (i in 1:24){
  n_matches_per_year[i]<- nrow(atp_matches_2000[which(atp_matches_2000$year == 1999+i),])
}

#checking the number of matches played on each surface aggregated by year
n_surface_per_year <- mat.or.vec(24,3)

for (i in 1:24){
  n_surface_per_year[i,1] = nrow(atp_matches_2000[which(atp_matches_2000$year == 1999+i & atp_matches_2000$surface == "Clay"),])
  n_surface_per_year[i,2] = nrow(atp_matches_2000[which(atp_matches_2000$year == 1999+i & atp_matches_2000$surface == "Grass"),])
  n_surface_per_year[i,3] = nrow(atp_matches_2000[which(atp_matches_2000$year == 1999+i & atp_matches_2000$surface == "Hard"),])
  
}
n_surface_per_year <- as.data.frame.matrix(n_surface_per_year)
colnames(n_surface_per_year) = c("Clay","Grass","Hard") #getting number of games played on each surface per year

#Get the pie chart for the total number of games played on each surface
surface_data <- atp_matches_2000 %>%
  count(surface) %>%
  mutate(perc = n / sum(n) * 100)  # Calculate percentages

#plotting pie chart for the surface distribution in the data set
ggplot(surface_data, aes(x = "", y = n, fill = surface)) + #plotting the chart
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +  # Place the legend on the right
  geom_text(aes(label = paste0(round(perc, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 2.5) +  # Add percentage labels
  labs(fill = "Tournament Name") +  # Legend title
  ggtitle("Match Distribution per Surface for Masters Tour")

#Get the min and max year that each player has played
get_min_max <- data.frame(player = c(atp_matches_2000$winner_name,atp_matches_2000$loser_loser),year_played = c(atp_matches_2000$year,atp_matches_2000$year)) #we repeat year since the year that the winner and loser played the match are the same. The idea is to get the list of all players (whether winner or loser) together with their 

#getting the minimum year the player has played - similar to what was done in the data cleaning script file
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

#adding extra statistics to the min_max_year
min_max_by_player$years_played <- min_max_by_player$max_year-min_max_by_player$min_year+1

#merging two of the data sets
years_played_with_total_wins_losses <- merge(total_wins_losses,min_max_by_player,by = "player_name")

#removing unnecessary objects
remove(get_min_max,min_year_by_player,max_year_by_player,min_year_df,max_year_df)

#finding the number of games per year and per tournament
get_games_by_year_tourn <- subset(atp_matches_2000,select = c(year,tourney_name))
games_by_year_tourn <- as.data.frame(table(get_games_by_year_tourn))

#finding the number of wins per player per tournament.
get_wins_players_by_tournament_by_year <- subset(atp_matches_2000,select = c(winner_name,year,tourney_name))
wins_players_by_tournament_by_year <- as.data.frame(table(get_wins_players_by_tournament_by_year))

#finding the number of losses per player per tournament.
get_losses_players_by_tournament_by_year <- subset(atp_matches_2000,select = c(loser_name,year,tourney_name))
losses_players_by_tournament_by_year <- as.data.frame(table(get_losses_players_by_tournament_by_year))
