## statsbombR ## 
#devtools::install_github("statsbomb/StatsBombR")

library(mongolite)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(StatsBombR)

comp <- FreeCompetitions()
StatMatches <- FreeMatches(comp)

Female_matches <- StatMatches %>% filter(home_team.home_team_gender == "female")
Female_matches_season <- Female_matches %>% filter(season.season_name == "2020/2021")

womens_statsR_matchids <- as.data.frame(Female_matches_season$match_id)

womens_events <- free_allevents(MatchesDF = Female_matches_season, Parallel = T)

saveRDS(womens_events, "Data/SB_womens_events.rds")

# mens events
male_matches_season <- StatMatches %>% filter(home_team.home_team_gender == "male" & season.season_name == "2015/2016" & competition.competition_name == "Premier League")

mens_events <- free_allevents(MatchesDF = male_matches_season, Parallel = T)

saveRDS(mens_events, "Data/SB_mens_events,rds")

mens_events <- readRDS("Data/SB_mens_events,rds")

# shot
mens_shots <- mens_events %>% filter(type.name == "Shot")

# sikre at der er ligeså mange kampe i begge datasæt, kvinderne har kun 131 kampe
selected_games <- head(unique(mens_events$match_id), 131)

mens_shots <- mens_shots %>% filter(match_id %in% selected_games)

shot_conversion_df <- data.frame(
  Total_Shots = nrow(mens_shots),
  Goals = sum(mens_shots$shot.outcome.name == "Goal"),
  Conversion_Percentage = round((sum(mens_shots$shot.outcome.name == "Goal") / nrow(mens_shots)) * 100, 2)
)

womens_shots <- womens_events %>% filter(type.name == "Shot")

womens_shot_conversion_df <- data.frame(
  Total_Shots = nrow(womens_shots),
  Goals = sum(womens_shots$shot.outcome.name == "Goal"),
  Conversion_Percentage = round((sum(womens_shots$shot.outcome.name == "Goal") / nrow(womens_shots)) * 100, 2)
)

both_conversions <- bind_rows(shot_conversion_df, womens_shot_conversion_df)

both_conversions <- both_conversions %>%
  mutate(Category = c("Men", "Women")) 


both_conversions_long <- pivot_longer(both_conversions, cols = c(Total_Shots, Goals), 
                                      names_to = "Metric", values_to = "Count")

# Create the bar plot for Total Shots and Goals
ggplot(both_conversions_long, aes(x = Category, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mænd har flere skud og skore mindre", x = "Køn", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "darkorange"))

# plot over converteringsrate for begge køn
ggplot(both_conversions, aes(x = Category, y = Conversion_Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  labs(title = "Kvinder har en højere konverteringsrate på skud", x = "Køn", y = "Konverteringsprocent") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

# kort

# mænd
mens_cards <- mens_events %>% filter(!is.na(foul_committed.card.name))

mens_cards <- mens_cards %>% filter(match_id %in% selected_games)
mens_cards_summary <- as.data.frame(table(mens_cards$foul_committed.card.name))
colnames(mens_cards_summary) <- c("Card_Type", "Count")

#kvinder
womens_cards <- womens_events %>% filter(!is.na(foul_committed.card.name))
womens_cards <- as.data.frame(womens_cards$foul_committed.card.name)
womens_cards_summary <- as.data.frame(table(womens_cards))
colnames(womens_cards_summary) <- c("Card_Type", "Count")

mens_cards_summary$Category <- "Men"
womens_cards_summary$Category <- "Women"

cards_combined <- rbind(mens_cards_summary, womens_cards_summary)

# cards plot
ggplot(cards_combined, aes(x = Card_Type, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  labs(title = "Mænd får flere kort i alle kategorier",
       x = "Card Type",
       y = "Number of Cards") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) 

# aflevringer

# mens
mens_passes <- mens_events %>% filter(type.name == "Pass" & play_pattern.name == "Regular Play")
mens_passes$pass.outcome.name <- replace(mens_passes$pass.outcome.name, is.na(mens_passes$pass.outcome.name), "Accurate")

# fjerne alle "afleveringer" som bliver spillet ud under en skade og som er "unknown"
mens_passes <- mens_passes %>%
  filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown"))

# erstatte spilninger som er offside og går ud til at være incomplete
mens_passes <- mens_passes %>%
  mutate(pass.outcome.name = ifelse(pass.outcome.name %in% c("Out", "Pass Offside"), "Incomplete", pass.outcome.name))

mens_pass_acc <- as.data.frame(table(mens_passes$pass.outcome.name))
mens_pass_acc <- mens_pass_acc %>% mutate(Percentage = round((Freq / nrow(mens_passes)) * 100, 2))

mens_pass_acc$Category <- "Men"

#womens
womens_passes <- womens_events %>% filter(type.name == "Pass" & play_pattern.name == "Regular Play")
womens_passes$pass.outcome.name <- replace(womens_passes$pass.outcome.name, is.na(womens_passes$pass.outcome.name), "Accurate")

# fjerne alle "afleveringer" som bliver spillet ud under en skade og som er "unknown"
womens_passes <- womens_passes %>%
  filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown"))

# erstatte spilninger som er offside og går ud til at være incomplete
womens_passes <- womens_passes %>%
  mutate(pass.outcome.name = ifelse(pass.outcome.name %in% c("Out", "Pass Offside"), "Incomplete", pass.outcome.name))

womens_pass_acc <- as.data.frame(table(womens_passes$pass.outcome.name))
womens_pass_acc <- womens_pass_acc %>% mutate(Percentage = round((Freq / nrow(womens_passes)) * 100, 2))

womens_pass_acc$Category <- "Women"

pass_acc_combined <- rbind(mens_pass_acc, womens_pass_acc)

#plot
ggplot(pass_acc_combined, aes(x = Var1, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  labs(title = "Pass Accuracy vs. Incompletion by Gender",
       x = "Pass Type",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))
