library (dplyr)
library(tidyverse)

df <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/combinedData_Anxiety_Cognitive_Maps.csv')

#participants from box
participants_df <- df %>%
  select(subject)
participants_df

unique_participants_df <- unique(participants_df$subject)
num_unique_participants <- length(unique_participants_df)

num_unique_participants

unique_participants_df <- data.frame(subject = unique(participants_df$subject))
unique_participants_df

#105 participants were actually collected

participant_info_df <- read_csv('/Users/brookesevchik/Downloads/Participants_CSV.csv')
participant_info_df

#change column names
colnames(participant_info_df)[colnames(participant_info_df) == "anxiety score"] <- "anxiety_score"
colnames(participant_info_df)[colnames(participant_info_df) == "Worker ID"] <- "subject"
participant_info_df

participant_info_df <- participant_info_df %>%
  select('subject', 'anxiety_score') %>%
  mutate(anxiety_split_score = case_when(
    anxiety_score %in% c("low", "moderate") ~ "low to moderate",
    anxiety_score == "high" ~ "high"
  ))
participant_info_df

participant_lm_df <- participant_info_df %>%
  filter(anxiety_split_score == 'low to moderate')
participant_lm_df
num_lm <- nrow(participant_lm_df)
num_lm

participant_h_df <- participant_info_df %>%
  filter(anxiety_split_score == "high")
participant_h_df
num_h <- nrow(participant_h_df)
num_h



