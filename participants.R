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

#105 participants were actually collected & I believe used in summer analyses


