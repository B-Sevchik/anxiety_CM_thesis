library(tidyverse)
library(psycho)

#path reference
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/combinedData_Anxiety_Cognitive_Maps.csv' 

#load data
df <- read_csv(data_path)
illegal_df <- df %>%
  filter(taskName == 'illegalTransitionTask')

acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc, transitionThreatKind) %>%
  group_by(transitionType, acc)
acc_illegal_df
write_csv(acc_illegal_df, 'data/illegalTransitionTask/acc_illegal_df.csv')

transition_type_df <- acc_illegal_df %>%
  mutate(acc_descriptor = case_when(
    (transitionType == 'i' & acc == 1) ~ "hits",
    (transitionType == 'i' & acc == 0) ~ "misses",
    (transitionType == 'l' & acc == 0) ~ "false_alarms",
    (transitionType == 'l' & acc == 1) ~ "correct_rejections"
  ))
transition_type_df

transition_type_df <- transition_type_df %>%
  mutate(transition_threat = case_when(
    (transitionThreatKind == 'threat-threat' | transitionThreatKind == 'threat-neutral' | transitionThreatKind == 'neutral-threat') ~ "contains_threat",
    (transitionThreatKind == 'neutral-neutral') ~ "no_threat"
  ))
transition_type_df

counts_transitionType_df <- transition_type_df %>%
  group_by(subject, acc_descriptor, transition_threat) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = acc_descriptor, values_from = count, values_fill = 0)
counts_transitionType_df

dprime_df <- counts_transitionType_df
dprime.stats <-psycho::dprime(counts_transitionType_df$hits, counts_transitionType_df$false_alarms, counts_transitionType_df$misses, counts_transitionType_df$correct_rejections)
dprime_df$dprime <- dprime.stats$dprime
dprime_df
write_csv(dprime_df, 'data/illegalTransitionTask/dprime_df.csv')



###
### include STAI

#include STAI scores in analyses

df_STAI_illegal <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/STAI_scores_calculated.csv')
df_STAI_illegal

df_pilot1_STAI_simple <- df_STAI_illegal %>%
  select(subjectID, anxiety_level)
df_pilot1_STAI_simple

names(df_pilot1_STAI_simple) <- c('subject', 'anxiety_level')
df_pilot1_STAI_simple

illegal_anxiety_df <- merge(dprime_df, df_pilot1_STAI_simple, by = "subject", all = TRUE)
illegal_anxiety_df
write_csv(illegal_anxiety_df, 'data/illegalTransitionTask/illegal_anxiety_df.csv')
