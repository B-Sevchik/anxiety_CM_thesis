library(plotrix)

check_answer_df <- read_csv(...)

# finding trial attempts by trial and subject
trialAttemptsByTrial <- check_answer_df %>% 
  group_by(subject, trialCount) %>% 
  summarize(n_attempts = max(trialAttempt))

included_sub_trials <- trialAttemptsByTrial %>% 
  filter(trialCount != 1 & n_attempts != 1) %>% 
  mutate(combined = paste(subject, trialCount, sep="_"))
#25 subjects usable

#list of column names for easy reference
correct_type_columns <- colnames(check_answer_df)[grepl("CorrectType", colnames(check_answer_df))]
correct_src_columns <- colnames(check_answer_df)[grepl("CorrectSRC", colnames(check_answer_df))]
current_src_columns <- colnames(check_answer_df)[grepl("CurrentSRC", colnames(check_answer_df))]
current_type_columns <- colnames(check_answer_df)[grepl("CurrentType", colnames(check_answer_df))]
acc_columns <- colnames(check_answer_df)[grepl("Acc", colnames(check_answer_df))]

#remove columns we don't need
new_check_answer_df <- check_answer_df  %>% 
  select(-correct_type_columns) %>% 
  select(-correct_src_columns) %>% 
  select(-RT) %>% 
  select(-nCorrect) %>% 
  filter(paste(subject, trialCount, sep="_") %in% included_sub_trials$combined)

slot_accuracies <- new_check_answer_df %>% 
  select(-current_src_columns) %>% 
  select(-current_type_columns) %>%
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "acc") %>% 
  mutate(slot = gsub("Acc", "", slot))

slot_srcs <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_type_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "src") %>% 
  mutate(slot = gsub("CurrentSRC", "", slot))

slot_threat_types <- new_check_answer_df %>% 
  select(-acc_columns) %>% 
  select(-current_src_columns) %>% 
  pivot_longer(cols = !c(subject, trialCount, trialAttempt), names_to = "slot", values_to = "threatType") %>% 
  mutate(slot = gsub("CurrentType", "", slot))

#join everything together
slot_images <- slot_srcs %>% 
  left_join(slot_threat_types, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  left_join(slot_accuracies, by=c("subject", "trialCount", "trialAttempt", "slot")) %>% 
  mutate(src = gsub(".jpg", "", src)) %>% 
  mutate(src = paste(src, "_", threatType, ".jpg", sep="")) %>% 
  select(-threatType)

# image_new_names = tibble(
#   src = unique(slot_images$src),
#   new_names = c(1:14) # <- this needs to be a handtyped list of e.g., snake.jpg, dog.jpg, etc, that corresponds to each row of images
# )
# 
# slot_images <- slot_images %>% 
#   left_join(image_new_names, by="src") %>% 
#   mutate(src = new_names) %>% 
#   select(-new_names)

slot_images

#score 1
#next step: use the summarise function on slot_images to calculate, for each (group by) unique subject, trialCount, and src,
#the sum of correct placements (sum of acc column) across trial attempts. 
sum_correct_trials_df <- slot_images %>%
  group_by(subject, trialCount, src) %>%
  summarise(sum_correct_trials = sum(acc))
sum_correct_trials_df

#SCORE 2
first_correct_df <- slot_images %>% 
  # filter(acc == 1 & subject == "a2vo8c41jjiqy9" & trialCount == 2) %>% 
  filter(acc == 1) %>% 
  group_by(subject, trialCount, src) %>% 
  summarise(first_correct = first(trialAttempt))

#SCORE 3

first_correct_no_further_mistakes_df <- slot_images %>% 
  filter(acc == 1) %>% 
  group_by(subject, trialCount, src) %>%
  mutate(trialAttempt_lag = lag(trialAttempt)) %>% 
  mutate(no_skip = ifelse(trialAttempt_lag == trialAttempt - 1, TRUE, FALSE),
         no_skip = ifelse(is.na(no_skip), FALSE, no_skip)) %>% 
  filter(no_skip == FALSE) %>% 
  summarise(first_correct_no_further_mistakes = last(trialAttempt))

#join together all scores
scores <- sum_correct_trials_df %>% 
  left_join(first_correct_df, by = c("subject", "trialCount", "src")) %>% 
  left_join(first_correct_no_further_mistakes_df, by = c("subject", "trialCount", "src"))

#next step after that, group by subject and src (collapse across trials) to find the average accuracy score for each trial
average_scores <- scores %>%
  group_by(subject, src) %>%
  summarise(n_trials = n(),
            score1 = mean(sum_correct_trials), 
            score2 = mean(first_correct),
            score3 = mean(first_correct_no_further_mistakes))
average_scores

#last step, group by just src to find mean accuracy score for each 
src_average_scores <- average_scores %>%
  group_by(src) %>%
  summarise(scores1_mean = mean(score1), 
            score1_sem = std.error(score1),
            scores2_mean = mean(score2), 
            score2_sem = std.error(score2),
            scores3_mean = mean(score3), 
            score3_sem = std.error(score3)) %>% 
  mutate(condition = ifelse(grepl("threat", src), "threat", "neutral"))
src_average_scores

ggplot(src_average_scores, aes(x = condition, y = scores3_mean, fill=condition)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter()
