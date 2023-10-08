library (dplyr)

#DRAG TASK exclude people from drag task if they have reached the 10th trial and still haven't learned it

drag_df <- df %>%
  filter(sectionType == 'mainTask' & taskName == 'networkDragTask') %>%
  select(subject, trialCount, trialAttempt, RT, nCorrect, dragThreat, dragAcc, swappedThreat, swappedAcc,
         slot0Acc, slot0CurrentType, slot0CorrectType, slot0CurrentSRC, slot0CorrectSRC, slot1Acc, 
         slot1CurrentType, slot1CorrectType,slot1CurrentSRC, slot1CorrectSRC,
         slot2Acc, slot2CurrentType, slot2CorrectType, slot2CurrentSRC, slot2CorrectSRC,
         slot3Acc, slot3CurrentType, slot3CorrectType, slot3CurrentSRC, slot3CorrectSRC,
         slot4Acc, slot4CurrentType, slot4CorrectType, slot4CurrentSRC, slot4CorrectSRC,
         slot5Acc, slot5CurrentType, slot5CorrectType, slot5CurrentSRC, slot5CorrectSRC,
         slot6Acc, slot6CurrentType, slot6CorrectType, slot6CurrentSRC, slot6CorrectSRC,
         slot7Acc, slot7CurrentType, slot7CorrectType, slot7CurrentSRC, slot7CorrectSRC,
         slot8Acc, slot8CurrentType, slot8CorrectType, slot8CurrentSRC, slot8CorrectSRC,
         slot9Acc, slot9CurrentType, slot9CorrectType, slot9CurrentSRC, slot9CorrectSRC)
drag_df


# this gets us how many trials each person had , and how many attempts they took on each attempt
drag_counts <- drag_df %>% 
  group_by(subject, trialCount) %>% 
  summarise(trialAttempts = max(trialAttempt))
drag_counts

# this gets us only those subjects who had a 10th trial, and where they still weren't getting it in 1 attempt on that trial (10 trials but got it in one attempt on that trial is ok)
excluded_drag_subjects <- drag_df %>% 
  filter(trialCount == 10 & trialAttempt > 1) %>%
  select(subject)
excluded_drag_subjects

###*idk why it mentions 2 subjects though????





#ILLEGAL TRANSITION TASK PERMUTATION-BASED EXCLUSION
illegal_df <- df %>% 
  filter(sectionType == 'mainTask' & taskName == "illegalTransitionTask") %>% 
  select(subject, transitionTpe, transitionThreatKind, trialCount, blockTrialCount, block, RT, acc, activeNodeCommunityCongruency:transitionThreatKind)
illegal_df

acc_illegal_df <- illegal_df %>%
  select(subject, transitionType, acc, transitionThreatKind) %>%
  group_by(transitionType, acc)
acc_illegal_df

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

excluded_illegal_subs <- dprime_df %>% 
  filter(dprime < 0.3573812)
excluded_illegal_subs



#ODD ONE OUT TASK exclude anyone below 40% accuracy
ooo_df <- df %>% 
  filter(taskName == "oddOneOutTest") %>% 
  select(subject, trialCount, RT, acc, partResp, chosenNode:chosenThreatStatus,
         option1CommunityNumber, option1ThreatStatus,
         option2CommunityNumber, option2ThreatStatus,
         option3CommunityNumber, option3ThreatStatus)
ooo_df

ooo_acc <- ooo_df %>% 
  group_by(subject) %>% 
  summarise(acc = mean(acc, na.rm = TRUE))
ooo_acc

excluded_ooo_subs <- ooo_acc %>%
  filter(acc < 0.4)
excluded_ooo_subs




#JOIN EXCLUDED SUBS DATA FRAMES
excluded_subjects <- excluded_drag_subjects %>% 
  full_join(excluded_illegal_subs, by="subject") %>% 
  full_join(excluded_ooo_subs, by="subject") %>% 
  select(subject)
excluded_subjects

#***check that this is right, maybe also do unique subs for first part





#DEMOGRAPHICS

demos <- read_csv('/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/SubjectInfo.csv') %>% 
  filter(! workerID %in% excluded_subjects$subject)
demos

get_sub_info <- function(demo){
  print("Sex")
  print(paste('F: ',sum(demo$Gender == 'F', na.rm=TRUE), sep=""))
  print(paste('M: ',sum(demo$Gender == 'M', na.rm=TRUE), sep=""))
  print('Age')
  print(paste('Min: ', min(demos$Age, na.rm=TRUE), sep=""))
  print(paste('Max: ', max(demos$Age, na.rm=TRUE), sep=""))
  print(paste('Mean: ', mean(demos$Age, na.rm=TRUE), sep=""))
  print(paste('SD: ', sd(demos$Age, na.rm=TRUE), sep=""))
}

get_sub_info(demos)
