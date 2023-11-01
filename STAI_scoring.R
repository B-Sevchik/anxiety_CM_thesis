#set-up
setwd("~/Documents/GitHub/anxiety_CM_thesis")
library(tidyverse)
library(dplyr)

#BE SURE TO EDIT PATH NAMES EACH TIME YOU RUN SCRIPT

#path references
data_path <- '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/STAIscores.csv' #EDIT PATH NAME EACH TIME

#load data
STAI_df <- read_csv(data_path)

#manipulate df into proper format
colnames(STAI_df) <- c('not_included', 'subjectID', paste0('s', 1:20))
num_rows = nrow(STAI_df)
num_rows

STAI_df <- STAI_df %>%
  slice(2:num_rows) %>%
  select('subjectID', 's1', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 's11', 's12', 's13', 's14', 's15', 's16', 's17', 's18', 's19', 's20')

#getting STAI scores into numeric form
STAI_df <- transform(STAI_df,s1 = as.numeric(s1))
STAI_df <- STAI_df %>%
  mutate(across(paste0('s', 1:20), as.numeric))

#changing non-responses (5) into NaN
STAI_df <- STAI_df %>%
  mutate(across(paste0('s', 1:20), ~ case_when(. == 5 ~ 0, 
                                               TRUE ~ .)))

#reverse score what needs to be reverse scored
STAI_df <- STAI_df %>%
  group_by(subjectID) %>%
  mutate(s1 = ifelse(s1== 0, 0, 5 - s1),
         s3 = ifelse(s3== 0, 0, 5 - s3),
         s6 = ifelse(s6== 0, 0, 5 - s6),
         s7 = ifelse(s7== 0, 0, 5 - s7),
         s10 = ifelse(s10== 0, 0, 5 - s10),
         s13 = ifelse(s13== 0, 0, 5 - s13),
         s14 = ifelse(s14== 0, 0, 5 - s14),
         s16 = ifelse(s16== 0, 0, 5 - s16),
         s19 = ifelse(s19== 0, 0, 5 - s19))


#get the mean & replace NaNs(0s) w the mean 
STAI_df <- STAI_df %>%
  mutate(meanVals = (s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20)/ 20) %>%
  mutate(across(paste0('s', 1:20), ~ case_when(. == 0 ~ meanVals, 
                                               TRUE ~ .)))

#sum STAI score
STAI_df <-STAI_df %>%
  group_by(subjectID) %>%
  mutate(sumVals = s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20)

#classify anxiety levels based on STAI sum
STAI_df <- STAI_df %>%
  group_by(subjectID) %>%
  mutate(anxiety_level = case_when(
    sumVals <= 37 ~ 'low trait anxiety',
    sumVals >= 38 & sumVals < 44 ~ 'moderate trait anxiety',
    sumVals >= 44 ~ 'high trait anxiety'
  ))
STAI_df

#save out the file
write.csv(STAI_df, 'data/STAI.csv')
write.csv(STAI_df, '/Users/brookesevchik/Box/Data/Anxiety_Cognitive_Maps/all_participants/STAI_scores_calculated.csv') #EDIT PATH NAME EACH TIME


