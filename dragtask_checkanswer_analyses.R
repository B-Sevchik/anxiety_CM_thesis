setwd("~/Documents/GitHub/anxiety_CM_thesis")
df <- read_csv('data/dragTask/SRCaverageScores.csv')

score3_df <- df %>%
  select(src, scores3_mean, score3_sem, condition)
score3_df

score3_avg <- score3_df %>%
  group_by(condition) %>%
  summarise(
    scores3_mean = mean(scores3_mean),
    scores3_sem = mean(score3_sem)
  )
score3_avg


t.test(score3_df$scores3_mean ~ score3_df$condition, mu = 0,
       alternative = "greater",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)

