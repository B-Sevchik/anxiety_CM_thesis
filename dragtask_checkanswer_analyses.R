setwd("~/Documents/GitHub/anxiety_CM_thesis")
df <- read_csv('data/dragTask/SRCaverageScores.csv')

score3_df <- df %>%
  select(src, scores3_mean, score3_sem, condition)
score3_df
write_csv(score3_df, 'data/dragTask/score3_df.csv')

score3_avg <- score3_df %>%
  group_by(condition) %>%
  summarise(
    scores3_mean = mean(scores3_mean),
    scores3_sem = mean(score3_sem)
  )
score3_avg
write_csv(score3_avg, 'data/dragTask/score3_avg.csv')

ggplot(src_average_scores, aes(x = condition, y = scores3_mean, fill=condition)) +
  geom_boxplot(alpha=0.5) +
  geom_jitter()

t.test(score3_df$scores3_mean ~ score3_df$condition, mu = 0,
       alternative = "greater",
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)

