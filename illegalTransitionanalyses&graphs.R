###BASIC MANIPULATION

#create just the STAI score and subject and anxiety level
STAI_correlation_df <- STAI_correlation_df <- read_csv('/Users/brookesevchik/Documents/GitHub/anxiety_CM_thesis/data/STAI.csv') %>%
  select('subjectID', 'sumVals')
STAI_correlation_df

#change column name 'subjectID' to 'subject' so can merge eventually
colnames(STAI_correlation_df)[1] <- "subject"
STAI_correlation_df

####illegal transition 

#load df with stuff you need
illegal_anxiety_df #make sure you run the NEW illegal transition analysis before this to separate by each threatKind

#merge the two dfs together to get the df you need for this linear correlation
illegal_anxiety_correlation_df <- merge(illegal_anxiety_df, STAI_correlation_df, by = "subject", all = TRUE)
illegal_anxiety_correlation_df
write_csv(illegal_anxiety_correlation_df, 'data/illegalTransitionTask/illegal_anxiety__correlation_df.csv')


#create a df for each kind of threat or no threat
illegal_anxiety_correlation_df_threat <- illegal_anxiety_correlation_df %>%
  filter(transition_threat == 'contains_threat')
illegal_anxiety_correlation_df_threat
write_csv(illegal_anxiety_correlation_df_threat, 'data/illegalTransitionTask/illegal_anxiety_correlation_df_threat.csv')

illegal_anxiety_correlation_df_noThreat <- illegal_anxiety_correlation_df %>%
  filter(transition_threat == 'no_threat')
illegal_anxiety_correlation_df_noThreat
write_csv(illegal_anxiety_correlation_df_noThreat, 'data/illegalTransitionTask/illegal_anxiety_correlation_df_noThreat.csv')



#linear model stuff for correlation - mean_acc and sumVals
#initial model

#contains threat
linear_model_illegal_threat <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_threat)
summary(linear_model_illegal_threat)

#no threat
linear_model_illegal_noThreat <- lm(dprime ~ sumVals, data = illegal_anxiety_correlation_df_noThreat)
summary(linear_model_illegal_noThreat)



#SCATTERPLOT THREAT
# Extract sumVals and dprime variables
x <- illegal_anxiety_correlation_df_threat$sumVals
y <- illegal_anxiety_correlation_df_threat$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_threat)

# Create scatter plot with line of best fit
plot(x, y, main = "Threat Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_threat, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_threat) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "Threat Scatterplot", x = "sumVals", y = "dprime")



#SCATTERPLOT NO THREAT
# Extract sumVals and dprime variables
x <- illegal_anxiety_correlation_df_noThreat$sumVals
y <- illegal_anxiety_correlation_df_noThreat$dprime

# Generate predicted values
predicted <- predict(linear_model_illegal_noThreat)

# Create scatter plot with line of best fit
plot(x, y, main = "No Threat Scatterplot", xlab = "sumVals", ylab = "dprime", pch = 16, col = "blue")
abline(linear_model_illegal_noThreat, col = "red", lwd = 2)

#create same scatter plot using ggplot (matter of preference)
library(ggplot2)

ggplot(data = illegal_anxiety_correlation_df_threat) +
  geom_point(aes(x = sumVals, y = dprime), color = "blue", size = 3) +
  geom_smooth(aes(x = sumVals, y = dprime), method = "lm", se = FALSE, color = "red", lwd = 2) +
  labs(title = "No Threat Scatterplot", x = "sumVals", y = "dprime")






#i don't know if i need to do this...

#evaluating assumptions - I honestly don't think these are met!!!! - haven't looked at these
ggplot(data = mod1, mapping = aes(x = mod1$residuals)) + 
  geom_histogram(color = "darkblue", 
                 fill = "lightgray",
                 binwidth = 1) + 
  labs(x = "Residuals", y = "Count",
       title = "Residuals are slightly left-skewed") + 
  theme_bw()
ggplot(data = mod1, mapping = aes(x = mod1$fitted.values, y = mod1$residuals)) + 
  geom_point() + 
  labs(x = "Fitted values", y = "Residuals",
       title = "Residual plot shows no clear pattern") +
  geom_hline(yintercept = 0, color = "red")


