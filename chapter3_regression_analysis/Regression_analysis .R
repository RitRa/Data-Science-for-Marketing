# Regression Analysis

# libraries used
# install.packages("dplyr")
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)


# read in file
df <- read.csv("~/Dropbox/Study/github/Data-Science-for-Marketing/data/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv",
               header = TRUE)

View(df)

# Encode Response as 0s and 1s
df$Engaged <- as.integer(df$Response) - 1


# Engagement Rate
engagementRate <- df %>% 
group_by(Engaged) %>% 
  summarise(Count=n()) %>%
  mutate(Percentage=Count/nrow(df)*100.0)

View(engagementRate)


# A tibble: 2 x 3
# Engaged Count Percentage
# <dbl> <int>      <dbl>
#   1       0  7826       85.7
# 2       1  1308       14.3

# Transpose
transposed <- t(engagementRate)

colnames(transposed) <- engagementRate$Engaged
transposed <- transposed[-1,]

#View(transposed)

## Sales Channels

salesChannel <- df %>% 
  group_by(Engaged, Channel=Sales.Channel) %>% 
  summarise(Count=n())


## Sales Channels engaged vs not engaged
ggplot(salesChannel, aes(x="", y=Count, fill=Channel))  + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Engaged) +
  ggtitle('Sales Channel (0: Not Engaged, 1: Engaged)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

# As you can see from these charts, more than half of the engaged customers were from agents, whereas non-engaged customers are more evenly distributed across all four different channels. 

# Total claim amounts
ggplot(df, aes(x="", y=Total.Claim.Amount)) + 
  geom_boxplot() +
  facet_wrap(~Engaged) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaged vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))


# without outliers
ggplot(df, aes(x="", y=Total.Claim.Amount)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(df$Total.Claim.Amount, c(0.1, 0.9))) +
  facet_wrap(~Engaged) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaged vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))

## Regression analysis

# get data types of each column
sapply(df, class)
# the type of the State column is "factor", which means that the variable is a categorical variable.


# summary statistics per column
summary(df)

# get numeric columns
continuousDF <- select_if(df, is.numeric)
colnames(continuousDF)

# Fit regression model with continuous variables
logit.fit <- glm(Engaged ~ ., data = continuousDF, family = binomial)


summary(logit.fit)


# Categorical variables

# a. Education
# Fit regression model with Education factor variables
logit.fit <- glm(Engaged ~ factor(Education), data = df, family = binomial)
summary(logit.fit)

# b. Education + Gender
# Fit regression model with Education & Gender variables
logit.fit <- glm(Engaged ~ factor(Education) + factor(Gender), data = df, family = binomial)

summary(logit.fit)


# Combining continuous and categorical variables
continuousDF$Gender <- factor(df$Gender)
continuousDF$Education <- factor(df$Education)

View(continuousDF)

# Fit regression model with Education & Gender variables
logit.fit <- glm(Engaged ~ ., data = continuousDF, family = binomial)
summary(logit.fit)



