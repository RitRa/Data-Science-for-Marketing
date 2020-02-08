# Decision trees and interpretations with R

#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("rpart")
install.packages("rattle")

library(dplyr)
library(ggplot2)
# decision tree models
library(rpart)
# insights
library(rattle)

# import data
df <- read.csv("~/Dropbox/Study/github/Data-Science-for-Marketing/data/bank/bank-full.csv",
               header = TRUE, 
               sep=";")
View(df)

# Data analysis and visualizations


# Encode conversions as 0s and 1s
df$conversion <- as.integer(df$y) - 1

# Conversion rate

sprintf("Conversion rate: %0.2f%%", sum(df$conversion)/nrow(df)*100.0)

# Conversion rates by job
conversionsByJob <- df %>% 
  group_by(Job=job) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

# bar plot
ggplot(conversionsByJob, aes(x=Job, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  coord_flip() +
  ggtitle('Conversion Rates by Job') +
  xlab("Job") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Default rates by conversions

defaultByConversion <- df %>% 
  group_by(Default=default, Conversion=conversion) %>% 
  summarise(Count=n())

ggplot(defaultByConversion, aes(x="", y=Count, fill=Default)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Default (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )


# Bank balance by conversions
ggplot(df, aes(x="", y=balance)) + 
  geom_boxplot() +
  facet_wrap(~conversion) +
  ylab("balance") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Balance") +
  theme(plot.title=element_text(hjust=0.5))


# remove outliers
ggplot(df, aes(x="", y=balance)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(-2000, 5000)) +
  facet_wrap(~conversion) +
  ylab("balance") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Balance") +
  theme(plot.title=element_text(hjust=0.5))

# Conversion rates by number of contacts

conversionsByNumContacts <- df %>% 
  group_by(Campaign=campaign) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

ggplot(conversionsByNumContacts, aes(x=Campaign, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Conversion Rates by Number of Contacts') +
  xlab("Number of Contacts") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5))


### Encoding categorical variables
unique(df$month)

# convert months to numbers
months = lapply(month.abb, function(x) tolower(x))
months

# test
match(unique(df$month), months)

# encode
df$month <- match(df$month, months)
# check
df %>% 
  group_by(month) %>% 
  summarise(Count=n())

## Encoding the job, housing, and marital variables
df$job <- factor(df$job)
df$housing <- factor(df$housing)
df$marital <- factor(df$marital)

fit <- rpart(
  conversion ~ age +balance +campaign + previous +housing +job +marital,
  method="class",
  data=df,
  control=rpart.control(maxdepth = 4, cp=0.0001)
)

# visualise
fancyRpartPlot(fit)