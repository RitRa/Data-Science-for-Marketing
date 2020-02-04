# Conversion Rate

# import data
df <- read.csv("~/Dropbox/Study/github/Data-Science-for-Marketing/data/bank-additional/bank-additional-full.csv",
               header = TRUE, 
               sep=";")

# Encode conversions as 0s and 1s
df$conversion <- as.integer(df$y) - 1

View(df)


# total  number of conversions
sum(df$conversion)

# total number of clients in the data (= number of records in the data)
nrow(df)

### 1. Aggregate Conversion Rate ####
sprintf("total conversions: %i out of %i", sum(df$conversion), nrow(df))
#  "total conversions: 4640 out of 41188"


### 2. Conversion Rate
sprintf("conversion rate: %0.2f%%", sum(df$conversion)/nrow(df)*100.0)


### Conversion rates by age
conversionsByAge <- df %>% 
  group_by(Age=age) %>% 
  summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)

#View(conversionsByAge)


# line chart of conversion by age
ggplot(data=conversionsByAge, aes(x=Age, y=ConversionRate)) +
  geom_line() +
  ggtitle('Conversion Rates by Age') +
  xlab("Age") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5))


# age groups
conversionsByAgeGroup <- df %>% 
  group_by(AgeGroup=cut(age, breaks=seq(20, 70, by = 10)) ) %>% 
  summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)

conversionsByAgeGroup$AgeGroup <- as.character(conversionsByAgeGroup$AgeGroup)
conversionsByAgeGroup$AgeGroup[6] <- "70+"


# bar chart of age groups
ggplot(conversionsByAgeGroup, aes(x=AgeGroup, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Conversion Rates by Age Groups') +
  xlab("Age") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5))



conversionsByMaritalStatus <- df %>% 
  group_by(Marital=marital, Conversion=conversion) %>% 
  summarise(Count=n())


# pie chart of martial status and conversions
ggplot(conversionsByMaritalStatus, aes(x="", y=Count, fill=Marital)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Marital Status (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

