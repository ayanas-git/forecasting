rm(list = ls())

library(fpp3)
library(gridExtra)
av <- read.csv("C:/Users/Ayana/VCU_Work/Data_IN_OUT/avocado.csv")

# Dataload----
str(av) #DF
head(av)
table(av$type)
# type = Organic and Conventional

table(av$year)
# Years 2015 - 2018
maxDate <- ymd(max(av$Date))
maxTrnDate <- ymd(max(av$Date)) - years(1)
fcTimeperiod <- (maxDate - ymd(format(maxTrnDate, "%Y-01-01")))/7 #calc weeks removed
fcTimeperiod #~64 weeks

# Tidy----

# Create tsibble of avocado data 
# Keep = TotalUS Conventional Avocado AveragePrices over time)
av_ts <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, AveragePrice) %>% 
  rename(Avg.Price = AveragePrice) %>%
  as_tsibble(index = Date)
av_ts


# TRAIN SET
av_trn <- av_ts %>%
  filter(Date < maxTrnDate)
av_trn 
# Time = 7D (Weekly)
# Multiple validation check: 169 - 116, rows diff = 53 [PASS] ~52 weeks in a year)

av_ts %>%
  autoplot(Avg.Price) +
  labs(title = "Average US Avocado Prices Over Time", 
       y = "Average Price ($)", 
       x = "Date")
# Seasonality (Y), Multiplicative?

av_trn %>%
  autoplot(log(Avg.Price))
# What if I split the set 80/20? Increase the historical points available for the 
# training model. The plot of the training set may/may not have 

av_train <-  av_ts %>% 
  filter(Date < (maxTrnDate + 133))
max(av_train$Date)
max(av_ts$Date) - max(av_train$Date)
# Perfect! The +133 days adds days to get to 80% of the data.
av_train
# Contains 80% of data (135/169 rows)

# Visualize
autoplot(av_train)

a <- gg_season(av_ts)+
  labs(title = "Full Data")
b <- gg_season(av_trn)+
  labs(title = "Split Data")
c <- gg_season(av_train)+
  labs(title = "Final (80/20) Train Set")

gridExtra::grid.arrange(a, b, c, ncol = 1,  top = "Representation of Data Between Sets")
# Q3 (really, sep, oct, nov) increase shown and drop in sales each Feb and May?!?
# Jan - March are peek growing seasons, prices lower not a surprise
# The US also gets a summer supplement from Peru, a winter bump from Chile
# The season runs July through April in Chile and May through August in Peru and Argentina. Maybe a Peru import ~May


# Train the models