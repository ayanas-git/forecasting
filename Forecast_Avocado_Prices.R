rm(list = ls())
#Ayana Andrews-Joseph----

# Check wd
getwd()

library(fpp3)
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

gg_season(av_ts)+
  labs(title = "Seasonality Assessment")
# Q3 (really, sep, oct, nov) increase shown and drop in sales each Feb and May?!?
# Jan - March are peek growing seasons, prices lower not a surprise
# The US also gets a summer supplement from Peru, a winter bump from Chile
# The season runs July through April in Chile and May through August in Peru and Argentina. Maybe a Peru import ~May


# Train the models=
av_modWork <- av_trn %>%
  model( SeasNaive = SNAIVE(Avg.Price ~ lag("year")),
         STL = decomposition_model(STL(Avg.Price), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(Avg.Price)), ETS(season_adjust)), 
         ETS = ETS(Avg.Price ~ error("A") + trend ("A") + season("N")))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) +
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast"))
# Not understanding ETS, the data is seasonal. Seasonal selections, no fc output.

av_modWork

