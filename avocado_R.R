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

# tsibble
av_ts <- av %>% 
  filter(region == 'TotalUS') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         type = replace(type, type == 'conventional', 'Conventional'), 
         type = replace(type, type == 'organic', 'Organic')) %>% 
  arrange(Date) %>%
  select(Date, AveragePrice, type) %>% 
  rename(Type = type, Avg.Price = AveragePrice) %>%
  as_tsibble(index = Date, key = Type)
duplicates(av_ts) # Validate NoDups==

# Create tsibble to demonstrate likeness in patterns of "type" AveragePrice


av_ts # Time = 7D (Weekly)

# preVisualize
av_ts %>%
  autoplot(Avg.Price) +
  labs(title = "Average US Avocado Prices Over Time", 
       y = "Average Price ($)", 
       x = "Date")

# The plots for both avocado types follow similar patterns with the exception
# of the dip exhibited mid-2015. Run with Conventional only or model out the dip?
# How would we address the dip?
# Discussion - group choosing to go w/conventional only

# Final tsibble
av_ts <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, AveragePrice) %>% 
  rename(Avg.Price = AveragePrice) %>%
  as_tsibble(index = Date)
av_ts

av_ts %>%
  autoplot(Avg.Price)

gg_season(av_ts)
# Drop in organic avocado prices happens July 2015
# Q3 increase shown

# Time Series Decomposition----
# Decomposition methods allow us to break time series into their components trend + season + remainder
dcmp <- av_ts %>%
  model(STL(Avg.Price))
components(dcmp)

# Plot all the components
components(dcmp) %>% 
  autoplot() + 
  xlab("Date")
# Monthly?, Definite 3rd quarter, and June/July 2015 (for Organic only?)

# RESIDUALS (wanted to take a look at seasonal method, not sure if necessary here)
av_ts %>%
  model(NAIVE(Avg.Price)) %>%
  gg_tsresiduals()
# Feb/March popping up again as a potential issue (~10Wk)

# TRAIN SET
av_trn <- av_ts %>%
  filter(year(Date) < year(maxTrnDate))
av_trn # Time = 7D (Weekly)

# Train the models=
av_trn %>%
  model( SeasNaive = SNAIVE(Avg.Price ~ lag("year")),
         Naive = NAIVE(Avg.Price),
         ETS = ETS(Avg.Price), 
         logETS = ETS(log(Avg.Price)),
         Reg = TSLM(log(Avg.Price) ~ trend() + season()))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) +
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast"))

# Seasonal Naive looks the best, but needs work.
# Would like to get an ETS working, stretch before?
# Why no ETS or Reg (because regression models?)

fitav <- av_trn %>%
  # mutate(festival = month(Month) == 3 & year(Month) != 1987) %>%
  model(reg = TSLM(Avg.Price ~ trend() + season() + festival))
fitav

log_ets <- av_trn %>%
  model(logETS(log(Avg.Price)))
report(log_ets)

ets <- av_trn %>%
  model(ETS(Avg.Price))
report(ets)

########################### WORKING SECTION.... GO AWAY ########################





av_trn_dummy <- av_trn %>%
  mutate(Q3dum = quarter(Date) == 3 ,
         Avopacolypse = month(Date) == 7 & year(Date) = 2015) %>%
  model(SNAIVE(Avg.Price ~ lag("year") + season() + Q3dum + Avopacolypse))
# Colinearity???

