rm(list = ls())
# Forecast Avocado Prices

# Check wd
getwd()

library(fpp3)
library(gridExtra)

# Dataload----
av <- read.csv("C:/Users/Ayana/VCU_Work/Data_IN_OUT/avocado.csv")

# Quick Diagnostics
str(av) #DF
head(av)
table(av$type)
# type = Organic and Conventional

table(av$year)
# Years 2015 - 2018

# Look at dates and different values for use in date calculations
maxDate <- ymd(max(av$Date))
maxTrnDate <- ymd(max(av$Date)) - years(1)
fcTimeperiod <- (maxDate - ymd(format(maxTrnDate, "%Y-01-01")))/7 #calc weeks removed
fcTimeperiod #~64 weeks

# Visualize and Describe----

# Tidy
# Create tsibble of avocado data 
# Keep = TotalUS Conventional Avocado AveragePrices over time)
av_ts <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, AveragePrice) %>% 
  as_tsibble(index = Date)
av_ts

av_ts %>%
  autoplot(AveragePrice) +
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  geom_line(aes(), lwd = 0.7) +
  labs(title = "Average US Avocado Prices Over Time", 
       y = "Average Price ($)", 
       x = "Date")

# NTS: 
# Seasonality looks possible. There are Peaks (and a mound in the ~first third of the data) during the third quarter/season of the year. 
# Multiplicative over time? The increase is bigger each time.
# Looking at the plot, prices of avocados are highest during US peak growing season ,
# trailing off into the forth quarter (calendar year).
# The peak growing season is April through September, corresponding to price increasing,
# overall, during this time through the end of the third quarter.
# Question - What happened during 2015 or was this the beginning of a significant increase in
# popularity and demand of avocados? Also noting, 2018 has started at higher average prices.
# Conservatively, I would expect the same shape for 2018 data. Dip in prices in the coldest
# months (although in general the average price during that time may be higher than previous
# years (popularity increase?), then an increase (growing season), trailing off into Q4,
# beginning of US winter.
        


# TRAIN SET selection----
av_trn <- av_ts %>%
  filter(Date < maxTrnDate)
av_trn 
# Time = 7D (Weekly)
# Multiple validation check: 169 - 116, rows diff = 53 [PASS] ~52 weeks in a year)

# What if I split the set 80/20? Increase the historical points available for the training model? 
# ### Calculating an actual 80/20 train-test ratio provided more “coverage” in data points.
# Great for more info about periodicity – whatever may/not be.



# Set 80/20 of data set
av_train <-  av_ts %>% 
  filter(Date < (maxTrnDate + 133))
av_train
# Contains 80% of data (135/169 rows)

max(av_train$Date)
max(av_ts$Date) - max(av_train$Date)
# Perfect! The +133 days adds days to get to 80% of the data.


# Visualize
autoplot(av_train)

a <- gg_season(av_ts) + 
  geom_line(aes(), lwd = 0.7) +
  labs(title = "Full Data")
b <- gg_season(av_trn) + 
  geom_line(aes(), lwd = 0.7) +
  labs(title = "Split Data")
c <- gg_season(av_train) + 
  geom_line(aes(), lwd = 0.7) +
  labs(title = "Final (80/20) Train Set")

gridExtra::grid.arrange(a, b, c, ncol = 1,  top = "Representation of Data Between Sets")
# Q3 (really, sep, oct, nov) increase shown and drop in average prices each Feb (Superbowl, nice catch SamW.) 
# and May?!?
# Superbowl is plausible in a price drop probably due to an increase in volume. 
# Jan - March are peek growing seasons, prices lower not a surprise
# The US also gets a summer supplement from Peru, a winter bump from Chile
# The season runs July through April in Chile and May through August in Peru and Argentina. Maybe a Peru import ~May

# Time Series Decomposition----
# Decomposition methods allow us to break time series into their components trend + season + remainder
dcmp <- av_ts %>%
  model(STL(AveragePrice))
components(dcmp)

# Plot all the components
components(dcmp) %>% 
  autoplot() + 
  xlab("Date")
# Monthly?, Definite 3rd quarter, and June/July 2015 (for Organic only?)


# Training data and Cross-Validation----

# Train the models

# Taking a look at a few models together
# Just to see the years out and get a visual line we can look at but understanding
# the forecast will be for a short period, i.e. 2-3 weeks

av_fits_try <- av_train %>%
  model( SeasNaive = SNAIVE(AveragePrice ~ lag("year")),
         STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(AveragePrice)), ETS(season_adjust)), 
         ETS = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")), 
         searchARIMA = ARIMA(AveragePrice, stepwise = FALSE))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) + 
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast")) 

av_fits_try
# (Leaving this for funny code commentary. Apparently I am yelling at R that there is seasonality. )
# Not understanding ETS, the data is seasonal. Seasonal selections, no fc output. 


# NTS: 
# What is happening?
# Plotting model after model to try and get something to reflect on the data set that looks more than decent, these are not even that. 
# 
# Thoughts post-model training workup:
# Continue to suspect a pattern, if not seasonal then possibly cyclical.
# Of the models, two had acceptable “start” points for the forecasted region, the ETS and ARIMA. 

# Time Series Decomposition
# Decomposition methods allow us to break time series into their components trend + season + remainder
dcmp <- av_ts %>%
  model(STL(AveragePrice))
components(dcmp)
# February not surprising from gg_season() visual and Superbowl possible tie-in.

# Plot all the components
components(dcmp) %>% 
  autoplot() + 
  xlab("Date")
# Monthly?, Definite 3rd quarter, and June/July 2015 (for Organic only?)

# Compare models

# Looking at what we have to chose from, whats is best? Comparing using an auto ETS here.
av_mods_all <- av_train %>%
  model(ARIMA = ARIMA(AveragePrice, stepwise = FALSE), 
        ETS = ETS(AveragePrice), 
        ETSaan = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")),             
        SeasNaive = SNAIVE(AveragePrice ~ lag("year")),
        STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)), 
        STL.log = decomposition_model(STL(log(AveragePrice)), ETS(season_adjust))) 
accuracy(av_mods_all)
# Best MAPE = STL decomposition, but clearly something is off!

# Cross-validation - toss SeasNaive (clearly the worst) and the log-transformed (simplicity)
# and it's values are very close to the non-transformed STL
av_cv <- av_train %>%
  stretch_tsibble(.init = 25, .step = 1) # Short step ahead

av_mods <- av_train %>% 
  model( ARIMA = ARIMA(AveragePrice, stepwise = FALSE), 
         ETS = ETS(AveragePrice),
         ETSaan = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")),
         STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)),) 

av_mods %>%
  forecast(h = 2) %>%
  accuracy(av_ts)  
# ETS is the best method based upon MAPE, MAE, and RMSE.
# Also, ETS spit out as (A,N,N) - but that does not work. Forced an (A, A, N) for 
# the ETS model and it fit better than the ARIMA (remember for short forecast only) and the default ETS model.
# I could see for just that short period of time, an increase is plausible. Keeping.

# MODEL Selection----
# Moving forward, keeping ETS(AveragePrice) and will compare to ETS(A, A, N)

# Model Information
av_fit_ARIMA <- av_train %>% 
  model(ARIMA = ARIMA(AveragePrice, stepwise = FALSE))
av_fit_ARIMA
report(av_fit_ARIMA)
# Get an ARIMA(0,1,0) - Random Walk

av_fit_ETS <- av_train %>% 
  model(ETS = ETS(AveragePrice ))
av_fit_ETS
report(av_fit_ETS)

av_fit_ETSaan <- av_train %>% 
  model(ETSaan = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")))
av_fit_ETSaan
report(av_fit_ETSaan)
# Get an ETS(A,N,N) - Simple exponential smoothing with additive errors
# alpha is almost 1, just about completely leveled
# While unsuitable for long-term forecasting looks consider for a very short forecast 
# (<= 1-2 weeks, absolutely no more than 2) In my limited opinion



# Ok, so is our data Stationary?
# A stationary time series is one whose STATISTICAL properties do not depend on the time at 
# which the series is observed. Thus, time series with trends, or with seasonality, 
# are not stationary.... (Forecasting: Principles and Practice)

# FURTHERMORE...
# In general, a stationary time series will have no predictable patterns in the long-term. 
# Time plots will show the series to be roughly horizontal (although some cyclic behavior 
# is possible), with constant variance.

av_ts %>% 
  mutate(diff_price = difference(AveragePrice)) %>% 
  features(diff_price, ljung_box, lag=2)

av_ts %>%
  model(ETS(AveragePrice)) %>%
  gg_tsresiduals() + 
  labs(title = "Changes in Average US Avocado Prices")

# Potential issue (~every 10Wks), cyclical - not seasonal?!?
# The tides turn every 9/10 weeks?
# One autocorrelation is outside of the 95% limits, Ljung statistic (.864 for h =2)
# The average price change is ~random amount which is uncorrelated with the previous week.
# Short forecast only!

# Stationary or not
av_ts %>%
  features(AveragePrice, unitroot_kpss)


#need to difference! there doesn't seem to be seasonality, so let's see if a first difference passes
av_ts%>%
  features(difference(AveragePrice), unitroot_kpss)

# it does.  We would use a value of d=1 for our models in p,d,q.
av_ts %>% 
  ACF(AveragePrice, lag_max = 35) %>% 
  autoplot() + 
  labs(title = "Average US Avocado Prices - Depreciating ACF")
# Depreciating

# View the ACF and PACF of the DIFFERENCED data
av_ts %>%
  gg_tsdisplay(difference(AveragePrice), plot_type = 'partial') + 
  labs(title = "US Avocado Average Weekly Prices")
# both ACF and PACF could be described as sinusoidal.

# Use the ETS model for a short forecast.
av_ARIMA_fc <- av_ts %>%
  model(ETSaan = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")))  %>%
  forecast(h=4) %>%
  autoplot(av_ts, level = c(80,95)) + 
  labs(title = "Average US Avocado Prices - Short Forecast ", y="$US")
av_ARIMA_fc


### Rabbit Hole Next Suggestions ----
# Another look at data – 
# - i.e., Specific regions instead of TotalUS
# - look at Price ~ Volume
# - is there an updated data set we could leverage?
#   Move onto something else! Another fruit? Vegetable?

av_volume <- av %>%
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, Total.Volume) %>%
  rename(Volume = Total.Volume) %>%
  as_tsibble(index = Date)
av_volume
# TRAIN SET
vm_trn <- av_volume %>%
  filter(Date < maxTrnDate)
vm_trn
av_volume %>%
  autoplot(Volume) +
  labs(title = "Example for Next Steps: Average US Avocado Volume Over Time",
       y = "Average Volume",
       x = "Date")

# Reshape the data (i.e., monthly) 
# Another model – 
# - ARIMA(0, 2, 2)
# - consider trying a Gaussian process. There may not be seasonality but there still may be some periodicity, i.e., cyclical (every 10 weeks)
# - Periodic Autoregressive Time Series modeling






  
