rm(list = ls())
#Ayana Andrews-Joseph----
# Forecast Avocado Prices

# Check wd
getwd()

library(fpp3)
library(gridExtra)

av <- read.csv("C:/Users/Ayana/VCU_Work/Data_IN_OUT/avocado.csv")

# Dataload----

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
# Q3 (really, sep, oct, nov) increase shown and drop in sales each Feb and May?!?
# Jan - March are peek growing seasons, prices lower not a surprise
# The US also gets a summer supplement from Peru, a winter bump from Chile
# The season runs July through April in Chile and May through August in Peru and Argentina. Maybe a Peru import ~May



# Training data and Cross-Validation----

# Train the models

# Taking a look at a few models together
# Just to see the years out and get a visual line we can look at but understanding
# the forecast will be for a short period, i.e. 2-3 weeks

av_fit <- av_train %>%
  model( SeasNaive = SNAIVE(AveragePrice ~ lag("year")),
         STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(AveragePrice)), ETS(season_adjust)), 
         ETS = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")), 
         searchARIMA = ARIMA(AveragePrice, stepwise = FALSE))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) + 
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast")) 

av_fit

# Not understanding ETS, the data is seasonal. Seasonal selections, no fc output. 
# (Leaving this for funny code commentary. Apparently I am yelling at R that there is seasonality. )

# NTS: 
# What is happening?
# Plotting model after model to try and get something to reflect on the data set that looks more than decent, these are not even that. 
# 
# Thoughts post-model training workup:
# Continue to suspect a pattern, if not seasonal then possibly cyclical.
# Of the models, two had acceptable “start” points for the forecasted region, the ETS and ARIMA. 


# MODEL Selection + Bonus Work Continued
##################################################################

# Moving forward, keeping ETS and ARIMA for acceptable comparisons 
# Even though the other models had some shape (forcing them to account for seasonality) 
# it is too far off. The final forecaset period, may be very short. 

# Model Trys
av_fit_ARIMA <- av_train %>% 
  model(searchARIMA = ARIMA(AveragePrice, stepwise = FALSE))
av_fit_ARIMA
report(av_fit_ARIMA)
# Get an ARIMA(0,1,0) - Random Walk

av_fit_ETS <- av_train %>% 
  model(ETS = ETS(AveragePrice))
av_fit_ETS
report(av_fit_ETS)
# Get an ETS(A,N,N) - Simple exponential smoothing with additive errors
# alpha is almost 1, just about completely leveled


av_fit_ETSaan <- av_train %>% 
  model(ETSaan = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")))
av_fit_ETSaa
report(av_fit_ETSann)
# ETS(A,A,N) Holt’s method with additive errors is plotting
# While unsuitable for long-term forecasting looks ok for a very short forecast 
# (<= 2-3 weeks, absolutely no more than 4) In my limited opinion

# Compare models
#we can smooth out the data to make it less noisy using a Moving Average (MA)
#we do that using the slide_dbl function
av_fit <- av_train %>%
  model( SeasNaive = SNAIVE(AveragePrice ~ lag("year")),
         STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(AveragePrice)), ETS(season_adjust)), 
         ETS = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")), 
         searchARIMA = ARIMA(AveragePrice, stepwise = FALSE))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) + 
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast")) 

av_fit

# Looking at what we have to chose from, whats is best?
av_fits <- av_train %>%
  model( SeasNaive = SNAIVE(AveragePrice ~ lag("year")),
         STL = decomposition_model(STL(AveragePrice), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(AveragePrice)), ETS(season_adjust)), 
         ETS = ETS(AveragePrice ~ error("A") + trend ("A") + season("N")), 
         searchARIMA = ARIMA(AveragePrice, stepwise = FALSE)) 
accuracy(av_fits)

# Ok, so is our data Stationary?
# A stationary time series is one whose STATISTICAL properties do not depend on the time at 
# which the series is observed. Thus, time series with trends, or with seasonality, 
# are not stationary.... (Forecasting: P rinciples and Practice)

# FURTHERMORE...
# In general, a stationary time series will have no predictable patterns in the long-term. 
# Time plots will show the series to be roughly horizontal (although some cyclic behavior 
# is possible), with constant variance.

av_ts %>% 
  mutate(diff_price = difference(AveragePrice)) %>% 
  features(diff_price, ljung_box, lag=2)

av_ts %>%
  model(ETS(Avg.Price)) %>%
  gg_tsresiduals()

av_ts %>%
  model(ARIMA(AveragePrice)) %>%
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

# it does.  We will use a value of d=1 for our models.
av_ts %>% 
  ACF(AveragePrice, lag_max = 35) %>% 
  autoplot() + 
  labs(title = "Average US Avocado Prices - Depreciating ACF")
# Depreciating

# let's view the ACF and PACF of the DIFFERENCED data
av_ts %>%
  gg_tsdisplay(difference(AveragePrice), plot_type = 'partial') + 
  labs(title = "US Avocado Average Weekly Prices")

# both ACF and PACF could be described as sinusoidal.

#let's also add in some automatic models 
av_arima_fits <- av_ts %>%
  model(arima010 = ARIMA(AveragePrice ~ pdq(0,1,0)),
        stepwise = ARIMA(AveragePrice),
        bestARIMA = ARIMA(AveragePrice, stepwise = FALSE)
  )
# First two are exactly the same, stepwise = TRUE (default)
av_arima_fits %>% pivot_longer(names_to = "Model name", values_to = "AveragePrice")

accuracy(av_arima_fits)
glance(av_arima_fits)

# Use the "bestARIMA" model for a short forecast.
av_ARIMA_fc <- av_ts %>%
  model(ARIMA = ARIMA(AveragePrice, stepwise = FALSE))  %>%
  forecast(h=4) %>%
  autoplot(av_ts, level = NULL) + 
  labs(title = "Average US Avocado Prices - Short Forecast ", y="$US") +
  guides(colour=guide_legend(title="Forecast")) 
av_ARIMA_fc

#######
# TIME PERMITTING

# One more try at a model. ARIMA was good, with potential cyclical activity trying Gaussian method a 
# type of autoregression (periodic)?

library(GauPro)


# (X, y)
av_gp <- GauPro(av_train$Date, av_train$Avg.Price, parallel = FALSE)
# Testing if worked, prediction for 1.15
predict(av_gp,1.15) # The predictor works

plot(av_train$Date, av_train$Avg.Price)
curve(av_gp$predict(av_train$Date), add = TRUE, col=2)


#####
library(GPfit)

av_gp <- GP_fit(av_train$Date, av_train$Avg.Price)

plot(av_gp)





# Monthly data try

# Tidy
# Create tsibble of avocado data 
# Keep = TotalUS Conventional Avocado AveragePrices over time)

# datStart = min(av_ts$Date) 2015-01-01

library(wktmo)
av$MonthYr <- strftime((av$Date), format = "%Y-%m")
table(av$MonthYr)

av_ts <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, AveragePrice) %>% 
  rename(Avg.Price = AveragePrice) %>%
  as_tsibble(index = Date)

av_ts <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"), 
         MonthYr = yearmonth(MonthYr, format = "%Y-%m"),
         AvgPrice = mean(AveragePrice)) %>%
  arrange(MonthYr) %>%
  select(MonthYr, AveragePrice) %>% 
  rename(Avg.Price = AveragePrice) %>%
  as_tsibble(index = MonthYr)


av_ts %>%
  autoplot(Avg.Price) +
  labs(title = "Average US Avocado Prices Over Time", 
       y = "Average Price ($)", 
       x = "Date")
# Seasonality (Y), Multiplicative over time?
# IT IS CYCLICAL... NOT SEASONAL! .... Hmmmmmmmm.


##########################################################################
av_modWork <- av_train %>%
  model( SeasNaive = SNAIVE(Avg.Price ~ lag("year")),
         STL = decomposition_model(STL(Avg.Price), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(Avg.Price)), ETS(season_adjust)), 
         ETS = ETS(Avg.Price ~ error("A") + trend ("A") + season("N")))
av_modWork
#view fitted values and residuals
augment(av_modWork)

#forecast
av_fc_all <- av_modWork %>%
  forecast(h = 104) #Doing it 104 time periods out, 104 time periods = 2 years
av_fc_all 

##########################################################################

# Reshape data to monthly, give it a go (ETS, ARIMA)





####################### TAKE A LOOK AT REG ###################


#Plot on the y consumption and income overlapping each other
av_ty <- av %>% 
  filter(region == 'TotalUS', type == 'conventional') %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date) %>%
  select(Date, AveragePrice, Total.Volume) %>% 
  rename(Avg.Price = AveragePrice) %>%
  as_tsibble(index = Date)
av_ty

av_ty %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Avg.Price, colour = "Price")) +
  geom_line(aes(y = log(Total.Volume), colour = "Vol")) +
  labs(x = "Year", y ="% change") +
  guides(colour=guide_legend(title="series"))


# You can see the relationship in a scatter plot (this is the preferred plot to view before a regression)
av_ty %>%
  ggplot(aes(x = Total.Volume, y = Avg.Price)) +
  labs(x = "Vol", y ="Price") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) #this line provides us with a simple linear regression of Consumption vs Income


# The tslm function fits a regression model with time series data
fit_consMR1 <- av_ty %>%
  model(TSLM(Total.Volume ~ Avg.Price)) 

report(fit_consMR1)

model()