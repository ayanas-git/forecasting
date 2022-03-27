rm(list = ls())
#Ayana Andrews-Joseph----
# Forecast Avocado Prices

# Check wd
getwd()

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

# Visualize and Describe----

# Tidy
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

av_ts %>%
  autoplot(Avg.Price) +
  labs(title = "Average US Avocado Prices Over Time", 
       y = "Average Price ($)", 
       x = "Date")
# Seasonality (Y), Multiplicative over time?
# Looking at the plot, prices of avocados are highest during US peak growing season ,
# trailing off into the forth quarter (calendar year). 
# The peak growing season is April through September, corresponding to price increasing,
# overall, during this time through the end of the third quarter. 
# Question - What happened during 2015 or was this the beginning of an increase in 
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

# What if I split the set 80/20? Increase the historical points available for the 
# training model. The plot of the training set may/may not have 

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


# Training data and Cross-Validation----

# Train the models




# MODEL Selection
##################################################################

# Model Trys
av_fit_ARIMA <- av_train %>% 
  model(searchARIMA = ARIMA(Avg.Price, stepwise = FALSE))
av_fit_ARIMA
report(av_fit_ARIMA)
# Get an ARIMA(0,1,0) - Random Walk

av_fit_ETS <- av_train %>% 
  model(ETS = ETS(Avg.Price))
av_fit_ETS
report(av_fit_ETS)
# Get an ETS(A,N,N) - Simple exponential smoothing with additive errors
# alpha is almost 1, just about completely leveled


av_fit_ETSaan <- av_train %>% 
  model(ETSaan = ETS(Avg.Price ~ error("A") + trend ("A") + season("N")))
av_fit_ETSaa
report(av_fit_ETSann)
# ETS(A,A,N) Holt’s method with additive errors is plotting
# While unsuitable for long-term forecasting looks ok for a very short forecast 
# (<= 2-3 weeks, absolutely no more than 4) In my limited opinion

av_fit_STL <- av_train %>% 
  model(STL = decomposition_model(STL(Avg.Price)))
av_fit_STL
report(av_fit_STL)


# Ok, so is our data Stationary?
# A stationary time series is one whose STATISTICAL properties do not depend on the time at 
# which the series is observed. Thus, time series with trends, or with seasonality, 
# are not stationary.... (Forecasting: P rinciples and Practice)

# FURTHERMORE...
# In general, a stationary time series will have no predictable patterns in the long-term. 
# Time plots will show the series to be roughly horizontal (although some cyclic behavior 
# is possible), with constant variance.

av_ts %>% 
  mutate(diff_price = difference(Avg.Price)) %>% 
  features(diff_price, ljung_box, lag=2)

av_ts %>%
  model(ETS(Avg.Price)) %>%
  gg_tsresiduals()

av_ts %>%
  model(ARIMA(Avg.Price)) %>%
  gg_tsresiduals() + 
  labs(title = "Changes in Average US Avocado Prices")
# Potential issue (~every 10Wks), cyclical - not seasonal?!?
# The tides turn every 9/10 weeks?
# One autocorrelation is outside of the 95% limits, Ljung statistic (.864 for h =2)
# The average price change is ~random amount which is uncorrelated with the previous week.
# Short forecast only!

av_ts %>% 
  ACF(Avg.Price, lag_max = 10) %>% 
  autoplot() + 
  labs(title = "Average US Avocado Prices")
# Depreciating

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


# Taking a look at a few models together
# Just to see the years out and get a visual line we can look at but understanding
# the forecast will be for a short period, i.e. 2-3 weeks

av_fit <- av_train %>%
  model( SeasNaive = SNAIVE(Avg.Price ~ lag("year")),
         STL = decomposition_model(STL(Avg.Price), ETS(season_adjust)), 
         STL.log = decomposition_model(STL(log(Avg.Price)), ETS(season_adjust)), 
         ETS = ETS(Avg.Price ~ error("A") + trend ("A") + season("N")), 
         searchARIMA = ARIMA(Avg.Price, stepwise = FALSE))  %>%
  forecast(h=104) %>%
  autoplot(av_ts, level = NULL) +
  labs(title = "Average US Avocado Prices", y="$US") +
  guides(colour=guide_legend(title="Forecast"))
# Not understanding ETS, the data is seasonal. Seasonal selections, no fc output.
av_fit

b <- av_ts %>% filter(year(Date)>= 2016)
b







av_ts %>% gg_tsdisplay(difference(Avg.Price), plot_type = 'partial')



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