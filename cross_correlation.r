library(astsa)
library(caret)

data <- read.csv("https://storage.googleapis.com/aggie-hacks-us-covid19/Data_AL")
colnames(data)

#standardized
data_scaled <- as.data.frame(scale(data[,c(3:8,10)]))
summary(data_scaled)

colnames(data_scaled) <- c('retail_and_recreation','grocery_and_pharmacy',
                           'parks','transit_stations','workplaces','residential','new_confirmed')

#correlation matrix
res <- cor(data_scaled)
round(res,2)

new_confirmed_cases   <- data_scaled[,'new_confirmed']
retail_and_recreation <- data_scaled[,'retail_and_recreation']
residential           <- data_scaled[,'residential']
parks                 <- data_scaled[,'parks']
workplaces            <- data_scaled[,'workplaces']
grocery               <- data_scaled[,'grocery_and_pharmacy']
transition            <- data_scaled[,'transit_stations']      

new_confirmed_cases   = ts(new_confirmed_cases)
retail_and_recreation = ts(retail_and_recreation)
residential           = ts(residential)
parks                 = ts(parks)
workplaces            = ts(workplaces)
grocery               = ts(grocery)
transition            = ts(transition)

acf(new_confirmed_cases)
pacf(new_confirmed_cases)
ccf(workplaces,new_confirmed_cases)

lag2.plot(retail_and_recreation,new_confirmed_cases,15)
#state_avg_residential 7

new_confirmed_cases_lag7 = lag(new_confirmed_cases,-7)
retail_lag7 = lag(retail_and_recreation,-7)
workplace_lag10 = lag(workplaces,-10)
residential_lag7 = lag(residential,-7)

#linear regression
alldata = ts.intersect(new_confirmed_cases,retail_lag7,
                       residential_lag7,parks,workplace_lag10,
                       grocery,transition)
lagged_lm = lm(new_confirmed_cases~retail_lag7+residential_lag7
               +parks+workplace_lag10+grocery+transition)
summary(lagged_lm)