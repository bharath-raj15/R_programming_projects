# Load demand data
d_df <- read.csv("C:/Users/rajbh/Downloads/Demand-Data.csv")
head(d_df)  # Display the first 5 rows of data

# Check for missing values in each column
colSums(is.na(d_df))

# Check if required packages are installed, and install if necessary
check <- c('forecast', 'data.table', 'ggplot2') %in% installed.packages()
if (!check[1]) install.packages("forecast")
if (!check[2]) install.packages("data.table")
if (!check[3]) install.packages("ggplot2")

# Load libraries
library(forecast)
library(data.table)
library(ggplot2)

# Convert Date column to Date type
d_df$Date <- as.Date(d_df$Date, format = "%Y-%m-%d")

# Check the class of Date column
class(d_df$Date)

# Convert data frame to data.table
setDT(d_df)

# Create new columns for weekday, week number, month, and year
library(lubridate)  # Ensure lubridate is loaded for date functions
d_df[, weekday := wday(Date)]
d_df[, weeknum := week(Date)]
d_df[, Month := month(Date)]
d_df[, Year := year(Date)]

# Plot daily demand for paint
ggplot(data = d_df, aes(x = Date, y = Paint)) +
  geom_line(color = 'blue') +
  xlab("Year") +
  ylab("Daily demand for paint")

#summarize demand in month
cnames=names(d_df)[2:7]
cnames

#summarizing interms of week
dtw=d_df[,lapply(.SD,sum,na.rm=TRUE), by=.(weeknum,Year), .SDcols = cnames]

#breaking demand interms of  months
dtm= d_df[,lapply(.SD,sum,na.rm=TRUE),by=.(Month,Year), .SDcols = cnames]

#filtering data tables
dtw[Year==2020]
dtm[Year>=2019]

dtm[, Paint] #columns to vector 
dtm[Year >= 2020, Paint]

#time series objects 
x = sample(10:20,100, replace = TRUE)

ts(x, frequency = 12, start = c(2014,2))

ts(x, frequency = 4, start = c(2014,3))

ts(x, frequency = 4, 
   start = c(2014,3), end = c(2020,4))

#creating time series for demand (monthly)
ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))

autoplot(ts_dtm) + 
  xlab("Year") +
  ylab("Monthly Demand for Paint") #checking the demand for paint data 

#checking the demand for garden equipment (weekly)
ts_dtw = ts(dtw[,GardEquip], frequency = 52, start = c(2018,1))

autoplot(ts_dtw) + 
  xlab("Year") +
  ylab("Weekly Demand for Garden Equipment")

# Check If It's Time Series Object
is.ts(dt)  
is.ts(ts_dtw)

#checking demand graph for compost (weekly demand)
ts_dtw = ts(dtw[,Compost], frequency =52, start = c(2018,1))
autoplot(ts_dtw)

#checking demand graph for the door lock (monthly demand)
ts_dtm = ts(dtm[,DoorLock], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

#lets check ACF & PACF
#ACF  = Auto-correlation Function 
#PACF = Partial Auto-correlation Function 
Acf(ts_dtm)
pacf(ts_dtm)

#decomposing the data 
ts_dtm  %>%
  decompose() %>%
  autoplot()

ts_dtw  %>%
  decompose() %>%
  autoplot()

#checking pacf after removing seasonality
decompose(ts_dtm)$trend
decompose(ts_dtm)$seasonal
decompose(ts_dtm)$random

autoplot(ts_dtm - decompose(ts_dtm)$seasonal)
autoplot(ts_dtm - decompose(ts_dtm)$trend)



Pacf(ts_dtm - decompose(ts_dtm)$seasonal)

#building the model using auto-ARIMA
auto.arima(ts_dtm)

#searching for model
auto.arima(ts_dtm, ic = "bic", trace = TRUE)

#stroing the model in variable mod
mod = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(mod)

#creating and evaluating Auto regressive integreate moving average
ts_dtm = ts(dtm[,Paint], frequency =12, start = c(2018,1))
autoplot(ts_dtm)

m = auto.arima(ts_dtm, ic = "bic", trace = TRUE)
print(m)

auto.arima(ts_dtm, stepwise = FALSE, trace = TRUE)

m = Arima(ts_dtm, order = c(0,0,0),
          seasonal=list(order=c(0,1,1), period=12),
          include.drift = TRUE)
print(m)

Pacf(ts_dtm)

summary(m)

checkresiduals(m)

#forcasting the demand 
f = forecast(m, h = 12) # h = number of periods forecasted 
autoplot(f)

#reducing it to two periods (24months)
autoplot(f, include = 24)

plot(f$residuals)

qqnorm(f$residuals)
qqline(f$residuals, col = 2)

Acf(f$residuals)
Pacf(f$residuals)

out = as.data.table(f)
out = round(out,0)
write.csv(out,"Forecast.csv")
