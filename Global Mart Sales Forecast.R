###############################################################################
############ Time Series Case Study- Global Mart Sales Forecasting ############
###############################################################################
# 1. Business Understading
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building and Evaluation
# 5. Conclusion
###############################################################################
####################### 1. Business Understanding #############################
###############################################################################
# "Global Mart" is an online store super giant having worldwide operations. It 
# takes orders and delivers across the globe and deals with all the major 
# product categories - consumer, corporate & home office
#
# Now as a sales/operations manager, you want to finalise the plan for the next
# 6 months.So, you want to forecast the sales and the demand for the next 6 
# months, that would help you manage the revenue and inventory accordingly.                                                      	
# The store caters to 7 different market segments and in 3 major categories. 
# You want to forecast at this granular level, so you subset your data into 
# 21 (7*3) buckets before analysing these data.		             													                                                                                  	#
# But not all of these 21 market buckets are important from the store's point 
# of view. So you need to find out 2 most profitable (and consistent) segment 
# from these 21 and forecast the sales and demand for these segments
###############################################################################
######################### 2. Data Understanding ###############################
###############################################################################
# Total Number of Orders: 51,290
# Number of Attributes : 24

library("ggplot2")
library("dplyr")
library("tidyr")
library("forecast")
library("tseries")
library("graphics")

#Read csv file
sales_df <- read.csv("Global Superstore.csv", header = T, sep = ',')

#Look at the structure of the data
str(sales_df)
summary(sales_df)

#Check for Missing values
sum(is.na(sales_df))
# Number of missing Values: 41296
colnames(sales_df)[colSums(is.na(sales_df)) > 0]
# Only Postal code has NA values.

###############################################################################
########################### 3. Data Preparation ###############################
###############################################################################

#Select only Columns reqired for our Analysis
sales_df <- sales_df[c("Order.Date","Segment","Market","Sales","Quantity","Profit")]

#Checking for Missing values
sum(is.na(sales_df))
# No missing Values

# Converting Order.Date into Date format
sales_df$Order.Date <-strptime(sales_df$Order.Date,"%d-%m-%Y")
# Retrieve Month-Year from Date
sales_df$Month_Year <- format(sales_df$Order.Date, "%Y-%m")
#Convert Back to Character for Grouping
sales_df$Order.Date <- as.character(sales_df$Order.Date)

# Aggregate Sales,Quantity and Profit by Segment,Market and Month_Year
sales_agg_df <- group_by(sales_df, Segment, Market, Month_Year) %>% 
				        summarise(monthly_sales=sum(Sales),
						              monthly_quantity=sum(Quantity),
						              monthly_profit=sum(Profit))

# Compute Cofficient of Variation(CV) for aggregated Profit 
sales_profitable_df <- group_by(sales_agg_df,Segment, Market)%>%
					             summarise(CV=sd(monthly_profit)/mean(monthly_profit))%>%
					             arrange(CV)

sales_profitable_df

ggplot(sales_profitable_df,aes(x= Segment,y=reorder(round(CV,2), CV), fill=factor(Market)))+
  geom_bar(stat= "identity",position="dodge")+
  xlab("CV") + 
  ylab("Coeff. of variance of Monthly Profit")+
  ggtitle("Coeff. of variance across all Market Segments")
# The Following two market segments have the least Coefficient of Variation among all the 21 segments
#1. EU Consumer
#2. APAC Consumer

#Filtering Data for EU-Consumer
eu_consumer_df <- filter(sales_agg_df, Segment=="Consumer", Market=="EU") %>% arrange(Month_Year)

#Filtering Data for APAC-Consumer
apac_consumer_df <- filter(sales_agg_df, Segment=="Consumer", Market=="APAC") %>% arrange(Month_Year)

# Create month column
eu_consumer_df$month=1:nrow(eu_consumer_df)
apac_consumer_df$month=1:nrow(apac_consumer_df)

#Separate train and test data
eu_consumer_df_in <- eu_consumer_df[1:42,]
eu_consumer_df_out <- eu_consumer_df[43:48,]
apac_consumer_df_in <- apac_consumer_df[1:42,]
apac_consumer_df_out <- apac_consumer_df[43:48,]

###############################################################################
####################### 4. Model Building and Evaluation ######################
###############################################################################

###############################################################################
################################ EU- Consumer##################################
########################## Sales Time Series Analysis #########################
###############################################################################

########################### Classical Decomposition ###########################
#Smoothing the series - Moving Average Smoothing
smoothening_function <- function(timeseries){
  w <-1
  smoothened_series <- stats::filter(timeseries, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  diff <- smoothened_series[w+2] - smoothened_series[w+1]
  for (i in seq(w,1,-1)) {
    smoothened_series[i] <- smoothened_series[i+1] - diff
  }
  
  #Smoothing right end of the time series
  n <- length(timeseries)
  diff <- smoothened_series[n-w] - smoothened_series[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothened_series[i] <- smoothened_series[i-1] + diff
  }
  return(smoothened_series)
}


eu_consumer_ts <- ts(eu_consumer_df$monthly_sales)
eu_consumer_in_ts <- ts(eu_consumer_df_in$monthly_sales)
tsdisplay(eu_consumer_in_ts,lag.max = 42)

#Smoothen the data
smooth_eu_consumer_in_ts <- smoothening_function(eu_consumer_in_ts)

#Plot the smoothed time series
plot(eu_consumer_in_ts, ylab="Sales" , main="EU Consumer Sales",col="red")
lines(smooth_eu_consumer_in_ts, col="green", lwd=2)

# Find Trend part of series
eu_consumer_df_in_timevals <- c(1:nrow(eu_consumer_df_in))
eu_consumer_df_in_new <- as.data.frame(cbind(eu_consumer_df_in_timevals,smooth_eu_consumer_in_ts))
colnames(eu_consumer_df_in_new) <- c("Month","Sales")

# Fit a additive model with trend and seasonality to the data
# Seasonality will be modeled using a sinusoid function
linear_model_1 <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=eu_consumer_df_in_new)
summary(linear_model_1) 
#Adjusted R-squared:  0.7053

linear_model_2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_2) 
#Adjusted R-squared:  0.7456

linear_model_3 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_3) 
#Adjusted R-squared:  0.835


linear_model_4 <- lm(Sales ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_4) 
#Adjusted R-squared:  0.8209

# Adjusted R-squared for poly degree 3 is greatest
# Consider linear_model_3 for determining global component
global_pred_3 <- predict(linear_model_3, Month=eu_consumer_df_in_timevals)
summary(global_pred_3)
plot(eu_consumer_in_ts)
lines(eu_consumer_df_in_new, col="green", lwd=2)
lines(eu_consumer_df_in_timevals, global_pred_3, col='red', lwd=2)

# Model using ARMA, the local predictable series
local_pred_series <- eu_consumer_in_ts-global_pred_3
plot(local_pred_series, col='red', type = "l")
#ACF And PACF Plots
acf(local_pred_series)
pacf(local_pred_series)
arma <- auto.arima(local_pred_series)
arma
tsdiag(arma)

# Check if residual series is white noise
residual_series <- local_pred_series-fitted(arma)

# ADF and KPSS test
adf.test(residual_series,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_series)
# p-value is 0.1 (>0.05)

# Mape 
eu_consumer_df_out_timevals <- eu_consumer_df_out$month

#Predict Using Linear Model
predict_global <- predict(linear_model_3,data.frame(Month=eu_consumer_df_out_timevals))
predict_local <- predict(arma, n.ahead = 6)
predict_total = predict_global + predict_local$pred

classical_pred <- c(ts(global_pred_3),ts(predict_total))
plot(eu_consumer_df$monthly_sales, xlab = "Month", ylab="Sales" , main="EU Consumer Sales",col="green")
lines(eu_consumer_ts, col = "green")
lines(eu_consumer_df_in_timevals,global_pred_3, col = "blue", lwd=2)
lines(classical_pred, col = "red")


# Compare prediction with the actual values using MAPE
mape_classic <- accuracy(predict_total,eu_consumer_df_out$monthly_sales)[5]
mape_classic
# Mape is 92.95788

########################### AUTO ARIMA ###########################
#Predict Using ARIMA Model
autoarima <- auto.arima(eu_consumer_df_in$monthly_sales)
autoarima
tsdiag(autoarima)

# Check if the residual series is white noise
residual_arima <- ts(eu_consumer_df_in$monthly_sales) - fitted(autoarima)
# ADF and KPSS test
adf.test(residual_arima,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_arima)
# p-value is 0.1 (>0.05)

forecast_arima <- predict(autoarima, n.ahead = 6)

mape_autoarima <- accuracy(forecast_arima$pred,eu_consumer_df_out$monthly_sales)[5]
mape_autoarima
# Mape is 28.9226

# Mape for Classical Decompostion is 92.95788
# Mape for Auto ARIMA is 28.9226
#Auto ARIMA performed better

auto_arima_pred <- c(fitted(autoarima),ts(forecast_arima$pred))
plot(eu_consumer_df$monthly_sales, xlab="Month", ylab="Sales" , main="EU Consumer Sales", col = "green")
lines(eu_consumer_ts, col = "green")
lines(auto_arima_pred[1:42],col="blue", lwd = 2)
lines(auto_arima_pred, col = "red")

# Forcasting for next 6 months using Auto ARIMA
eu = auto.arima(eu_consumer_df$monthly_sales) %>% predict( n.ahead = 6)
eu
# Time Series:
# Start = 49 
# End = 54 
# Frequency = 1 
# 49358.71 58063.62 59714.33 54191.79 56811.55 58010.84

eu_sales = data.frame(eu$pred)

###############################################################################
################################ EU- Consumer##################################
####################### Quantity Time Series Analysis #########################
###############################################################################

########################### Classical Decomposition ###########################
eu_consumer_ts <- ts(eu_consumer_df$monthly_quantity)
eu_consumer_in_ts <- ts(eu_consumer_df_in$monthly_quantity)
tsdisplay(eu_consumer_in_ts,lag.max = 42)

#Smoothen the data
smooth_eu_consumer_in_ts <- smoothening_function(eu_consumer_in_ts)

#Plot the smoothed time series
plot(eu_consumer_in_ts, ylab="Quantity" , main="EU Consumer Quantity",col="red")
lines(smooth_eu_consumer_in_ts, col="green", lwd=2)

# Find Trend part of series
eu_consumer_df_in_timevals <- c(1:nrow(eu_consumer_df_in))
eu_consumer_df_in_new <- as.data.frame(cbind(eu_consumer_df_in_timevals,smooth_eu_consumer_in_ts))
colnames(eu_consumer_df_in_new) <- c("Month","Quantity")

# Fit a additive model with trend and seasonality to the data
# Seasonality will be modeled using a sinusoid function
linear_model_1 <- lm(Quantity ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=eu_consumer_df_in_new)
summary(linear_model_1) 
#Adjusted R-squared:  0.8393


linear_model_2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_2) 
#Adjusted R-squared:  0.8666


linear_model_3 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_3) 
#Adjusted R-squared:  0.8814


linear_model_4 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
            + Month, data=eu_consumer_df_in_new)
summary(linear_model_4) 
#Adjusted R-squared:  0.8731

# Adjusted R-squared for poly degree 3 is greatest
# Consider linear_model_3 for determining global component

global_pred_3 <- predict(linear_model_3, Month=eu_consumer_df_in_timevals)
summary(global_pred_3)
plot(eu_consumer_in_ts)
lines(eu_consumer_df_in_new, col="green", lwd=2)
lines(eu_consumer_df_in_timevals, global_pred_3, col='red', lwd=2)

# Model using ARMA, the local predictable series
local_pred_series <- eu_consumer_in_ts-global_pred_3
plot(local_pred_series, col='red', type = "l")
#ACF And PACF Plots
acf(local_pred_series)
pacf(local_pred_series)
arma <- auto.arima(local_pred_series)
arma
tsdiag(arma)

# Check if residual series is white noise
residual_series <- local_pred_series-fitted(arma)

# ADF and KPSS test
adf.test(residual_series,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_series)
# p-value is 0.1 (>0.05)

# Mape 
eu_consumer_df_out_timevals <- eu_consumer_df_out$month

#Predict Using Linear Model
predict_global <- predict(linear_model_3,data.frame(Month=eu_consumer_df_out_timevals))
predict_local <- predict(arma, n.ahead = 6)
predict_total = predict_global + predict_local$pred

classical_pred <- c(ts(global_pred_3),ts(predict_total))
plot(eu_consumer_df$monthly_quantity, xlab = "Month", ylab="Sales" , main="EU Consumer Quantity",col="green")
lines(eu_consumer_ts, col = "green")
lines(eu_consumer_df_in_timevals,global_pred_3, col = "blue", lwd=2)
lines(classical_pred, col = "red")
# Compare prediction with the actual values using MAPE
mape_classic <- accuracy(predict_total,eu_consumer_df_out$monthly_quantity)[5]
mape_classic
# Mape is 31.45475

########################### AUTO ARIMA ###########################
#Predict Using ARIMA Model
autoarima <- auto.arima(eu_consumer_df_in$monthly_quantity)
autoarima
tsdiag(autoarima)

# Check if the residual series is white noise
residual_arima <- ts(eu_consumer_df_in$monthly_quantity) - fitted(autoarima)
# ADF and KPSS test
adf.test(residual_arima,alternative = "stationary") 
# p-value is 0.04521 (<0.05)
kpss.test(residual_arima)
# p-value is 0.1 (>0.05)

forecast_arima <- predict(autoarima, n.ahead = 6)

mape_autoarima <- accuracy(forecast_arima$pred,eu_consumer_df_out$monthly_quantity)[5]
mape_autoarima
# Mape is 30.13319

# Mape for Classical Decompostion is 31.45475
# Mape for Auto ARIMA is 30.13319
#Auto ARIMA performed better

auto_arima_pred <- c(fitted(autoarima),ts(forecast_arima$pred))
plot(eu_consumer_df$monthly_quantity, xlab = "Month", ylab="Sales" , main="EU Consumer Quantity",col="green")
lines(eu_consumer_ts, col = "green")
lines(auto_arima_pred[1:42],col="blue", lwd = 2)
lines(auto_arima_pred, col = "red")
# Forcasting for next 6 months using Auto ARIMA
eu = eu_consumer_df$monthly_quantity %>% auto.arima() %>% predict( n.ahead = 6)

# Time Series:
# Start = 49 
# End = 54 
# Frequency = 1 
# [1] 626.2009 786.6056 842.9179 704.8258 768.6274 807.6497

eu_quan = data.frame(eu$pred)

###############################################################################
############################## APAC- Consumer##################################
######################### Sales Time Series Analysis ##########################
###############################################################################

########################### Classical Decomposition ###########################
apac_consumer_ts <- ts(apac_consumer_df$monthly_sales)
apac_consumer_in_ts <- ts(apac_consumer_df_in$monthly_sales)
tsdisplay(apac_consumer_in_ts,lag.max = 42)

#Smoothen the data
smooth_apac_consumer_in_ts <- smoothening_function(apac_consumer_in_ts)

#Plot the smoothed time series
plot(apac_consumer_in_ts, ylab="Sales" , main="APAC Consumer Sales",col="red")
lines(smooth_apac_consumer_in_ts, col="green", lwd=2)

# Find Trend part of series
apac_consumer_df_in_timevals <- c(1:nrow(apac_consumer_df_in))
apac_consumer_df_in_new <- as.data.frame(cbind(apac_consumer_df_in_timevals,smooth_apac_consumer_in_ts))
colnames(apac_consumer_df_in_new) <- c("Month","Sales")

# Fit a additive model with trend and seasonality to the data
# Seasonality will be modeled using a sinusoid function
linear_model_1 <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=apac_consumer_df_in_new)
summary(linear_model_1) 
#Adjusted R-squared:  0.718


linear_model_2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_2) 
#Adjusted R-squared:  0.7958


linear_model_3 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_3) 
#Adjusted R-squared:  0.816


linear_model_4 <- lm(Sales ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_4) 
#Adjusted R-squared: 0.8071

# Adjusted R-squared for poly degree 3 is greatest
# Consider linear_model_3 for determining global component

global_pred_3 <- predict(linear_model_3, Month=apac_consumer_df_in_timevals)
summary(global_pred_3)
plot(apac_consumer_in_ts)
lines(apac_consumer_df_in_new, col="green", lwd=2)
lines(apac_consumer_df_in_timevals, global_pred_3, col='red', lwd=2)

# Model using ARMA, the local predictable series
local_pred_series <- apac_consumer_in_ts-global_pred_3
plot(local_pred_series, col='red', type = "l")
#ACF And PACF Plots
acf(local_pred_series)
pacf(local_pred_series)
arma <- auto.arima(local_pred_series)
arma
tsdiag(arma)

# Check if residual series is white noise
residual_series <- local_pred_series-fitted(arma)

# ADF and KPSS test
adf.test(residual_series,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_series)
# p-value is 0.1 (>0.05)

# Mape 
apac_consumer_df_out_timevals <- apac_consumer_df_out$month

#Predict Using Linear Model
predict_global <- predict(linear_model_3,data.frame(Month=apac_consumer_df_out_timevals))
predict_local <- predict(arma, n.ahead = 6)
predict_total = predict_global + predict_local$pred

classical_pred <- c(ts(global_pred_3),ts(predict_total))
plot(apac_consumer_df$monthly_sales, xlab = "Month", ylab="Sales" , main="APAC Consumer Sales",col="green")
lines(apac_consumer_ts, col = "green")
lines(apac_consumer_df_in_timevals,global_pred_3, col = "blue", lwd=2)
lines(classical_pred, col = "red")
# Compare prediction with the actual values using MAPE
mape_classic <- accuracy(predict_total,apac_consumer_df_out$monthly_sales)[5]
mape_classic
# Mape is 31.07429

########################### AUTO ARIMA ###########################
#Predict Using ARIMA Model
autoarima <- auto.arima(apac_consumer_df_in$monthly_sales)
autoarima
tsdiag(autoarima)

# Check if the residual series is white noise
residual_arima <- ts(apac_consumer_df_in$monthly_sales) - fitted(autoarima)
# ADF and KPSS test
adf.test(residual_arima,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_arima)
# p-value is 0.1 (>0.05)

forecast_arima <- predict(autoarima, n.ahead = 6)

mape_autoarima <- accuracy(forecast_arima$pred,apac_consumer_df_out$monthly_sales)[5]
mape_autoarima
# Mape is 27.68952

# Mape for Classical Decompostion is 31.07429
# Mape for Auto ARIMA is 27.68952
#Auto ARIMA performed better

auto_arima_pred <- c(fitted(autoarima),ts(forecast_arima$pred))
plot(apac_consumer_df$monthly_sales, xlab = "Month", ylab="Sales" , main="APAC Consumer Sales",col="green")
lines(apac_consumer_ts, col = "green")
lines(auto_arima_pred[1:42],col="blue", lwd = 2)
lines(auto_arima_pred, col = "red")
# Forcasting for next 6 months using Auto ARIMA
apac = apac_consumer_df$monthly_sales %>% auto.arima() %>% predict( n.ahead = 6)

# Time Series:
# Start = 49 
# End = 54 
# Frequency = 1 
# [1] 64494.89 64494.89 64494.89 64494.89 64494.89 64494.89

apac_sales = data.frame(apac$pred)

###############################################################################
############################## APAC- Consumer##################################
###################### Quantity Time Series Analysis ##########################
###############################################################################

########################### Classical Decomposition ###########################
apac_consumer_ts <- ts(apac_consumer_df$monthly_quantity)
apac_consumer_in_ts <- ts(apac_consumer_df_in$monthly_quantity)
tsdisplay(apac_consumer_in_ts,lag.max = 42)

#Smoothen the data
smooth_apac_consumer_in_ts <- smoothening_function(apac_consumer_in_ts)

#Plot the smoothed time series
plot(apac_consumer_in_ts, ylab="Sales" , main="APAC Consumer Quantity",col="red")
lines(smooth_apac_consumer_in_ts, col="green", lwd=2)

# Find Trend part of series
apac_consumer_df_in_timevals <- c(1:nrow(apac_consumer_df_in))
apac_consumer_df_in_new <- as.data.frame(cbind(apac_consumer_df_in_timevals,smooth_apac_consumer_in_ts))
colnames(apac_consumer_df_in_new) <- c("Month","Quantity")

# Fit a additive model with trend and seasonality to the data
# Seasonality will be modeled using a sinusoid function
linear_model_1 <- lm(Quantity ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=apac_consumer_df_in_new)
summary(linear_model_1) 
#Adjusted R-squared:  0.7027


linear_model_2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_2) 
#Adjusted R-squared:  0.819


linear_model_3 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_3) 
#Adjusted R-squared:  0.8193


linear_model_4 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
            + Month, data=apac_consumer_df_in_new)
summary(linear_model_4) 
#Adjusted R-squared: 0.8216

# Adjusted R-squared for poly degree 4 is greatest but is complex
# Consider linear_model_2 for determining global complonent

global_pred_2 <- predict(linear_model_2, Month=apac_consumer_df_in_timevals)
summary(global_pred_2)
plot(apac_consumer_in_ts)
lines(apac_consumer_df_in_new, col="green", lwd=2)
lines(apac_consumer_df_in_timevals, global_pred_2, col='red', lwd=2)

# Model using ARMA, the local predictable series
local_pred_series <- apac_consumer_in_ts-global_pred_2
plot(local_pred_series, col='red', type = "l")
#ACF And PACF Plots
acf(local_pred_series)
pacf(local_pred_series)
arma <- auto.arima(local_pred_series)
arma
tsdiag(arma)

# Check if residual series is white noise
residual_series <- local_pred_series-fitted(arma)

# ADF and KPSS test
adf.test(residual_series,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_series)
# p-value is 0.1 (>0.05)

# Mape 
apac_consumer_df_out_timevals <- apac_consumer_df_out$month

#Predict Using Linear Model
predict_global <- predict(linear_model_3,data.frame(Month=apac_consumer_df_out_timevals))
predict_local <- predict(arma, n.ahead = 6)
predict_total = predict_global + predict_local$pred

classical_pred <- c(ts(global_pred_2),ts(predict_total))
plot(apac_consumer_df$monthly_quantity, xlab = "Month", ylab="Sales" , main="APAC Consumer Quantity",col="green")
lines(apac_consumer_ts, col = "green")
lines(apac_consumer_df_in_timevals,global_pred_2, col = "blue", lwd=2)
lines(classical_pred, col = "red")

# Compare prediction with the actual values using MAPE
mape_classic <- accuracy(predict_total,apac_consumer_df_out$monthly_quantity)[5]
mape_classic
# Mape is 62.10289

########################### AUTO ARIMA ###########################
#Predict Using ARIMA Model
autoarima <- auto.arima(apac_consumer_df_in$monthly_quantity)
autoarima
tsdiag(autoarima)

# Check if the residual series is white noise
residual_arima <- ts(apac_consumer_df_in$monthly_quantity) - fitted(autoarima)
# ADF and KPSS test
adf.test(residual_arima,alternative = "stationary") 
# p-value is 0.01 (<0.05)
kpss.test(residual_arima)
# p-value is 0.1 (>0.05)

forecast_arima <- predict(autoarima, n.ahead = 6)

mape_autoarima <- accuracy(forecast_arima$pred,apac_consumer_df_out$monthly_quantity)[5]
mape_autoarima
# Mape is 26.24458

# Mape for Classical Decompostion is 62.10289
# Mape for Auto ARIMA is 26.24458
#Auto ARIMA performed better

auto_arima_pred <- c(fitted(autoarima),ts(forecast_arima$pred))
plot(apac_consumer_df$monthly_quantity, xlab = "Month", ylab="Sales" , main="APAC Consumer Quantity",col="green")
lines(apac_consumer_ts, col = "green")
lines(auto_arima_pred[1:42],col="blue", lwd = 2)
lines(auto_arima_pred, col = "red")
# Forcasting for next 6 months using Auto ARIMA
apac = auto.arima(apac_consumer_df$monthly_quantity) %>% predict( n.ahead = 6)
# Time Series:
# Start = 49 
# End = 54 
# Frequency = 1 
# [1] 804.4143 804.4143 804.4143 804.4143 804.4143 804.4143

apac_quan = data.frame(apac$pred)


eu_consumer_pred = cbind(c(49:54),eu_sales,eu_quan)
colnames(eu_consumer_pred) <- c("Month","Sales Predicted", "Quantity Predicted")

apac_consumer_pred = cbind(c(49:54),apac_sales,apac_quan)
colnames(apac_consumer_pred) <- c("Month","Sales Predicted", "Quantity Predicted")

#EU Consumer Predictions of sales and quantity for next 6 Months
eu_consumer_pred
#APAC Consumer Predictions of sales and quantity for next 6 Months
apac_consumer_pred

###############################################################################
################################# 5. Conclusion ###############################
###############################################################################
# The EU Consumer and APAC Consumer Market Segments are the two most 
# consistently profitable segments.
# In all the predictions, Auto ARIMA performed better than classical
# decomposition method.
# MAPE values is low for all Auto ARIMA models and the residuals of all the 
# segment predictions turned out to be white noise.
#########################################END###################################