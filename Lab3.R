rm(list = ls())

 # en liten forandring
 # en branch

setwd("C:/Users/hoffs/OneDrive/Dokument")

library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich) # For robust standard errors
library(stargazer) # To create a table of regression results
library(dplyr) # Data manipulation and transformation (filter, mutate, summarize)
library(tidyverse)
library(haven)
library(modelsummary)
library(lubridate)
library(stargazer)
library(estimatr)
library(broom)# Tidying the output of statistical models (tidy, glance, augment)
library(tseries) # ADF-test
library(gridExtra) # Display side-by-side plots
library(caret) # Tasks related to predictive modeling

# Monthly observations on value (in 1000 NOK), and weight (tonnes) of Norwegian
# seafood exports for selected countries and in total, jan 2022 to aug 2022
cds_seafood <- read.csv("seafood_exports.csv") 
View(cds_seafood)

# (a) Compute price in NOK per kilogram. Plot price over time for total exports.
# Comment on trend and seasonality

# Calculate price in NOK per kilogram.
cds_seafood$price_per_kg <- cds_seafood$value / (cds_seafood$weight)


ggplot(cds_seafood, aes(x = as.Date(paste(year, month, "01", sep = "-")), y =
                          price_per_kg)) +
  geom_line() +
  labs(x = "Date", y = "Price (NOK per kg)", title = 
         "Total Value of Norwegian Seafood Exports over Time") +
  theme_minimal()

# Visually, theret seems to be an existing uprising trend over time in the 
# price per kg of Norwegian Seafood Exports. This could be due to inflation
# or specific price-increases of the goods. There also seems to be a seasonal
# pattern, where price-increases and decreses are repeating over time.
# This could be due to seasonal changes in demand and supply, which then occur
# at the same point of time each year.



# (b) Plot prices over time for Germany and Spain in the same graph. Why are
# Germans getting a better deal? Why do you think the seasonality is different?

# Combine the "year" and "months" columns into a date variable
cds_seafood$date <- ymd(paste(cds_seafood$year, cds_seafood$month, "01"))

# Subset data for Germany and Spain
germany <- subset(cds_seafood, country == "DE")
spain <- subset(cds_seafood, country == "ES")


# Create a time series plot for Germany and Spain
ggplot() +
  geom_line(data = germany, aes(x = date, y = price_per_kg), color = "blue", linetype = "solid") +
  geom_line(data = spain, aes(x = date, y = price_per_kg), color = "red", linetype = "solid") +
  labs(x = "Date", y = "Price (NOK per kg)") +
  scale_x_date(date_labels = "%b %Y") +
  ggtitle("Monthly Seafood Prices in Germany (blue) and Spain (red)") +
  theme_minimal()

# Interpretation of the plot:
# It seems clear that for a long time, the price per kg of Norwegian Seafood
# exports, has been cheaper for Germany, compared to Spain. This should not be
# due to differences in exchange rates, as both countries use EUR/NOK
# conversion rate (assumption). Neither should it be due to differences in 
# demand, as the
# variable displays price per kg. It could, however, be related to what kind of 
# Seafood the two countries are demanding. "Seafood" is a wide term, which
# could include multiple types of fish, or different parts of fish, which could 
# vary in
# price per kg. Transportation costs might also have a say in this, whereas
# it should be cheaper transporting to Germany by
# either sea or road as it is closer to the distributor, Norway. This can enable
# Germany to exploit economies of scale, where a larger quantity results in 
# lower prices.  

# The differences in seasonality could be due to different cultures related to
# food or other trends that are country-specific. The internal fishing seasons
# or conditions could also differ, leading to an increased demand of imported
# seafood in a specific country. 







# (c) Regress log prices for total exports on i) a linear and ii) a quadratic 
# trend , report the results in a single table and interpret them. 
# Which one do you prefer?

# We want to regress "value" for just the == total exports on both a linear
# and quadratic trend.

# Create logarithmic version of value
cds_seafood$log_value <- log(cds_seafood$value)

# Create linear and quadratic "trend/time" variables
cds_seafood$linear_trend <- 1:nrow(cds_seafood)
cds_seafood$quadratic_trend <- cds_seafood$linear_trend^2

# Create a subset of the data frame including only the first 272 rows (to inlude
# only the "total" and not the other countries.)
subset_cds_seafood <- cds_seafood[1:272, ]

# Perform linear and quadratic trend regressions
model_linear_trend <- lm(log_value ~ linear_trend, data = subset_cds_seafood)
model_quadratic_trend <- lm(log_value ~ quadratic_trend, data = subset_cds_seafood)

stargazer(model_linear_trend, model_quadratic_trend, type = "text")

# SCATTER PLOT 1) Linear and Quadratic line in the same plot
ggplot(subset_cds_seafood, aes(x = linear_trend, y = log_value)) +
  geom_point(aes(color = "Log Value"), alpha = 0.7) +
  geom_line(data = subset_cds_seafood, aes(x = linear_trend, y = predict(model_linear_trend)), color = "blue", size = 1, linetype = "solid") +
  geom_line(data = subset_cds_seafood, aes(x = linear_trend, y = predict(model_quadratic_trend)), color = "red", size = 1, linetype = "dotted") +
  labs(x = "Linear Trend", y = "Log Value") +
  scale_color_manual(values = c("blue" = "Blue", "red" = "Red")) +
  theme_minimal()

# SCATTER PLOT 2) log_value vs linear trend
plot_linear <- ggplot(subset_cds_seafood, aes(x = linear_trend, y = log_value)) +
  geom_point(aes(color = "Log Value"), alpha = 0.7) +
  geom_line(data = subset_cds_seafood, aes(x = linear_trend, y = predict(model_linear_trend)), color = "blue", size = 1, linetype = "solid") +
  labs(x = "Linear Trend", y = "Log Value") +
  scale_color_manual(values = c("blue" = "Blue")) +
  theme_minimal()

# SCATTER PLOT 3) log_value vs quadratic trend
plot_quadratic <- ggplot(subset_cds_seafood, aes(x = linear_trend, y = log_value)) +
  geom_point(aes(color = "Log Value"), alpha = 0.7) +
  geom_line(data = subset_cds_seafood, aes(x = linear_trend, y = predict(model_quadratic_trend)), color = "red", size = 1, linetype = "dotted") +
  labs(x = "Quadratic Trend", y = "Log Value") +
  scale_color_manual(values = c("red" = "Red")) +
  theme_minimal()

# Plot them side by side
grid.arrange(plot_linear, plot_quadratic, ncol = 2)

# Both models appear to fit the data quite well, with high adjusted R-squared 
# values (linear: 0,88, quadratic: 0,87), indicating a good fit to the data.
# The effect of the linear trend is stronger than the quadratic trend, 
# as indicated by the coefficient estimates (linear: 0,007, quadratic: 0,00002),
# both statistically significant. 

# Visually, the data suggests a clear trend in the time series of log_value, 
# with both linear and quadratic patterns appearing plausible. When visually
# comparing the trend lines with the data points, both models seem to provide 
# a reasonable fit to the observed data.The data suggests a clear trend in the
# time series of log_value, with both linear and quadratic patterns appearing 
# plausible. When visually comparing the trend lines with the data points, 
# both models seem to provide a reasonable fit to the observed data.

# (d) Control for monthly seasonality in your model, report the results and 
# interpret

# Create binary month indicators for all 12 months
cds_seafood <- cds_seafood %>%
  mutate(january = ifelse(month(date) == 1, 1, 0),
         february = ifelse(month(date) == 2, 1, 0),
         march = ifelse(month(date) == 3, 1, 0),
         april = ifelse(month(date) == 4, 1, 0),
         may = ifelse(month(date) == 5, 1, 0),
         june = ifelse(month(date) == 6, 1, 0),
         july = ifelse(month(date) == 7, 1, 0),
         august = ifelse(month(date) == 8, 1, 0),
         september = ifelse(month(date) == 9, 1, 0),
         october = ifelse(month(date) == 10, 1, 0),
         november = ifelse(month(date) == 11, 1, 0),
         december = ifelse(month(date) == 12, 1, 0)
  )

# Remove rows with missing (NA) values in the log_value column
cds_seafood <- cds_seafood[complete.cases(cds_seafood), ]

model_lags <- lm(log_value ~ linear_trend + january + february + march + april 
                 + may + june + july + august + september + october + 
                  november + december, data = cds_seafood)

stargazer(model_lags, type = "text")

# Several month indicators show significant coefficient estimates, meaning
# that todays value of log_value is affected by which specific month we are in,
# no matter the year. This indicates that there is seasonality in the variable.
# linear_trend: The coefficient of -0.0005*** suggests that for each unit
# increase in the linear trend, the log_value decreases by 0.0005 on average, 
# holding all other variables constant. This is opposite of what can be observed
# in the plots, where the trend seems to have a positive relationship to 
# log_value, with increasing values over time (trend)
# january: The coefficient of -0,158 indicates that when january is present,
# log_value is expected to decrease by 0,158 units. (Significant at the 10% level)

# In summary, the regression analysis underscores the seasonality of "log_value"
# with some months having more substantial and statistically significant
# effects, while others do not. The omission of "December" is likely due
# to multicollinearity. These results indicate that there is significant signs 
# of monthly seasonality within the data. 

# (e) Estimate the previous model using data up until December 2019. Create an
# out of-sample forecast of log price until August 2022 and plot it against 
# observed data. Calculate the root mean-square error (RMSE) of the forecast 
# and compare it with the RMSE of the in-sample fitted values obtained using
# the model fitted to the full sample in (d).
# Define the end of the in-sample period (December 2019)

# Same regression model as earlier, limit the analysis to observations 1 to 240
model_lags_dec2019 <- lm(log_value ~ linear_trend + january + february + march + april + may +
                          june + july + august + september + october + 
                          november + december, data = cds_seafood[1:240, ])

# Print the summary of the model
stargazer(model_lags_dec2019, type = "text")

# Forecast the log_value for rows 241 (jan20) to 272 (aug22)
forecast_values <- predict(model_lags_dec2019, 
                           newdata = cds_seafood[241:272, ],
                           interval = "prediction")

# Assign the forecasted values to a new variable in the dataset
cds_seafood[241:272, "forecasted_log_value"] <- forecast_values

# This generates This results in three columns: the point estimate, the lower 
# prediction interval, and the upper prediction interval. For simplicity, we
# remove the lower and upper prediction interval, leaving us with just the 
# point estimates (forecast values) BONUS: Apparently this shit does not work
# because the three generated forecast columns seem to be paired or related 
# somehow. Them being chained like this makes me unable to interfer with them...

# Copy the forecasted values to a new variable with an easier name, since
# we cant modify the original one for some mystical reason
cds_seafood$log_value_forecast <- cds_seafood$forecasted_log_value[,1]

# Create a time series plot for real and forecast data for rows 241 to 272
ggplot(data = cds_seafood[241:272, ], aes(x = date)) +
  geom_line(aes(y = log_value, color = "Real Value"), linetype = "solid") +
  geom_line(aes(y = log_value_forecast, color = "Forecast Value"), linetype = "solid") +
  labs(x = "Date", y = "Value") +
  scale_x_date(date_labels = "%b %Y") +
  ggtitle("Real log_value vs. forecasted log_value") +
  theme_minimal()



# Fit the model to the full sample
model_lags <- lm(log_value ~ linear_trend + january + february + march + april + 
                   may + june + july + august + september + october + 
                   november + december, data = cds_seafood)

# Calculate RMSE for the model fitted to the full sample
rmse_full_sample <- sqrt(mean((cds_seafood$log_value - predict(model_lags))^2))

# Estimate the model using data up until December 2019
model_lags_dec2019 <- lm(log_value ~ linear_trend + january + february + march 
                         + april +  may + june + july + august + september + 
                           october +  november + december, 
                         data = cds_seafood[1:240, ])

# Forecast the log_value for rows 241 (jan20) to 272 (aug22)
forecast_values <- predict(model_lags_dec2019, newdata = cds_seafood[241:272, ]
                           , interval = "prediction")

# Calculate RMSE for the forecasted values
rmse_forecast <- sqrt(mean((cds_seafood[241:272, "log_value"] -
                              forecast_values)^2))

# Compare the RMSE values
comparison <- data.frame(
  Model = c("Full Sample", "Forecast"),
  RMSE = c(rmse_full_sample, rmse_forecast)
)

print(comparison)

# RMSE of 0,2670728 suggests that, on average, the model's predictions are off 
# by approximately 0,27 units of the dependent variable (log_value) for the 
# forecasting data.



# (f) Create a new variable which contains a rolling moving average of total 
# export weight over 6 and 12 months and plot it together with the original 
# total export weight series over time.

# Filter the data for the "country" equal to "total"
total_data <- cds_seafood %>% filter(country == "Total")

# Calculate rolling moving averages for 6 and 12 months for the "weight" variable
total_data$rolling_6_months <- zoo::rollmean(total_data$weight, k = 6, align = "right", fill = NA)
total_data$rolling_12_months <- zoo::rollmean(total_data$weight, k = 12, align = "right", fill = NA)

# Create a time series plot for the "weight" variable with rolling moving averages
ggplot(data = total_data, aes(x = date)) +
  geom_line(aes(y = weight, color = "Total Export Weight"), linetype = "solid") +
  geom_line(aes(y = rolling_6_months, color = "6-Month Rolling Avg"), linetype = "dashed") +
  geom_line(aes(y = rolling_12_months, color = "12-Month Rolling Avg"), linetype = "dotted") +
  labs(x = "Date", y = "Total Export Weight") +
  scale_x_date(date_labels = "%b %Y") +
  ggtitle("Total Export Weight with Rolling Moving Averages") +
  theme_minimal()

# The 12-month rolling averages smooth out extreme data points, 
# making the trends more visible. These rolling averages lessen the influence 
# of short-term ups and downs, meaning that they reduce the impact of seasonal 
# fluctuations, revealing a clearer overall pattern. However, the data still
# shows a long-term upward trend, indicating consistent growth over time.








# (g) The file exr_95_22.csv contains monthly average exchange rates from 
# selected currrencies to Norwegian krone (the exchange rates are all measured
# as NOK per unit of the foreign currency). Merge the two datasets, i.e. add 
# exchange rates for EUR and USD to the seafood export data.


# Importing dataset cds_exr
cds_exr <- read.csv("exr_95_22.csv")
View(cds_exr)

# Remove all rows except the first and the 272nd row. This excludes all the
# country-specific observations (to avoid interference upon merging)
cds_seafood_total <- cds_seafood[c(1:272), ]
View(cds_seafood_total)


# Create a new variable 'month' in cds_seafood with the format 'YYYY-MM'
cds_seafood_total$month <- format(as.Date(cds_seafood_total$date), format = "%Y-%m")

# Merge the data frames based on the "month" column
cds_seafood_exr <- merge(cds_seafood_total, cds_exr, by = "month", all.x = TRUE)
View(cds_seafood_exr)

# (h) Determine whether to add USDNOK, EURNOK or both to your model of prices.
# Justify your choice, interpret the results and explain the findings.


# Calculate the correlation between USDNOK and EURNOK
correlation <- cor(cds_seafood_exr$USDNOK, cds_seafood_exr$EURNOK)

# Print the correlation coefficient
print(correlation)

# USDNOK and EURNOK are highly correlated with a coefficient of 0,68. This 
# strong positive correlation suggests that the two variables move in tandem, 
# which can lead to multicollinearity issues. Multicollinearity can make it 
# challenging to distinguish the unique contribution of each variable to the
# response variable. I will choose to include EURNOK in the model, under 
# the assumption that most of the trades are more related to EUR or EURNOK
# in this case.

model_exr <- lm(log_value ~ linear_trend + EURNOK + january + february + march 
                + april + may + june + july + august + september + october + 
                   november + december, data = cds_seafood_exr)

stargazer(model_exr, type = "text")

# A positive coefficient of 0.054 for EURNOK suggests that an increase in the
# EURNOK exchange rate is associated with an increase in seafood prices. 
# In other words, as the value of the Euro (EUR) strengthens against the 
# Norwegian Krone (NOK), seafood prices are expected to increase.
# The coefficient estimate is significant at a 1% level. 

# linear_trend (c) model: -0,0005
# linear_trend (h) model: 0,006* 

# The significant difference in the "linear_trend" coefficient between this 
# model, which includes EURNOK, compared to the one without, from (c), is 
# likely due to the inclusion of the "EURNOK" variable. When "EURNOK" is
# included, it captures the effect of changes in the 
# exchange rate on the "log_value." As a result, the "linear_trend" in the 
# this model may reflect an overall positive trend that includes the influence
# of the exchange rate.

# (i) Add the lag of log price to your regression in (h), that is estimate an 
# autoregressive model. Interpret the results.

# It is not specified how many lags to add, therefore we just add the first lag

# Create the lag of 'log_value'
cds_seafood_exr <- cds_seafood_exr %>%
  mutate(log_value_lag = lag(log_value))

# Remove the first line as it is NA




# Regression including L1 of log_value
model_exr_lag_1 <- lm(log_value ~ linear_trend + EURNOK + log_value_lag +
                        january + february + 
                      march + april + may + june + july + august + september +
                      october + november + december, data = cds_seafood_exr)

stargazer(model_exr_lag_1, type = "text")

# The coefficient for log_value_lag is 0.743 and is statistically significant 
# at a 1%-level. This indicates that there is a strong positive autoregressive
# relationship between the current "log_value" and the lagged "log_value." 
# In other words, past values of "log_value" have a strong influence on the
# current value.

# Filter the dataset to include rows for both "RU" and "Total"
russian_and_total_seafood <- cds_seafood %>%
  filter(country %in% c("RU", "Total"))

# Create a time series plot for Russian and Total seafood exports
ggplot(data = russian_and_total_seafood, aes(x = date, y = log_value, color = country)) +
  geom_line() +
  labs(x = "Date", y = "Log Value") +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("Norwegian Seafood Exports, Russia vs Total")

# The plot shows the total log value of norwegian seafood exports over time.
# It shows a big decline in the log value exported to russia in 2014, yet the 
# total log value of exports seem unaffected by this specific deline. 
# This indicates that the total effect on the total value of exports are 
# more or less unaffected by the Russian ban of Norwegian seafood, which could
# be due to the following increase of demand from the EU.

cds_seafood$to_russia <- ifelse(cds_seafood$country == "RU", 1, 0)

model <- lm(log_value ~ linear_trend + to_russia + linear_trend * to_russia, 
            data = cds_seafood)

stargazer(model, type = "text")

