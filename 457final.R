
#| message: false
#| warning: false
# 1) Check that both have the same length
length(fc$mean)   # horizon of the forecast
length(test_ts)   # length of the test set

# 2) Convert both to numeric
fc_vec   <- as.numeric(fc$mean)
test_vec <- as.numeric(test_ts)

# 3) Confirm they match in length
if(length(fc_vec) != length(test_vec)){
  stop("Forecast length != Test set length. Adjust horizon or test split.")
}

# 4) Now do accuracy on numeric vectors
accuracy(fc_vec, test_vec)



#| message: false
#| warning: false
# Create a new variable: daily log returns
data$return <- c(NA, diff(log(data$`ICCO daily price (US$/tonne)`)))

# Remove the initial NA from returns
var_data <- na.omit(data[, c("Date", "ICCO daily price (US$/tonne)", "return")])
colnames(var_data) <- c("Date", "Price", "Return")

# Convert the Price and Return columns into a time series object.
# (You may adjust the frequency if needed; here we use 365 for daily data.)
start_year <- year(min(var_data$Date))
start_doy  <- yday(min(var_data$Date))
var_ts <- ts(var_data[, c("Price", "Return")], frequency = 365, start = c(start_year, start_doy))

# Quick check of the resulting time series:
print(head(var_ts))


#| message: false
#| warning: false
# Determine optimal lag length using VARselect (try up to 10 lags)
lag_selection <- VARselect(var_ts, lag.max = 10, type = "const")
print(lag_selection$selection)

# For demonstration, assume the optimal lag is 1 (adjust based on lag_selection).
var_model <- VAR(var_ts, p = 1, type = "const")
summary(var_model)

#| message: false
#| warning: false
# Check for serial correlation in the residuals (Portmanteau test)
serial_test <- serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")
print(serial_test)

# Check the stability of the VAR model
stability_results <- stability(var_model)
par(mar = c(2, 4, 2, 2))
plot(stability_results)

#| message: false
#| warning: false
# Produce impulse response functions:
# For example, assess the response of both Price and Return to a shock in Price.
irf_price <- irf(var_model, impulse = "Price", response = c("Price", "Return"), n.ahead = 10, boot = TRUE)
plot(irf_price)

#| message: false
#| warning: false
# Forecast the VAR model for the next 30 periods
var_forecast <- predict(var_model, n.ahead = 30)
par(mar = c(2, 4, 2, 2))
plot(var_forecast)

#| message: false
#| warning: false
# Forecast the VAR model for the next 365days
var_forecast <- predict(var_model, n.ahead = 365)

# Extract the forecast for the Price series
price_fc <- var_forecast$fcst$Price

# Plot the forecasted values for Price along with prediction intervals
plot.ts(price_fc[, "fcst"],
        main = "Forecast for Price",
        ylab = "Price",
        col = "blue",
        lwd = 2,
        ylim = range(price_fc))
lines(price_fc[, "lower"], col = "red", lty = 2)
lines(price_fc[, "upper"], col = "red", lty = 2)
legend("bottomright",
       legend = c("Forecast", "Prediction Interval"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = c(2, 1))

#| message: false
#| warning: false
# Forecast the VAR model for the next 30 periods
var_forecast <- predict(var_model, n.ahead = 30)

# Extract the forecast for the Return series
return_fc <- var_forecast$fcst$Return
# And then similar plotting code...

# Plot the forecasted values for Return along with prediction intervals
plot.ts(return_fc[, "fcst"],
        main = "Forecast for Return",
        ylab = "Price",
        col = "blue",
        lwd = 2,
        ylim = range(price_fc))
lines(price_fc[, "lower"], col = "red", lty = 2)
lines(price_fc[, "upper"], col = "red", lty = 2)
legend("topleft",
       legend = c("Forecast", "Prediction Interval"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = c(2, 1))



