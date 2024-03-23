#' ARIMA Forecast Plot with Confidence Intervals
#'
#' This function fits an ARIMA model to the provided time series data, makes forecasts, and plots the observed data along with the forecasted values and 95% confidence intervals.
#'
#' @param timeseries A time series vector containing the observed data
#' @return A plot showing the observed data, forecasted values, and 95% confidence intervals
#' @export

arima_forecast_plot <- function(timeseries) {
  arimaModel_1 <- arima(timeseries, order = c(1, 1, 0))

  forecast1 <- predict(arimaModel_1, n.ahead = 10, se.fit = TRUE)
  x_range <- 1:(length(timeseries) + length(forecast1$pred))
  y_range <- range(timeseries, forecast1$pred)

  plot(x_range, c(timeseries, forecast1$pred), type = "l", ylim = y_range,
       xlab = "Time", ylab = "Value", main = "ARIMA Forecast")
  lines(timeseries, col = "blue")
  lines(x_range[length(timeseries) + 1:length(forecast1$pred)], forecast1$pred, col = "red")

  lower_bound <- forecast1$pred - 1.96 * forecast1$se
  upper_bound <- forecast1$pred + 1.96 * forecast1$se

  lines(c(rep(NA, length(timeseries)), lower_bound), col = "darkcyan", lty = 2)
  lines(c(rep(NA, length(timeseries)), upper_bound), col = "darkcyan", lty = 2)

  legend("topleft", legend = c("Actual", "Forecast", "95% CI"),
         col = c("blue", "red", "darkcyan"), lty = c(1, 1, 2))
}
