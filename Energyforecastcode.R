library(forecast)



elecpercapita <- pivot_longer(population, cols = everything())
population <- pivot_longer(population, cols = everything())
gdp <- pivot_longer(gdp, cols = everything())

model <- lm(elecpercapita ~ population + gdp, data = result)

population_ts <- ts(result$population, start = 1971, end = 2019, frequency = 1)

gdp_ts <- ts(result$gdp, start = 1971, end = 2019, frequency = 1)


population_model <- auto.arima(population_ts)
gdp_model <- auto.arima(gdp_ts)

# Forecast the population and GDP for 2021-2030
population_forecast <- forecast(population_model, h = 11)
gdp_forecast <- forecast(gdp_model, h = 11)

# Extract the forecasted values
future_population <- population_forecast$mean
future_gdp <- gdp_forecast$mean

future_data <- data.frame(
  year = 2020:2030,
  population = future_population,
  gdp = future_gdp
)


names(result)
# Fit the model
model <- lm(elecpercapita ~ population + gdp, data = result)

# Forecast electricity consumption for 2021-2030
future_data$elecpercapita <- predict(model, newdata = future_data)


# Convert 'year' column to integer in both data frames
result$year <- as.integer(result$year)
future_data$year <- as.integer(future_data$year)

# Combine the data frames
projection <- bind_rows(result, future_data)


projection$electconsumption <- projection$population * projection$elecpercapita


ggplot(projection, aes(x = year)) +
  geom_line(aes(y = electconsumption, color = "Electricity Consumption")) +
  geom_line(aes(y = population, color = "Population")) +
  geom_line(aes(y = gdp, color = "GDP")) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Population & GDP")) +
  labs(title = "Electricity Consumption, Population, and GDP (1970-2030)",
       x = "Year",
       y = "Electricity Consumption",
       color = "Variable") +
  theme_minimal()
