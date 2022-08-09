library(RobinHood)
library(tidyverse)
library(anomalize)
library(plotly)

#install.packages("RobinHood")
#devtools::install_github("jestonblu/RobinHood")

RH = RobinHood(
  username = Sys.getenv("personal_email"), 
  password = Sys.getenv("robinhood_password"),
  mfa_code = "784354")

portfolio <- get_portfolios(RH, interval = "day", span = "3month")

plot <- portfolio %>%
  ggplot(aes(x=begins_at, y = open_equity)) +
  geom_line()
ggplotly(plot)

# Returns a data frame of stock ownership positions
positions <- get_positions(RH)

# Returns a data frame of current crypto currency holdings
crypto_positions <- get_positions_crypto(RH)

positions_trends <- positions %>%
  mutate(trends = map(symbol, get_historicals, RH = RH, "day", "2year")) %>%
  unnest(cols = c(trends))

plot_positions <- positions_trends %>%
  ggplot(aes(x=begins_at, y = open_price, color = symbol)) +
  geom_line()
ggplotly(plot_positions)


analyzed_trends <- positions_trends %>%
  select(begins_at,open_price, symbol) %>%
  group_by(symbol) %>%
  # Data Manipulation / Anomaly Detection
  time_decompose(open_price, method = "stl") %>%
  anomalize(remainder, method = "GESD") %>%
  time_recompose() %>%
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Portfolio Anomalies", subtitle = "STL + GESD Methods") 

analyzed_trends
