# simulation app
library(lubridate)

# threshold parameters
max_trade_limit <- 100
sell_signal_threshold <- 0
sell_percentage <- 0
start_date <- date("2021-6-1")
end_date <- date("2022-1-1")
simulation_timeline_months <- 6
simulation_dates <- tibble(sim_dates = c(seq(start_date,end_date, by = "day")))

starting_positions <- positions %>%
  select(simple_name,symbol,quantity) %>% 
  mutate(start_date = start_date) %>%
  inner_join(positions_trends %>% select(symbol, open_price, begins_at), by = c("start_date" = "begins_at", "symbol" = "symbol")) %>%
  mutate(start_value = open_price * quantity)
