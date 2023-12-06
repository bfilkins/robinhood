plot <- portfolio %>%
  ggplot(aes(x=begins_at, y = open_equity)) +
  geom_line()
ggplotly(plot)


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
