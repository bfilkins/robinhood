

predict_symbol_next <- function(symbol_selected, trade_date){

  modeling_data <- tickers |>
    filter(
      date <= trade_date,
      date >= trade_date-365,
      symbol == symbol_selected
    ) |>
    select(symbol, ds = date, y = open) |>
    group_by(symbol) |>
    nest()

  future_frame <- tibble(
    ds = trade_date+1
  )

  modeled <- modeling_data |>
    mutate(
      model = map(data, prophet),
      predictions = map(model,  predict, future_frame)
    ) |>
    unnest_wider(predictions) |>
    ungroup() |>
    select(ds, yhat_lower, yhat_upper, yhat)
  # Add model performance metrics for decision making
  # measure variance and moving averages
  return(modeled)
}

# This takes ~1.4 seconds per call
tictoc::tic()
scenario_base <- tickers |>
  # sampling
  inner_join(
    tickers |>
      group_by(symbol)|>
      summarise()|>
      ungroup() |>
      sample_n(200)
    , by = c("symbol" = "symbol")) |>
  # end sampling
  select(symbol,date,open) |>
  filter(date >= date("2023-12-1")) |>
  ungroup() |>
  rowwise() |>
  mutate(
    predictions = map(symbol, safely(predict_symbol_next, otherwise = NA), date),
    predictions$result
    ) |>
  ungroup() |>
  filter(!is.na(yhat)) |>
  select(-predictions)
tictoc::toc()


#saveRDS(scenario_base, "scenario_base_jan_nov_23.RDS")
#scenario_base <- readRDS("scenario_base_jan_nov_23.RDS")

model_evaluation <- scenario_base |>
  group_by(symbol) |>
  mutate(next_actual = lead(open, 1)) |>
  ungroup() |>
  filter(!is.na(next_actual)) |>
  mutate(
    actual_difference = yhat - next_actual,
    predicted_difference = yhat - open,
    percent_change =  predicted_difference/open,
    outcome_class = case_when(
      actual_difference > 0 & predicted_difference > 0  ~ "true_positive",
      actual_difference < 0 & predicted_difference > 0 ~ "false_positive",
      actual_difference < 0 & predicted_difference < 0 ~ "true_negative",
      actual_difference > 0 & predicted_difference < 0 ~ "false_negative")
    ) |>
  group_by(date) |>
  mutate(tile = ntile(desc(predicted_difference),10)) |>
  ungroup() |>
  group_by(symbol) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n == max(n))


# # 
# diagnostic_plot <- model_evaluation |>
#   filter(date >= "2023-7-1") |>
#   group_by(outcome_class) |>
#   summarise(n = n()) |>
#   hchart("column", hcaes(x=outcome_class, y = n))
# diagnostic_plot
# # 
# # extreme_diagnostic <- model_evaluation |>
# #   filter(tile == 1 | tile == 4) |>
# #   group_by(outcome_class) |>
# #   summarise(n = n()) |>
# #   hchart("column", hcaes(x=outcome_class, y = n))
# # extreme_diagnostic
