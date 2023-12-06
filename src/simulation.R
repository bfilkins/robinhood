# Define back test function and parameters

back_test <- function(
    initial_investment = 10000,
    symbol_count = 20,
    symbols_exchanged = 10,
    start_date = date("2023-01-04"),
    end_date = date("2023-11-30")
    ) {
  simulation <- model_evaluation |>
    left_join( 
      model_evaluation |>
        filter(date == start_date) |>
        group_by(symbol) |>
        summarise() |>
        ungroup() |>
        sample_n(symbol_count) |>
        rowwise() |>
        mutate(num = rnorm(1,43,6)) |>
        ungroup() |>
        mutate(
          total = sum(num),
          allocation = num / total,
          invested_amount = initial_investment * allocation,
          date = start_date), 
      by = c("symbol" = "symbol", "date" = "date")) |>
    filter(date == start_date) |>
    mutate(
      initial_shares = invested_amount/open
    ) |>
    group_by(symbol) |>
    mutate(
      position_shares = if_else(is.na(initial_shares),0,initial_shares),
      position_value = position_shares * open) |>
    ungroup() |>
    mutate(
      held_position = if_else(position_shares > 0 , percent_change, NA ),
    ) |>
    group_by(date) |>
    mutate(
      held_prediction_rank = rank(held_position),
      sold_value = sum(if_else(held_prediction_rank <= symbols_exchanged, position_value, 0)),
      sold_shares = if_else(held_prediction_rank <= symbols_exchanged, position_shares, 0),
      highest_rank_prediction = rank(desc(percent_change)),
      purchased_shares = if_else(highest_rank_prediction <= symbols_exchanged, sold_value/open,0)
    ) |>
    ungroup()
  
  #wtf
  print(sum(simulation$allocation, na.rm = TRUE))
  print(sum(simulation$position_value, na.rm = TRUE))
  # fuckk <- simulation_side |> 
  #   bind_rows(
  #     model_evaluation |>
  #     filter(date == x)
  #     ) |>
  #   group_by(symbol) |>
  #   mutate(
  #     position_shares = if_else(row_number() > 1, lag(position_shares) + lag(sold_shares) + lag(purchased_shares), position_shares),
  #     position_value = position_shares * open
  #     )
  
  
  for (i in seq(start_date+1, end_date, by = "day")) {
    simulation <- simulation |> 
      bind_rows(
        model_evaluation |>
          filter(date == i)
      ) |>
      group_by(symbol) |>
      mutate(
        position_shares = if_else(row_number() > 1, lag(position_shares) + lag(-sold_shares) + lag(purchased_shares), position_shares),
        position_value = position_shares * open
      ) |>
      ungroup() |>
      mutate(
        held_position = if_else(position_shares > 0 , percent_change, NA ),
      ) |>
      group_by(date) |>
      mutate(
        held_prediction_rank = rank(held_position),
        sold_value = sum(if_else(held_prediction_rank <= symbols_exchanged, position_value, 0)),
        sold_shares = if_else(held_prediction_rank <= symbols_exchanged, position_shares, 0),
        highest_rank_prediction = rank(desc(percent_change)),
        overlap = if_else(held_prediction_rank <= symbols_exchanged & highest_rank_prediction <= symbols_exchanged, 1, 0),
        total_overlap = sum(overlap),
        purchased_shares = if_else(
          highest_rank_prediction <= symbols_exchanged+total_overlap & overlap != 1, (sold_value/symbols_exchanged)/open,0)
      ) |>
      ungroup()
    
}

#sum(simulation_side$position_value, na.rm = TRUE)
out<<-simulation

simulation |>
  #filter(date <= date("2023-11-30")) |>
  group_by(date) |>
  summarise(position_value = sum(position_value, na.rm = TRUE)) |>
  hchart("line", hcaes(x = date, y = position_value))
}

back_test()

simulation_side |> group_by(date) |>
  summarise(n = n()) |>
  arrange(n)
