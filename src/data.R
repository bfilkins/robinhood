

#portfolio <- get_portfolios(RH, interval = "day", span = "year")

# Returns a data frame of stock ownership positions
#positions <- get_positions(RH)

nasdaq2023_12_1 <- read.csv("data/nasdaq2023_12_1.csv") |>
 as_data_frame()

tictoc::tic() #takes about 15 mins?
tickers = nasdaq2023_12_1 |>
  #sample_n(150) |>
  select(symbol = Symbol) |>
  mutate(
    data = map(
      symbol,safely(insistently(tq_get, rate = rate_backoff(max_times = 10, jitter = TRUE))),
      get = "stock.prices", from = "2021-01-01", to = "2026-1-1" # update this to paste today?
      )
    )
tictoc::toc()

saveRDS(tickers, "data.RDS")

tickers <- readRDS("data.RDS") |>
  unnest_longer(data) |>
  select(-symbol) |>
  unnest(data)
  mutate(join = "x")

get_random_tickers <- function(n){
  tickers |>
    group_by(symbol) |>
    summarise() |>
    ungroup() |>
    sample_n(n)
  }



