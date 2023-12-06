df <- tibble(
  labels = c("control", "test"),
  raw_count = c(2,38),
  per_meter = c(90, 1704)
  ) |>
  mutate(
    scale = raw_count/per_meter,
    numerator = scale*75*1000
  ) 
