"02_population_data" |>
list.files(pattern="^aggregate_population_\\d{4}.R$",full.names=TRUE) |>
lapply(source)

