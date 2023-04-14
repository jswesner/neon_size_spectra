library(neonUtilities)

# NEON stream temperatures
raw_stream_temperatures <- loadByProduct(
  "DP1.20053.001",
  package = "basic",
  startdate = "2016-01",
  enddate = "2021-12",
  timeIndex = 30,   # ignore the one-minute data. Only download 30 minute data
  check.size = FALSE
)

saveRDS(raw_stream_temperatures, file = "data/raw_data/temperature_raw-data.rds")

