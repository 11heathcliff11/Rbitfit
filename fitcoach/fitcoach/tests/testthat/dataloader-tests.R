BulkRequest <- DataLoader$new()

for(i in paste("2016-02-", c("01", "02", "03", "04", "05"), sep = "")) {
  BulkRequest$get(type = 'summary', start_date = i)
  BulkRequest$get(type = 'time', activity = 'steps', end_date = "7d", start_date = i)
  BulkRequest$get(type = 'intraday', activity = 'steps', start_date = i, end_date = "1d", detail_level = "15min")
  BulkRequest$get(type = 'intraday', activity = 'steps', start_date = i, end_date = "1d", detail_level = "1min")
}

### Read JSON files and convert them to Data Frames

file_name <- "intraday_steps_2016-02-01_1d_15min.json"
file_path <- "./inst/extdata/tests/"
json_raw <- BulkRequest$readToDF(file = file_name, path = file_path)
