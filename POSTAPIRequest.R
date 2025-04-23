library(httr)      # For making HTTP requests
library(jsonlite)  # For converting R list to JSON


url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/PersBilarA"

query <- list(
  query = list(
    list(
      code = "Region",
      selection = list(
        filter = "item",
        values = list("01")
      )
    ),
    list(
      code = "Agarkategori",
      selection = list(
        filter = "item",
        values = list("000")
      )
    ),
    list(
      code = "Tid",
      selection = list(
        filter = "item",
        values = list("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
      )
    )
  ),
  response = list(
    format = "xlsx" 
  )
)

query_json <- toJSON(query, auto_unbox = TRUE)

response <- POST(url, body = query_json, encode = "json", content_type("application/json"))


if (status_code(response) == 200) {
  
  writeBin(content(response, "raw"), "scb_passenger_cars.xlsx")
  
  cat("Excel file 'scb_passenger_cars.xlsx' created successfully.\n")
} else {
  cat("Error fetching data: ", status_code(response), "\n")
  cat(content(response, as = "text"), "\n")
  stop("Failed to fetch data from the API.")
}