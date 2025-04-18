# Load required packages
library(httr)      # For making HTTP requests
library(jsonlite)  # For converting R list to JSON

# Define the API URL for the PersBilarA table
url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/PersBilarA"

# Define the JSON query as an R list, with response format set to "xlsx"
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
    format = "xlsx"  # Set the response format to Excel (xlsx)
  )
)

# Convert the query to JSON
query_json <- toJSON(query, auto_unbox = TRUE)

# Send the POST request
response <- POST(url, body = query_json, encode = "json", content_type("application/json"))

# Check if the request was successful
if (status_code(response) == 200) {
  # Save the response content directly as an Excel file
  writeBin(content(response, "raw"), "scb_passenger_cars.xlsx")
  
  cat("Excel file 'scb_passenger_cars.xlsx' created successfully.\n")
} else {
  cat("Error fetching data: ", status_code(response), "\n")
  cat(content(response, as = "text"), "\n")
  stop("Failed to fetch data from the API.")
}