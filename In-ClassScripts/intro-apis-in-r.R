library(httr2)
library(tidyverse)

# Temporarily down - likely due to gov. funding
mars_base_url <- "https://api.nasa.gov/mars-photos/api/v1/rovers/curiosity/photos"
nasa_api_token <- "DEMO_KEY"

## Create , but don't run query. See what will be sent to server
request <- request(mars_base_url) |>
  req_url_query(sol = "1000", camera = "fhaz", api_key = nasa_api_token)

# Check the request
req_verbose(request)
response <- req_perform(request)

images <- response |>
  resp_body_json() |>
  pluck('photos')

print(images)
str(data, max.level = 2)


###############
##- Weather.gov
###############

weather_base_url <- "https://api.weather.gov"

request <- request(weather_base_url) |>
  req_url_path_append(
    'points',
    '39.296318, -76.592941'
  )

req_verbose(request)

response <- req_perform(request)


response |>
  resp_body_json() |>
  # Shows structure of nested list
  glimpse()

response_json <- response |>
  resp_body_json()
pluck('@context', 'type')
#  pluck('properties', 'forecastHourly')

obj1 <- list("a", list(1, elt = "foo"))
obj2 <- list("b", list(2, elt = "bar"))
x <- list(obj1, obj2)
