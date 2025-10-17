library(httr2)
library(tidyverse)
library(purrr)

## Note - use pluck for easy nested JSON field extraction

###############
##- PokeAPI
###############

base_url <- 'https://pokeapi.co/api/v2/'

## Use req_url_path_append to construct the endpoint path

## Let's use the pokemon endpoint

request <- request(base_url) |>
  req_url_path_append('pokemon')

response <- req_perform(request)

## If we take a look at the response body, we see an odd set of hexadecimal values.
response$body
## These are the raw bytes representing the contents of the response.
## httr2 doesn’t assume what format the content is (could be JSON, image, binary file, etc.) until you tell it.
response |>
  resp_body_json() |>
  lapply(function(x) x$name)

response |>
  resp_body_json() |>
  glimpse()

## Let's look at a specific pokemon, e.g. bulbasaur

request <- request(base_url) |>
  req_url_path_append('pokemon', 'bulbasaur')

response <- req_perform(request)

response |>
  resp_body_json() |>
  glimpse()


## What if we want stats for multiple pokemon?
## We need to use pagination

request <- request(base_url) |>
  req_url_path_append('pokemon') |>
  req_throttle(rate = 10, fill_time_s = 60) |>
  req_url_query(limit = 20)

#Explain the difference between zero based indexing (pokeapi) and R, and why offset
## tells the API to start after the first 19 items (the zero indexed starting point for the 20th item is 19).
## HERE THERE BE DRAGONS - STICK WITH NEXT, EDGE CASE FAILURES HERE
responses <- req_perform_iterative(
  request,
  next_req = iterate_with_offset("offset", offset = 19),
  on_error = "return",
  max_reqs = 5
)


## If time allows, build a page handler

next_page_handler <- function(resp, req) {
  response_body <- resp_body_json(resp)
  next_url <- response_body$`next`

  if (is.null(next_url)) {
    return(NULL)
  } else {
    req |> req_url(next_url)
  }
}

responses <- req_perform_iterative(
  request,
  next_req = next_page_handler,
  max_reqs = Inf,
  on_error = "return"
)

pokemon_names <-
  map_dfr(responses, function(response) {
    body <- resp_body_json(response)
    map_dfr(body$results, as_tibble)
  })


poke_url <- "https://pokeapi.co/api/v2/pokemon/20/"
response <- request(poke_url) |> req_perform() |> resp_body_json()
stats <- map_dfr(response$stats, ~ tibble(
                                          stat_name = .x$stat$name,
                                       base_state = .x$base_stat))
response |>
  pluck('stats') |> 
  map_dfr(
          \(x) {
            tibble(
                   speed = x |> pluck('defense')
                   )
            


pokemon_details <- function(url) {
  response <- request(url) |> req_perform() |> resp_body_json()
  map_df(response$stats, as.tibble)
}
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
