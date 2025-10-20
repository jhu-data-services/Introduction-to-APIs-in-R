base_url <- 'https://pokeapi.co/api/v2/'

## Use req_url_path_append to construct the endpoint path
## Let's use the pokemon endpoint

## Note: We are using namespace qualification, e.g. package::function()
request <- httr2::request(base_url) |>
  httr2::req_url_path_append('pokemon')

## Perform request
response <- httr2::req_perform(request)

## If we take a look at the response body, we see an odd set of hexadecimal values.
response$body

## These are the raw bytes representing the contents of the response.
## httr2 doesn’t assume what format the content is (could be JSON, image, binary file, etc.) until you tell it.
## We know the pokeAPI returns a JSON, so we will ask it to return the response as JSON

response |>
  httr2::resp_body_json()

## That's hard to read, we can use glimpse to get a better view
## Use glimpse to get better overview

response |>
  httr2::resp_body_json() |>
  dplyr::glimpse()

## Let's look at a specific pokemon, e.g. bulbasaur
## Use req_url_path_append to construct the endpoint path

request <- httr2::request(base_url) |>
  httr2::req_url_path_append('pokemon', 'ninetales')

response <- httr2::req_perform(request) |> httr2::resp_body_json()

response |>
  dplyr::glimpse()

## How do we extract data from a JSON?
## JSON is highly nested - this can make it more difficult than working with a data table.
## Let's try to coerce the data we care about into a tibble.

bulbasaur_stats <- tibble(
  sprite = response$sprites$front_default,
  name = response$name,
  species = response$species$name,
  height = response$height,
  weight = response$weight,
)

## What happens when we have multiple names stats that we would like to extract?
## We could use pluck if we knew the positions of the stats would be consistent, but
## we don't, so we will aggregate them into a DF and extract the ones we need.
stats <- purrr:::map_df(
  response$stats,
  ~ tibble(stat_name = .x$stat$name, stat = .x$base_stat)
)

## Now with our stats in a dataframe
bulbasaur_stats <- tibble(
  sprite = response$sprites$front_default,
  name = response$name,
  species = response$species$name,
  height = response$height,
  weight = response$weight,
  hp = stats$stat[stats$stat_name == "hp"],
  defense = stats$stat[stats$stat_name == "defense"],
  attack = stats$stat[stats$stat_name == "attack"]
) |>
  mutate(sprite = paste0('<img src="', sprite, '" height="50"></img>'))


## What if we want stats for multiple pokemon?
## We need to use pagination

## Let's request the first 100 pokemon

request <- httr2::request(base_url) |>
  httr2::req_url_path_append('pokemon') |>
  #  req_throttle(rate = 10, fill_time_s = 60) |>
  req_url_query(limit = 20)

next_page_handler <- function(resp, req) {
  response_body <- resp_body_json(resp)
  next_url <- response_body$`next`
  ## Error handling
  if (is.null(next_url)) {
    return(NULL)
  } else {
    req |> req_url(next_url)
  }
}

responses <- httr2::req_perform_iterative(
  request,
  next_req = next_page_handler,
  max_reqs = 5,
  on_error = "return"
)

pokemon_names <-
  purrr::map_dfr(responses, function(response) {
    body <- httr2::resp_body_json(response)
    purrr::map_dfr(body$results, tibble::as_tibble)
  })

## Now how do we make 100 API calls to get the statistics for each Pokemon?

get_pokemon_details <- function(url) {
  response <- request(url) |> req_perform() |> resp_body_json()

  ##
  Sys.sleep(.1)
  stats <- purrr:::map_df(
    response$stats,
    ~ tibble(stat_name = .x$stat$name, stat = .x$base_stat)
  )

  tibble(
    sprite = response$sprites$front_default,
    name = response$name,
    height = response$height,
    weight = response$weight,
    hp = stats$stat[stats$stat_name == "hp"],
    defense = stats$stat[stats$stat_name == "defense"],
    attack = stats$stat[stats$stat_name == "attack"]
  ) |>
    mutate(sprite = paste0('<img src="', sprite, '" height="50"></img>'))
}

pokemon_stats <- map_dfr(pokemon_names$url, get_pokemon_details)

# Don't escape HTML, we want it to run
datatable(data = pokemon_stats, escape = FALSE)

## Be Kind - Cache your Data!
