# This script obtains census tract boundaries for the entire US in 2000 through 2020.
# 1990 tracts are unavailable but not very accurate due to low resolution
# Working here in WGS84 (EPSG 4326) for simplicity. This takes a while to run, just a warning.

# I also generated lists of the neighbors (bordering tracts) which can be used
# to do spatial weights and other things. If you wanted to be fancy, that could
# be useful for doing a spatial smooth over the demographic measures.

library(tidyverse)
library(sf)
library(spdep) # For getting spatial neighbors

get_neighbors <- function(df, id, snap = 0.001){
  out <- df |>
    select(all_of(id), "geometry") |>
    # Reclassing poly2nb lets it be used in a normal mutate call
    mutate(neighbors = magrittr::set_class(poly2nb(geometry, snap = snap), "list")) |>
    st_drop_geometry() |>
    unnest(neighbors) |>
    filter(neighbors != 0) |> # Drop isolates; these are typically islands
    mutate(neighbors = df[[id]][neighbors])
  return(out)
}

# DC is not in the state.abb object built into R but is needed to get DC tracts
states_and_dc <- c(state.abb, "DC")

if(file.exists("./data/derived/boundaries/us_tracts_2000.RData")){
  load("./data/derived/boundaries/us_tracts_2000.RData")
} else {
us_tracts_2000 <- map_df(states_and_dc, ~tigris::tracts(.x, cb = FALSE, year = 2000)) |>
  mutate(tract_2000 = CTIDFP00)  |>
  st_transform(4326) |>
  select(tract_2000, geometry, land_area = ALAND00)
save(us_tracts_2000, file = "./data/derived/boundaries/us_tracts_2000.RData")
}

if(file.exists("./data/derived/neighbors/us_tracts_2000_neighbors.RData")){
  load("./data/derived/neighbors/us_tracts_2000_neighbors.RData")
} else {
us_tracts_2000_neighbors <- get_neighbors(us_tracts_2000, "tract_2000")
save(us_tracts_2000_neighbors, file = "./data/derived/neighbors/us_tracts_2000_neighbors.RData")
}


if(file.exists("./data/derived/boundaries/us_tracts_2010.RData")){
  load("./data/derived/boundaries/us_tracts_2010.RData")
} else {
us_tracts_2010 <- map_df(states_and_dc, ~tigris::tracts(.x, cb = FALSE, year = 2010)) %>%
  mutate(tract_2010 = GEOID10) %>%
  st_transform(4326) |>
  select(tract_2010, geometry, land_area = ALAND10)
save(us_tracts_2010, file = "./data/derived/boundaries/us_tracts_2010.RData")
}
1
if(file.exists("./data/derived/neighbors/us_tracts_2010_neighbors.RData")){
  load("./data/derived/neighbors/us_tracts_2010_neighbors.RData")
} else {
  us_tracts_2010_neighbors <- get_neighbors(us_tracts_2010, "tract_2010")
  save(us_tracts_2010_neighbors, file = "./data/derived/neighbors/us_tracts_2010_neighbors.RData")
}

if(file.exists("./data/derived/boundaries/us_tracts_2020.RData")){
  load("./data/derived/boundaries/us_tracts_2020.RData")
} else {
  us_tracts_2020 <- map_df(states_and_dc, ~tigris::tracts(.x, cb = FALSE, year = 2020)) %>%
    mutate(tract_2020 = GEOID) %>%
    st_transform(4326) |>
    select(tract_2020, geometry, land_area = ALAND)  
  save(us_tracts_2020, file = "./data/derived/boundaries/us_tracts_2020.RData")
}

if(file.exists("./data/derived/neighbors/us_tracts_2020_neighbors.RData")){
  load("./data/derived/neighbors/us_tracts_2020_neighbors.RData")
} else {
  us_tracts_2020_neighbors <- get_neighbors(us_tracts_2020, "tract_2020")
  save(us_tracts_2020_neighbors, file = "./data/derived/neighbors/us_tracts_2020_neighbors.RData")
}
