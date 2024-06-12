# This script process data used to construct measures of latent neighbourhood social structural variables.
# I load the 1980 LTDB data but end up not using them due to some questionable
# values. Much of the US wasn't tracted in 1980 anyway. If you need 1980 measures
# you'll need to modify accordingly.

library(tidyverse)
library(sf)
load("./data/derived/file_paths.RData")
load("./data/derived/boundaries/us_tracts_2010.RData")
`%!in%` <- Negate(`%in%`)

# Here I manually set the number of columns in each because I'll need that in 
# the next step when I set column types.

ltdb_std_fullcount_files <- data.frame(files = list.files(ltdb_full_path, pattern = "td_(1|2)(9|0)(8|9|0|1|2)", full.names = TRUE), ncols = c(39,54,51,47,8))
ltdb_std_sample_files    <- data.frame(files = list.files(ltdb_sample_path, pattern = "td_(1|2)(9|0)(8|9|0|1|2)", full.names = TRUE), ncols = c(91,75,77,177,177))

# This unholy mess reads each file in and makes sure the tractid column is read
# in as character so it doesn't drop leading zeroes.
ltdb_std_fullcount <- with(ltdb_std_fullcount_files,
                           map2(files, 
                                ncols, 
                           ~ read_csv(.x, col_types = paste0(c("c", rep("?", .y-1)), collapse = "")) |>
                             janitor::clean_names())) |>
  setNames(paste0("ltdb_", str_extract(ltdb_std_fullcount_files$files, "[0-9]{4}")))

ltdb_std_sample <-  with(ltdb_std_sample_files,
                         map2(files, 
                              ncols, 
                        ~ read_csv(.x, col_types = paste0(c("cc", rep("?", .y-2)), collapse = "")) |> 
                          janitor::clean_names())) |>
  setNames(paste0("ltdb_", str_extract(ltdb_std_sample_files$files, "[0-9]{4}")))

# Next, I do each set of years independently because they have idiosyncracies.
# You could turn it into a single step but it'd take longer to write that code
# up than it takes to do them separately.

# d_ variables indicate denominators
# n_ variables indicate numerators

# 1980
ltdb_1980_counts <- inner_join(ltdb_std_fullcount[["ltdb_1980"]] |> 
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0")) ,
                               ltdb_std_sample[["ltdb_1980"]] |> 
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0"))) |>
  mutate(year = 1980) |>
  rename_with(~str_remove(., "[0-9]{2}$")) |>
  select(tract_2010 = trtid,
         year,
         d_POP              = pop,
         n_POP_black        = nhblk,
         n_POP_hispanic     = hisp,
         n_POP_under18      = a18und,
         d_HU_total         = hu,
         d_HU_occupied      = ohu,
         n_HU_owned         = own,
         n_FAM_fhh          = fhh,
         d_FAM              = family,
         d_population       = pop80sf3,
         n_foreign          = fb,
         d_ED               = ag25up,
         n_ED_highschool    = hs,
         n_ED_college       = col,
         d_EMP              = clf,
         n_EMP_unemployed   = unemp,
         n_EMP_professional = prof,
         d_poverty          = dpov,
         n_poverty          = npov,
         n_HU_moved10yrs    = h10yrs,
         d_HU_occupied_sp   = ohu80sp)

ltdb_1980_percents <- ltdb_1980_counts |>
  transmute(
    tract_2010        = tract_2010,
    year              = year,
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_POP,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population        = d_POP
  )

# 1990
ltdb_1990_counts <- inner_join(ltdb_std_fullcount[["ltdb_1990"]] |> 
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0")) ,
                               ltdb_std_sample[["ltdb_1990"]] |> 
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0"))) |>
  mutate(year = 1990) |>
  rename_with(~str_remove(., "[0-9]{2}$")) |>
  select(tract_2010 = trtid,
         year,
         d_POP              = pop,
         n_POP_black        = nhblk,
         n_POP_hispanic     = hisp,
         n_POP_under18      = a18und,
         d_HU_total         = hu,
         d_HU_occupied      = ohu,
         n_HU_owned         = own,
         n_FAM_fhh          = fhh,
         d_FAM              = family,
         d_population       = pop90sf3,
         n_foreign          = fb,
         d_ED               = ag25up,
         n_ED_highschool    = hs,
         n_ED_college       = col,
         d_EMP              = clf,
         n_EMP_unemployed   = unemp,
         n_EMP_professional = prof,
         d_poverty          = dpov,
         n_poverty          = npov,
         n_HU_moved10yrs    = h10yrs,
         d_HU_occupied_sp   = ohu90sp)

ltdb_1990_percents <- ltdb_1990_counts |>
  transmute(
    tract_2010 = tract_2010,
    year = year,
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_POP,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population   = d_POP
  )

# 2000
ltdb_2000_counts <- inner_join(ltdb_std_fullcount[["ltdb_2000"]] |>
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0")),
                               ltdb_std_sample[["ltdb_2000"]] |>
                                 mutate(trtid10 = str_pad(trtid10, 11, "left", "0"))) |>
  mutate(year = 2000) |>
  rename_with(~str_remove(., "[0-9]{2}$")) |>
  select(tract_2010 = trtid,
         year,
         d_POP              = pop,
         n_POP_black        = nhblk,
         n_POP_hispanic     = hisp,
         n_POP_under18      = a18und,
         d_HU_total         = hu,
         d_HU_occupied      = ohu,
         n_HU_owned         = own,
         n_FAM_fhh          = fhh,
         d_FAM              = family,
         d_population       = pop00sf3,
         n_foreign          = fb,
         d_ED               = ag25up,
         n_ED_highschool    = hs,
         n_ED_college       = col,
         d_EMP              = clf,
         n_EMP_unemployed   = unemp,
         n_EMP_professional = prof,
         d_poverty          = dpov,
         n_poverty          = npov,
         n_HU_moved10yrs    = h10yrs,
         d_HU_occupied_sp   = ohu00sp)

ltdb_2000_percents <- ltdb_2000_counts |>
  transmute(
    tract_2010 = tract_2010,
    year = year,
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_POP,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population   = d_POP
  )

# 2010
# Potential issue: Different numbers of tracts in fullcount and sample, and 20 
# tracts in one but the other.
ltdb_2010_counts <- inner_join(ltdb_std_fullcount[["ltdb_2010"]] |>
                                 mutate(tractid = str_pad(tractid, 11, "left", "0")),
                               ltdb_std_sample[["ltdb_2008"]] |> 
                                 mutate(tractid = str_pad(tractid, 11, "left", "0"))) |>
  mutate(year = 2010) |>
  select(tract_2010 = tractid,
         year,
         d_POP              = pop10,
         n_POP_black        = nhblk10,
         n_POP_hispanic     = hisp10,
         n_POP_under18      = a18und10,
         d_HU_total         = hu10,
         d_HU_occupied      = ohu10,
         n_HU_owned         = own10,
         n_FAM_fhh          = fhh10,
         d_FAM              = family10,
         d_population       = pop12,
         n_foreign          = fb12,
         d_ED               = ag25up12,
         n_ED_highschool    = hs12,
         n_ED_college       = col12,
         d_EMP              = clf12,
         n_EMP_unemployed   = unemp12,
         n_EMP_professional = prof12,
         d_poverty          = dpov12,
         n_poverty          = npov12,
         n_HU_moved10yrs    = h10yrs12,
         d_HU_occupied_sp   = ohu12)

ltdb_2010_percents <- ltdb_2010_counts |>
  transmute(
    tract_2010 = tract_2010,
    year = year,
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_POP,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population   = d_POP
  )

# 2020
# 2020 has very few measures in census, most shifted to ACS. ACS here is actually
# 2015-2019 5 year ACS. This will need to be updated later with more
# recent ACS when available.
ltdb_2020_counts <- inner_join(ltdb_std_fullcount[["ltdb_2020"]] |> 
                                 mutate(tractid = str_pad(trtid2010, 11, "left", "0")) |> 
                                 select(-trtid2010) ,
                               ltdb_std_sample[["ltdb_2015"]] |> 
                                 mutate(tractid = str_pad(tractid, 11, "left", "0"))) |>
  mutate(year = 2020) |> 
  select(tract_2010 = tractid,
         year,
         d_POP              = pop20,
         n_POP_black        = nhblk20,
         n_POP_hispanic     = hisp20,
         n_POP_under18      = a18und19,
         # d_HU_total         = hu10,
         d_HU_total         = hu19,
         # d_HU_occupied      = ohu10,
         d_HU_occupied      = ohu19,
         # n_HU_owned         = own10,
         n_HU_owned         = own19,
         # n_FAM_fhh          = fhh10,
         n_FAM_fhh          = fhh19,
         # d_FAM              = family10,
         d_FAM              = family19,
         d_population       = pop19,
         n_foreign          = fb19,
         d_ED               = ag25up19,
         n_ED_highschool    = hs19,
         n_ED_college       = col19,
         d_EMP              = clf19,
         n_EMP_unemployed   = unemp19,
         n_EMP_professional = prof19,
         d_poverty          = dpov19,
         n_poverty          = npov19,
         n_HU_moved10yrs    = h10yrs19,
         d_HU_occupied_sp   = ohu19) 

ltdb_2020_percents <- ltdb_2020_counts |>
  transmute(
    tract_2010 = tract_2010,
    year = year,
    perc_black        = n_POP_black/d_POP,
    perc_hisp         = n_POP_hispanic/d_POP,
    perc_under18      = n_POP_under18/d_population,
    perc_owned        = n_HU_owned/d_HU_occupied,
    perc_fhh          = n_FAM_fhh/d_FAM,
    perc_foreign      = n_foreign/d_population,
    perc_edhighschool = n_ED_highschool/d_ED,
    perc_edcollege    = n_ED_college/d_ED,
    perc_unemployed   = n_EMP_unemployed/d_EMP,
    perc_professional = n_EMP_professional/d_EMP,
    perc_poverty      = n_poverty/d_poverty,
    perc_moved        = n_HU_moved10yrs/d_HU_occupied_sp,
    population   = d_POP
  )


# A quick glimpse at the 1980 data shows obviously invalid values. I think the
# raw population counts are probably fine but everything else is suspect. As a
# result I am not including 1980 values in any calculations. This is prob just a
# result of untracted areas but I am not looking further into it as I do not
# need pre-1990 values

# Use tract boundaries to get land areas and identify zero area tracts to remove
zero_area_tracts <- us_tracts_2010 |> 
  st_drop_geometry() |>
  filter(near(land_area, 0)) |>
  select(tract_2010)

# Bind and make implicit missings into explicit missings

ltdb_percents <- 
  bind_rows(ltdb_1990_percents, 
            ltdb_2000_percents, 
            ltdb_2010_percents, 
            ltdb_2020_percents) |>
  filter(tract_2010 %!in% zero_area_tracts) |>
  arrange(tract_2010, year) |>
  complete(tract_2010, year)

ltdb_counts <- 
  bind_rows(ltdb_1990_counts, 
            ltdb_2000_counts, 
            ltdb_2010_counts, 
            ltdb_2020_counts) |>
  filter(tract_2010 %!in% zero_area_tracts) |>
  arrange(tract_2010, year) |>
  complete(tract_2010, year)

save(ltdb_percents, file = "./data/derived/demographics/ltdb_percents.RData")
save(ltdb_counts, file = "./data/derived/demographics/ltdb_counts.RData")

