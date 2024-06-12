# This script interpolates the demographic data to individual years to get a 
# measure for age-specific neighborhood conditions that has less abrupt jumps.
# Given the LTDB data in 1980 appear to have problems, but it would be nice to have
# data going back before 1990, it might be worth consider doing linear extrapolation.

library(tidyverse)
library(zoo)

load("./data/derived/demographics/ltdb_factors.RData")

na_spline <- function(x) if (all(is.na(x))) NA else na.spline(x, na.rm = FALSE)
# Thanks https://stackoverflow.com/questions/66969126/interpolate-and-extrapolated-by-group-using-na-spline-and-case-when


ltdb_factors_2010_interpolated <- ltdb_factors %>%
  select(tract_2010, year, area_sqkm, population, FAC_disadv, FAC_hispimm, FAC_stability, PCA_disadv, matches("^perc")) %>%
  complete(year = 1990:2020, nesting(tract_2010, area_sqkm)) %>%
  group_by(tract_2010) %>%
  arrange(year) %>%
  mutate(across(c(population, FAC_disadv, FAC_hispimm, FAC_stability, PCA_disadv, matches("^perc")), ~na_spline(.), .names = "{.col}_spline")) %>%
  ungroup() %>%
  select(tract_2010, year, area_sqkm, matches("spline")) %>%
  arrange(tract_2010, year) %>%
  mutate(pop_per_sqkm_spline = population_spline / area_sqkm)

save(ltdb_factors_2010_interpolated, file = "./data/derived/demographics/ltdb_factors_interpolated.RData")
# 
# write_csv(ltdb_factors_2010_interpolated, file = "./demographics/data/export/ltdb_factors_2010_interpolated.csv")

