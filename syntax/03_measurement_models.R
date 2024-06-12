library(tidyverse)
library(GPArotation) # For factor analysis
library(units)

load("./data/derived/boundaries/us_tracts_2010.RData")
load("./data/derived/demographics/ltdb_percents.RData")
load("./data/derived/demographics/ltdb_counts.RData")

# DISADVANTAGE BY PCA

# I first operate under the assumption that disadvantage is a formative construct
# with a composition that is fixed over time (i.e., fixed loadings). Thus, I use
# a principal component analysis. My tests indicate you get basically identical
# estimates if you use loadings that are allowed to vary across years, i.e., they
# pretty much satisfy measurement invariance -except- for unemployment, which
# has declined in importance. Again, inconsequential for PCA / factor scores.

# Can only use nonmissing values in a PCA, so drop the small number of NAs. Note
# that some tracts are included that have missing years of data, e.g., a tract
# missing data for 1990 but not 2000 will have a measure for 2000 (but not 1990).

ltdb_percents_nonmissing <- ltdb_percents |>
  arrange(tract_2010, year) |>
  # Zero population tracts contribute no information and cannot have a measure
  filter(population > 0) |>
# Select only vars used to calculate disadvantage
  select(tract_2010, year, perc_fhh, perc_unemployed, perc_poverty, perc_edhighschool, perc_edcollege, perc_professional) |>
  # Remove missing
  na.omit() 

# Pull first principal component
ltdb_pca <- ltdb_percents_nonmissing |>
  select(-tract_2010, -year) |>
  prcomp(scale. = TRUE, center = TRUE) |>
  (\(prcomp_output){prcomp_output$x[,"PC1"]})() |>
  cbind(ltdb_percents_nonmissing, PCA_disadv = _) |> 
  select(tract_2010, year, PCA_disadv) |>
  as_tibble()

# SOCIAL DISORGANIZATION ANTECEDENTS

# Next I get the three classic social disorg antecedents under assumption they are reflective constructs where the indicators are
# manifestations of each underlying concept (but I'm a bit agnostic about how that works). Thus, I use a simple factor model 
# here and produce regression scores. IIRC, the most appropriate for use in
# secondary analyses are regression scores from an oblimin rotation calculated
# via maximum likelihood. You may know better; change if so.

ltdb_factors <- ltdb_percents |>
  arrange(tract_2010, year) |>
  # Zero population tracts contribute no information and cannot have measures
  filter(population > 0) |>
  (\(x){
   fac_scores <- x |>
      select(perc_black, perc_hisp, perc_under18, perc_owned, perc_fhh, perc_foreign, perc_unemployed, perc_poverty, perc_moved) |>
      psych::fa(nfactors = 3,  scores="regression", rotate="oblimin", fm="ml") |> 
      pluck("scores")
    out <- cbind(x |> 
            select(tract_2010, year, population, everything()), fac_scores) |>
      as_tibble() |> 
      rename(FAC_stability = ML2, FAC_disadv = ML3, FAC_hispimm = ML1)
    return(out)
  })() |>
  full_join(ltdb_pca) |>
  # Use tract boundaries to get land areas and calculate population density
  left_join(us_tracts_2010 |> 
              st_drop_geometry() |>
              transmute(tract_2010, 
                        area_sqkm = as.numeric(set_units(as_units(land_area, "m^2"), "km^2")))) |>
  mutate(pop_per_sqkm = as.numeric(population / area_sqkm))

save(ltdb_factors, file = "./data/derived/demographics/ltdb_factors.RData")
write_csv(ltdb_factors, file= "./data/export/ltdb_factors.csv")

# Quick diagnostics
ltdb_factors |> 
  sapply(\(x) c("Missing" = sum(is.na(x)), "Non-missing" = sum(!is.na(x)))) |> 
  t()

ltdb_factors |> 
  select(PCA_disadv, FAC_disadv, FAC_hispimm, FAC_stability) |> 
  cor(use = "pairwise.complete")




