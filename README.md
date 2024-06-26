# us_sociodemographic_structures
 
The R code in this repository is used to process data from [Logan et al.'s 2021 Longitudinal 
Tract Data Base](https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm) into 
neighborhood sociodemographic composite measures such as concentrated disadvantage for the entire United States, 1990–2020.
It also produces interpolated year-specific values for linking to life-course panel data such as the PHDCN.

Using this repository requires data from [https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm](https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm). Specifically, you should obtain the "LTDB Standard data set" for both "Full Data" and "Sample" data files for all available years.

See 00_file_paths.R for the expected file paths for the LTDB files.

To generate the files, just run the scripts in numeric order. This will produce
`.RData` files with four decennial years of data for each tract, reconciled to
2010 tract boundaries. In doing so, it will also produce a tract boundary file
that can be used to merge geolocated data with the tract data.