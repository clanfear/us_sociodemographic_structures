# Using this repository requires data from https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm
# Specifically, you should obtain the "LTDB Standard data set" for both "Full Data" and "Sample" data files for all available years.

# Set directory that contains LTDB files
ltdb_path <- "data/raw/"

# Set dubdirs with full and sample data
# I assume here you keep thje folder names from the LTDB zip files, but you can
# edit as needed.
ltdb_full_path <- paste0(ltdb_path, "ltdb_std_all_fullcount/")
ltdb_sample_path <- paste0(ltdb_path, "ltdb_std_all_sample/")

save(ltdb_path, ltdb_full_path, ltdb_sample_path, file = "./data/derived/file_paths.RData")

# If your LTDB dir is set up correctly, you should be able to run this:

# list.files(ltdb_path, recursive = TRUE)

# and find at least these (required) files:

# "ltdb_std_all_fullcount/LTDB_Std_1970_fullcount.csv"             
# "ltdb_std_all_fullcount/LTDB_Std_1980_fullcount.csv"             
# "ltdb_std_all_fullcount/LTDB_Std_1990_fullcount.csv"             
# "ltdb_std_all_fullcount/LTDB_Std_2000_fullcount.csv"             
# "ltdb_std_all_fullcount/LTDB_Std_2010_fullcount.csv"             
# "ltdb_std_all_fullcount/ltdb_std_2020_fullcount.csv"             
# "ltdb_std_all_sample/ltdb_std_1970_sample.csv"                   
# "ltdb_std_all_sample/ltdb_std_1980_sample.csv"                   
# "ltdb_std_all_sample/ltdb_std_1990_sample.csv"                   
# "ltdb_std_all_sample/ltdb_std_2000_sample.csv"                   
# "ltdb_std_all_sample/LTDB_std_200812_Sample.csv"                 
# "ltdb_std_all_sample/LTDB_std_201519_Sample.csv"

# Note the exact files will change as the LTDB is updated, likely resulting in
# a need for edits to the code.