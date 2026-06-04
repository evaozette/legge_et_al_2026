
## Notes ----
# Data prep for Eva Legge by Randy Swaty
# May 25, 2026
# Take initial output from ArcGIS combine of LANDFIRE Map Zones, Biophysical Settings and Succession Classes
# Join in helper data such as species, reference percent; calculate current percents


## Dependencies -----

# packages
library(tidyverse)
library(janitor)

# read raw data
bps_scls_raw <- read_csv("inputs/bps_scl_apps_mzs.csv")

# read helper data
sclass_species <- read_csv("inputs/sclass_species_conus.csv") |>
  unite(model_label, c(bps_model_id, class)) # for joining

sclass_descriptions <- read_csv("inputs/scls_descriptions.csv") # not used here, but made available in case it's useful
ref_percents <- read_csv("inputs/ref_con_long.csv")


## Initial cleaning of input data ----

bps_scls_clean <- bps_scls_raw |>
  select(-c(apps_mzs_rast, LC20_BPS_220, LC24_SCla_250)) |>  # don't need these
  unite(model_label, c(BPS_MODEL, LABEL), sep = "_", remove = FALSE) |>  # create label
  relocate(model_label, .after = last_col()) |>  # move it to end
  filter(LABEL != "Water") |>  # no myco in water
  clean_names()


## Join in reference percents, calculate current percents ----

# join in reference percents
bps_scls_clean <- bps_scls_clean |>
  left_join(ref_percents |> 
              select(model_label, ref_percent), 
            by = "model_label")

# calculate current percents
bps_scls_clean <- bps_scls_clean |>
  group_by(bps_model) |> # important to use this for grouping due to variants within bps_names that are possible
  mutate(
    total_bps_count = sum(count, na.rm = TRUE),
    cur_percent = round((count / total_bps_count) * 100, 0)) |>
  ungroup() |>
  relocate(total_bps_count, .after = count)

# calculate and indicate percent over or under representation of current compared to reference percent

bps_scls_clean <- bps_scls_clean |>
  mutate(
    class_percent_difference = cur_percent - ref_percent,
    class_representation = case_when(
      cur_percent < ref_percent ~ "under",
      cur_percent > ref_percent ~ "over",
      TRUE ~ "equal"
    )
  )


## Join in S-Class Species and clean ----

bps_scls_clean <- bps_scls_clean |>
  left_join(
    sclass_species,
    by = "model_label",
    relationship = "many-to-many" )

# replace NAs with sclass value per row

bps_scls_clean <- bps_scls_clean |>
  mutate(across(
    c(symbol, scientific_name, common_name, canopy_position),
    ~ coalesce(., label)))


# Write output ----

write.csv(bps_scls_clean, file = "outputs/bps_scls_clean.csv", row.names = FALSE )



