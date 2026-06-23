
## Notes ----
# Explore UN and UE for Eva Legge by Randy Swaty
# June 23, 2026
# Take initial output from ArcGIS combine of EVC/H/T, BpS and SCL (as .csv), convert to parquet
# Explore and learn what is in those categories for most widespread BpSs

# Set up ----

options(scipen=999)

library(arrow)
library(janitor)
library(scales)
library(tidyverse)
library(jpeg)
library(grid)
library(patchwork)

# convert .csv to parquet which is much smaller 

# it is smaller than csv because it uses built-in compression and avoids repeating
# column names and text formatting for every row, unlike plain text csv files

### csv_dataset <- open_dataset("inputs/monster.csv", format = "csv")  # now deleted-was 1GB !

### write_parquet(csv_dataset, "inputs/monster.parquet") # completed, ~73mb

# read in sclass attribute table for join to monster combine which I forgot to do in ArcGIS pro
scls_df <- read_csv("inputs/LF24_SCla_250.csv")

# read in combine, join sclass labels, add BPS_MODEL count and acres

combine_df <- read_parquet("inputs/monster.parquet") |>
  left_join(
    select(scls_df, VALUE, LABEL),
    by = c("ExtractByMask_OutRaster_LC24_SCla_250_tif" = "VALUE")) |>
  group_by(BPS_MODEL) |>
  mutate(
    bps_model_count = sum(Count, na.rm = TRUE),
    bps_model_acres = bps_model_count * 0.2223945) |>
  ungroup()


# read in locator map
img <- readJPEG("outputs/apps_mzs_map.jpg")




locator_grob <- grobTree(
  rasterGrob(
    img,
    interpolate = TRUE,
    width = unit(1, "npc"),
    height = unit(1, "npc")
  ),
  rectGrob(gp = gpar(col = "grey30", fill = NA, lwd = 2))
)





# Wrangle and explore UE  ----

ue_df_out <- combine_df |>
  filter(LABEL == "UE") |>
  select(
    Count,
    BPS_MODEL,
    BPS_NAME,
    EVT_NAME,
    EVT_PHYS,
    LABEL,
    bps_model_acres ) |>
  clean_names() |>
  
  group_by(bps_model, bps_name, evt_name, evt_phys, label) |>
  summarize(
    # total count per evt within bps
    count = sum(count, na.rm = TRUE),
    # carry total acres (same within bps)
    bps_model_acres = first(bps_model_acres),
    .groups = "drop") |>
  
  group_by(bps_model) |>
  mutate(
    # total count per bps
    total_count_bps = sum(count, na.rm = TRUE),
    # convert to acres
    evt_name_acres = (count / total_count_bps) * bps_model_acres) |>
  ungroup() |>
  
  mutate(percent_evt = (evt_name_acres/bps_model_acres)*100) |>
  
  mutate(
    # round everything
    evt_name_acres = round(evt_name_acres, 0),
    bps_model_acres = round(bps_model_acres, 0),
    total_count_bps = round(total_count_bps, 0),
    percent_evt = round(percent_evt, 0)) |>
  arrange(desc(bps_model_acres), desc(evt_name_acres))  
  

    

# Make chart of most common EVTs for UE ----

ue_grouped <- ue_df_out |>
  group_by(evt_name) |>
  summarize(evt_name_acres = sum(evt_name_acres)) |>
  arrange(desc(evt_name_acres))


ue_evt_name_plot <- 
  ggplot(data = ue_grouped, aes(x = evt_name, y = evt_name_acres)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = rev(ue_grouped$evt_name),
                   labels = function(x) str_wrap(x, width = 30)) +
  labs(
    title = "Breaking down the UE succession class for the Appalachian Region",
    subtitle = "Result of combining Succession Class and Existing Vegetation Type data",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Acres")
  
ue_evt_name_plot 

h <- nrow(img)
w <- ncol(img)
aspect <- h / w


ue_evt_name_plot <-
  ue_evt_name_plot +
  inset_element(
    locator_grob,
    left = 0.72,
    right = 0.98,
    bottom = 0.03,
    top = 0.03 + (0.98 - 0.72) * aspect
  )



ggsave(
  "outputs/evt_names_ue.jpg",
  ue_evt_name_plot,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)


