# ─── 1. Load Libraries ────────────────────────────────────────────────
library(sf)
library(dplyr)
library(readxl)
library(tmap)

# ─── 2. Read County Shapefile ─────────────────────────────────────────
county_shapefile <- st_read("County.shp")

# Clean county names (lowercase, remove special chars)
county_shapefile$COUNTY <- tolower(county_shapefile$COUNTY)
county_shapefile$COUNTY <- gsub("'", "", county_shapefile$COUNTY)
county_shapefile$COUNTY <- gsub("[^a-z\\s-]", "", county_shapefile$COUNTY)

# ─── 3. Read Tea Production Data ──────────────────────────────────────
tea_production_2020 <- read_excel("tea_production_2020.xlsx")

# Clean names in tea data
tea_production_2020$County <- tolower(tea_production_2020$County)
tea_production_2020$County <- gsub("'", "", tea_production_2020$County)
tea_production_2020$County <- gsub("[^a-z\\s-]", "", tea_production_2020$County)

# ─── 4. Add Missing Counties with NA ──────────────────────────────────
missing_counties <- setdiff(county_shapefile$COUNTY, tea_production_2020$County)

# Add NA rows for missing counties
missing_data <- data.frame(
  County = missing_counties,
  `2020 tea production` = NA_real_
)

tea_production_full <- bind_rows(tea_production_2020, missing_data)

# ─── 5. Merge with Spatial Data ───────────────────────────────────────
kenya_tea <- left_join(county_shapefile, tea_production_full, 
                       by = c("COUNTY" = "County"))

# ─── 6. Plot the Map ──────────────────────────────────────────────────
tmap_mode("plot")  # for static map

tea_map <- tm_shape(kenya_tea) +
  tm_polygons(
    col = "2020 tea production",
    style = "cont",
    palette = "Greens",
    border.col = "black",
    lwd = 0.4,
    colorNA = "lightgrey",
    textNA = "No Data",
    fill.legend = tm_legend(title = "Tea Production (tons)")
  ) +
  tm_layout(
    title = "Tea Production by County in Kenya (2020)",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.frame = TRUE,
    frame = FALSE
  )

# ─── 7. Export the Map (Optional) ─────────────────────────────────────
tmap_save(tea_map, filename = "tea_production_map.png", width = 10, height = 7)

# (Optional: interactive version)
# tmap_mode("view")
# tea_map
