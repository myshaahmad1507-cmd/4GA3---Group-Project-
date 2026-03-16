# =========================================================
# ENVSOCTY 4GA3 Final Project- Mysha Ahmad, Connor Gitt, Maahir Patel
# Part 1: Boundary + Hamilton CTs + Greenspace Percentage
# =========================================================

# -------------------------------
# STEP 0 — Install packages + load packages
# -------------------------------

install.packages(c("sf", "dplyr", "ggplot2", "readr"))

library(sf)
library(dplyr)
library(ggplot2)
library(readr)

# -------------------------------
# STEP 0.5 — Check working directory
# -------------------------------
getwd()

# =========================================================
# STEP 1 — Load boundary data
# =========================================================

# 1.1 Read Canadian Census Tract shapefile
ct <- st_read("data_raw:/ct_boundary:/lct_000b21a_e.shp")

# 1.2 Check the tract file
names(ct)
nrow(ct)
plot(st_geometry(ct))

# 1.3 Read Hamilton boundary shapefiledata_raw:/list.files("data_raw/greenspace")oundary:/lct_000b21a_e.shp
ham <- st_read("data_raw:/hamilton_boundary:/City_boundary.shp")

# 1.4 Make sure Hamilton boundary uses same CRS as tract file
ham <- st_transform(ham, st_crs(ct))

# 1.5 Clip all Canadian tracts to Hamilton only
ct_ham <- st_intersection(ct, ham)

# 1.6 Check the Hamilton-only tract layer
names(ct_ham)
nrow(ct_ham)
plot(st_geometry(ct_ham))

# 1.7 Save progress (optional but recommended)
saveRDS(ct_ham, "data_processed/ct_ham.rds")

# If you want to save as GeoPackage too:
# st_write(ct_ham, "data_processed/ct_ham.gpkg", delete_dsn = TRUE)

# =========================================================
# STEP 2 — Load greenspace data
# =========================================================

# 2.1 Read the parks / greenspace shapefile
greenspace <- st_read("data_raw:/greenspace:/Parks.shp")

# 2.2 Check the greenspace layer
names(greenspace)
nrow(greenspace)
plot(st_geometry(greenspace))

# 2.3 Transform greenspace CRS to match Hamilton census tracts
greenspace <- st_transform(greenspace, st_crs(ct_ham))

# 2.4 Make sure geometries are valid
ct_ham <- st_make_valid(ct_ham)
greenspace <- st_make_valid(greenspace)

# =========================================================
# STEP 3 — Intersect greenspace with census tracts
# =========================================================

# 3.1 Intersect greenspace polygons with tract polygons (keeping only CTUID from tract file for clean output)
gs_intersection <- st_intersection(
  ct_ham %>% select(CTUID),
  greenspace
)

# 3.2 Calculate area of each greenspace piece
gs_intersection <- gs_intersection %>%
  mutate(gs_area_m2 = as.numeric(st_area(geometry)))

# 3.3 Summarize total greenspace area by census tract
gs_summary <- gs_intersection %>%
  st_drop_geometry() %>%
  group_by(CTUID) %>%
  summarise(gs_area_m2 = sum(gs_area_m2, na.rm = TRUE))

# =========================================================
# STEP 4 — Join greenspace totals back to CT layer
# =========================================================

# 4.1 Join greenspace area to tract layer
ct_ham <- ct_ham %>%
  left_join(gs_summary, by = "CTUID")

# 4.2 Replace NA with 0 for tracts that have no greenspace
ct_ham <- ct_ham %>%
  mutate(gs_area_m2 = ifelse(is.na(gs_area_m2), 0, gs_area_m2))

# 4.3 Calculate total tract area
ct_ham <- ct_ham %>%
  mutate(ct_area_m2 = as.numeric(st_area(geometry)))

# 4.4 Calculate greenspace percentage
ct_ham <- ct_ham %>%
  mutate(gs_pct = 100 * gs_area_m2 / ct_area_m2)

# =========================================================
# STEP 5 — Check results
# =========================================================

# 5.1 Summary of greenspace percentage
summary(ct_ham$gs_pct)

# 5.2 Quick choropleth map
ggplot(ct_ham) +
  geom_sf(aes(fill = gs_pct), color = NA) +
  theme_minimal() +
  labs(
    title = "Greenspace Percentage by Census Tract",
    fill = "Greenspace %"
  )

# =========================================================
# STEP 6 — Save updated dataset
# =========================================================

# Save as R object
saveRDS(ct_ham, "data_processed/ct_ham_with_greenspace.rds")

# Optional: save as GeoPackage
# st_write(ct_ham, "data_processed/ct_ham_with_greenspace.gpkg", delete_dsn = TRUE)

# Optional: save attribute table as CSV
write.csv(
  st_drop_geometry(ct_ham),
  "data_processed/ct_ham_with_greenspace.csv",
  row.names = FALSE
)

# =========================================================
# STEP 7 — Load median household income data (cancensus)
# =========================================================

# 7.1 Load cancensus package
library(cancensus)

#Statistics Canada API key to obtain Income Data
set_cancensus_api_key("CensusMapper_6db0e7648296f86cb1a02a6fe6b2ccc2", install = TRUE)

# 7.2 Set a persistent cache path
# This prevents repeated downloads every session
set_cancensus_cache_path("data_raw/census_cache", install = TRUE)

# 7.3 Set your CensusMapper API key
# Replace with your own key if needed
set_cancensus_api_key("YOUR_API_KEY_HERE", install = TRUE)

# 7.4 Download Hamilton CMA census tract income data
# Correct vector:
# v_CA21_906 = Median total income of household in 2020 ($)
income <- get_census(
  dataset = "CA21",
  regions = list(CMA = 35537),
  level = "CT",
  vectors = "v_CA21_906",
  labels = "short",
  geo_format = "sf",
  use_cache = FALSE
)

# 7.5 Check that the values look correct
nrow(income)
names(income)
summary(income$v_CA21_906)
head(income$v_CA21_906, 20)

# 7.6 Rename columns for a clean join
income <- income %>%
  rename(
    CTUID = GeoUID,
    med_income = v_CA21_906
  )

# 7.7 Join income to Hamilton census tract dataset
ct_ham <- ct_ham %>%
  select(-any_of("med_income")) %>%
  left_join(
    income %>% sf::st_drop_geometry() %>% select(CTUID, med_income),
    by = "CTUID"
  )

# 7.8 Verify the joined income values
summary(ct_ham$med_income)
head(ct_ham$med_income, 20)

# 7.9 Optional quick map check
ggplot(ct_ham) +
  geom_sf(aes(fill = med_income), color = NA) +
  theme_minimal() +
  labs(
    title = "Median Household Income by Census Tract",
    fill = "Income ($)"
  )

# 7.10 Save updated dataset
saveRDS(ct_ham, "data_processed/ct_ham_with_income.rds")

# Optional: save attribute table as CSV
write.csv(
  st_drop_geometry(ct_ham),
  "data_processed/ct_ham_with_income.csv",
  row.names = FALSE
)

# Optional: save as GeoPackage
# st_write(ct_ham, "data_processed/ct_ham_with_income.gpkg", delete_dsn = TRUE)

# =========================================================
# STEP 8 — Land Surface Temperature (LST) Analysis - Landsat 8
# =========================================================

# 8.1 Install terra package if not installed and load terra
if (!require(terra)) {
  install.packages("terra")
  library(terra)
}

library(terra)


# 8.2 Load Landsat surface temperature raster
lst <- rast("data_raw:/temp/LC08_L2SP_018030_20240814_20240822_02_T1_ST_B10.TIF")

# 8.3 Plot raster to confirm it loaded correctly
plot(lst)

# 8.4 Convert Landsat scaled values to Celsius
lst_celsius <- lst * 0.00341802 + 149.0 - 273.15

plot(lst_celsius)

# 8.5 Match coordinate systems with census tracts
ct_ham <- st_transform(ct_ham, crs(lst_celsius))


# 8.5 Convert census tracts to terra vector
ct_vect <- vect(ct_ham)

# 8.6 Extract mean temperature per census tract

temp_extract <- terra::extract(
  lst_celsius,
  ct_vect,
  fun = mean,
  na.rm = TRUE
)

# 8.7 Attach temperature to census tract dataset
ct_ham$mean_temp <- temp_extract[,2]

# 8.9 Check results
summary(ct_ham$mean_temp)

# 8.10 Save processed dataset
saveRDS(ct_ham, "data_processed/ct_ham_with_income_temp.rds")

# =========================================================
# STEP 9 — Final Data Saving
# =========================================================

# 9.1 Save as R object
saveRDS(ct_ham_final, "data_processed/ct_ham_final.rds")

# 9.2 Save as Csv file
write.csv(
  st_drop_geometry(ct_ham_final),
  "data_processed/ct_ham_final.csv",
  row.names = FALSE
)

# 9.3 Save as Geopackage
st_write(ct_ham_final, "data_processed/ct_ham_final.gpkg", delete_dsn = TRUE)

