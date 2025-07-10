
library(sf)
library(tmap)

aq <- fread("data/clean_dataset.csv")

# in December 2013, there was an inversion basically the entire month

aq_inv <- aq[which(aq$week == 50), ]


shp <- st_read('/Users/brenna/Documents/School/Research/aq-pregnancy/data/shapefiles/NO2Grid(Polygons).shp')

aq_shp <- merge(shp, aq_inv[, c("grid_id", "max_pm", "week")], by = "grid_id")

aq_shp <- aq_shp |>
  filter(week == 50)

tm_shape(aq_shp) +
  tm_polygons(fill = "max_pm", fill.scale = tm_scale_continuous(values = "viridis"), lwd = 0)


# only weber, davis, slc, provo counties

library(tidycensus)

var <- load_variables(2015, "acs5", cache = TRUE)[1, "name"] |>
  as.character()

ut_shp <- get_acs(geography = "county",
                  variables = var,
                  year = 2015,
                  state = "UT",
                  geometry = TRUE)

ut_shp <- ut_shp |>
  rename_with(tolower) |>
  mutate(county_fips = str_sub(geoid, start = 3, end = 5)) |>
  filter(county_fips %in% c("011", "057", "049", "035"))

head(ut_shp)


tm_shape(ut_shp) +
  tm_borders(lwd = 0) +
  tm_shape(aq_shp) +
  tm_polygons(fill = "max_pm", fill.scale = tm_scale_continuous(values = "viridis"), lwd = 0)


# don't love the gaps.. let's try daily data for an inversion day, before the population masking

no2_13 <- readRDS("/Users/brenna/Documents/School/Research/aq-pregnancy/data/Grid_1km_UT/2013/20131215.rds")
no2_13 <- data.table(no2_13)

no2_13_t <- transpose(no2_13)[-1, ]
# rownames(no2_13_t) <- colnames(no2_13)
no2_13_t$cell <- str_split_fixed(rownames(no2_13_t), "_", 2)[, 1]
head(no2_13_t)

no2_13_t$date <- "2013/12/15"

# pm_shp <- read.csv("/Users/brenna/Documents/School/Research/aq-pregnancy/data/aq/PM25Grid.csv")

no2_shp <- cbind(shp, no2_13_t)

# remove the funny cells
no2_shp <- no2_shp |>
  filter(grid_id < 10000000) # out of bounds, or something

no2_shp$pm25 <- as.numeric(no2_shp$V1)

tm_shape(ut_shp) +
  tm_borders(lwd = 0) +
  tm_shape(no2_shp) +
  tm_polygons(fill = "pm25", fill.scale = tm_scale_continuous(values = "magma"), lwd = 0)

summary(no2_shp$grid_id)

ut_shp <- st_transform(ut_shp, crs = st_crs(no2_shp))

ut_shp_union <- st_union(ut_shp)
ut_shp_union


test <- st_join(no2_shp, ut_shp, st_intersects)

test <- test |>
  filter(!is.na(geoid))

tm_shape(test) +
  tm_polygons(fill = "pm25", fill.scale = tm_scale_continuous(values = "magma"), lwd = 0)


test <- test[, c("grid_id", "date", "pm25", "name", "geometry")]

st_write(test, "data/inversion_map data.shp")




