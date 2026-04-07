###############################
### Create new mining Data ###
###############################

rm(list=ls())
#load packages
library(readxl)
library(sf)
library(countrycode)
library(dplyr)
library(devtools)
library(ggplot2)
library(cshapes)
library(lubridate)
library(data.table)
library(haven)
library(tidyr)
library(zoo)
library(terra)

# read cshapes for country maps
cshapes_shp <- st_read("data/CShapes/CShapes-2.0.shp")
unique(cshapes_shp$gwcode)

# only retain most recent observations
cshapes_latest <- cshapes_shp %>%
  group_by(cntry_name) %>%
  filter(gwsyear == max(gwsyear, na.rm = TRUE)) %>%
  ungroup()

cshapes_latest <- subset(cshapes_latest, gweyear == 2019)

# plot global map
map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  ggtitle("") +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(legend.position = "bottom")
map_world

# read mining data
mining_shp <- st_read("Data/Mining/Global_mine_polygons_74726/Global_mine_polygons_74726.shp")
unique(mining_shp$status)

# we have three categories: active, undefined, and closed.

# plot global map with new mining data
map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = mining_shp$geometry[mining_shp$status == 1], fill = "red", col = "red", linewidth = 0.075) +
  geom_sf(data = mining_shp$geometry[mining_shp$status == 0], fill = "blue", col = "blue", linewidth = 0.075) +
  geom_sf(data = mining_shp$geometry[mining_shp$status == -1], fill = "pink", col = "pink", linewidth = 0.075) +
  ggtitle("") +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(legend.position = "bottom")
map_world

map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = mining_shp$geometry, fill = "black", col = "black", linewidth = 0.0075) +
  ggtitle("") +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(legend.position = "bottom")
map_world

##############################################################
### Read mineral data to identify mineral type of polygons ###
##############################################################

#'* 1st Mining Dataset: GRD DATA *

GRD_world <- read_dta("Data/Mining/dfhsw_GRD_public_v1.dta")

# save full version of mining dataset
GRD_all <- GRD_world

# only mines and extraction sites
GRD_world <- GRD_world[GRD_world$minetype == "mine", ]

# subset those with high precision (exact, nearby city, or district-level)
GRD_world <- subset(GRD_world, precisioncode <= 3)

# create sf object
GRD_world_sf <- st_as_sf(GRD_world, coords = c("longitude", "latitude"), 
                         crs = 4326, remove = FALSE)

# increased activity map with GRD
map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = GRD_world_sf$geometry, fill = "black", col = "black", size = 0.00075) +
  geom_sf(data = mining_shp$geometry[mining_shp$status == 1], fill = "red", col = "red", linewidth = 0.075) +
  ggtitle("") +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(legend.position = "bottom")
map_world

#'* 2nd Mining Dataset: Global USGS Data *
usgs_com <- data.frame(read.csv("Data/Mining/ofr20051294/commodity.csv"))
usgs_shp <- st_read("Data/Mining/ofr20051294/ofr20051294.shp")

#'* 3rd Mining Dataset: USGS China *
china_deposits <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                          layer = "CHN_Mineral_Deposits")

china_coal <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                      layer = "CHN_Mineral_Resources_Coal")

china_phosphate <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                           layer = "CHN_Mineral_Resources_Phosphate")

china_copper <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                        layer = "CHN_Mineral_Resources_Copper")

china_antimony <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                          layer = "CHN_Mineral_Resources_Antimony")

china_potash <- st_read("Data/Mining/China_shp/a00000004.gdbtable",
                        layer = "CHN_Mineral_Resources_Potash")

china_1 <- china_deposits %>% select(COUNTRY, DsgAttr01, Shape)
china_1 <- st_transform(china_1, st_crs(cshapes_shp))
names(china_1) <- c("Country", "DsgAttr01", "Shape")

#'* 4th Mining Dataset: USGS Coal only *
usa_coal <- st_read("Data/Mining/USA/CoalMines_US_2020.shp")

#'* 5th Mining Dataset: Western Australia *
west_australia <- st_read("Data/Mining/Australia/Mindex_DMIRS_001_WA_GDA2020_Public_Shapefile/Mindex_DMIRS_001.shp")
unique(west_australia$site_type_)

# Subset to keep only 'mine' or 'deposit'
west_australia <- west_australia %>%
  filter(site_type_ %in% c('Mine', 'Deposit'))

# Verify the subset
table(west_australia$site_type_)
unique(west_australia$target_com)

# plot all datasets together
map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = GRD_world_sf$geometry, fill = "black", col = "black", size = 0.00075) +
  geom_sf(data = usgs_shp$geometry, fill = "black", col = "black", size = 0.00075) +
  geom_sf(data = china_1$Shape, fill = "black", col = "black", size = 0.00075) +
  geom_sf(data = usa_coal$geometry, fill = "black", col = "black", size = 0.00075) +
  geom_sf(data = west_australia$geometry, fill = "black", col = "black", size = 0.00000075) +
  geom_sf(data = mining_shp$geometry, fill = "red", col = "red", linewidth = 0.075) +
  ggtitle("") +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(legend.position = "bottom")
map_world

#'* Can we add more sources? *

#############################
## Merge Datasets together ##
#############################

usa_coal$commodity <- "coal"

df_1 <- GRD_world_sf %>% select(locationname, resource, geometry)
df_2 <- usgs_shp %>% select(DEP_NAME, COMMODITY, geometry)
df_3 <- usa_coal %>% select(name, commodity, geometry)
df_4 <- china_1 %>% select(Country, DsgAttr01, Shape)
df_5 <- west_australia %>% select(short_name, target_com, geometry)

names(df_1)[1] <- "name"
names(df_2)[1] <- "name"
names(df_3)[1] <- "name"
names(df_4)[1] <- "name"
names(df_5)[1] <- "name"

names(df_1)[2] <- "commodity"
names(df_2)[2] <- "commodity"
names(df_3)[2] <- "commodity"
names(df_4)[2] <- "commodity"
names(df_5)[2] <- "commodity"

names(df_4)[3] <- "geometry"
st_geometry(df_4) <- "geometry"

df_1 <- st_transform(df_1, st_crs(cshapes_shp))
df_2 <- st_transform(df_2, st_crs(cshapes_shp))
df_3 <- st_transform(df_3, st_crs(cshapes_shp))
df_4 <- st_transform(df_4, st_crs(cshapes_shp))
df_5 <- st_transform(df_5, st_crs(cshapes_shp))

all_mines <- rbind(df_1, df_2, df_3, df_4, df_5)

# apply mining type
mining_shp$type <- NA
unique(all_mines$commodity)

# More robust cleaning - remove parentheses BEFORE splitting
all_mines$commodity_first <- all_mines$commodity %>%
  # Convert to lowercase first
  tolower() %>%
  # Remove leading/trailing whitespace
  trimws() %>%
  # Remove anything in parentheses (including the brackets)
  gsub("\\(.*?\\)", "", .) %>%
  # Split by comma, hyphen, semicolon, or slash
  sub("[,;/-].*", "", .) %>%
  # Remove extra whitespace again
  trimws() %>%
  # Remove any trailing periods or other punctuation
  gsub("[.;:]+$", "", .)

# Check results
sort(unique(all_mines$commodity_first))

# 
# # Extract first commodity (split by either comma or hyphen), then lowercase
# all_mines$commodity_first <- sub("[,-].*", "", all_mines$commodity)
# all_mines$commodity_first <- trimws(all_mines$commodity_first)
# all_mines$commodity_first <- tolower(all_mines$commodity_first)
# 
# sort(unique(all_mines$commodity_first))

# Ensure both datasets have the same CRS
mining_shp <- st_transform(mining_shp, crs = st_crs(cshapes_latest))
all_mines <- st_transform(all_mines, crs = st_crs(cshapes_latest))

# Get centroids of mining polygons (for distance calculation)
mining_centroids <- st_centroid(mining_shp)

# Find nearest GRD point for each mining polygon
nearest_indices <- st_nearest_feature(mining_centroids, all_mines)

# Assign the resource type from nearest GRD mine
mining_shp$type <- all_mines$commodity_first[nearest_indices]

# Check results
table(mining_shp$type, useNA = "always")

# View distribution
head(mining_shp %>% select(type, status))

sort(unique(mining_shp$type))
mining_shp$ETM <- 0
mining_shp$ETM[mining_shp$type == "aluminum"] <- 1
mining_shp$ETM[mining_shp$type == "bauxite"] <- 1
mining_shp$ETM[mining_shp$type == "cobalt"] <- 1
mining_shp$ETM[mining_shp$type == "copper"] <- 1
mining_shp$ETM[mining_shp$type == "graphite"] <- 1
mining_shp$ETM[mining_shp$type == "iron"] <- 1
mining_shp$ETM[mining_shp$type == "iron ore"] <- 1
mining_shp$ETM[mining_shp$type == "iron and steel"] <- 1
mining_shp$ETM[mining_shp$type == "lead"] <- 1
mining_shp$ETM[mining_shp$type == "lithium"] <- 1
mining_shp$ETM[mining_shp$type == "manganese"] <- 1
mining_shp$ETM[mining_shp$type == "manganese ore"] <- 1
mining_shp$ETM[mining_shp$type == "nickel"] <- 1
mining_shp$ETM[mining_shp$type == "rare"] <- 1
mining_shp$ETM[mining_shp$type == "rare earths"] <- 1
mining_shp$ETM[mining_shp$type == "rare earth elements"] <- 1
mining_shp$ETM[mining_shp$type == "silver"] <- 1
mining_shp$ETM[mining_shp$type == "tin"] <- 1
mining_shp$ETM[mining_shp$type == "titanium"] <- 1
mining_shp$ETM[mining_shp$type == "zinc"] <- 1
unique(mining_shp$type[mining_shp$ETM == 1])

sort(unique(mining_shp$type))
mining_shp$TCM <- 0
mining_shp$TCM[mining_shp$type == "tungsten"] <- 1
mining_shp$TCM[mining_shp$type == "diamond"] <- 1
mining_shp$TCM[mining_shp$type == "gold"] <- 1
mining_shp$TCM[mining_shp$type == "wolframite"] <- 1
mining_shp$TCM[mining_shp$type == "tantalum"] <- 1
mining_shp$TCM[mining_shp$type == "gemstones"] <- 1
mining_shp$TCM[mining_shp$type == "gem"] <- 1
mining_shp$TCM[mining_shp$type == "gem & semi"] <- 1
mining_shp$TCM[mining_shp$type == "gem amethyst"] <- 1
mining_shp$TCM[mining_shp$type == "gem diamond"] <- 1
mining_shp$TCM[mining_shp$type == "gem emerald"] <- 1
mining_shp$TCM[mining_shp$type == "gem jade"] <- 1
mining_shp$TCM[mining_shp$type == "gem topaz"] <- 1
mining_shp$TCM[mining_shp$type == "jade"] <- 1
mining_shp$TCM[mining_shp$type == "opal"] <- 1
mining_shp$TCM[mining_shp$type == "ruby"] <- 1

unique(mining_shp$type[mining_shp$TCM == 1])

sort(unique(mining_shp$type))
mining_shp$COAL <- 0
mining_shp$COAL[mining_shp$type == "coal"] <- 1

# Read GeoPIPE data
fields_spatial <- st_read("Data/Fossil/spatial_final.gpkg")
fields_spatial <- st_as_sf(fields_spatial, sf_column_name = "geom")

str(fields_spatial)


# Create a data frame for the legend
legend_data <- data.frame(
  category = c("Oil & Gas Fields", "ETM Mines", "TCM Mines", "Coal Mines"),
  color = c("red3", "blue", "gold1", "black")
)

# Plot with legend
map_world <- ggplot() +
  geom_sf(data = cshapes_latest, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = fields_spatial, aes(fill = "Oil & Gas Fields", col = "Oil & Gas Fields"), linewidth = 0.015) +
  geom_sf(data = mining_shp[mining_shp$ETM == 1, ], aes(fill = "ETM Mines", col = "ETM Mines"), linewidth = 0.075) +
  geom_sf(data = mining_shp[mining_shp$TCM == 1, ], aes(fill = "TCM Mines", col = "TCM Mines"), linewidth = 0.075) +
  geom_sf(data = mining_shp[mining_shp$COAL == 1, ], aes(fill = "Coal Mines", col = "Coal Mines"), linewidth = 0.075) +
  scale_fill_manual(
    name = "Resource Type",
    values = c("Oil & Gas Fields" = "red3", 
               "ETM Mines" = "blue", 
               "TCM Mines" = "gold1", 
               "Coal Mines" = "black")
  ) +
  scale_color_manual(
    name = "Resource Type",
    values = c("Oil & Gas Fields" = "red3", 
               "ETM Mines" = "blue", 
               "TCM Mines" = "gold1", 
               "Coal Mines" = "black")
  ) +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )
map_world

# Just USA
cs_usa <- subset(cshapes_latest, cntry_name == "United States of America")
fields_usa <- subset(fields_spatial, COUNTRY == "United States of Ameri")
mines_usa <- subset(mining_shp, country_na == "United States")

map_USA <- ggplot() +
  geom_sf(data = cs_usa, fill = "grey94", col = "grey90", linewidth = 0.075) +
  geom_sf(data = fields_usa, aes(fill = "Oil & Gas Fields", col = "Oil & Gas Fields"), linewidth = 0.015) +
  geom_sf(data = mines_usa[mines_usa$ETM == 1, ], aes(fill = "ETM Mines", col = "ETM Mines"), linewidth = 0.075) +
  geom_sf(data = mines_usa[mines_usa$TCM == 1, ], aes(fill = "TCM Mines", col = "TCM Mines"), linewidth = 0.075) +
  geom_sf(data = mines_usa[mines_usa$COAL == 1, ], aes(fill = "Coal Mines", col = "Coal Mines"), linewidth = 0.075) +
  scale_fill_manual(
    name = "Resource Type",
    values = c("Oil & Gas Fields" = "red3", 
               "ETM Mines" = "blue", 
               "TCM Mines" = "gold1", 
               "Coal Mines" = "black")
  ) +
  scale_color_manual(
    name = "Resource Type",
    values = c("Oil & Gas Fields" = "red3", 
               "ETM Mines" = "blue", 
               "TCM Mines" = "gold1", 
               "Coal Mines" = "black")
  ) +
  theme_void() +  
  coord_sf(expand = FALSE) +  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )
map_USA


