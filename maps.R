setwd("/home/antortjim/MEGA/Master/PG/project")
source("scripts/map_functions.R")
source("scripts/read_data.R")
## Antonio Ortega Jimenez
## This script reads how many zebras come from which country,
## maps their exact location (latitute, longitude)
## downloads a map of the Southern cone of Africa
## and plots the info on it.

# Select countries to plot and format their names
countries <- c("Kenya", "Tanzania, United Republic of", "Botswana", "Uganda", "Namibia", "Zambia",
               "Rwanda", "Burundi", "Angola", "South Africa","Mozambique",
               "Congo, The Democratic Republic of the", "Congo", "Zimbabwe",
               "Madagascar", "Malawi", "Lesotho", "Swaziland", "Gabon", "Equatorial Guinea")

# Download polygons (shapes) of each country for posterior plotting with geom_polygon
temp <- download_RDS(countries)
countries <- temp[[1]]
mycountries <- temp[[2]]
zebra_countries <- temp[[2]]

# Fortify (make the downloaded info available for ggplot2)
fortified_countries <- lapply(mycountries, function(x) {fortify(spTransform(x, CRS("+proj=longlat")))})

# Read zebra data
samples <- read.table("zebra_package/ourPlainsQuaggaGrevys_popInfo.txt", header = T) %>%
  arrange(species, subspecies)
temp <- data_preprocessing(samples)
zebras <- temp[[1]]
zebra_countries <- temp[[2]]

# Add number of zebras to each polygon in the list fortified_countries
for (i in 1:length(fortified_countries)) {
  current_country <- countries[i]
  print(current_country)
  fortified_countries[[i]]$country <- current_country
  n_zebras <- filter(zebras, country == current_country) %>% .$n_zebras
  print(n_zebras)
  fortified_countries[[i]]$n_zebras <- n_zebras 
}

samples %>% group_by(species, subspecies) %>% summarise(count = n()) # count of zebras by subspecies
zebra_origin_count <- samples %>% group_by(country) %>% summarise(count = n()) %>% na.omit() # count of zebras by countries


samples %>% filter(species == "plains") %>% dplyr::select(subspecies) %>% unique



# Geocoding (loading final data frame instead)
# Geocoding maps a common name to a pair of coordinates
complete_samples <- samples[complete.cases(samples$country),]
localities_clean <- localities_cleaner(complete_samples$locality)
lands <- complete_samples$country %>% as.character
locations <- paste(localities_clean, lands, sep = " ")

#df <- loc_to_coord(locations, lands)
df <- read.table("zebra_package/longlat.txt")
df$species <- species[-63]
df$subspecies <- subspecies[-63]
openxlsx::write.xlsx(file = "zebra_package/longlat.xlsx", df)


# Points text
active_locations <- localities_clean[complete.cases(df)]
unique_selector <- !duplicated(active_locations)
myCount <- (active_locations %>% table)[active_locations][unique_selector]
myLocalities <- localities_clean[complete.cases(df)][unique_selector]
myText <- paste(myLocalities, ": ", myCount, sep = "")


# Plot three pictures

# determine the bounding box of the spatial object
df2 <- plyr::rbind.fill(fortified_countries)
b <- bbox(SpatialPoints(df2[,1:2]))

# Download a satellite picture of Africa :)
Africa <- get_map(location = b, maptype = "satellite", zoom = 4,
                  scale = TRUE)
saveRDS(object = Africa, ".Africa.RDS")

Africa_terrain <- get_stamenmap(bbox = b, maptype = "watercolor", zoom = 4,
                  scale = TRUE)
saveRDS(object = Africa_terrain, ".Africa_terrain.RDS")

saveRDS(object = fortified_countries, ".fortified_countries.RDS")

# Satellite image with points corresponding to our zebras

p <- ggmap(Africa_terrain) + geom_point(data=df, aes(x=lon, y=lat), colour="coral1", size=3)
p
ggsave(filename = "figures/satellite_simple.png", plot = p)

# Satellite image with polygons plotted and zebras shown as dots
satellite <- ggmap(Africa) +
  theme_void()

for (i in 1:length(countries)) {
  current_country <- countries[i]
  if(current_country %in% zebra_countries) {
    satellite <- satellite + geom_polygon(data = fortified_countries[[i]],
                                          aes(long, lat, group = group, fill = n_zebras),
                                          color = "white", linetype = "longdash", size = 0.3, alpha = 0.5)
  }
}

satellite <- satellite + scale_fill_brewer(palette = "Reds", limits = levels(zebras$n_zebras)) +
  guides(fill = FALSE) +
  geom_point(data=na.omit(df)[unique_selector,], aes(x=lon, y=lat),
             colour="black", size=3) 
satellite
ggsave(filename = "figures/satellite.png", plot = satellite)

# The same as above but no satellite (advantage: satellite images are not compatible with plotly)
# As long as plogly is not compatible with maps, this is the only way we can make the interactive plot

polygon_map <- ggplot() +
  theme_void()

for (i in 1:length(fortified_countries)) {
  polygon_map <- polygon_map + geom_polygon(data = fortified_countries[[i]],
                                        aes(long, lat, group = group, fill = n_zebras),
                                        color = "white", linetype = "longdash", size = 0.3, alpha = 0.5)
}

polygon_map <- polygon_map + scale_fill_brewer(palette = "Reds", limits = levels(zebras$n_zebras)) +
  guides(fill = FALSE) +
  geom_point(data=na.omit(df)[unique_selector,], aes(x=lon, y=lat),
             colour="black", size=3) 

ggsave(filename = "figures/polygon_map.png", plot = polygon_map)

interactive_map <- plotly_build(polygon_map)
interactive_map$x$data[[length(interactive_map$x$data)]]$text <- myText
interactive_map
plotly_POST(interactive_map, filename = "zebras")