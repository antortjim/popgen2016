library("sp")  # you need this library to work with SpatialPolygons
library("ggplot2") # can't work without it
library("mapproj")
library("magrittr")
library("dplyr")
library("compare")
library("ISOcodes")
data("ISO_3166_1")
library("plotly")
library("ggmap")
library("maptools")
library("maps")
library("stringr")
library("rgdal")
library("png")
library("gridGraphics")

download_RDS <- function(countries) {
  library("ISOcodes")
  data("ISO_3166_1")
  # Select countries to plot and format their names
  countries[!(countries %in% ISO_3166_1$Name)]  # should return character(0), meaning that all countries are named correctly
  
  # Extract 3 character country ID
  countries_id <- ISO_3166_1[ISO_3166_1$Name %in% countries, "Alpha_3"]
  
  countries <- ISO_3166_1[ISO_3166_1$Name %in% countries, "Name"]
  mycountries <- list()
  for (c in countries_id) {
    print(c)
    myfile <- paste("shapefiles/AfricanCountries/", c, "_adm0.rds", sep = "")
    if (!file.exists(myfile)) {
      myurl <- paste("http://biogeo.ucdavis.edu/data/gadm2.8/rds/", c, "_adm0.rds", sep = "")
      download.file(myurl, destfile = myfile)
    }
    
    mycountries[[c]] <- readRDS(myfile)
    
  }
  
  return(list(countries, mycountries))
}

data_preprocessing <- function(data) {
  samples <- data
  
  zebra_countries <- samples$country %>% unique %>% na.omit %>% as.character
  zebra_countries <- countries[unique(grep(paste(zebra_countries,collapse="|"),  # formatted according to ISO 
                                           countries, value=FALSE))]
  
  non_zebra_countries <- countries[!(countries %in% zebra_countries)]
  zeros <- data.frame(country = non_zebra_countries, n_zebras = 0)
  zebras <- samples %>% group_by(country) %>% summarise(n_zebras = n()) %>% na.omit() %>% data.frame()
  zebras$country <- zebra_countries
  zebras <- rbind(zeros, zebras)
  zebras <- arrange(zebras, as.character(country))
  zebras$n_zebras <- zebras$n_zebras %>% as.factor # prevent continuous color mapping
  return(list(zebras, zebra_countries))
}


localities_cleaner <- function(localities) {
  localities <- complete_samples$locality %>% as.character
  localities[is.na(localities)] <- complete_samples[is.na(localities),]$country %>% as.character
  
  # Manual fix
  localities[localities == "Rufunsa,Eastern"] <- "Rufunsa"
  localities[localities == "IkiriRungwa"] <- "Rungwa"
  localities[localities == "OvitaFarm"] <- "Okahandja"
  localities_clean <- gsub("N[[:punct:]]R[[:punct:][:space:]]"," National Reserve ", localities)
  localities_clean <- gsub("N[[:punct:]]P[[:punct:][:space:]]*"," National Park ", localities_clean)
  localities_clean <- gsub("\\s*\\([^\\)]+\\)|\\.|\\,|/"," ", localities_clean)
  localities_clean <- gsub("\\)","", localities_clean)
  localities_clean <- gsub("Kafue National Park.*", "Kafue National Park", localities_clean)
  localities_clean <- gsub("NG20", "Kwara Camp - Kwando Safaris Lodge", localities_clean)
  localities_clean <- str_trim(localities_clean)
  
  return(localities_clean)
}


loc_to_coord <- function(locations, lands) {
  temp <- geocode(locations)
  df <- temp
  #non_mapped <- localities_clean[!complete.cases(temp)]
  #lands[!complete.cases(temp)]
  burko <- c(36.216667, -3.316667)
  kizigo <- c(34.733333, -6.433333)
  df[localities_clean == "Burko",1] <- burko[1]
  df[localities_clean == "Burko",2] <- burko[2]
  df[localities_clean == "KizigoW",1] <- kizigo[1]
  df[localities_clean == "KizigoW",2] <- kizigo[2]
  #localities_clean[!complete.cases(df)]
  
  # map those untrazable to just the country
  df[!complete.cases(df), ] <- geocode(lands[!complete.cases(df)])
  df$loc <- localities_clean
  df$country <- lands
  write.table(df, "zebra_package/longlat.txt")
  return(df)
}