#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Update: 26-04-2022
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # require pacman
p_load(tidyverse,rio,sf,leaflet,viridis, # require and/or install packages
       osmdata, # API Open Street Maps
       ggmap,   # API Google Maps
       ggsn,    # Barras de escalas
       gtools)  # Funcion quantcut

#==== Hoy veremos ====# 

## 1. Plotting Simple Features
## 2. OpenStreetMap
## 3. Google Maps

#===============================#
# [1.] Plotting Simple Features #
#===============================#

#=== 1.1. Help ===#
vignette("sf5")

## data
## mapping
## geometry

#=== 1.2. load data ===#
bog = import("input/Median Housing Values Bogota.rds")
bog$mhv_q = quantcut(bog$mhv/1000000)

#=== 1.3.1. plot basic map ===#
ggplot(data=bog) + 
geom_sf(fill="orange" , col="black" , size=0.3) 

#=== 1.3.2. mapping ===#
p = ggplot(data=bog) + 
    geom_sf(mapping = aes(fill=mhv_q) , size=0.3 , col=NA)  +
    scale_fill_manual(values=c("yellow","#FF9900","#FF6600","#CC0000","#990000"),
                      name="Median Housing \nValues") 
p

#=== 1.3.3. add theme ===#
p = p + 
    theme_bw()
p 

# add scalebar and north symbol
p = p + 
    north(data=bog , location="topleft") + 
    scalebar(data=bog , dist=5 , dist_unit="km" , transform=T , model="WGS84")
p

# remove axis-labels
p = p + 
    labs(x="",y="")
p

# save plot
ggsave(plot=p , filename="output/map_bogota.pdf" , width=6.5 , height=8)

#====================#
# [2.] OpenStreetMap #
#====================#

#=== 2.1. About OMS ===#

# view keys
browseURL("https://wiki.openstreetmap.org/wiki/Map_features")

# get avaliables values for amenity
available_tags("amenity")
available_tags("building")
available_tags("landuse")

# get bbox
getbb("Bogotá Colombia")

#=== 2.2. Get amenities ===#

# get osm data
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
      #add_osm_feature(key = "amenity", value = "bar") %>%
      add_osm_feature(key = "amenity", value = "bus_station") %>%
      #add_osm_feature(key = "amenity", value = "restaurant") %>%
      osmdata_sf()
osm %>% class()
osm

# get sf object
amenities = osm$osm_points %>% select(osm_id,amenity)

# plot data
leaflet() %>% 
addTiles() %>% 
addCircleMarkers(data=amenities , weight=1 , col="green")


#=== 2.3. Get street ===#

# get osm data
street = opq(bbox = getbb("Bogotá Colombia")) %>%
         add_osm_feature(key = "highway") %>%
         osmdata_sf()

# get sf object
street = street$osm_lines %>% select(osm_id,name)
street = street %>%
         subset(str_detect(name,"Avenida")==T | str_detect(name,"TransMilenio")==T)

#==================#
# [3.] Google Maps #
#==================#

# You can get the Google key here: https://developers.google.com/maps/documentation/geocoding/get-api-key?hl=es
register_google(key = "Write the appy key here" , write = T) 

#=== 3.1. Geocode  ===#
google = geocode(location="Casa de Nariño, Bogotá D.C.",
                 output="latlon",
                 source="google")
google

# google = import("input/google.rds") # load data
google = st_as_sf(x = google , coords=c("lon","lat") , crs=4326)
google$name = "Casa de Nariño"

leaflet() %>%
addTiles() %>%
addCircleMarkers(data=google)

#=== 3.2. IPS  ===#
ips = import("input/IPS Bogota.rds") %>% 
      subset(is.na(lon_google)==F)
ips = st_as_sf(x=ips , coords=c("lon_google","lat_google"), crs=4326) 

# view data
leaflet() %>%
addTiles() %>%
addCircleMarkers(data=ips , label=ips$nombre_prestador)

#=====================#
# Para seguir leyendo #
#=====================#

##  Lovelace, R., Nowosad, J., & Muenchow, J. (2019). Geocomputation with R. [Ver aquí]

    # Cap. 4: Spatial Data Operations
    # Cap. 5: Geometry Operations
    # Cap. 6: Reprojecting geographic data
    # Cap. 11: Statistical learning

## Bivand, R. S., Pebesma, E. J., Gómez-Rubio, V., & Pebesma, E. J. (2013). Applied spatial data analysis with R. [Ver aquí]

    # Cap. 7: Spatial Point Pattern Analysis
    # Cap. 8: Interpolation and Geostatistics
