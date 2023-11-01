#extract climate variables using GEE

#based on this example: https://stackoverflow.com/questions/67182129/translation-of-google-earth-engine-javascript-functions-to-python-functions

#This doesn't quite work yet -- TODO speed things up, probably by stripping the geometry somewhere


library("rgee")
library("sf")
library("tidyverse")

ee_Initialize(user = "swflake@ncsu.edu", drive = TRUE)

#what date range and climate variables do we want? 
date_start <- "1980-01-01"
date_end <- "1980-01-02"
climate_layers <- c("tmmx", "tmmn", "pr", "rmax", "rmin", "vs", "th") #names of variables in GridMET
# climate_layers <- "tmmx"
climate_names <- c("Tmax", "Tmin", "Prcp", "maxRH", "minRH", "wind_speed", "winddirection") #names needed by Climate Library

#load gridmet dataset
gridmet = ee$ImageCollection('IDAHO_EPSCOR/GRIDMET')$
  filterDate(date_start, date_end)$
  select(climate_layers)


# #create regions, or read in from external source
# #there should be an attribute called "label" that matches the ecoregion name
regions = ee$FeatureCollection(
  list(
    ee$Feature(    # San Francisco.

                 ee$Geometry$Rectangle(-122.45, 37.74, -122.4, 37.8),
                 list(ecoregion = 'City')
                 ),
   ee$Feature(  # Tahoe National Forest.
               ee$Geometry$Rectangle(-121, 39.4, -120.8, 39.8),
               list(ecoregion = 'Forest')
               ),
   ee$Feature(  # Black Rock Desert.
               ee$Geometry$Rectangle(-119.15, 40.8, -119, 41),
               list(ecoregion = 'Desert')
               )
   )
)

# regions <- sf::st_read("./Parameterization/Parameterization data/ecoregions_vector.gpkg") %>%
regions <- sf::st_read("./elev_reg_vector.gpkg") %>%
  dplyr::rename(ecoregion = climateregion) %>%
  dplyr::group_by(ecoregion) %>%
  sf::st_make_valid() %>%
  dplyr::summarise(geometry = sf::st_union(geom)) %>%
  dplyr::ungroup() %>%
  sf::st_transform("EPSG:4326") %>%
  dplyr::mutate(ecoregion = as.character(ecoregion))
  rgee::sf_as_ee()

st_write(regions, "ecoregions_elev.shp")

#plot them to make sure things line up right
Map$addLayer(gridmet$select("tmmx")$first()) + 
  Map$addLayer(regions)

# regions$select('label')$getInfo()
# ee_print(regions)

imfunc <- function(image) {
  return(
    image$select(climate_layers)$
      reduceRegions(
          collection = regions, 
          reducer = ee$Reducer$mean(), #get mean of raster within each (multi)polygon
          scale = 120)$
      map(function(f) { 
        return(f$set(list('imageId' = image$id())))} #add imageID (date) to tabular data
      )
  )
}

#map the extracting function over every date in the gridmet stack
triplets = gridmet$map(imfunc)$flatten()

# triplets$first()$getInfo()

#this might take a long time, depending on the length of the timeseries, number
#of climate variables, and number of ecoregions
system.time(clim_data <- ee_as_sf(triplets, via = "drive"))




