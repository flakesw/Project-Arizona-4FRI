# increase the Nitrogen available
library(terra)

surfc <- terra::rast("./NECNv6_drought/NECNmaps/SOM1Csurf.tif")

soilc <- terra::rast("./NECNv6_drought/NECNmaps/SOM1Csoil.tif")

soil2c <- terra::rast("./NECNv6_drought/NECNmaps/SOM2C.tif")
  
soil3c <- terra::rast("./NECNv6_drought/NECNmaps/SOM3C.tif")

surfn <- surfc * 0.1
terra::writeRaster(surfn, "./NECNv6_drought/NECNmaps/SOM1Nsurf.tif", overwrite = TRUE)

soiln <- soilc * 0.1
terra::writeRaster(soiln, "./NECNv6_drought/NECNmaps/SOM1Nsoil.tif", overwrite = TRUE)

soil2n <- soil2c * .04
terra::writeRaster(soil2n, "./NECNv6_drought/NECNmaps/SOM2N.tif", overwrite = TRUE)

soil3n <- soil3c * 0.112
terra::writeRaster(soil3n, "./NECNv6_drought/NECNmaps/SOM3N.tif", overwrite = TRUE)

# soildepth <- terra::rast("./NECNv6_drought/NECNmaps/SoilDepth.tif")
# values(soildepth) <- 100
# terra::writeRaster(soildepth, "./SoilDepth_constant.tif")
