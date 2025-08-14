shapefile <- memoise(function() {
  shape <- st_read('inst/county.gpkg', 'county', quiet = TRUE)
  return(sf_geojson(shape))
})
