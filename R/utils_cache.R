shapefile <- memoise(function() {
  shape <- st_read('inst/county.gpkg', 'county', quiet = TRUE)
  return(sf_geojson(shape))
})

cached_generate_indicators <- memoise(function(level, period) {
  khis_data %>%
    generate_indicators(level, period)
})

memoised_month_choices <- memoise(function(year_col, year_val) {
  khis_data %>%
    filter(!!sym(year_col) == year_val) %>%
    distinct(month, year) %>%
    mutate(label = paste0(month, " ", year)) %>%
    arrange(desc(year), desc(match(month, month.name))) %>%
    transmute(label, value = month)
})
