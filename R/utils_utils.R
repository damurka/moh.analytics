
cached_generate_indicators <- memoise(function(.data, level, period) {
  generate_indicators(.data, level, period)
})
