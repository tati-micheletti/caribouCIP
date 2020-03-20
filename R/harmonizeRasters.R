harmonizeRasters <- function(stk, currentTime = NULL){ # TODO Make it flexible for more rasters!
  stk_norm <- raster::stack(pemisc::normalizeStack(stk))
  totalCellsLayers <- raster::ncell(stk_norm)
  stk_table <- data.table(pixelID = 1:totalCellsLayers, raster::getValues(stk_norm))
  stk_table <- na.omit(stk_table)
  pixelID <- stk_table$pixelID
  stk_table[, pixelID := NULL]
  stk_table[, `:=` (average = rowSums(stk_table, na.rm = TRUE)/2,
                    stdDev = apply(stk_table, 1, sd, na.rm = TRUE))] #TODO make this more efficient!
  listRas <- list(average = populateRaster(pixelID = pixelID, rasTemplate = stk[[1]],
                                           values = as.numeric(stk_table$average), 
                                           rasterName = paste0("averagestk_Year", currentTime,".tif")),
                                           stdDev = populateRaster(pixelID = pixelID, rasTemplate = stk[[1]],
                                                                   values = as.numeric(stk_table$stdDev), 
                                                                   rasterName = paste0("stdDevstk_Year", currentTime,".tif")))
  return(listRas)
}