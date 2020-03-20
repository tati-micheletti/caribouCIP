populateRaster <- function(pixelID, rasTemplate, values, rasterName = NULL){
  if (length(pixelID) != length(values))
    stop("length pixelID and values needs to match")
  
  aveValues <- data.table(pixelID = pixelID, average = as.numeric(values))
  fullAveValues <- merge(data.table(pixelID = 1:raster::ncell(rasTemplate)), aveValues, on = "pixelID", all.x = TRUE)
  setkey(fullAveValues, "pixelID")
  predictedAveMap <- raster::setValues(rasTemplate, values = fullAveValues[["average"]])
  
  if (!is.null(rasterName))
    writeRaster(predictedAveMap, rasterName,
                format = "GTiff", overwrite = TRUE)
  predMap <- raster::raster(rasterName)
  return(predMap)
}

