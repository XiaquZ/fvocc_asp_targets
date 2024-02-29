calc_asp_vel <- function(tile_name,
                             tolerance,
                             max_distance,
                             present_files, # must contain
                             future_files) {
  print(paste("Now calculating:", tile_name))

  ## Load data
  pre <- rast(grep(tile_name, present_files, value = T))
  ## names(pre) <- "pre"
  fut <- rast(grep(tile_name, future_files, value = T))
  ## names(fut) <- "fut"

  ## Round pre and fut to one decimal
  pre_round <- round(pre, 1)
  pre_values <- freq(pre_round, digits = 1)$value
  fut_round <- round(fut, 1)

  ## Set tolerace for matching analogues

  ## apply over all of the pre_values in lis
  analogue_aspects <- lapply(pre_values, function(pre_value) {
    ## Filter only values in pre_round that are equal to pre_value
    pre_filt <- terra::mask(pre_round,
      pre_round == pre_value,
      maskvalues = F
    )

    ## Filter future analogues which are within tolerance of pre_value
    fut_filt <- terra::mask(fut_round,
      fut_round >= pre_value - tolerance & fut_round <= pre_value + tolerance,
      maskvalues = F
    )
    # Calculate distance to closest analogue
    fut_distance <- distance(fut_filt)
    names(fut_distance) <- "distance"
    # Calculate the aspects to the closest analogue
    fut_aspect <- terrain(fut_distance, v="aspect", neighbors = 8, unit = "degrees")
    names(fut_aspect) <- "aspect"
    
    ## Crop to tile to exclude buffer around tile
    analogue_asp <- mask(crop(fut_aspect, pre_filt), pre_filt)
  })

    analogue_aspects <- rast(analogue_aspects)
    asps <- app(analogue_aspects, fun = sum, na.rm = T) # Sum all layers of ds rast to make complete map
    names(asps) <- "aspects"
  # Save results as rasters.
    forward_asp_file <- paste0("/lustre1/scratch/348/vsc34871/output/VoCC/CentralEU/aspect/fvocc_asp_", tile_name, ".tif")
    forward_asp <- round(asps,1)
    print(forward_asp)
    writeRaster(forward_asp, forward_asp_file, overwrite = T) # write

    return(forward_asp_file) # Return filename
}
