processGreenupDormancy <- function(url, studyAreaRas, varPrefix = NULL) {
  dPath <- "~/tmp/"
  dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
  
  message("Loading raster data from: ", url)
  browser()
  ras <- prepInputs(
    url = url,
    fun = quote({
      tfp <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
      b <- terra::rast(tfp)
      
      if (!is.null(varPrefix)) {
        original_names <- basename(tfp)
        
        # Extract 4-digit years from filenames
        years <- stringr::str_extract(original_names, "\\d{4}")
        new_names <- ifelse(!is.na(years),
                            paste0(varPrefix, "_1km_", years),
                            paste0(varPrefix, "_1km"))
        
        names(b) <- new_names
      }
      
      b
    }),
    destinationPath = dPath,
    to = studyAreaRas
  ) |> Cache()
  
  return(ras)
}

# 
# library(terra)
# 
# files <- list.files(
#   "/home/sdas/tmp",
#   pattern = "^Cleaned500mGreenup_.*\\.tif$",
#   full.names = TRUE
# )
# 
# # Reference extent from first file
# ref_r <- rast(files[1])
# ref_ext <- ext(ref_r)
# ref_res <- res(ref_r)
# 
# mismatch <- data.frame(
#   file = character(),
#   year = integer(),
#   extent_match = logical(),
#   res_match = logical(),
#   stringsAsFactors = FALSE
# )
# 
# for (f in files) {
#   r <- rast(f)
#   e <- ext(r)
#   res_r <- res(r)
#   
#   extent_ok <- all(e[] == ref_ext[])
#   res_ok     <- all(res_r == ref_res)
#   
#   mismatch <- rbind(
#     mismatch,
#     data.frame(
#       file = basename(f),
#       year = as.integer(sub(".*_(\\d{4})\\.tif$", "\\1", basename(f))),
#       extent_match = extent_ok,
#       res_match    = res_ok,
#       stringsAsFactors = FALSE
#     )
#   )
# }
# 
# # Which ones don't match?
# mismatch %>%
#   filter(!extent_match | !res_match)
# 
# 
# 
# 
# 
# library(terra)
# library(dplyr)
# 
# files <- list.files(
#   "/home/sdas/tmp",
#   pattern = "^Cleaned500mGreenup_.*\\.tif$",
#   full.names = TRUE
# )
# 
# # Build extent table
# ext_df <- do.call(rbind, lapply(files, function(f) {
#   r <- rast(f)
#   e <- ext(r)
#   data.frame(
#     file = basename(f),
#     year = as.integer(sub(".*_(\\d{4})\\.tif$", "\\1", basename(f))),
#     xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4],
#     stringsAsFactors = FALSE
#   )
# }))
# 
# # Find majority extent (most common combination)
# majority_ext <- ext_df %>%
#   group_by(xmin, xmax, ymin, ymax) %>%
#   tally() %>%
#   arrange(desc(n)) %>%
#   slice(1) %>%
#   ungroup()
# 
# # Extract as scalars (length-1 numerics)
# ref_xmin <- as.numeric(majority_ext$xmin[1])
# ref_xmax <- as.numeric(majority_ext$xmax[1])
# ref_ymin <- as.numeric(majority_ext$ymin[1])
# ref_ymax <- as.numeric(majority_ext$ymax[1])
# 
# # Compute mismatch
# ext_df <- ext_df %>%
#   mutate(
#     dxmin = xmin - ref_xmin,
#     dxmax = xmax - ref_xmax,
#     dymin = ymin - ref_ymin,
#     dymax = ymax - ref_ymax,
#     d_width  = (xmax - xmin) - (ref_xmax - ref_xmin),
#     d_height = (ymax - ymin) - (ref_ymax - ref_ymin)
#   )
# 
# # Inspect
# ext_df %>%
#   select(year, dxmin, dxmax, dymin, dymax, d_width, d_height) %>%
#   arrange(year)
# 
# 
# old_greenup<- terra::rast("~/tmp/Cleaned500mdor_2009.tif")
# new_greenup<- terra::rast("~/tmp/greenup_cleaned/Cleaned500mGreenup_2009.tif")
# 
# terra::crs(old_greenup, proj = TRUE)
# terra::crs(new_greenup, proj = TRUE)
# terra::plot(old_greenup)
# terra::plot(new_greenup)
# 
# summary(old_greenup)
# summary(new_greenup)
