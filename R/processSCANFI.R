# processSCANFI <- function(SCANFIurl, studyAreaRas, processed = TRUE) {
#   library(googledrive)
#   library(terra)
#   library(reproducible)
#   
#   dPath <- "~/tmp/SCANFI/"
#   dir.create(dPath, showWarnings = FALSE, recursive = TRUE)
#   
#   # Standardize SCANFI layer names
#   standardizeSCANFIlayerNames <- function(names_vector) {
#     names_vector <- gsub("\\.tif$", "", names_vector)
#     names_vector <- gsub("SCANFI_att_", "SCANFI", names_vector)
#     names_vector <- gsub("SCANFI_sps_", "SCANFI", names_vector)
#     names_vector <- gsub("_1km", "", names_vector)
#     names_vector <- gsub("_5x5", "", names_vector)
#     names_vector <- gsub("_S_", "_", names_vector)
#     names_vector <- gsub("_v[0-9]+$", "", names_vector)
#     return(names_vector)
#   }
#   
#   #browser()
#   # Handle extent mismatch
#   # resampleIfMismatch <- function(ras, template) {
#   #   if (!compareGeom(ras, template, stopOnError = FALSE)) {
#   #     message("Resampling due to mismatch in extent/resolution")
#   #     ras <- resample(ras, template)
#   #   }
#   #   return(ras)
#   # }
#   
#   resampleIfMismatch <- function(ras, template) {
#     if (!compareGeom(ras, template, stopOnError = FALSE)) {
#       message("Resampling due to mismatch in extent/resolution")
#       
#       # Ensure CRS match before resampling
#       if (!terra::same.crs(ras, template)) {
#         ras <- terra::project(ras, template)
#       }
#       
#       ras <- terra::resample(ras, template)
#     }
#     return(ras)
#   }
#   
#   
#   main_folder_id <- sub(".*/folders/([^/]+)$", "\\1", SCANFIurl)
#   drive_folders <- drive_ls(as_id(main_folder_id), type = "folder")
#   
#   #  Processed SCANFI
#   if (processed) {
#     result_list <- list()
#     for (res in c("1km", "5x5")) {
#       folder_id <- drive_folders$id[drive_folders$name == res]
#       tfp <- prepInputs(
#         url = paste0("https://drive.google.com/drive/folders/", folder_id),
#         destinationPath = file.path(dPath, res),
#   #       fun = quote({
#   #         tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
#   #         b <- rast(tf)
#   #         names(b) <- standardizeSCANFIlayerNames(basename(tf)) #, resolution = res
#   #         names(b) <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", res, "_\\3"), names(b))
#   #         
#   #         b
#   #       }),
#   #       to = studyAreaRas
#   #     )|> Cache()
#   #     result_list[[res]] <- tfp
#   #   }
#   #   return(result_list)
#   # }
#   fun = quote({
#     tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
#     layers <- lapply(tf, function(f) {
#       r <- rast(f)
#       r <- resampleIfMismatch(r, studyAreaRas)
#       names(r) <- gsub("\\.tif$", "", basename(f))
#       r
#     })
#     rast(layers)
#   }),
#   to = studyAreaRas
#       ) |> Cache()
#       
#       # Apply naming convention
#       names(tfp) <- standardizeSCANFIlayerNames(names(tfp))
#       names(tfp) <- sub("^(SCANFI[^_]+)(?:_)?([0-9]{4})$", paste0("\\1_", res, "_\\2"), names(tfp))
#       result_list[[res]] <- tfp
#     }
#     return(result_list)
#   }
#   
#   # Unprocessed SCANFI
#   variable_types <- c("biomass", "height", "sps", "closure")
#   cv <- function(x) {
#     y <- na.omit(sample(x, size = min(10, length(x)), replace = FALSE))
#     sd(y) / mean(y)
#   }
#   
#   subfolders <- drive_folders$name
#   result_list <- list("1km" = list(), "5x5" = list())
#   browser()
#   for (subfolder in subfolders) {
#     year <- gsub("SCANFI_", "", subfolder)
#     folder_id <- drive_folders$id[drive_folders$name == subfolder]
#     
#     message("Processing year: ", year)
#     files <- prepInputs(
#       url = paste0("https://drive.google.com/drive/folders/", folder_id),
#       destinationPath = file.path(dPath, subfolder),
#       fun = quote({
#         tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
#         rast(tf)
#       }),
#       to = studyAreaRas
#     )|> Cache()
#     
#     for (i in 1:nlyr(files)) {
#       name_i <- names(files)[i]
#       ras_i <- files[[i]]
#       
#       var_type <- dplyr::case_when(
#         grepl("biomass", name_i, ignore.case = TRUE) ~ "biomass",
#         grepl("height", name_i, ignore.case = TRUE) ~ "height",
#         grepl("sps", name_i, ignore.case = TRUE) ~ "sps",
#         grepl("closure", name_i, ignore.case = TRUE) ~ "closure",
#         TRUE ~ NA_character_
#       )
#       if (is.na(var_type)) next
#       
#       ras_1km <- project(ras_i, studyAreaRas)
#       ras_5x5 <- focal(ras_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#       
#       if (var_type == "height") {
#         ras_cv <- aggregate(ras_1km, fact = 33, fun = cv)
#         #ras_cv_1km <- project(ras_cv, studyAreaRas)
#         ## making sure biliear (BAM) rather than default nearest
#         ras_cv_1km <- terra::project(ras_cv,studyAreaRas,res = 1000,method = "bilinear",align = TRUE)
#         
#         ras_cv_5x5 <- focal(ras_cv_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#         
#         result_list[["1km"]][[paste0("SCANFIheightcv_1km_", year)]] <- ras_cv_1km
#         result_list[["5x5"]][[paste0("SCANFIheightcv_5x5_", year)]] <- ras_cv_5x5
#       }
#       
#       if (var_type == "sps" && paste0("SCANFIBiomass_1km_", year) %in% names(result_list[["1km"]])) {
#         biomass_ras <- result_list[["1km"]][[paste0("SCANFIBiomass_1km_", year)]]
#         ras_1km <- (ras_1km / 100) * biomass_ras
#         ras_5x5 <- focal(ras_1km, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
#       }
#       #browser()
#       base_name <- standardizeSCANFIlayerNames(name_i)
#       base_name_1km <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", "1km", "_\\3"), base_name)
#       base_name_5x5 <- sub("^(SCANFI[^_]+)(_?)([0-9]{4})$", paste0("\\1_", "5x5", "_\\3"), base_name)
#       result_list[["1km"]][[base_name_1km]] <- ras_1km
#       result_list[["5x5"]][[base_name_5x5]] <- ras_5x5
#       
#     }
#   }
#   
#   return(list("1km" = rast(result_list[["1km"]]), "5x5" = rast(result_list[["5x5"]])))
# }




### new funciton that incorporates SCANFI V2

# library(terra)
# 
# v <- terra::vect("~/SpaDES_book/LandRDemo_coreVeg/future/ian/study_area_6_1_1.gpkg")
# studyArea = {
#   # create and use a random study area
#   # Lambert Conformal Conic for Canada: this is used in NRCan's "KNN" products
#   targetCRS <- crs(v, proj=TRUE)
#   studyArea <- v
#   studyArea <- terra::project(studyArea, targetCRS)
# }
# 
# studyAreaLarge = terra::buffer(studyArea, width = 100000)
# 
# rasterToMatchLarge = {
#   rtml<- terra::rast(studyAreaLarge,  res = c(1000, 1000))   #res = c(250, 250))
#   rtml[] <- 1
#   rtml <- terra::mask(rtml, studyAreaLarge)
# }
# 
# rasterToMatch = {
#   reproducible::postProcess(rasterToMatchLarge, cropTo = studyArea, maskTo = studyArea)
# }
# 
# studyAreaRas = rasterToMatch
# 
# ## URL FOR V2
# SCANFIurl2 <- "https://drive.google.com/drive/u/0/folders/1i7w4tO52Sh45tRwlUiUEnwidOeE5Zq68"
# 
# ## URL for V1
# SCANFIurl1 <- "https://drive.google.com/drive/folders/1BBs55yCcqUPZL65Q7wRYNyPOXWogK6Ag"
# 
# ## URL for processed
# SCANFIurlV0 <- "https://drive.google.com/drive/folders/1zF0PozF8j7u3K6x8gMblFwAZ41ZhmVPs"

#------------------------------------------------------------------------------
# Process SCANFI predictors for the bird module.
#
# Supports both processed SCANFI products (ready-to-use 1 km and 5x5 rasters)
# and unprocessed SCANFI datasets (native-resolution rasters requiring
# processing). Compatible with both SCANFI V1 and V2 naming conventions.
#
# Arguments:
#   SCANFIurl          Google Drive folder containing SCANFI data.
#
#   studyAreaRas       Final study area used for cropping/masking outputs.
#
#   studyAreaLarge     Buffered study area used when processing raw rasters
#                      (required for focal calculations and height CV).
#
#   rasterToMatchLarge Buffered 1 km template used to align all predictors.
#
#   processed
#       TRUE  = download complete processed 1 km and 5x5 folders.
#       FALSE = download and process raw SCANFI rasters individually.
#
#   version
#       "v1" = legacy SCANFI naming convention.
#       "v2" = new SCANFI V2 naming convention
#              (e.g. SCANFI_spsCC_ABIE_BAL_1985_v2_20260119.tif).
#
#   years
#       NULL           = process all available years.
#       Numeric vector = process only selected inventory years
#                        (e.g. c(1985, 1990, 1995)).
#
#   variables
#       NULL           = process all variables.
#
#       Character vector selecting variables to process, e.g.
#         "height"
#         "closure"
#         "biomass"
#         "species" (or "sps") (for V1)
#         "landcover"
#         "age" (for V2)
#         individual species codes (V2), e.g. "ABIE_BAL", "PINU_BAN", "PSEU_MEN", "PINU_CON", "PINU_PON", "LARI_LAR" (for V2)
#
# Notes:
#   • Height is processed separately to calculate native-resolution CV before
#     resampling to the standard 1 km grid.
#   • Species percentage layers are converted to species biomass using the
#     corresponding total biomass raster before generating 5x5 predictors.
#   • All outputs follow the standard bird-module naming convention:
#       <variable>_1km_<year>
#       <variable>_5x5_<year>
#------------------------------------------------------------------------------

# processSCANFI <- function(
#     SCANFIurl,
#     studyAreaRas,
#     studyAreaLarge = studyAreaRas,
#     rasterToMatchLarge = studyAreaRas,
#     processed = TRUE,
#     version = c("v1", "v2"),
#     years = NULL,
#     variables = NULL
# ) {
#   library(googledrive)
#   library(terra)
#   library(reproducible)
#   browser()
#   version <- match.arg(version)
#   dPath <- file.path("~/tmp/SCANFI", version)
#   dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
#   
#   # ---------------------------------------------------------------------------
#   # Helper functions
#   # ---------------------------------------------------------------------------
#   
#   extractYear <- function(x) {
#     m <- regexec("_((?:19|20)[0-9]{2})_v[0-9]+_[0-9]{8}", basename(x), perl = TRUE)
#     z <- regmatches(basename(x), m)[[1]]
#     if (length(z) > 1L) return(z[2])
#     
#     z <- regmatches(basename(x), regexpr("(19|20)[0-9]{2}", basename(x)))
#     if (!length(z) || identical(z, "")) NA_character_ else z
#   }
#   
#   standardizeName <- function(x) {
#     x <- basename(x)
#     x <- gsub("\\.tiff?$", "", x, ignore.case = TRUE)
#     x <- gsub("_v[0-9]+_[0-9]{8}$", "", x, ignore.case = TRUE)
#     x <- gsub("_v[0-9]+$", "", x, ignore.case = TRUE)
#     x <- gsub("^SCANFI_att_", "SCANFI", x, ignore.case = TRUE)
#     x <- gsub("^SCANFI_spsCC_", "SCANFI", x, ignore.case = TRUE)
#     x <- gsub("^SCANFI_sps_", "SCANFI", x, ignore.case = TRUE)
#     x <- gsub("_1km|_5x5", "", x, ignore.case = TRUE)
#     x <- gsub("_S_", "_", x)
#     gsub("_+", "_", x)
#   }
#   
#   addResolution <- function(x, resolution, fallbackYear = NULL) {
#     x <- standardizeName(x)
#     if (grepl("_[0-9]{4}$", x)) {
#       sub("_([0-9]{4})$", paste0("_", resolution, "_\\1"), x)
#     } else if (!is.null(fallbackYear)) {
#       paste0(x, "_", resolution, "_", fallbackYear)
#     } else {
#       paste0(x, "_", resolution)
#     }
#   }
#   
#   identifyType <- function(x) {
#     x <- tolower(basename(x))
#     if (grepl("biomass", x)) return("biomass")
#     if (grepl("age", x)) return("age")
#     if (grepl("height", x)) return("height")
#     if (grepl("closure", x)) return("closure")
#     if (grepl("spscc|sps_|species", x)) return("sps")
#     NA_character_
#   }
#   resampleIfMismatch <- function(ras, template) {
#     if (!compareGeom(ras, template, stopOnError = FALSE)) {
#       message("Resampling due to mismatch in extent/resolution")
#       
#       # Ensure CRS match before resampling
#       if (!terra::same.crs(ras, template)) {
#         ras <- terra::project(ras, template)
#       }
#       
#       ras <- terra::resample(ras, template)
#     }
#     return(ras)
#   }
#   cv <- function(x, ...) {
#     x <- x[is.finite(x) & !is.na(x)]
#     if (length(x) < 2L) return(NA_real_)
#     x <- sample(x, min(10L, length(x)))
#     xMean <- mean(x)
#     if (!is.finite(xMean) || xMean == 0) return(NA_real_)
#     stats::sd(x) / xMean
#   }
#   
#   make5x5 <- function(x) {
#     terra::focal(x, w = matrix(1, 5, 5), fun = "mean", na.rm = TRUE)
#   }
#   
#   finalize <- function(x) {
#     reproducible::postProcess(x, cropTo = studyAreaRas, maskTo = studyAreaRas)
#   }
#   
#   variableMatch <- function(fileName, variables) {
#     if (is.null(variables)) return(TRUE)
#     any(vapply(variables, function(v) {
#       if (tolower(v) %in% c("species", "sps", "spscc")) {
#         grepl("spsCC|sps_|species", fileName, ignore.case = TRUE)
#       } else {
#         grepl(v, fileName, ignore.case = TRUE, fixed = TRUE)
#       }
#     }, logical(1)))
#   }
#   
#   mainFolderID <- sub(".*/folders/([^/?]+).*$", "\\1", SCANFIurl)
#   driveFolders <- drive_ls(as_id(mainFolderID), type = "folder")
#   
#   # ===========================================================================
#   # Processed V1/V2 products
#   # ===========================================================================
#   
#   if (processed) {
#     result <- list()
#     for (res in c("1km", "5x5")) {
#       folderID <- driveFolders$id[tolower(driveFolders$name) == tolower(res)]
#       if (!length(folderID)) {
#         warning("Could not find processed SCANFI folder: ", res)
#         next
#       }
#       
#       tfp <- prepInputs(
#         url = paste0("https://drive.google.com/drive/folders/", folderID[1]),
#         destinationPath = file.path(dPath, "processed", res),
#         fun = quote({
#           tf <- sort(targetFilePath[grepl("\\.tiff?$", targetFilePath, ignore.case = TRUE)])
#           if (!length(tf)) stop("No TIFF files found in processed folder.")
#           
#         #   x <- terra::rast(tf)
#         #   names(x) <- basename(tf)
#         #   x
#         # }),
#         # fun = quote({
#         #   tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath)])
#           layers <- lapply(tf, function(f) {
#             r <- rast(f)
#             r <- resampleIfMismatch(r, studyAreaRas)
#             names(r) <- gsub("\\.tif$", "", basename(f))
#             r
#           })
#           rast(layers)
#         }),
#         to = studyAreaRas
#       ) |> Cache()
#       
#       names(tfp) <- vapply(
#         names(tfp),
#         addResolution,
#         resolution = res,
#         FUN.VALUE = character(1)
#       )
#       
#       result[[res]] <- tfp
#     }
#     
#     if (!all(c("1km", "5x5") %in% names(result))) {
#       stop("Both processed 1km and 5x5 folders are required.")
#     }
#     
#     return(result)
#   }
#   
#   # ===========================================================================
#   # Unprocessed V1/V2 products
#   # ===========================================================================
#   
#   folderYears <- vapply(driveFolders$name, extractYear, character(1))
#   keepFolders <- !is.na(folderYears)
#   
#   if (!is.null(years)) {
#     keepFolders <- keepFolders & folderYears %in% as.character(years)
#   }
#   
#   yearFolders <- driveFolders[keepFolders, ]
#   if (!nrow(yearFolders)) stop("No SCANFI year folders matched the requested years.")
#   
#   result <- list("1km" = list(), "5x5" = list())
#   
#   for (j in seq_len(nrow(yearFolders))) {
#     year <- extractYear(yearFolders$name[j])
#     message("Processing SCANFI ", toupper(version), " year: ", year)
#     
#     driveFiles <- drive_ls(as_id(yearFolders$id[j]))
#     driveFiles <- driveFiles[
#       grepl("\\.tif?$", driveFiles$name, ignore.case = TRUE) &
#         !grepl("\\.(aux\\.xml|ovr)$", driveFiles$name, ignore.case = TRUE),
#     ]
#     
#     keep <- vapply(driveFiles$name, variableMatch, variables = variables, logical(1))
#     selectedFiles <- driveFiles[keep, ]
#     
#     # selectedTypes <- vapply(selectedFiles$name, identifyType, character(1))
#     # if ("sps" %in% selectedTypes) {
#     #   allTypes <- vapply(driveFiles$name, identifyType, character(1))
#     #   selectedFiles <- unique(rbind(
#     #     selectedFiles,
#     #     driveFiles[allTypes == "biomass", ]
#     #   ))
#     # }
#     
#     selectedTypes <- vapply(selectedFiles$name, identifyType, character(1))
#     
#     # V2 species layers contain species-specific crown closure.
#     # Overall closure is required to convert them back to the V1 percentage
#     # representation used to train the BAM bird models.
#     if (version == "v2" && "sps" %in% selectedTypes) {
#       allTypes <- vapply(driveFiles$name, identifyType, character(1))
#       selectedFiles <- unique(rbind(
#         selectedFiles,
#         driveFiles[allTypes == "closure", ]
#       ))
#     }
#     if (!nrow(selectedFiles)) {
#       warning("No matching files in folder: ", yearFolders$name[j])
#       next
#     }
#     
#     fileTypes <- vapply(selectedFiles$name, identifyType, character(1))
#     # priority <- match(fileTypes, c("biomass", "height", "closure","age", "sps"))
#     priority <- match(fileTypes,c("closure", "biomass", "height", "age", "sps")
#     )
#     selectedFiles <- selectedFiles[order(priority, na.last = TRUE), ]
#     
#     closureRaw <- NULL
#     
#     for (i in seq_len(nrow(selectedFiles))) {
#       fileName <- selectedFiles$name[i]
#       varType <- identifyType(fileName)
#       
#       if (is.na(varType)) {
#         message("Skipping unrecognized SCANFI layer: ", fileName)
#         next
#       }
#       
#       fileURL <- paste0("https://drive.google.com/open?id=", selectedFiles$id[i])
#       destination <- file.path(dPath, "unprocessed", year)
#       ## new addition
#       # Keep V2 total closure at native resolution because it is needed
#       # to convert V2 species crown closure back to V1 species percentage.
#       if (version == "v2" && varType == "closure") {
#         
#         closureRaw <- prepInputs(
#           url = fileURL,
#           targetFile = fileName,
#           destinationPath = destination,
#           cropTo = studyAreaLarge,
#           fun = "terra::rast"
#         ) |> Cache()
#         
#         ras1kmLarge <- postProcess(
#           closureRaw,
#           projectTo = rasterToMatchLarge
#         )
#         
#         ras1km <- finalize(ras1kmLarge)
#         ras5x5 <- finalize(make5x5(ras1kmLarge))
#         
#         name1km <- addResolution(fileName, "1km", year)
#         name5x5 <- addResolution(fileName, "5x5", year)
#         
#         names(ras1km) <- name1km
#         names(ras5x5) <- name5x5
#         
#         result[["1km"]][[name1km]] <- ras1km
#         result[["5x5"]][[name5x5]] <- ras5x5
#         
#         rm(ras1kmLarge)
#         gc()
#         next
#       }
#       ## new additon ends 19th August  
#       
#       if (varType == "height") {
#         rasRaw <- prepInputs(
#           url = fileURL,
#           targetFile = fileName,
#           destinationPath = destination,
#           cropTo = studyAreaLarge,
#           fun = "terra::rast"
#         ) |> Cache()
#         browser()
#         ras1kmLarge <- postProcess(rasRaw, projectTo = rasterToMatchLarge)
#         rasCV <- terra::aggregate(rasRaw, fact = 33, fun = cv)
#         rasCV1kmLarge <- postProcess(rasCV, projectTo = rasterToMatchLarge)
#         
#         ras1km <- finalize(ras1kmLarge)
#         ras5x5 <- finalize(make5x5(ras1kmLarge))
#         rasCV1km <- finalize(rasCV1kmLarge)
#         rasCV5x5 <- finalize(make5x5(rasCV1kmLarge))
#         
#         name1km <- addResolution(fileName, "1km", year)
#         name5x5 <- addResolution(fileName, "5x5", year)
#         nameCV1km <- paste0("SCANFIheightcv_1km_", year)
#         nameCV5x5 <- paste0("SCANFIheightcv_5x5_", year)
#         
#         names(ras1km) <- name1km
#         names(ras5x5) <- name5x5
#         names(rasCV1km) <- nameCV1km
#         names(rasCV5x5) <- nameCV5x5
#         
#         result[["1km"]][[name1km]] <- ras1km
#         result[["5x5"]][[name5x5]] <- ras5x5
#         result[["1km"]][[nameCV1km]] <- rasCV1km
#         result[["5x5"]][[nameCV5x5]] <- rasCV5x5
#         
#         rm(rasRaw, rasCV, ras1kmLarge, rasCV1kmLarge)
#         gc()
#         next
#       }
#       
#       ## NEW add 19th august
#       # V2 species rasters are species-specific crown closure.
#       # Convert to the V1 percentage representation expected by BAM:
#       # species % = species crown closure / total crown closure * 100
#       if (version == "v2" && varType == "sps") {
#         
#         if (is.null(closureRaw)) {
#           stop(
#             "V2 total closure is required before processing species layer: ",
#             fileName
#           )
#         }
#         
#         speciesRaw <- prepInputs(
#           url = fileURL,
#           targetFile = fileName,
#           destinationPath = destination,
#           cropTo = studyAreaLarge,
#           fun = "terra::rast"
#         ) |> Cache()
#         
#         # Ensure native species and closure rasters are spatially aligned.
#         if (!terra::compareGeom(speciesRaw, closureRaw, stopOnError = FALSE)) {
#           closureForSpecies <- terra::resample(
#             closureRaw,
#             speciesRaw,
#             method = "bilinear"
#           )
#         } else {
#           closureForSpecies <- closureRaw
#         }
#         
#         speciesV1 <- terra::ifel(
#           closureForSpecies > 0,
#           (speciesRaw / closureForSpecies) * 100,
#           0
#         )
#         
#         # Only after converting to V1 representation do we create the
#         # standard 1 km and 5x5 BAM predictors.
#         ras1kmLarge <- postProcess(
#           speciesV1,
#           projectTo = rasterToMatchLarge
#         )
#         
#         ras1km <- finalize(ras1kmLarge)
#         ras5x5 <- finalize(make5x5(ras1kmLarge))
#         
#         name1km <- addResolution(fileName, "1km", year)
#         name5x5 <- addResolution(fileName, "5x5", year)
#         
#         names(ras1km) <- name1km
#         names(ras5x5) <- name5x5
#         
#         result[["1km"]][[name1km]] <- ras1km
#         result[["5x5"]][[name5x5]] <- ras5x5
#         
#         rm(speciesRaw, speciesV1, ras1kmLarge)
#         
#         if (exists("closureForSpecies")) rm(closureForSpecies)
#         
#         gc()
#         next
#       }
#       
#       ## new addition ends 19th August
#       
#       ras1kmLarge <- prepInputs(
#         url = fileURL,
#         targetFile = fileName,
#         destinationPath = destination,
#         fun = "terra::rast",
#         to = rasterToMatchLarge
#       ) |> Cache()
#       
#       # if (varType == "sps") {
#       #   biomassNames <- names(result[["1km"]])
#       #   biomassName <- biomassNames[
#       #     grepl(paste0("biomass_1km_", year, "$"), biomassNames, ignore.case = TRUE)
#       #   ]
#       #   
#       #   if (length(biomassName) == 1L) {
#       #     biomassLarge <- postProcess(
#       #       result[["1km"]][[biomassName]],
#       #       projectTo = rasterToMatchLarge
#       #     )
#       #     ras1kmLarge <- (ras1kmLarge / 100) * biomassLarge
#       #   } else {
#       #     warning("Biomass missing for species layer: ", fileName,
#       #             ". Species-to-biomass conversion skipped.")
#       #   }
#       # }
#       
#       ras1km <- finalize(ras1kmLarge)
#       ras5x5 <- finalize(make5x5(ras1kmLarge))
#       
#       name1km <- addResolution(fileName, "1km", year)
#       name5x5 <- addResolution(fileName, "5x5", year)
#       
#       names(ras1km) <- name1km
#       names(ras5x5) <- name5x5
#       
#       result[["1km"]][[name1km]] <- ras1km
#       result[["5x5"]][[name5x5]] <- ras5x5
#       
#       rm(ras1kmLarge)
#       gc()
#     }
#   }
#   
#   if (!length(result[["1km"]]) || !length(result[["5x5"]])) {
#     stop("No SCANFI products were generated.")
#   }
#   
#   list(
#     "1km" = terra::rast(result[["1km"]]),
#     "5x5" = terra::rast(result[["5x5"]])
#   )
# }
# 
# scanfiTrialv1 <- processSCANFI(
#   SCANFIurl = SCANFIurl1,
#   studyAreaRas = studyAreaRas,
#   studyAreaLarge = studyAreaRas,
#   rasterToMatchLarge = studyAreaRas,
#   processed = FALSE,
#   version = "v1",
#   years = c(2015,2020),
#   variables = c("biomass", "height")
# )
# 
# scanfiTrialv2 <- processSCANFI(
#   SCANFIurl = SCANFIurl2,
#   studyAreaRas = studyAreaRas,
#   studyAreaLarge = studyAreaRas,
#   rasterToMatchLarge = studyAreaRas,
#   processed = FALSE,
#   version = "v2",
#   years = c(2015,2020),
#   variables = c("biomass", "height")
# )
# 
# scanfiTrialv0 <- processSCANFI(
#   SCANFIurl = SCANFIurlV0,
#   studyAreaRas = studyAreaRas,
#   studyAreaLarge = studyAreaRas,
#   rasterToMatchLarge = studyAreaRas,
#   processed = TRUE,
#   version = "v1",
#   years = c(2015,2020),
#   variables = c("biomass", "height")
# )
# 
# 
# 
# 
# ###latest function with scanfi v2 integration, with scanfi portal
# processSCANFI <- function(
#     SCANFIurl,
#     studyAreaRas,
#     studyAreaLarge = studyAreaRas,
#     rasterToMatchLarge = studyAreaRas,
#     processed = TRUE,
#     version = c("v1", "v2"),
#     years = NULL,
#     variables = NULL
# ) {
#   library(terra)
#   library(reproducible)
#   
#   version <- match.arg(version)
#   dPath <- file.path("~/tmp/SCANFI", version)
#   dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
#   
#   # Helper Utilities
#   extractYear <- function(x) {
#     x <- basename(x)
#     m <- regexec("_((?:19|20)[0-9]{2})_v[0-9]+_", x, perl = TRUE)
#     z <- regmatches(x, m)[[1]]
#     if (length(z) > 1L) return(z[2])
#     z <- regmatches(x, regexpr("(19|20)[0-9]{2}", x))
#     if (!length(z) || identical(z, "")) NA_character_ else z
#   }
#   
#   standardizeName <- function(x) {
#     x <- basename(x)
#     x <- gsub("\\.tif$", "", x, ignore.case = TRUE)
#     x <- gsub("_v[0-9]+(?:\\.[0-9]+)?_[0-9]{8}$", "", x, ignore.case = TRUE)
#     x <- gsub("_v[0-9]+(?:\\.[0-9]+)?$", "", x, ignore.case = TRUE)
#     x <- gsub("^SCANFI_(att_|sps_)?", "SCANFI", x, ignore.case = TRUE)
#     x <- gsub("_?(1km|5x5)_?", "_", x, ignore.case = TRUE)
#     gsub("_+", "_", x)
#   }
#   
#   addResolution <- function(x, resolution, fallbackYear = NULL) {
#     x <- standardizeName(x)
#     if (grepl("_[0-9]{4}$", x)) {
#       sub("_([0-9]{4})$", paste0("_", resolution, "_\\1"), x)
#     } else if (!is.null(fallbackYear)) {
#       paste0(x, "_", resolution, "_", fallbackYear)
#     } else {
#       paste0(x, "_", resolution)
#     }
#   }
#   
#   identifyType <- function(x) {
#     x <- tolower(basename(x))
#     if (grepl("biomass", x)) return("biomass")
#     if (grepl("height", x))  return("height")
#     if (grepl("closure", x)) return("closure")
#     if (grepl("age", x))     return("age")
#     if (grepl("spscc|sps_|species", x)) return("sps")
#     NA_character_
#   }
#   
#   cv <- function(x, ...) {
#     x <- x[is.finite(x) & !is.na(x)]
#     if (length(x) < 2L) return(NA_real_)
#     x <- sample(x, min(10L, length(x)))
#     xMean <- mean(x)
#     if (!is.finite(xMean) || xMean == 0) return(NA_real_)
#     stats::sd(x) / xMean
#   }
#   
#   # Standardized pipeline worker to process 1km and 5x5 grids to remove code bloat
#   processGridPair <- function(rawRast, layerNameBase) {
#     ras1kmLarge <- postProcess(rawRast, projectTo = rasterToMatchLarge)
#     ras1km      <- reproducible::postProcess(ras1kmLarge, cropTo = studyAreaRas, maskTo = studyAreaRas)
#     ras5x5      <- reproducible::postProcess(
#       terra::focal(ras1kmLarge, w = matrix(1, 5, 5), fun = "mean", na.rm = TRUE),
#       cropTo = studyAreaRas, maskTo = studyAreaRas
#     )
#     
#     name1km <- paste0(layerNameBase, "_1km")
#     name5x5 <- paste0(layerNameBase, "_5x5")
#     
#     names(ras1km) <- name1km
#     names(ras5x5) <- name5x5
#     
#     list("1km" = ras1km, "5x5" = ras5x5, "name1km" = name1km, "name5x5" = name5x5)
#   }
#   browser()
#   # --- PROCESSED ROUTINE ---
#   if (processed) {
#     library(googledrive)
#     mainFolderID <- sub(".*/folders/([^/?]+).*$", "\\1", SCANFIurl)
#     driveFolders <- drive_ls(as_id(mainFolderID), type = "folder")
#     result <- list()
#     
#     for (res in c("1km", "5x5")) {
#       folderID <- driveFolders$id[tolower(driveFolders$name) == tolower(res)]
#       if (!length(folderID)) {
#         warning("Could not find processed SCANFI folder: ", res)
#         next
#       }
#       
#       tfp <- prepInputs(
#         url = paste0("https://drive.google.com/drive/folders/", folderID[1]),
#         destinationPath = file.path(dPath, "processed", res),
#         fun = quote({
#           tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath, ignore.case = TRUE)])
#           if (!length(tf)) stop("No TIFF files found.")
#           x <- terra::rast(tf)
#           names(x) <- basename(tf)
#           x
#         }),
#         to = rasterToMatchLarge
#       )|> Cache()
#       
#       names(tfp) <- vapply(names(tfp), addResolution, resolution = res, FUN.VALUE = character(1))
#       result[[res]] <- reproducible::postProcess(tfp, cropTo = studyAreaRas, maskTo = studyAreaRas)
#     }
#     return(result)
#   }
#   
#   # --- UNPROCESSED V1 ROUTINE ---
#   if (version == "v1") {
#     library(googledrive)
#     mainFolderID <- sub(".*/folders/([^/?]+).*$", "\\1", SCANFIurl)
#     driveFolders <- drive_ls(as_id(mainFolderID), type = "folder")
#     
#     folderYears <- vapply(driveFolders$name, extractYear, character(1))
#     keepFolders <- !is.na(folderYears)
#     if (!is.null(years)) keepFolders <- keepFolders & folderYears %in% as.character(years)
#     
#     yearFolders <- driveFolders[keepFolders, ]
#     result <- list("1km" = list(), "5x5" = list())
#     
#     for (j in seq_len(nrow(yearFolders))) {
#       year <- extractYear(yearFolders$name[j])
#       files <- Cache(prepInputs(
#         url = paste0("https://drive.google.com/drive/folders/", yearFolders$id[j]),
#         destinationPath = file.path(dPath, "unprocessed", year),
#         fun = quote({
#           tf <- sort(targetFilePath[grepl("\\.tif$", targetFilePath, ignore.case = TRUE)])
#           x <- terra::rast(tf)
#           names(x) <- basename(tf)
#           x
#         }),
#         cropTo = studyAreaLarge
#       ))
#       
#       for (i in seq_len(nlyr(files))) {
#         rasRaw <- files[[i]]
#         varType <- identifyType(names(files)[i])
#         baseName <- addResolution(names(files)[i], "", year)
#         
#         # Grid Pair Construction
#         resPair <- processGridPair(rasRaw, baseName)
#         result[["1km"]][[resPair$name1km]] <- resPair$`1km`
#         result[["5x5"]][[resPair$name5x5]] <- resPair$`5x5`
#         
#         if (varType == "height") {
#           rasCV <- terra::aggregate(rasRaw, fact = 33, fun = cv)
#           resCVPair <- processGridPair(rasCV, paste0("SCANFIheightcv_", year))
#           result[["1km"]][[resCVPair$name1km]] <- resCVPair$`1km`
#           result[["5x5"]][[resCVPair$name5x5]] <- resCVPair$`5x5`
#         }
#       }
#     }
#     return(list("1km" = terra::rast(result[["1km"]]), "5x5" = terra::rast(result[["5x5"]])))
#   }
#   
#   # --- UNPROCESSED V2 ROUTINE ---
#   v2ToBAM <- c(
#     "balsamFir" = "BalsamFir", "blackSpruce" = "BlackSpruce", 
#     "broadleaf" = "prcD", "douglasFir" = "DouglasFir", 
#     "jackPine" = "JackPine", "lodgepolePine" = "LodgepolePine", 
#     "otherConiferous" = "prcC", "ponderosaPine" = "PonderosaPine", 
#     "tamarack" = "Tamarack", "whiteRedPine" = "WhiteRedPine"
#   )
#   
#   portalHTML <- readLines(SCANFIurl, warn = FALSE)
#   hrefs <- regmatches(portalHTML, gregexpr('href="[^"]+\\.tif"', portalHTML, perl = TRUE, ignore.case = TRUE))
#   fileNames <- unique(gsub('^href="|"$', "", unlist(hrefs)))
#   
#   parsed <- lapply(fileNames, function(f) {
#     yr <- extractYear(f)
#     if (grepl("biomass", f, ignore.case=TRUE)) return(c(yr, "biomass", "biomass", "biomass"))
#     if (grepl("closure", f, ignore.case=TRUE)) return(c(yr, "closure", "closure", "closure"))
#     if (grepl("height", f, ignore.case=TRUE)) return(c(yr, "height", "height", "height"))
#     if (grepl("age", f, ignore.case=TRUE)) return(c(yr, "age_median", "age_median", "age"))
#     if (grepl("spsCC", f, ignore.case=TRUE)) {
#       src <- sub("^SCANFI_spsCC_", "", sub("_(?:19|20)[0-9]{2}_.*\\.tif$", "", f, perl=TRUE), ignore.case=TRUE)
#       bam <- if (src %in% names(v2ToBAM)) unname(v2ToBAM[src]) else src
#       return(c(yr, src, bam, "sps"))
#     }
#     c(yr, NA, NA, NA)
#   })
#   
#   fileMeta <- data.frame(
#     fileName = fileNames,
#     year = vapply(parsed, `[[`, character(1), 1),
#     source = vapply(parsed, `[[`, character(1), 2),
#     BAM = vapply(parsed, `[[`, character(1), 3),
#     type = vapply(parsed, `[[`, character(1), 4),
#     stringsAsFactors = FALSE
#   )
#   fileMeta <- fileMeta[!is.na(fileMeta$type) & !is.na(fileMeta$year), ]
#   
#   if (!is.null(years)) fileMeta <- fileMeta[fileMeta$year %in% as.character(years), ]
#   
#   result <- list("1km" = list(), "5x5" = list())
#   
#   for (year in unique(fileMeta$year)) {
#     yearFiles <- fileMeta[fileMeta$year == year, ]
#     closureRaw <- NULL
#     
#     # Priority sort to fetch closure first for species ratio math
#     yearFiles <- yearFiles[order(match(yearFiles$type, c("closure", "biomass", "height", "age", "sps"))), ]
#     
#     for (i in seq_len(nrow(yearFiles))) {
#       fMeta <- yearFiles[i, ]
#       fileURL <- paste0(sub("/$", "", SCANFIurl), "/", fMeta$fileName)
#       destination <- file.path(dPath, "unprocessed", year)
#       
#       rasRaw <- prepInputs(
#         url = fileURL, targetFile = fMeta$fileName, destinationPath = destination,
#         cropTo = studyAreaLarge, fun = "terra::rast"
#       )|> Cache()
#       
#       if (fMeta$type == "closure") {
#         closureRaw <- rasRaw
#       } else if (fMeta$type == "sps") {
#         if (is.null(closureRaw)) stop("V2 total closure missing before species layer: ", fMeta$fileName)
#         
#         if (!terra::compareGeom(rasRaw, closureRaw, stopOnError = FALSE)) {
#           closureRaw <- terra::resample(closureRaw, rasRaw, method = "bilinear")
#         }
#         rasRaw <- terra::ifel(closureRaw > 0, (rasRaw / closureRaw) * 100, 0)
#       }
#       
#       resPair <- processGridPair(rasRaw, paste0("SCANFI", fMeta$BAM, "_", year))
#       result[["1km"]][[resPair$name1km]] <- resPair$`1km`
#       result[["5x5"]][[resPair$name5x5]] <- resPair$`5x5`
#       
#       if (fMeta$type == "height") {
#         rasCV <- terra::aggregate(rasRaw, fact = 33, fun = cv)
#         resCVPair <- processGridPair(rasCV, paste0("SCANFIheightcv_", year))
#         result[["1km"]][[resCVPair$name1km]] <- resCVPair$`1km`
#         result[["5x5"]][[resCVPair$name5x5]] <- resCVPair$`5x5`
#       }
#       gc()
#     }
#   }
#   
#   return(list(
#     "1km" = terra::rast(result[["1km"]]),
#     "5x5" = terra::rast(result[["5x5"]])
#   ))
# }
# 
# 
# 
# scanfiTrial <- processSCANFI(
#   SCANFIurl = "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/",
#   studyAreaRas = studyAreaRas,
#   studyAreaLarge = studyAreaLarge,
#   rasterToMatchLarge = rasterToMatchLarge,
#   processed = FALSE,
#   version = "v2",
#   years = 2020,
#   variables = "BalsamFir"
# )

processSCANFI <- function(
    SCANFIurl,
    studyAreaRas,
    studyAreaLarge = studyAreaRas,
    rasterToMatchLarge = studyAreaRas,
    processed = TRUE,
    version = c("v1", "v2"),
    years = NULL,
    variables = NULL
) {
  library(terra)
  library(reproducible)
  
  version <- match.arg(version)
  dPath <- file.path("~/tmp/SCANFI", version)
  dir.create(dPath, recursive = TRUE, showWarnings = FALSE)
  
  # ---------------------------------------------------------------------------
  # Helper functions
  # ---------------------------------------------------------------------------
  
  extractYear <- function(x) {
    x <- basename(x)
    m <- regexec("_((?:19|20)[0-9]{2})_v[0-9]+_[0-9]{8}", x, perl = TRUE)
    z <- regmatches(x, m)[[1]]
    if (length(z) > 1L) return(z[2])
    
    z <- regmatches(x, regexpr("(19|20)[0-9]{2}", x))
    if (!length(z) || identical(z, "")) NA_character_ else z
  }
  
  standardizeName <- function(x) {
    x <- basename(x)
    x <- gsub("\\.tif$", "", x, ignore.case = TRUE)
    x <- gsub("_v[0-9]+_[0-9]{8}$", "", x, ignore.case = TRUE)
    x <- gsub("_v[0-9]+$", "", x, ignore.case = TRUE)
    x <- gsub("^SCANFI_att_", "SCANFI", x, ignore.case = TRUE)
    x <- gsub("^SCANFI_spsCC_", "SCANFI", x, ignore.case = TRUE)
    x <- gsub("^SCANFI_sps_", "SCANFI", x, ignore.case = TRUE)
    x <- gsub("_1km|_5x5", "", x, ignore.case = TRUE)
    x <- gsub("_S_", "_", x)
    gsub("_+", "_", x)
  }
  
  addResolution <- function(x, resolution, fallbackYear = NULL) {
    x <- standardizeName(x)
    if (grepl("_[0-9]{4}$", x)) {
      sub("_([0-9]{4})$", paste0("_", resolution, "_\\1"), x)
    } else if (!is.null(fallbackYear)) {
      paste0(x, "_", resolution, "_", fallbackYear)
    } else {
      paste0(x, "_", resolution)
    }
  }
  
  identifyType <- function(x) {
    x <- tolower(basename(x))
    if (grepl("biomass", x)) return("biomass")
    if (grepl("age", x)) return("age")
    if (grepl("height", x)) return("height")
    if (grepl("closure", x)) return("closure")
    if (grepl("spscc|sps_|species", x)) return("sps")
    NA_character_
  }
  
  resampleIfMismatch <- function(ras, template) {
    if (!compareGeom(ras, template, stopOnError = FALSE)) {
      message("Resampling due to mismatch in extent/resolution")
      if (!terra::same.crs(ras, template)) ras <- terra::project(ras, template)
      ras <- terra::resample(ras, template)
    }
    ras
  }
  
  cv <- function(x, ...) {
    x <- x[is.finite(x) & !is.na(x)]
    if (length(x) < 2L) return(NA_real_)
    x <- sample(x, min(10L, length(x)))
    xMean <- mean(x)
    if (!is.finite(xMean) || xMean == 0) return(NA_real_)
    stats::sd(x) / xMean
  }
  
  make5x5 <- function(x) {
    terra::focal(x, w = matrix(1, 5, 5), fun = "mean", na.rm = TRUE)
  }
  
  finalize <- function(x) {
    reproducible::postProcess(x, cropTo = studyAreaRas, maskTo = studyAreaRas)
  }
  
  variableMatch <- function(fileName, variables) {
    if (is.null(variables)) return(TRUE)
    
    any(vapply(variables, function(v) {
      if (tolower(v) %in% c("species", "sps", "spscc")) {
        grepl("spsCC|sps_|species", fileName, ignore.case = TRUE)
      } else {
        grepl(v, fileName, ignore.case = TRUE, fixed = TRUE)
      }
    }, logical(1)))
  }
  browser()
  # ===========================================================================
  # PROCESSED V1 / V2
  #
  # Same Google Drive 1km / 5x5 folder structure as the original function.
  # ===========================================================================
  
  if (processed) {
    library(googledrive)
    
    mainFolderID <- sub(".*/folders/([^/?]+).*$", "\\1", SCANFIurl)
    driveFolders <- drive_ls(as_id(mainFolderID), type = "folder")
    result <- list()
    
    for (res in c("1km", "5x5")) {
      folderID <- driveFolders$id[tolower(driveFolders$name) == tolower(res)]
      if (!length(folderID)) {
        warning("Could not find processed SCANFI folder: ", res)
        next
      }
      
      tfp <- prepInputs(
        url = paste0("https://drive.google.com/drive/folders/", folderID[1]),
        destinationPath = file.path(dPath, "processed", res),
        fun = quote({
          tf <- sort(targetFilePath[
            grepl("\\.tif$", targetFilePath, ignore.case = TRUE)
          ])
          if (!length(tf)) stop("No TIFF files found in processed folder.")
          
          layers <- lapply(tf, function(f) {
            r <- rast(f)
            r <- resampleIfMismatch(r, studyAreaRas)
            names(r) <- gsub("\\.tif$", "", basename(f))
            r
          })
          rast(layers)
        }),
        to = studyAreaRas
      ) |> Cache()
      
      names(tfp) <- vapply(
        names(tfp), addResolution,
        resolution = res,
        FUN.VALUE = character(1)
      )
      result[[res]] <- tfp
    }
    
    if (!all(c("1km", "5x5") %in% names(result))) {
      stop("Both processed 1km and 5x5 folders are required.")
    }
    return(result)
  }
  
  result <- list("1km" = list(), "5x5" = list())
  
  # ===========================================================================
  # UNPROCESSED V1
  #
  # Preserve the existing Google Drive nested year-folder workflow.
  # ===========================================================================
  
  if (version == "v1") {
    library(googledrive)
    
    mainFolderID <- sub(".*/folders/([^/?]+).*$", "\\1", SCANFIurl)
    driveFolders <- drive_ls(as_id(mainFolderID), type = "folder")
    
    folderYears <- vapply(driveFolders$name, extractYear, character(1))
    keepFolders <- !is.na(folderYears)
    if (!is.null(years)) {
      keepFolders <- keepFolders & folderYears %in% as.character(years)
    }
    
    yearFolders <- driveFolders[keepFolders, ]
    if (!nrow(yearFolders)) stop("No SCANFI V1 year folders matched requested years.")
    
    for (j in seq_len(nrow(yearFolders))) {
      year <- extractYear(yearFolders$name[j])
      message("Processing SCANFI V1 year: ", year)
      
      driveFiles <- drive_ls(as_id(yearFolders$id[j]))
      driveFiles <- driveFiles[
        grepl("\\.tif$", driveFiles$name, ignore.case = TRUE),
      ]
      
      keep <- vapply(
        driveFiles$name,
        variableMatch,
        variables = variables,
        logical(1)
      )
      selectedFiles <- driveFiles[keep, ]
      
      if (!nrow(selectedFiles)) {
        warning("No matching V1 files for year: ", year)
        next
      }
      
      fileTypes <- vapply(selectedFiles$name, identifyType, character(1))
      priority <- match(fileTypes, c("biomass", "height", "closure", "age", "sps"))
      selectedFiles <- selectedFiles[order(priority, na.last = TRUE), ]
      
      for (i in seq_len(nrow(selectedFiles))) {
        fileName <- selectedFiles$name[i]
        varType <- identifyType(fileName)
        
        if (is.na(varType)) {
          message("Skipping unrecognized V1 layer: ", fileName)
          next
        }
        
        fileURL <- paste0("https://drive.google.com/open?id=", selectedFiles$id[i])
        destination <- file.path(dPath, "unprocessed", year)
        
        # Height must remain native resolution until CV is calculated.
        if (varType == "height") {
          rasRaw <- prepInputs(
            url = fileURL,
            targetFile = fileName,
            destinationPath = destination,
            cropTo = studyAreaLarge,
            fun = "terra::rast"
          ) |> Cache()
          
          ras1kmLarge <- postProcess(rasRaw, projectTo = rasterToMatchLarge)
          rasCV <- terra::aggregate(rasRaw, fact = 33, fun = cv)
          rasCV1kmLarge <- postProcess(rasCV, projectTo = rasterToMatchLarge)
          
          ras1km <- finalize(ras1kmLarge)
          ras5x5 <- finalize(make5x5(ras1kmLarge))
          rasCV1km <- finalize(rasCV1kmLarge)
          rasCV5x5 <- finalize(make5x5(rasCV1kmLarge))
          
          name1km <- addResolution(fileName, "1km", year)
          name5x5 <- addResolution(fileName, "5x5", year)
          nameCV1km <- paste0("SCANFIheightcv_1km_", year)
          nameCV5x5 <- paste0("SCANFIheightcv_5x5_", year)
          
          names(ras1km) <- name1km
          names(ras5x5) <- name5x5
          names(rasCV1km) <- nameCV1km
          names(rasCV5x5) <- nameCV5x5
          
          result[["1km"]][[name1km]] <- ras1km
          result[["5x5"]][[name5x5]] <- ras5x5
          result[["1km"]][[nameCV1km]] <- rasCV1km
          result[["5x5"]][[nameCV5x5]] <- rasCV5x5
          
          rm(rasRaw, rasCV, ras1kmLarge, rasCV1kmLarge)
          gc()
          next
        }
        
        # Biomass, closure, age and V1 species can go directly to 1 km.
        ras1kmLarge <- prepInputs(
          url = fileURL,
          targetFile = fileName,
          destinationPath = destination,
          fun = "terra::rast",
          to = rasterToMatchLarge
        ) |> Cache()
        
        ras1km <- finalize(ras1kmLarge)
        ras5x5 <- finalize(make5x5(ras1kmLarge))
        
        name1km <- addResolution(fileName, "1km", year)
        name5x5 <- addResolution(fileName, "5x5", year)
        
        names(ras1km) <- name1km
        names(ras5x5) <- name5x5
        
        result[["1km"]][[name1km]] <- ras1km
        result[["5x5"]][[name5x5]] <- ras5x5
        
        rm(ras1kmLarge)
        gc()
      }
    }
  }
  
  # ===========================================================================
  # UNPROCESSED V2
  #
  # Flat SCANFI portal:
  # 1. Read filename links only.
  # 2. Select requested year/variables.
  # 3. Pass exact TIFF URLs to prepInputs().
  # ===========================================================================
  
  if (version == "v2") {
    
    # Portal source name -> BAM V1 predictor name.
    # prcD/prcC mappings remain the items to verify against BAM naming.
    v2ToBAM <- c(
      "balsamFir"       = "BalsamFir",
      "blackSpruce"     = "BlackSpruce",
      "broadleaf"       = "prcD",
      "douglasFir"      = "DouglasFir",
      "jackPine"        = "JackPine",
      "lodgepolePine"   = "LodgepolePine",
      "otherConiferous" = "prcC",
      "ponderosaPine"   = "PonderosaPine",
      "tamarack"        = "Tamarack",
      "whiteRedPine"    = "WhiteRedPine"
    )
    
    message("Reading SCANFI V2 portal listing...")
    
    portalHTML <- readLines(SCANFIurl, warn = FALSE)
    hrefs <- regmatches(
      portalHTML,
      gregexpr('href="[^"]+\\.tif"', portalHTML,
               perl = TRUE, ignore.case = TRUE)
    )
    fileNames <- unique(gsub('^href="|"$', "", unlist(hrefs)))
    
    if (!length(fileNames)) stop("No .tif files found at SCANFI V2 portal.")
    
    parseV2 <- function(f) {
      year <- extractYear(f)
      
      if (grepl("^SCANFI_att_biomass_", f, ignore.case = TRUE))
        return(c(year, "biomass", "biomass", "biomass"))
      if (grepl("^SCANFI_att_closure_", f, ignore.case = TRUE))
        return(c(year, "closure", "closure", "closure"))
      if (grepl("^SCANFI_att_height_", f, ignore.case = TRUE))
        return(c(year, "height", "height", "height"))
      if (grepl("^SCANFI_age_median_", f, ignore.case = TRUE))
        return(c(year, "age_median", "age_median", "age"))
      
      if (grepl("^SCANFI_spsCC_", f, ignore.case = TRUE)) {
        source <- sub("^SCANFI_spsCC_", "", f, ignore.case = TRUE)
        source <- sub(
          "_(?:19|20)[0-9]{2}_v[0-9]+_[0-9]{8}\\.tif$",
          "", source, ignore.case = TRUE, perl = TRUE
        )
        
        BAM <- if (source %in% names(v2ToBAM)) {
          unname(v2ToBAM[source])
        } else {
          source
        }
        
        return(c(year, source, BAM, "sps"))
      }
      
      c(year, NA, NA, NA)
    }
    
    parsed <- lapply(fileNames, parseV2)
    
    fileMeta <- data.frame(
      fileName = fileNames,
      year = vapply(parsed, `[[`, character(1), 1),
      source = vapply(parsed, `[[`, character(1), 2),
      BAM = vapply(parsed, `[[`, character(1), 3),
      type = vapply(parsed, `[[`, character(1), 4),
      stringsAsFactors = FALSE
    )
    
    fileMeta <- fileMeta[!is.na(fileMeta$type) & !is.na(fileMeta$year), ]
    
    if (!is.null(years)) {
      fileMeta <- fileMeta[fileMeta$year %in% as.character(years), ]
    }
    if (!nrow(fileMeta)) stop("No V2 files matched requested years.")
    
    availableMeta <- fileMeta
    
    # NULL means all recognized V2 variables.
    if (!is.null(variables)) {
      v <- tolower(variables)
      
      keep <- tolower(fileMeta$source) %in% v |
        tolower(fileMeta$BAM) %in% v
      
      if (any(v %in% c("sps", "species", "spscc"))) {
        keep <- keep | fileMeta$type == "sps"
      }
      
      fileMeta <- fileMeta[keep, ]
      if (!nrow(fileMeta)) stop("No V2 files matched requested variables.")
    }
    
    # Species need total closure even when closure was not explicitly requested.
    speciesYears <- unique(fileMeta$year[fileMeta$type == "sps"])
    
    if (length(speciesYears)) {
      closureMeta <- availableMeta[
        availableMeta$type == "closure" &
          availableMeta$year %in% speciesYears,
      ]
      fileMeta <- unique(rbind(fileMeta, closureMeta))
    }
    
    priority <- match(
      fileMeta$type,
      c("closure", "biomass", "height", "age", "sps")
    )
    fileMeta <- fileMeta[
      order(as.numeric(fileMeta$year), priority),
    ]
    
    for (year in unique(fileMeta$year)) {
      message("Processing SCANFI V2 year: ", year)
      
      yearFiles <- fileMeta[fileMeta$year == year, ]
      closureRaw <- NULL
      
      for (i in seq_len(nrow(yearFiles))) {
        fileName <- yearFiles$fileName[i]
        varType <- yearFiles$type[i]
        BAMname <- yearFiles$BAM[i]
        
        # Exact TIFF URL -- prepInputs never receives the flat folder itself.
        fileURL <- paste0(sub("/$", "", SCANFIurl), "/", fileName)
        destination <- file.path(dPath, "unprocessed", year)
        
        # Closure is kept native for V2 species normalization.
        if (varType == "closure") {
          closureRaw <- prepInputs(
            url = fileURL,
            #targetFile = fileName,
            destinationPath = destination,
            cropTo = studyAreaLarge,
            fun = "terra::rast"
          ) |> Cache()
          
          ras1kmLarge <- postProcess(
            closureRaw,
            projectTo = rasterToMatchLarge
          )
          
          ras1km <- finalize(ras1kmLarge)
          ras5x5 <- finalize(make5x5(ras1kmLarge))
          
          name1km <- paste0("SCANFIclosure_1km_", year)
          name5x5 <- paste0("SCANFIclosure_5x5_", year)
          
          names(ras1km) <- name1km
          names(ras5x5) <- name5x5
          
          result[["1km"]][[name1km]] <- ras1km
          result[["5x5"]][[name5x5]] <- ras5x5
          
          rm(ras1kmLarge)
          gc()
          next
        }
        
        # Height must remain native until BAM-style CV is calculated.
        if (varType == "height") {
          rasRaw <- prepInputs(
            url = fileURL,
            #targetFile = fileName,
            destinationPath = destination,
            cropTo = studyAreaLarge,
            fun = "terra::rast"
          ) |> Cache()
          
          ras1kmLarge <- postProcess(rasRaw, projectTo = rasterToMatchLarge)
          rasCV <- terra::aggregate(rasRaw, fact = 33, fun = cv)
          rasCV1kmLarge <- postProcess(rasCV, projectTo = rasterToMatchLarge)
          
          ras1km <- finalize(ras1kmLarge)
          ras5x5 <- finalize(make5x5(ras1kmLarge))
          rasCV1km <- finalize(rasCV1kmLarge)
          rasCV5x5 <- finalize(make5x5(rasCV1kmLarge))
          
          name1km <- paste0("SCANFIheight_1km_", year)
          name5x5 <- paste0("SCANFIheight_5x5_", year)
          nameCV1km <- paste0("SCANFIheightcv_1km_", year)
          nameCV5x5 <- paste0("SCANFIheightcv_5x5_", year)
          
          names(ras1km) <- name1km
          names(ras5x5) <- name5x5
          names(rasCV1km) <- nameCV1km
          names(rasCV5x5) <- nameCV5x5
          
          result[["1km"]][[name1km]] <- ras1km
          result[["5x5"]][[name5x5]] <- ras5x5
          result[["1km"]][[nameCV1km]] <- rasCV1km
          result[["5x5"]][[nameCV5x5]] <- rasCV5x5
          
          rm(rasRaw, rasCV, ras1kmLarge, rasCV1kmLarge)
          gc()
          next
        }
        
        # V2 species crown closure -> V1 canopy-composition percentage.
        if (varType == "sps") {
          if (is.null(closureRaw)) {
            stop("V2 total closure required before: ", fileName)
          }
          
          speciesRaw <- prepInputs(
            url = fileURL,
            #targetFile = fileName,
            destinationPath = destination,
            cropTo = studyAreaLarge,
            fun = "terra::rast"
          ) |> Cache()
          
          closureForSpecies <- closureRaw
          
          if (!terra::same.crs(speciesRaw, closureForSpecies)) {
            closureForSpecies <- terra::project(
              closureForSpecies, speciesRaw, method = "bilinear"
            )
          } else if (!terra::compareGeom(
            speciesRaw, closureForSpecies,
            stopOnError = FALSE
          )) {
            closureForSpecies <- terra::resample(
              closureForSpecies, speciesRaw, method = "bilinear"
            )
          }
          
          speciesV1 <- terra::ifel(
            closureForSpecies > 0,
            (speciesRaw / closureForSpecies) * 100,
            0
          )
          
          ras1kmLarge <- postProcess(
            speciesV1,
            projectTo = rasterToMatchLarge
          )
          
          ras1km <- finalize(ras1kmLarge)
          ras5x5 <- finalize(make5x5(ras1kmLarge))
          
          name1km <- paste0("SCANFI", BAMname, "_1km_", year)
          name5x5 <- paste0("SCANFI", BAMname, "_5x5_", year)
          
          names(ras1km) <- name1km
          names(ras5x5) <- name5x5
          
          result[["1km"]][[name1km]] <- ras1km
          result[["5x5"]][[name5x5]] <- ras5x5
          
          rm(speciesRaw, speciesV1, closureForSpecies, ras1kmLarge)
          gc()
          next
        }
        
        # Biomass / age require no native-resolution transformation.
        ras1kmLarge <- prepInputs(
          url = fileURL,
          targetFile = fileName,
          destinationPath = destination,
          fun = "terra::rast",
          to = rasterToMatchLarge
        ) |> Cache()
        
        ras1km <- finalize(ras1kmLarge)
        ras5x5 <- finalize(make5x5(ras1kmLarge))
        
        name1km <- paste0("SCANFI", BAMname, "_1km_", year)
        name5x5 <- paste0("SCANFI", BAMname, "_5x5_", year)
        
        names(ras1km) <- name1km
        names(ras5x5) <- name5x5
        
        result[["1km"]][[name1km]] <- ras1km
        result[["5x5"]][[name5x5]] <- ras5x5
        
        rm(ras1kmLarge)
        gc()
      }
      
      rm(closureRaw)
      gc()
    }
  }
  
  if (!length(result[["1km"]]) || !length(result[["5x5"]])) {
    stop("No SCANFI products were generated.")
  }
  
  list(
    "1km" = terra::rast(result[["1km"]]),
    "5x5" = terra::rast(result[["5x5"]])
  )
}


# scanfiTrial <- processSCANFI(
#   SCANFIurl = SCANFIurlV0, #"https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/",
#   studyAreaRas = studyAreaRas,
#   studyAreaLarge = studyAreaLarge,
#   rasterToMatchLarge = rasterToMatchLarge,
#   processed = TRUE,
#   version = "v1",
#   years = 2020,
#   variables = c("BalsamFir","BlackSpruce","prcD","closure")
# )
