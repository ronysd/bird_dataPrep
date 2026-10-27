# buildRasterStackAllYears <- function(outSim, lag_df, vars_available, years = seq(1985, 2020, 5)) {
# 
#   stack_list <- list()
#   covariates <- unique(vars_available$base)
#   year.out <- data.frame()
# 
#   for (cov in covariates) {
#     vars_cov <- vars_available |> filter(base == cov, !is.na(year))
# 
#     if (nrow(vars_cov) == 0) {
#       year.out <- bind_rows(year.out, data.frame(base = cov, year = NA, predyear = years))
#     } else {
#       is_lagged <- cov %in% lag_df$Label
#       years_cov <- unique(vars_cov$year)
#       if (is_lagged) years_cov <- years_cov + 1
# 
#       dt <- data.table(year = years_cov, val = years_cov)
#       setattr(dt, "sorted", "year")
#       setkey(dt, year)
#       match_years <- dt[J(years), roll = "nearest"]$val
#       if (is_lagged) match_years <- match_years - 1
# 
#       year.out <- bind_rows(year.out, data.frame(base = cov, year = match_years, predyear = years))
#     }
#   }
# 
#   matched_rows <- left_join(year.out, vars_available, by = c("base", "year")) |> filter(!is.na(full))
# 
#   for (prediction_year in years) {
#     message("Building raster stack for year: ", prediction_year)
#     vars_year <- matched_rows |> filter(predyear == prediction_year)
#     loaded_rasters <- list()
# 
#     for (i in seq_len(nrow(vars_year))) {
#       row <- vars_year[i, ]
#       message(
#         "Trying: ", row$base,
#         if (!is.na(row$year)) paste0(" (year = ", row$year, ")"),
#         " from ", row$source, " using layer: ", row$full
#       )
#       lyr <- tryCatch(
#         {
#           r <- outSim
#           for (key in strsplit(row$source, "\\$")[[1]][-1]) {
#             r <- r[[key]]
#           }
#           r <- r[[row$full]]
#           names(r) <- row$base
#           r
#         },
#         error = function(e) {
#           message("Failed to extract ", row$base, " → ", e$message)
#           NULL
#         }
#       )
#       if (!is.null(lyr)) loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
#     }
# 
#     if (length(loaded_rasters) == 0) next
# 
#     template_ras <- loaded_rasters[[1]]
#     method_ras <- rast(template_ras)
#     values(method_ras) <- "PC"
#     names(method_ras) <- "method"
#     year_ras <- rast(template_ras)
#     values(year_ras) <- prediction_year
#     names(year_ras) <- "year"
# 
#     stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
#     stack_list[[as.character(prediction_year)]] <- stack_out
# 
#     message("stack built with ", nlyr(stack_out), " layers for ", prediction_year)
#   }
#   message("Finished building stacks for ", length(stack_list), " years.")
#   return(stack_list)
# }



buildRasterStackAllYears <- function(outSim, lag_df, vars_available,
                                     predictStartYear = 1985,
                                     predictEndYear = 2020,
                                     predictInterval = 5) {
  
  stack_list <- list()
  covariates <- unique(vars_available$base)
  year.out <- data.frame()
  
  # build prediction years dynamically
  years <- seq(predictStartYear, predictEndYear, predictInterval)
  
  for (cov in covariates) {
    vars_cov <- vars_available |> dplyr::filter(base == cov, !is.na(year))
    
    if (nrow(vars_cov) == 0) {
      year.out <- dplyr::bind_rows(year.out, data.frame(base = cov, year = NA, predyear = years))
    } else {
      

      lag_row <- lag_df |> dplyr::filter(Label == cov)
      if (nrow(lag_row) > 0) {
        year_shift <- lag_row$YearMatch[1]
        message("YearMatch found for ", cov, ": shift = ", year_shift)
      } else if (grepl("Dormancy", cov, ignore.case = TRUE) ||
                 grepl("Greenup", cov, ignore.case = TRUE)) {
        year_shift <- 0
        message("Fallback lag rule for ", cov, ": shift = ", year_shift)
      } else {
        year_shift <- 0
      }
      #browser()
      years_cov <- unique(vars_cov$year)
      if (year_shift < 0) {
        years_cov_adj <- years_cov - year_shift  # reverse sign for negative lags
      } else {
        years_cov_adj <- years_cov + year_shift
      }
      
      dt <- data.table::data.table(year = years_cov_adj, val = years_cov) #years_cov_adj
      data.table::setkey(dt, year)
      
      # Strict lag alignment for non-zero YearMatch, nearest fallback otherwise
      if (!is.na(year_shift) && year_shift != 0) {
        # Strict alignment: use shifted year only
        match_years <- dt[data.table::J(years)]$val
        # If lagged year missing, allow minimal fallback
        missing_idx <- which(is.na(match_years))
        if (length(missing_idx) > 0) {
          message("Missing lagged year(s) for ", cov, 
                  " → using nearest available raster for ", length(missing_idx), " year(s).")
          match_years[missing_idx] <- dt[data.table::J(years[missing_idx]), roll = "nearest"]$val
        }
      } else {
        # No lag → allow nearest neighbour match
        match_years <- dt[data.table::J(years), roll = "nearest"]$val
      }
      # --- Diagnostic reporting block ---
      if (!is.na(year_shift) && year_shift != 0) {
        match_years <- dt[data.table::J(years), roll = "nearest"]$val
        
        #  reporting only for lagged variables
        msg_tbl <- data.frame(
          PredictionYear = years,
          YearMatch = year_shift,
          RasterYearUsed = match_years
        )
        message("\n[Lagged variable] ", cov, " (YearMatch = ", year_shift, ")")
        print(msg_tbl)
      } else {
        # Non-lagged → nearest available year
        match_years <- dt[data.table::J(years), roll = "nearest"]$val
      }
      year.out <- dplyr::bind_rows(
        year.out,
        data.frame(base = cov, year = match_years, predyear = years)
      )
    }
  }
  #browser()
  # Join matched year info with available variable metadata
  matched_rows <- dplyr::left_join(year.out, vars_available, by = c("base", "year")) |>
    dplyr::filter(!is.na(full))
  
##build raster stack for the years
  for (prediction_year in years) {
    message("Building raster stack for year: ", prediction_year)
    vars_year <- matched_rows |> dplyr::filter(predyear == prediction_year)
    loaded_rasters <- list()
    
    for (i in seq_len(nrow(vars_year))) {
      row <- vars_year[i, ]
      message(
        "Trying: ", row$base,
        if (!is.na(row$year)) paste0(" (year = ", row$year, ")"),
        " from ", row$source, " using layer: ", row$full
      )
      
      lyr <- tryCatch(
        {
          r <- outSim
          for (key in strsplit(row$source, "\\$")[[1]][-1]) {
            r <- r[[key]]
          }
          r <- r[[row$full]]
          names(r) <- row$base
          r
        },
        error = function(e) {
          message("Failed to extract ", row$base, " → ", e$message)
          NULL
        }
      )
      
      if (!is.null(lyr)) loaded_rasters[[length(loaded_rasters) + 1]] <- lyr
    }
    
    if (length(loaded_rasters) == 0) next
    
    # Metadata layers
    template_ras <- loaded_rasters[[1]]
    method_ras <- terra::rast(template_ras)
    terra::values(method_ras) <- "PC"
    names(method_ras) <- "method"
    
    year_ras <- terra::rast(template_ras)
    terra::values(year_ras) <- prediction_year
    names(year_ras) <- "year"
    
    stack_out <- c(method_ras, year_ras, do.call(c, loaded_rasters))
    stack_list[[as.character(prediction_year)]] <- stack_out
    
    message("Stack built with ", terra::nlyr(stack_out), " layers for ", prediction_year)
  }
  
  message("Finished building stacks for ", length(stack_list), " years.")
  return(stack_list)
}




