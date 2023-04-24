#' Crop Selector
#'
#' @description Propose crops to bring forward in analysis
#'
#' @param excel_data List object returned from `load_data_from_excel()`.
#' @param start_year Integer. Start year of analysis period (year 1 of 5-year window)
#' @param stop_year Integer. End year of analysis (year 5 5-year window)
#' @param target_years Integer. Target years per crop per risk and ag type. Ideally no crop is included if it has less than 3 years of data.
#' @param target_coverage_pct Integer. Target percent coverage to include
#'
#' @details This function takes in the cleaned Excel
#' data loaded through `load_data_from_excel()` to
#' produce a table of candidate crops per risk area and Ag Type dry/wet.
#' Select crops based on n-years of data (ideally should be greater than
#' or equal to 3) and total risk area composition
#' (should not be less than 1%). Including crops with limited coverage
#' will reduce data quality and produce poor estimates.
#'
#' @importFrom dplyr group_by summarize top_n n
#'
#' @returns A data frame of risk area crop and Ag type with candidate crops
#
crop_selector <-
  function(excel_data,
           start_year = NA,
           stop_year = NA,
           target_years = 3,
           target_coverage_pct = 1) {

    # Loop through unique combinations of Risk Zone and Ag Inputs
    ay <- excel_data$`Average Yields`

    # Define start_year and stop_year (if not set)
    if (is.na(stop_year)) {
      pstop <- max(ay$YEAR, na.rm = TRUE)
      stop_year <- pstop
    }

    # Default to a 5 year window - if possible
    if (is.na(start_year)) {
      pstart <- min(ay$YEAR, na.rm = TRUE)
      start_year <- pstart
      # Default to 5 year period
      mdefault <- stop_year - 4
      if (mdefault > start_year) {
        start_year <- mdefault
      }
    }


    # Loop through unique combinations of Risk Zone and Ag Inputs
    ay_adj <- ay[!(is.na(ay$ACRES)), ]
    ay_adj <- ay_adj[ay_adj$ACRES > 0, ]


    # Apply filters for years
    ay_adj <-
      ay_adj[which(ay_adj$YEAR >= start_year &
                     ay_adj$YEAR <= stop_year),]


    # Summarize totals by year and risk area
    ay_adj$ra_year <- paste0(ay_adj$`RISK AREA`, "_", ay_adj$YEAR)

    # Summarize values by risk area and year
    ay_sum_ra_yr <-
      ay_adj %>% dplyr::group_by(ra_year) %>%
      dplyr::summarize(total_coverage = sum(ACRES, na.rm = TRUE))

    # Merge to group
    ay_adj2 <- merge(
      ay_adj,
      ay_sum_ra_yr,
      by.x = "ra_year",
      by.y = "ra_year",
      all.x = TRUE,
      all.y = FALSE
    )


    # Add column for percent coverage per year
    ay_adj2$pct_coverage_yr <-
      ay_adj2$ACRES / ay_adj2$total_coverage
    ay_adj2$pct_coverage_yr <-
      round(ay_adj2$pct_coverage_yr * 100, 3)


    # Summarize by group - take the mean of percent coverage by year
    ay_sum <-
      ay_adj2 %>% dplyr::group_by(`RISK AREA`, `CROP NAME`, `AG INPUT`) %>%
      dplyr::summarize(
        n_years = dplyr::n(),
        crop_coverage = sum(ACRES, na.rm = TRUE),
        pct_coverage = mean(pct_coverage_yr, na.rm = TRUE)
      )

    ay_sum$pct_coverage <- round(ay_sum$pct_coverage, 3)

    # Check sums
    if (sum(ay_adj$ACRES, na.rm = TRUE) != sum(ay_sum$crop_coverage, na.rm = TRUE)) {
      warning("ACRES summary does not match group summary in crop_selector...")
    }


    # Identify selected
    ay_sum$selected <- FALSE
    ay_sum$selected <-
      ifelse(ay_sum$n_years >= target_years &
               ay_sum$pct_coverage >= 1,
             1,
             ay_sum$selected)

    ay_sum$proposed <- ay_sum$selected


    # Check sums
    if (sum(ay_sum$selected) == 0) {
      warning("Criteria too strict. No crops selected in crop_selector...")
    }

    return(ay_sum)



  }
