#' Crop Selector
#'
#' @description Propose crops to bring forward in analysis
#'
#' @param excel_data List object returned from `load_data_from_excel()`.
#' @param start_year Integer. Start year of analysis period (year 1 of 5-year window)
#' @param stop_year Integer. End year of analysis (year 5 5-year window)
#' @param target_years Integer. Target years per crop per risk and ag type. Ideally no crop is included if it has less than 3 years of data.
#' @param target_coverage_pct Integer. Target percent coverage to include.
#' @param max_select Integer. Maximum number of crops to select per risk area and ag type.
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
           target_coverage_pct = 1,
           max_select = NA) {
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
                     ay_adj$YEAR <= stop_year), ]


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
      ifelse(
        ay_sum$n_years >= target_years &
          ay_sum$pct_coverage >= target_coverage_pct,
        1,
        ay_sum$selected
      )

    ay_sum$proposed <- ay_sum$selected


    # Check sums
    if (sum(ay_sum$selected) == 0) {
      warning("Criteria too strict. No crops selected in crop_selector...")
    }


    # Determine if a maximum selection per risk area is set
    if (length(max_select) > 0) {
      if (!(is.na(max_select))) {

        max_select <- ifelse(max_select < 1, 999, max_select)

        # Select the top n (max_select) crops rows for each unique
        # combination of `RISK AREA`, `CROP NAME`, `AG INPUT`
        # sort by pct_coverage and then by n_years select top n
        tmp <- ay_sum %>%
          group_by(`RISK AREA`, `AG INPUT`) %>%
          arrange(desc(pct_coverage), desc(n_years)) %>%
          slice_head(n = max_select) %>%
          ungroup()

        # Update selected from 1 to 0 if crop in ay_sum is not in tmp
        ay_sum$selected <- ifelse(
          paste0(ay_sum$`RISK AREA`, ay_sum$`AG INPUT`, ay_sum$`CROP NAME`) %in% paste0(tmp$`RISK AREA`, tmp$`AG INPUT`, tmp$`CROP NAME`),
          ay_sum$selected,
          0
        )

        # Make sure proposed column is updated too
        ay_sum$proposed <- ay_sum$selected
      }
    }




    return(ay_sum)



  }
