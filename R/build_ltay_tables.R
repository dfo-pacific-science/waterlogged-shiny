#' Build LTAY Tables
#'
#' @description Builds the LTAY Tables
#'
#' @param excel_data List object returned from `load_data_from_excel()`.
#' @param crops_selected Dataframe object returned from `crop_selector()` listing selected crops.
#' @param start_year Integer. Start year of analysis.
#' @param stop_year Integer. Stop year of analysis.
#'
#'
#' @details This function takes in the cleaned Excel
#' data loaded through `load_data_from_excel()` and the selected crops chosen for analysis from `crop_selector()` to produce a list object containing the LTAY (long term average yield tables).
#' @importFrom dplyr group_by summarise top_n n
#'
#' @returns A list object of the LTAY tables.
#
build_ltay_tables <- function(excel_data,
                              crops_selected = NA,
                              start_year = NA,
                              stop_year = NA) {

  # crops_selected <- cs; excel_data <- dat

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

  target_years <- seq(from = start_year, to = stop_year, by = 1)

  # Filter to selected crops
  cs <- crops_selected[which(crops_selected$selected == 1),]
  cs$id <-
    paste0(cs$`RISK AREA`, "__", cs$`CROP NAME`, "__", cs$`AG INPUT`)

  # Create ID
  ay$id <-
    paste0(ay$`RISK AREA`, "__", ay$`CROP NAME`, "__", ay$`AG INPUT`)

  # Filter ay by selected crops
  ay_filter <- ay[which(ay$id %in% unique(cs$id)),]



  # ----------------------------------------------
  # Build main LTAY table
  # ----------------------------------------------

  ltay_ids <- unique(cs$id)
  data_object <- list()
  counter <- 1

  for (i in seq_along(ltay_ids)) {

    this_id <- ltay_ids[i]

    this_set <- ay_filter[ay_filter$id == this_id,]

    # Header cols
    sub1 <- this_set[, c("RISK AREA", "CROP NAME", "AG INPUT")]
    sub1 <- sub1[!(duplicated(sub1)),]


    # Loop through years to add annual yields
    for (y in seq_along(target_years)) {

      this_year <- target_years[y]

      # Calculate average yields
      ty_ay <-
        this_set$`AVERAGE YIELD (kg/acre)`[this_set$YEAR == this_year]
      ty_ay <- mean(ty_ay, na.rm = TRUE) # if multiple
      ty_ay <- ifelse(is.na(ty_ay), NA, ty_ay)

      sub1$new_col <- ty_ay
      colnames(sub1)[ncol(sub1)] <- paste0("ay_", this_year)
    }


    # Loop through years to add acres grown
    for (y in seq_along(target_years)) {

      this_year <- target_years[y]

      # Calculate acres grown
      ty_ag <- this_set$ACRES[this_set$YEAR == this_year]
      ty_ag <- sum(ty_ag, na.rm = TRUE) # if multiple

      # Set to NA if no data for year
      if(!(this_year %in% this_set$YEAR)) {
        ty_ag <- NA
      }

      sub1$new_col <- ty_ag
      colnames(sub1)[ncol(sub1)] <- paste0("ag_", this_year)
    }

    # Calculate averages
    average_yield_kg_ac <-
      rowMeans(sub1[, grepl("ay_", colnames(sub1))], na.rm = TRUE)

    sub1$average_yield_kg_ac <- average_yield_kg_ac

    # Calculate averages
    average_acres_grown <-
      rowMeans(sub1[, grepl("ag_", colnames(sub1))], na.rm = TRUE)

    sub1$average_acres_grown <- average_acres_grown

    data_object[[counter]] <- sub1
    counter <- 1 + counter

  }

  ltay_tab1 <- do.call("rbind", data_object)



  # ----------------------------------------------
  # Calculate Totals per Risk Zone and Ag Type
  # ----------------------------------------------

  ltay_tab1$id <-
    paste0(ltay_tab1$`RISK AREA`, "__", ltay_tab1$`AG INPUT`)

  # Create unique list of irrigation type and zone
  # Summarize by sum of average acres grown per commodity type

  ltay_tab1b <-
    ltay_tab1 %>% dplyr::group_by(id) %>% dplyr::summarise(acres_per_risk = sum(average_acres_grown, na.rm = TRUE))


  # Append to existing table
  ltay_tab2 <-
    merge(
      ltay_tab1,
      ltay_tab1b,
      by.x = "id",
      by.y = "id",
      all.x = TRUE,
      all.y = FALSE
    )

  # Drop temp ID col
  ltay_tab2$id <- NULL

  ltay_tab2$average_crop_compostion <-
    round(ltay_tab2$average_acres_grown / ltay_tab2$acres_per_risk, 3)



  # ----------------------------------------------
  # Add on price fractions
  # ----------------------------------------------

  pf_df <- excel_data$`Price Fractions`
  pf_df <- pf_df[, c("CROP NAME", "PRICE ($/kg)", "YEAR")]
  colnames(pf_df) <- c("CROP NAME", "price", "year")

  # If duplicate prices take the most recent year
  pf_df_2 <- pf_df %>% dplyr::group_by(`CROP NAME`) %>% dplyr::top_n(1, year)
  pf_df_2$year <- NULL
  pf_df_2 <- pf_df_2[!(duplicated(pf_df_2)), ]

  ltay_tab3 <-
    merge(
      ltay_tab2,
      pf_df_2,
      by.x = "CROP NAME",
      by.y = "CROP NAME",
      all.x = TRUE,
      all.y = FALSE
    )


  # If spelling is not consistent between worksheets for crop name
  # price data will not match and price data will be blank
  # determine how to infill missing price data.

  # Warn message to user about bad match
  missing_price <- sum(is.na(ltay_tab3$price))

  if (missing_price > 0) {
    bad_crop <- ltay_tab3$`CROP NAME`[which(is.na(ltay_tab3$price))]
    bad_crop <- unique(bad_crop)
    warning(
      paste0(
        "Bad match between yeilds and price fractions. Missing data for crops: ",
        paste(bad_crop, collapse = ", ")
      )
    )
  }


  # ------------------------------------------------------
  # Run the [NCRF] New Cultivation Rent Formula
  # ------------------------------------------------------

  # (Ci*Pi*Yi/A)*S
  # Ci = all insured commodity in risk zone
  # Pi = commodity price per kg forecast
  # Yi = average annual yield
  #    (5-year moving window). per risk zone per commodity (kg/ac)
  # A = total acres in risk zone
  # S = soil coefficient (0.18 - 0.12)***
  # (ac. * $/kg. * kg./ac.) / ac -- $/ac

  # Gross Revenue Per Commodity Per Acre ($/ac)
  ltay_tab3$grpcpa <-
    (ltay_tab3$average_acres_grown *
       ltay_tab3$average_yield_kg_ac *
       ltay_tab3$price)  / ltay_tab3$acres_per_risk




  # ------------------------------------------------------
  # LTAY Summaries by Risk Zone
  # ------------------------------------------------------

  risk_areas <- sort(unique(ltay_tab3$`RISK AREA`))

  # unique(ltay_tab3$`AG INPUT`)

  ra_gr <- ltay_tab3 %>% dplyr::group_by(`RISK AREA`) %>%
    dplyr::summarise(
      dry_gross_rev = sum(grpcpa[`AG INPUT` == "Dry"], na.rm = TRUE),
      irr_gross_rev = sum(grpcpa[`AG INPUT` == "Irr"], na.rm = TRUE)
      # all_gross_rev = sum(grpcpa, na.rm = TRUE) # NOT USED
    )

  # Summarize data by soil class

  lsrs <- excel_data$LSRS
  lsrs_ids <- unique(lsrs$`Reference ID`)


  # Dry gross rev
  dry_gr <- ra_gr$dry_gross_rev %*% t(lsrs$`Percentage (%)`/100)
  dry_gr <- as.data.frame(dry_gr)
  colnames(dry_gr) <- paste0("dry_", lsrs$`Reference ID`)

  # Irr gross rev
  irr_gr <- ra_gr$irr_gross_rev %*% t(lsrs$`Percentage (%)`/100)
  irr_gr <- as.data.frame(irr_gr)
  colnames(irr_gr) <- paste0("irr_", lsrs$`Reference ID`)

  # ALL gross rev
  #all_gr <- ra_gr$all_gross_rev %*% t(lsrs$`Percentage (%)`/100)
  #all_gr <- as.data.frame(all_gr)
  #colnames(all_gr) <- paste0("all_", lsrs$`Reference ID`)


  ra_gr2 <- cbind(ra_gr, dry_gr)
  ra_gr3 <- cbind(ra_gr2, irr_gr)
  #ra_gr4 <- cbind(ra_gr3, all_gr)


  # ------------------------------------------------------
  # Build return object
  # ------------------------------------------------------

  ret_obj <- list()

  ret_obj$ltay <- ltay_tab3

  ret_obj$risk_area_lsrs <- ra_gr3

  return(ret_obj)


}
