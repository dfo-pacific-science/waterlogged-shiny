#' Parcel Data GIS
#'
#' @description Populates FDL property lease data
#'
#' @param excel_data List object returned from `load_data_from_excel()`.
#' @param ltay_tables List object returned from `build_ltay_tables()`.
#'
#'
#' @details This function takes in the cleaned Excel
#' data loaded through `load_data_from_excel()` and the LTAY long-term average
#' yield reference tables from `build_ltay_tables()` to populate attributes onto
#' each of the FDL parcels properties.
#'
#' @returns A data frame table of the GIS table attributes for each FDL parcel
#
parcel_data_gis <- function(excel_data = NA,
                            ltay_tables = NA) {

  # ltay_tables <- ltay; excel_data <- dat; method <- "all"

  # Gather GIS data
  gis <- excel_data$`FDL GIS`

  # Merge Risk Area data onto GIS data
  # for each LSRS rate
  lsrs <- ltay_tables$risk_area_lsrs



  # ---------------------------------------
  # Add on LSRS rates for Dry fraction
  # ---------------------------------------

  target_cols <- colnames(lsrs)[grepl("dry_", colnames(lsrs))]
  lsrs_sub <- lsrs[, c("RISK AREA", target_cols)]

  # Drop to avoid confusion
  lsrs_sub$dry_gross_rev <- NULL
  # Make name pretty
  colnames(lsrs_sub) <- gsub("dry_", "Rate Dry ", colnames(lsrs_sub))
  # Round to currency
  lsrs_sub[, 2:ncol(lsrs_sub)] <- round(lsrs_sub[, 2:ncol(lsrs_sub)], 2)


  gis2 <-
    merge(
      gis,
      lsrs_sub,
      by.x = "AFSC Crop Risk Zone",
      by.y = "RISK AREA",
      all.x = TRUE,
      all.y = FALSE
    )


  lsrs_ids <- excel_data$LSRS$`Reference ID`
  lsrs_ids <- unique(lsrs_ids)

  # Calculate values from rates and areas

  for (l in seq_along(lsrs_ids)) {

    this_lsrs <- lsrs_ids[l]
    t_area <- as.numeric(gis2[, paste0(this_lsrs, " Dry Acres")])
    t_rate <- as.numeric(gis2[, paste0("Rate Dry ", this_lsrs)])
    t_value <- t_area * t_rate

    # Keep currency significant figures
    t_value <- round(t_value, 2)

    # Add data and column
    gis2$newdat <- t_value
    colnames(gis2)[ncol(gis2)] <-
      paste0(this_lsrs, " Dry Value")

  }


  # ---------------------------------------
  # Add on LSRS rates for Irr fraction
  # ---------------------------------------

  target_cols <- colnames(lsrs)[grepl("irr_", colnames(lsrs))]
  lsrs_sub <- lsrs[, c("RISK AREA", target_cols)]

  # Drop to avoid confusion
  lsrs_sub$dry_gross_rev <- NULL
  # Make name pretty
  colnames(lsrs_sub) <- gsub("irr_", "Rate Irr ", colnames(lsrs_sub))
  # Round to currency
  lsrs_sub[, 2:ncol(lsrs_sub)] <- round(lsrs_sub[, 2:ncol(lsrs_sub)], 2)



  gis3 <-
    merge(
      gis2,
      lsrs_sub,
      by.x = "AFSC Crop Risk Zone",
      by.y = "RISK AREA",
      all.x = TRUE,
      all.y = FALSE
    )


  lsrs_ids <- excel_data$LSRS$`Reference ID`
  lsrs_ids <- unique(lsrs_ids)

  # Calculate values from rates and areas

  for (l in seq_along(lsrs_ids)) {

    this_lsrs <- lsrs_ids[l]
    t_area <- as.numeric(gis3[, paste0(this_lsrs, " Irr Acres")])
    t_rate <- as.numeric(gis3[, paste0("Rate Irr ", this_lsrs)])
    t_value <- t_area * t_rate

    # Keep currency significant figures
    t_value <- round(t_value, 2)

    # Add data and column
    gis3$newdat <- t_value
    colnames(gis3)[ncol(gis3)] <-
      paste0(this_lsrs, " Irr Value")

  }






  # Get total value for FDL parcel
  target_cols <- colnames(gis3)[grepl("Dry Value", colnames(gis3))]
  gis3$`Total Value Dry` <- rowSums(gis3[, target_cols])

  target_cols <- colnames(gis3)[grepl("Irr Value", colnames(gis3))]
  gis3$`Total Value Irr` <- rowSums(gis3[, target_cols])


  # Add total value as sum of dry and irr
  gis3$`Total Value` <- gis3$`Total Value Dry` + gis3$`Total Value Irr`

  gis3$`Value Per Acre` <-
    gis3$`Total Value` / (gis3$`Total Acres (GIS FDL Boundary)` - gis3$`Not Cultivated Acres`)


  # ---------------------------------------
  # Clean up column naming
  # ---------------------------------------

  gisn <- gis3

  gisn$`Value Per Acre`    <- round(gisn$`Value Per Acre`, 2)
  gisn$`Total Value`       <- round(gisn$`Total Value`, 2)
  gisn$`Total Value Irr`   <- round(gisn$`Total Value Irr`, 2)
  gisn$`Total Value Dry`   <- round(gisn$`Total Value Dry`, 2)

  parcel_data <- gisn

  return(parcel_data)



}
