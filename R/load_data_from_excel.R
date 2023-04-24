#' Load FDL Inputs From Excel
#'
#' @description Loads all input data from Excel workbooks.
#'
#' @param filename String. File name of Excel workbook to load.
#'
#' @details This function takes the `FDL Rental Rates Inputs.xlsx` file for
#' a given year and returns a list object with the imported data. Various
#' error checks are performed to ensure data formats are valid
#'
#' @returns A list object of all data from Excel workbook.
#
load_data_from_excel <- function(filename) {

  # filename <-
  #   "./inst/extdata/02 - sample data/FDL Rental Rates Inputs - 2019.xlsx"


  # Sheet names to read
  sheet_names <- c("Average Yields",
                   "Price Fractions",
                   "FDL GIS",
                   "LSRS")

  # Ensure workbook has all the necessary worksheets
  has_sheets <- readxl::excel_sheets(path = filename)

  mdiff <- setdiff(sheet_names, has_sheets)

  if (length(mdiff) > 0) {
    err_msg <-
      paste0("Missing Excel worksheets: ", paste(mdiff, collapse = ", "))
    return(err_msg)
  }


  # Read all worksheets to list object
  list_all <-
    lapply(sheet_names, function(x)
      readxl::read_excel(path = filename, sheet = x))

  names(list_all) <- sheet_names



  # ==========================================
  # Clean: Average Yields Worksheet
  # ==========================================

  # Check columns
  exp_cols <-
    c("YEAR",
      "RISK AREA",
      "CROP NAME",
      "AG INPUT",
      "AVERAGE YIELD (kg/acre)",
      "ACRES")

  df <- list_all$`Average Yields`
  has_cols <- colnames(df)

  mdiff <- setdiff(exp_cols, has_cols)

  if (length(mdiff) > 0) {
    err_msg <-
      paste0("Missing columns in Excel worksheet Average Yields: ",
             paste(mdiff, collapse = ", "))
    return(err_msg)
  }

  # Filter to only include expected columns
  df <- df[, exp_cols]

  # Drop rows with all NA values
  df <- df[!(rowSums(is.na(df)) == ncol(df)),]

  # Drop rows that are exact duplicates
  df <- df[!(duplicated(df)),]

  # Check and fix column formats
  df$YEAR <- as.numeric(as.character(df$YEAR))
  df$`RISK AREA` <- as.character(df$`RISK AREA`)
  df$`CROP NAME` <- as.character(df$`CROP NAME`)
  df$`AG INPUT` <- as.character(df$`AG INPUT`)
  df$`AVERAGE YIELD (kg/acre)` <-
    as.numeric(as.character(df$`AVERAGE YIELD (kg/acre)`))
  df$ACRES <- as.numeric(as.character(df$ACRES))


  # AG INPUT can only be Dry, Irr or blank
  mcheck <- unique(df$`AG INPUT`)
  mdiff <- setdiff(mcheck, c("Dry", "Irr", NA))
  if (length(mdiff) > 0) {
    err_msg <-
      paste0(
        "AG INPUT column in the Average Yields
        worksheet be Dry, Irr or blank (if not
        applicable). Unable to process value(s): ",
        paste(mdiff, collapse = ", ")
      )
    return(err_msg)
  }

  # Duplicate representations of the same crop
  check_dup <-
    paste0(df$YEAR,
           " ",
           df$`RISK AREA`,
           " ",
           df$`CROP NAME`,
           " ",
           df$`AG INPUT`)
  if (any(duplicated(check_dup))) {
    dups <- check_dup[duplicated(check_dup)]
    dups <- dups[1:5]
    dups <- dups[!(is.na(dups))]
    err_msg <-
      paste0(
        "Duplicate values exist in Average Yields
        worksheet. Filter for the following values in
        Excel and remove duplicates: ",
        paste(dups, collapse = ", ")
      )
    return(err_msg)
  }

  # Update list object with clean data
  list_all$`Average Yields` <- df






  # ==========================================
  # Clean: Price Fraction
  # ==========================================

  # Check columns
  exp_cols <-
    c("YEAR",
      "CROP NAME",
      "AFSC Commodity Forecast (Reported As)",
      "GRADE",
      "PRICE ($/kg)")
  df <- list_all$`Price Fractions`
  has_cols <- colnames(df)

  mdiff <- setdiff(exp_cols, has_cols)
  if (length(mdiff) > 0) {
    err_msg <-
      paste0("Missing columns Excel worksheet Price Fraction: ",
             paste(mdiff, collapse = ", "))
    return(err_msg)
  }

  # Filter to only include expected columns
  df <- df[, exp_cols]

  # Drop rows with all NA values
  df <- df[!(rowSums(is.na(df)) == ncol(df)),]

  # Drop rows that are exact duplicates
  df <- df[!(duplicated(df)),]

  # Check and fix column formats
  df$YEAR <- as.numeric(as.character(df$YEAR))
  df$`CROP NAME` <- as.character(df$`CROP NAME`)
  df$`AFSC Commodity Forecast (Reported As)` <-
    as.character(df$`AFSC Commodity Forecast (Reported As)`)
  df$GRADE <- as.character(df$GRADE)
  df$`PRICE ($/kg)` <- as.numeric(as.character(df$`PRICE ($/kg)`))

  set_pf <- unique(df$`CROP NAME`)
  set_ay <- unique(list_all$`Average Yields`$`CROP NAME`)


  # Quick check to find miss matching names in AY and Price Fraction sheet
  mdiff <- setdiff(set_pf, set_ay)
  if (length(mdiff) > 0) {
    warn_msg <-
      paste0(
        "The following CROP NAMES in the Price
        Fractions worksheet are not found in the
        Average Yields worksheet (please check spelling): ",
        paste(mdiff, collapse = ", ")
      )
    warning(warn_msg)
  }

  # Update list object with clean data
  list_all$`Price Fractions` <- df





  # ==========================================
  # Clean: FDL GIS Worksheet
  # ==========================================

  # Check columns
  exp_cols <-
    c("FDL",
      "Not Cultivated Acres",
      "Total Acres (GIS FDL Boundary)",
      "AFSC Crop Risk Zone")

  df <- list_all$`FDL GIS`
  colnames(df) <- gsub("\r\n", "", colnames(df))
  has_cols <- colnames(df)

  mdiff <- setdiff(exp_cols, has_cols)
  if (length(mdiff) > 0) {
    err_msg <-
      paste0("Missing columns in Excel worksheet FDL GIS: ",
             paste(mdiff, collapse = ", "))
    return(err_msg)
  }


  # Check special LSRS columns
  df_lsrs <- list_all$LSRS
  lsrs_ids <- as.character(df_lsrs$`Reference ID`)

  # Should be in reference
  lsrs_cols <- has_cols[grepl("LSRS", has_cols)]

  # Must have Dry but Irr columns irrelevant
  lsrs_cols <- lsrs_cols[grepl("Dry|Irr", lsrs_cols)]

  # mcheck <- lapply(lsrs_ids, grepl, x = lsrs_cols)
  # mcheck <- unlist(mcheck)


  # Filter to only include expected columns
  gis_exp_cols <- c(exp_cols, lsrs_cols)
  df <- df[, gis_exp_cols]


  # Drop rows with all NA values
  df <- df[!(rowSums(is.na(df)) == ncol(df)),]

  # Drop rows that are exact duplicates
  df <- df[!(duplicated(df)),]


  # Check and fix column formats
  df$FDL <- as.character(df$FDL)
  df$`AFSC Crop Risk Zone` <- as.character(df$`AFSC Crop Risk Zone`)
  df$`Not Cultivated Acres` <-
    as.numeric(as.character(df$`Not Cultivated Acres`))
  df$`Total Acres (GIS FDL Boundary)` <-
    as.numeric(as.character(df$`Total Acres (GIS FDL Boundary)`))

  # Fix LSRS columns
  for (l in seq_along(lsrs_cols)) {

    c_index <- which(colnames(df) == lsrs_cols[l])

    df[, c_index] <- apply(df[, c_index], 2,
                           function(x)
                             as.numeric(as.character(x)))

  }


  # Check column sums for total acres
  mcol_sums <- df[, c(lsrs_cols, "Not Cultivated Acres")]

  mdiff <- rowSums(mcol_sums) / df$`Total Acres (GIS FDL Boundary)`
  mdiff2 <- round(abs(mdiff - 1), 2)
  mdiff3 <- which(mdiff2 > 0.03)

  if (length(mdiff3) > 0) {
    bad_prop <- df$FDL[mdiff3]
    err_msg <-
      paste0(
        "Total Acres does not equal sum of
             LSRS and non-cultivated acres in the
             FDL GIS worksheet for FDL(s): ",
        paste(bad_prop, collapse = ", ")
      )
    warning(err_msg)
  }


  # Update list object with clean data
  list_all$`FDL GIS` <- df





  # ==========================================
  # Clean: LSRS Worksheet
  # ==========================================

  # Check columns
  exp_cols <-
    c("Reference ID",
      "Full Name",
      "Percentage (%)",
      "Limitation")

  df <- list_all$LSRS
  colnames(df) <- gsub("\r\n", "", colnames(df))
  has_cols <- colnames(df)

  mdiff <- setdiff(exp_cols, has_cols)
  if (length(mdiff) > 0) {
    err_msg <-
      paste0("Missing columns in Excel worksheet LSRS: ",
             paste(mdiff, collapse = ", "))
    return(err_msg)
  }

  # Filter to only include expected columns
  df <- df[, exp_cols]

  # Drop rows with all NA values
  df <- df[!(rowSums(is.na(df)) == ncol(df)),]

  # Drop rows that are exact duplicates
  df <- df[!(duplicated(df)),]

  # Check and fix column formats
  df$`Reference ID` <- as.character(df$`Reference ID`)
  df$`Full Name` <- as.character(df$`Full Name`)
  df$`Percentage (%)` <-
    as.numeric(as.character(df$`Percentage (%)`))
  df$Limitation <- as.character(df$Limitation)

  # Check that percentages add to 100
  check100 <- sum(df$`Percentage (%)`)

  if (check100 != 100) {
    err_msg <-
      paste0("See LSRS Worksheet. LSRS
             Percentages (%) do not add to
             100 across LSRS classes")
    warning(err_msg)
  }


  # Update list object with clean data
  list_all$LSRS <- df




  # return cleaned data object after all checks
  return(list_all)



}
