#' Export Excel FDL
#'
#' @description Exports all FDL data to Excel
#'
#' @param excel_data List object returned from `load_data_from_excel()`.
#' @param crops_selected List object returned from `crop_selector()`.
#' @param ltay_tables List object returned from `build_ltay_tables()`.
#' @param parcel_data Data frame returned from `parcel_data_gis()`.
#' @param filename String. name of the Excel file to write. Example `output.xlsx`.
#'
#'
#' @details This function exports all data into a neatly formatted Excel workbook.
#'
#' @returns A writes and Excel workbook to the file system
#
export_excel_fdl <- function(excel_data = NA,
                             crops_selected = NA,
                             ltay_tables = NA,
                             parcel_data = NA,
                             filename = "output.xlsx") {

  excel_data <- dat
  crops_selected <- cs
  ltay_tables <- ltay

  names(excel_data)
  names(ltay_tables)




  # Fix column names for LTAY
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "grpcpa"] <- "Gross Revenue Per Crop Per Acre"
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "price"] <- "Price Per Kilogram"
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "average_crop_compostion"] <- "Average Crop Compostion"
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "acres_per_risk"] <- "Acres Per Risk Zone"
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "average_acres_grown"] <- "Average Acres Grown"
  colnames(ltay_tables$ltay)[colnames(ltay_tables$ltay) == "average_yield_kg_ac"] <- "Average Yield Kg per Acre"
  colnames(ltay_tables$ltay) <- gsub("ay_", "Average Yield ", colnames(ltay_tables$ltay))
  colnames(ltay_tables$ltay) <- gsub("ag_", "Acres Grown ", colnames(ltay_tables$ltay))




  # Start with this list
  exp_data <- excel_data

  # Add the selected crops
  exp_data[[length(exp_data) + 1]] <- crops_selected
  names(exp_data)[length(exp_data)] <- "Crops Selected"

  # Add on the LTAY tables
  names(ltay_tables) <- c("LTAY Crops", "LTAY Risk Areas")
  exp_data <- c(exp_data, ltay_tables)

  # Add on parcel data
  exp_data[[length(exp_data) + 1]] <- parcel_data
  names(exp_data)[length(exp_data)] <- "FDL Summary"


  # names(exp_data)
  # length(exp_data)

  writexl::write_xlsx(exp_data, filename)



}





