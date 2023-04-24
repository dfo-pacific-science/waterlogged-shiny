#----------------------------------------------------
# Sample Run
#----------------------------------------------------
library(usethis)
library(testthat)
library(devtools)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
# devtools::test()  # Run tests


# Test that 2019 and 2020 data is processed accuratly

qa_check_value <- system.file("extdata/02_sample_data/qa_check/",
                              "qa_check_total_values.csv",
                              package = "FDLToolbox")
qa_check_value <- read.csv(qa_check_value)

fname <- system.file("extdata/02_sample_data/qa_check/",
                     "qa_check_selected_crops.csv",
                     package = "FDLToolbox")
qa_check_crops <- read.csv(fname)


filename1 <-
  system.file("extdata/02_sample_data",
              "FDL_Rental_Rates_Inputs_2019.xlsx",
              package = "FDLToolbox")
filename2 <-
  system.file("extdata/02_sample_data",
              "FDL_Rental_Rates_Inputs_2020.xlsx",
              package = "FDLToolbox")

filename3 <-
  system.file("extdata/02_sample_data",
              "FDL_Rental_Rates_Inputs_2021.xlsx",
              package = "FDLToolbox")

filenames <- c(filename1, filename2, filename3)


for (f in seq_along(filenames)) {

  print(f)

  # f = 1
  filename <- filenames[f]

  # Filter qa Data ----------------------------
  qcv <- qa_check_value[which(qa_check_value$fname == basename(filename)), ]
  # Filter QA Crops Data ----------------------
  qa_cv <- qa_check_crops[which(qa_check_crops$fname == basename(filename)), ]


  # Load Excel Data ----------------------
  dat <- suppressWarnings({
    load_data_from_excel(filename)
  })


  # Special use of custom year sequence
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2019.xlsx") {
    cs <- suppressWarnings({
      crop_selector(excel_data = dat,
                    start_year = 2014,
                    stop_year = 2018
      )
    })
  }
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2020.xlsx") {
    cs <- suppressWarnings({
      crop_selector(excel_data = dat,
                    start_year = 2015,
                    stop_year = 2019
      )
    })
  }
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2021.xlsx") {
    cs <- suppressWarnings({
      crop_selector(excel_data = dat,
                    start_year = 2016,
                    stop_year = 2020
      )
    })
  }





  # Only for 2019 and 2020
  if(f < 3) {
    cs$tmp_id <- paste0(cs$`RISK AREA`, "__", cs$`CROP NAME`, "__", cs$`AG INPUT`)
    qa_cv$tmp_id <- paste0(qa_cv$risk_zone, "__", qa_cv$crop, "__", qa_cv$ag_input)
    u_ids <- unique(qa_cv$tmp_id)

    # Only keep matching crops
    cs_sub <- cs[which(cs$tmp_id  %in% u_ids), ]

    # We do not expect any missmatch here...
    miss1 <- setdiff(cs_sub$tmp_id, u_ids)
    miss2 <- setdiff(u_ids, cs_sub$tmp_id)
    cs_sub$tmp_id <- NULL
  } else {
    cs_sub <- cs
  }





  # Special use of custom year sequence
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2021.xlsx") {
    ltay <- suppressWarnings({
      build_ltay_tables(excel_data = dat,
                        crops_selected = cs_sub,
                        start_year = 2016,
                        stop_year = 2020
      )
    })
  }
  # Special use of custom year sequence
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2020.xlsx") {
    ltay <- suppressWarnings({
      build_ltay_tables(excel_data = dat,
                        crops_selected = cs_sub,
                        start_year = 2015,
                        stop_year = 2019
      )
    })
  }
  # Special use of custom year sequence
  if(basename(filename) == "FDL_Rental_Rates_Inputs_2019.xlsx") {
    ltay <- suppressWarnings({
      build_ltay_tables(excel_data = dat,
                        crops_selected = cs_sub,
                        start_year = 2014,
                        stop_year = 2018
      )
    })
  }


  # head(ltay$ltay)




  # Test parcel data ---------------------------------
  parcel_data <- parcel_data_gis(excel_data = dat,
                                 ltay_tables = ltay)


  fname1 <- paste0("Results_", basename(filename))
  fname2 <- paste0("./inst/extdata/02_sample_data/qa_check/processed/", fname1)

  # Export ---------------------------------
  export_excel_fdl(excel_data = dat,
                   crops_selected = cs_sub,
                   ltay_tables = ltay,
                   parcel_data = parcel_data,
                   filename = fname2)


}

