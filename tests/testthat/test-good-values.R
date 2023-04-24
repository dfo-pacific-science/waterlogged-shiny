test_that("test-good-values", {

  library(dplyr)

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

  filenames <- c(filename1, filename2)


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

    # Select Crops ----------------------
    cs <- suppressWarnings({
      crop_selector(excel_data = dat)
    })


    # Special use of custom year sequence
    if(basename(filename2) == "FDL_Rental_Rates_Inputs_2020.xlsx") {
      cs <- suppressWarnings({
        crop_selector(excel_data = dat,
                      start_year = 2016,
                      stop_year = 2020
                      )
      })
    }

    cs$tmp_id <- paste0(cs$`RISK AREA`, "__", cs$`CROP NAME`, "__", cs$`AG INPUT`)
    qa_cv$tmp_id <- paste0(qa_cv$risk_zone, "__", qa_cv$crop, "__", qa_cv$ag_input)
    u_ids <- unique(qa_cv$tmp_id)

    # Only keep matching crops
    cs_sub <- cs[which(cs$tmp_id  %in% u_ids), ]

    # We do not expect any missmatch here...
    miss1 <- setdiff(cs_sub$tmp_id, u_ids)
    miss2 <- setdiff(u_ids, cs_sub$tmp_id)

    expect_true(length(miss1) < 2)
    expect_true(length(miss2) < 2)

    cs_sub$tmp_id <- NULL


    # Run LTAY ----------------------
    ltay <- suppressWarnings({
      build_ltay_tables(excel_data = dat,
                        crops_selected = cs_sub
                        )
    })


    # Special use of custom year sequence
    if(basename(filename2) == "FDL_Rental_Rates_Inputs_2020.xlsx") {
      ltay <- suppressWarnings({
        build_ltay_tables(excel_data = dat,
                          crops_selected = cs_sub,
                          start_year = 2016,
                          stop_year = 2020
        )
      })
    }

    # head(ltay$ltay)




    # Test parcel data ---------------------------------
    parcel_data <- parcel_data_gis(excel_data = dat,
                                   ltay_tables = ltay)



    # Check against ref ---------------------------------
    qcvb <- as.data.frame(qcv)

    qcv2 <- merge(qcvb, parcel_data, by.x = "FDL",
                  by.y = "FDL", all.x = TRUE, all.y = FALSE)


    plot(qcv2$total_value, qcv2$`Total Value`,
          xlab = "Total Value (reference)",
          ylab = "Total Value (tool)",
          col = as.factor(qcv2$`AFSC Crop Risk Zone`),
          main = basename(filename)
    )
    abline(0, 1)

    text(qcv2$total_value, qcv2$`Total Value`, qcv2$`AFSC Crop Risk Zone`, cex = 0.5)

    # text(qcv2$total_value, qcv2$`Total Value`, qcv2$FDL, cex = 0.5)


    qcv2$y <- qcv2$`Total Value`

    z <- lm(y ~ total_value, qcv2)
    abline(z, col = "red")
    rsq <- summary(z)$adj.r.squared
    expect_true(rsq > 0.1)

    # Look at rates...
    qcv2$ref_rate <-  suppressWarnings({ as.numeric(qcv2$ref_rate) })
    plot(qcv2$ref_rate, qcv2$`Value Per Acre`,
         xlab = "Total Value/Acre (reference)",
         ylab = "Total Value/Acre (tool)",
         col = as.factor(qcv2$`AFSC Crop Risk Zone`),
         main = basename(filename)
    )
    abline(0, 1)

    text(qcv2$ref_rate, qcv2$`Value Per Acre`,
         qcv2$`AFSC Crop Risk Zone`, cex = 0.5)

    qcv2$y <- qcv2$`Value Per Acre`
    z <- lm(qcv2$y ~ qcv2$ref_rate)
    rsq <- summary(z)$adj.r.squared

    expect_true(rsq > 0.3)




    # names(ltay)
    # View(ltay$risk_area_lsrs)
    # View(ltay$ltay)
    # write.csv(ltay$ltay, file = "tmp2.csv")


  }



})
