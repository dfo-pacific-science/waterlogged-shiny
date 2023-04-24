test_that("test-load-data", {

  # Test that three sample files can be loaded

  qa_check_value <- system.file("extdata/02_sample_data/qa_check/",
                                "qa_check_total_values.csv",
                                package = "FDLToolbox")
  qa_check_value <- read.csv(qa_check_value)

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



    # Test data load ----------------------
    dat <- suppressWarnings({
      load_data_from_excel(filename)
    })

    expect_true(length(dat) == 4)
    c1 <- unlist(lapply(dat, nrow))
    expect_true(all(c1 > 1))



    # Test selector ----------------------
    # Run selector
    cs <- suppressWarnings({
      crop_selector(
        excel_data = dat,
        start_year = 2001,
        stop_year = 2099,
        target_years = 5,
        target_coverage_pct = 3
      )
    })

    nrow(cs) > 0
    expect_true(nrow(cs) > 0)
    check_1 <- sum(cs$selected)

    cs <- suppressWarnings({
      crop_selector(excel_data = dat)
    })

    check_2 <- sum(cs$selected)
    expect_true(check_2 >= check_1)


    cs <- suppressWarnings({
      crop_selector(
        excel_data = dat
      )
    })



    # Test LTAY ----------------------
    ltay <- suppressWarnings({
      build_ltay_tables(excel_data = dat,
                        crops_selected = cs)
    })

    expect_true(length(ltay) == 2)
    expect_true(median(ltay$ltay$grpcpa, na.rm = TRUE) > 0)

    df <- ltay$risk_area_lsrs
    irr <- df[, grepl("irr_", colnames(df))]
    expect_true(sum(irr) > 0)

    df <- ltay$risk_area_lsrs
    irr <- df[, grepl("dry_", colnames(df))]
    expect_true(sum(irr) > 0)

    # ltay_tables <- ltay
    # excel_data  <- dat




    # Test parcel data ---------------------------------
    parcel_data <- parcel_data_gis(excel_data = dat,
                                   ltay_tables = ltay)

    expect_true(median(parcel_data$`Total Value`) > 0)
    expect_true(median(parcel_data$`Total Value`) < 50000)
    expect_true(median(parcel_data$`Value Per Acre`) > 0)
    expect_true(median(parcel_data$`Value Per Acre`) < 10000)


    # View(parcel_data[parcel_data$`AFSC Crop Risk Zone` == 2, ])




    # Check against ref ---------------------------------
    qcv <-
      qa_check_value[which(qa_check_value$fname == basename(filename)),]

    if(nrow(qcv) < 1) { next }

    qcv <- as.data.frame(qcv)
    qcv$total_value_ref <- qcv$total_value
    qcv$total_value <- NA
    qcv$total_value <- NULL
    qcv <- as.data.frame(qcv)


    qcv2 <- merge(qcv, parcel_data, by.x = "FDL",
                  by.y = "FDL", all.x = TRUE, all.y = FALSE)


    plot( qcv2$total_value_ref, qcv2$`Total Value`,
         xlab = "Total Value (reference)",
         ylab = "Total Value (tool)",
         col = as.factor(qcv2$`AFSC Crop Risk Zone`),
         main = basename(filename)
         )
    abline(0, 1)

    text( qcv2$total_value_ref, qcv2$`Total Value`,
         qcv2$`AFSC Crop Risk Zone`, cex = 0.5)

    # Check out problem
    # View(parcel_data[parcel_data$`AFSC Crop Risk Zone` == '3', ])
    # View(ltay$ltay[ltay$ltay$`RISK AREA` == '3', ])

    qcv2$y <- qcv2$`Total Value`

    z <- lm(y ~ total_value_ref, qcv2)
    abline(z, col = "red")
    rsq <- summary(z)$adj.r.squared
    expect_true(rsq > 0.6)


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

    expect_true(rsq > 0.2)




  }



})
