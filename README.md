# waterlogged Shiny Application

**Water Level & Water Quality Data Processing**

waterlogged is a data processing application for water data from continuous water data loggers. The goal of waterlogged is to streamline the process from raw data to clean data, document QC processing via R scripts, and do so without relying on proprietary software.

This version of the package was built for data from Onset HOBO barometric pressure (U20), dissolved oxygen (U26), conductivity (U24), and TidbiT loggers.

## Workflow Overview

This application guides you through a step-by-step workflow for processing your water logger data:

1. **Upload Data** — Download and upload raw data from HOBO loggers
2. **Step 1: Bind Logger Data** — Combine data from multiple logger files for each station
3. **Step 2: Convert Metrics** — Convert raw measurements to usable metrics (water level, dissolved oxygen)
4. **Step 3: QC** — Quality control checks
5. **Step 4: Save Data** — Export clean site data
6. **Step 5: Summarize** — Generate summary statistics and visualizations

![Process timeseries data. Apply filters and select ranges to modify.](www/img/app_overview_01.png)

## Supported Loggers

| Logger | Metric |
|--------|--------|
| HOBO U20 | Water Level |
| HOBO U20 | Barometric Pressure |
| HOBO U26 | Dissolved Oxygen |
| HOBO U24 | Conductivity |
| HOBO TidbiT | Temperature |

## Key Functions

| Processing Stage | Function |
|------|----------|
| Data Binding | `bind_logger_files()` |
| Barometric Compensation | `add_nearest_baro()` |
| Water Level Conversion | `convert_waterlevel_kPa_m()` |
| DO Conversion | `convert_do_mgl_percsat()` |
| Conductivity Conversion | `convert_conductivity_spc_uScm()` |
| QC | `qc_waterlevel()`, `qc_dissox()`, `qc_conductivity()` , `qc_barometric`, `qc_airtemp`, `qc_watertemp` |  
| Manual Adjustments | `adjust_logger_NA` , `adjust_waterlevel_offset`, `adjust_waterlevel_spike`, `adjust_waterlevel_zero`, `adjust_waterlevel_datum`|
| Data Export | `write_clean_data()` |
| Summarize Data | `summarise_clean_data()` |
| Visualize Data | `plot_clean_timeseries()` |

## Documentation & Tutorials

- [waterlogged R package manual](inst/misc/documentation/waterlogged_manual.pdf)
- [Tutorial: Barometric timeseries](inst/misc/documentation/barometric-pressure.pdf)
- [Tutorial: Water level timeseries](inst/misc/documentation/water-level.pdf)
- [Tutorial: Dissolved oxygen timeseries](inst/misc/documentation/dissolved-oxygen.pdf)
- [Tutorial: Conductivity timeseries](inst/misc/documentation/conductivity.pdf)

## Getting Started

1. Navigate to **Upload Data**
2. Follow each step in sequence
3. Review instructions and examples
4. Process and visualize your data

## Running the Application

```r
shiny::runApp()
```

## Repositories

- R package: [github.com/dfo-pacific-science/waterlogged](https://github.com/dfo-pacific-science/waterlogged)
- Shiny app: [github.com/dfo-pacific-science/waterlogged-shiny](https://github.com/dfo-pacific-science/waterlogged-shiny)

## Authors & Contributors

**Department of Fisheries and Oceans, Canada**

**Authors/Developers:** Daniella LoScerbo, Amanda Martens, Julian Gan, Paula Soto

**Project Supervisors:** Douglas Braun, Sean Naman, David Reid, Joel Harding

**Technical Support:** Matthew Bayly (MJBA, App Development), Frank Poulsen (Ashlu, Technical Support)
