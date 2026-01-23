# Waterlogged Shiny Application

A Shiny application for processing water level and water quality data from HOBO data loggers.

## Overview

This application provides a step-by-step workflow to guide users through the data processing pipeline:

1. **Upload Data** - Download and upload raw data from HOBO loggers
2. **Step 1: Bind Logger Data** - Combine data from multiple logger files for each station
3. **Step 2: Convert Metrics** - Convert raw measurements to usable metrics (water level, dissolved oxygen)
4. **Step 3: QAQC** - Quality assurance and quality control checks
5. **Step 4: Save Data** - Export clean site data
6. **Step 5: Summarize** - Generate summary statistics and visualizations

## Supported Logger Types

- HOBO U20 Water Level Logger
- HOBO U20 Barometric Pressure Logger
- HOBO U26 Dissolved Oxygen Logger

## Running the Application

```r
# Install required packages (if not already installed)
# install.packages(c("shiny", "shinydashboard", "shinyjs", ...))

# Run the app
shiny::runApp()
```
