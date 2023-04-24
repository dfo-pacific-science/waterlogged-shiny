<a name="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Issues][issues-shield]][issues-url]

<!-- PROJECT LOGO -->
<br />


<h1 align="center">CCIRA: Watershed Restoration Decision Support Tool</h1>

<div align="center">
  <a href="https://github.com/essatech/ccira-watersheds">
    <img src="/www/img/logos.png" alt="Logo" height="80">
  </a>

  <p align="center">
  The Central Coast Indigenous Resource Alliance (CCIRA) Watershed Restoration Decision Support Tool
    <br />
    <a href="https://github.com/essatech/ccira-watersheds" style="pointer-events: none;">View Live App</a>
    ·
    <a href="https://github.com/essatech/ccira-watersheds/issues">Report Bug</a>
    ·
    <a href="https://github.com/essatech/ccira-watersheds" style="pointer-events: none;">Video Tutorial</a>
    ·
    <a href="https://bit.ly/restoration-planning-tool">Figma Wireframe</a>
    ·
    <a href="https://github.com/essatech/ccira-watersheds/tree/main/www/docs">Design Memo</a>
  </p>
</div>



<!-- ABOUT THE PROJECT -->
## About The Project

CCIRA's vision for the restoration tool is to provide a web-based, interactive mapping portal, similar to the Central Coast Escapement Monitoring Tool, which will provide information to support Nation-led prioritization of freshwater salmon habitat restoration projects. We propose to develop a tool which will provide summarized information to Heiltsuk, Kitasoo / Xai’xais, Nuxalk and Wuikinuxv Nations who will use place-based understanding to inform their use and interpretation of the tool outputs. The intent of the tool is not to dictate exactly what restoration should occur or where. Instead, it is meant to provide a platform for exploration of data and to suggest probable restoration opportunities for further consideration based on the best available evidence from both Western Science and Local and Indigenous Knowledge. Those candidate restoration opportunities can then be included in a broader planning process outside of the tool itself, using guidance provided by the Legacy Restoration Framework that is also a component of this project, by further incorporating ancestral and observational knowledge held by community members.

[![App Screen Shot][app-screenshot]](www/img/preview.png)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

See instructional video tutorial (link placeholder TODO) and documentation (link placeholder TODO) for help getting started.

### Prerequisites and Installation

If you are accessing the online version of the application then there are not special steps for installation. If you want to run the application locally (on your own computer) then you will need to do the following:
1. Download and install the lastest version of [R from cran](https://cran.r-project.org/)
2. Download and install R-Studio from [rstudio.com](https://rstudio.com/products/rstudio/download/#download)
3. Download (or clone) this repository. On the top of this page click the green "Code" button and select "Download ZIP". Unzip the file and open the folder in R-Studio.
4. Install the required R packages. In the R-Studio console type the following commands:
```r
# TODO update this..
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinydashboardPlus")
install.packages("dplyr")
install.packages("R.utils")
install.packages("readxl")
install.packages("sf")
install.packages("leaflet")
```
5. Run the application. In the R-Studio console type the following command:
```r
shiny::runApp()
```


## Input Data

Each time the application is initiated it loads the input data from the Excel workbook located in the "data" folder.

[input_data.xlsx](ccira-watersheds/data/)


This workbook contains all of the data that is used to populate the application. This workbook is loaded each time the app is initiated. Changes made to the data in this workbook will change the data that appears within the application. Please review this Readme document to understand the structure of the input data.

The CCIRA Restoration Tool was designed with the following data model (schema) to provide maximum flexibility and extendibility for perceived use cases. Numerous components of the tool can be changed simply by updating the underlying data in this workbook. This is advantageous since it allows for numerous contributors to edit, update, and improve the application without having knowledge of the intricacies of the R-Shiny development framework. Large structural changes can also be implemented without editing any code. For example, the default input data currently uses polygons from the BCFWA Assessment Watersheds as the primary spatial unit of assessment with metrics (stressors) developed from the Pacific Salmon Foundation. If the project team later decides to switch to fundamental watersheds of spawning streams then the input data can simply be updated to reflect those changes without updating source code within the application.

This workbook also controls how metrics are categorized and summarized. We chose a data model that enables maximum flexibility to link individual metrics to stressor categories with customized benchmarks and relationships. The data model assumes that metrics and relationships will change over time and that these changes these relationships should not be hard-coded into the application. For example, watersheds with high road densities are perceived to be a greater risk for sedimentation issues. However, high road densities are also linked to exacerbated runoff (peak flows) that may scour stream and reduce instream habitat complexity. The data model is designed to allow for these relationships to be defined within this Excel workbook rather than being hard coded inside the application.

### Relational Database:

The data model (schema) for the CCIRA Restoration Tool follows the conventional format of a relational database. A relational database organizes data with predefined relationships between data tables. Each Excel worksheet represents a unique data table. Each data table has a primary key (a unique identifier for each row) and associated information for each attribute. For example, the `watersheds` table will have a `watershed_id` field (the primary key), a field for the watershed name (as it appears in the Gazette) and potentially other key attributes. A `nations` table is also included listing member nations of the CCIRA. The `nations` table is relatively simple as it only includes fields for the `nation_id` (primary key), `name` (name of the nation) and `description` (an open-ended text field).

[![Data Model][data-model]](www/img/data-model.png)

### Junction Tables (Associative Entity):

The CCIRA Restoration Tool schema makes use of junction tables do define relationships between different entities. An associative entity (or junction table) maps two or more tables together by referencing the primary keys of each data table. Junction tables are useful to map many-to-one or many-to-many relationships. For example, if we consider a hypothetical database with a table for actors and a table for films, we can see how it’s necessary to have a third table containing the relationships between actors and films. This third table is a junction table with each row representing a unique relationship between an actor and a film.

Example junction table for actors and films (https://en.wikipedia.org/wiki/Associative_entity)
Junction tables are used throughout the CCIRA Restoration Tool schema to map many-to-many relationships. In our previous example with `watersheds` and `nations`, a third table `watershed_nation` is included to represent the relationships between each watershed unit and each nation. In many instances a single watershed will have relationships with more than one nation. Additional attributes and custom functionality can be included within junction tables. For example, a custom name can be specified for each nation-watershed relationship to supersede the name specified in the Gazette (where appropriate).

Other examples of junction tables in the schema for this tool include the relationships between watersheds and `fish` species within each watershed. These relationships are specified by the `watershed_fish` junction table. The `watershed_fish` junction table specifies the presence of target fish species within a watershed and includes additional attributes for each relationship (such as the use of the watershed for spawning of species X etc.). Relationships between individual `metrics` (stressor variables) are linked to `watersheds` with the `watershed_metric` junction table. However, `metrics` are also linked to stressor `categories` with a `metric_category` junction table. Representing the data in this format allows for a given metric to have customized relationships with one or more stressor categories.

### Data Tables:
* **watersheds**: Primary data table for watershed listing the watershed_id (primary key) and Gazette name. Other attributes can be added to this table but watershed stressor metrics should not be included as columns in this table.
nations: Basic table for nations with nation_id (primary key), name, description, and other attributes (to be added where appropriate).
* **watershed_nation**: (junction table) defining many-to-many relationships between watersheds (watershed_id) and nations (nation_id). Each watershed may be associated with one or more nations. Nation-specific app instances will use relationships in this table to filter out watersheds not relevant to a given nation.
name_custom: A `name_custom` field is included to specify nation-specific names for a given watershed (where appropriate). If values are included in this field, the default Gazette name of the watershed will be updated with the nation-specific value.
* **fish**: Table of all target fish species. Attribute fields include `species_fiss_code` (for simplified reference to DFO/provincial datasets), `species_common_name` and `species_latin_name`. 
    * target_species: A special field `target_species` (TRUE/FALSE) is included to identify target fish species. If the `target_species` is set to TRUE, the species will appear in the filter list in the map view of the application. The presence of other non-target species will only appear in the watershed detail view.
* **watershed_fish**: (junction table) defining many-to-many relationships between watersheds (watershed_id) and fish (fish_id). Each watershed may be associated with one or more fish species.
    * spawning (TRUE/FALSE): A special field used as a flag to specify whether there is significant spawning habitat for species X in watershed Y. Additional attributes may be included at a later date (depending on data availability) for rearing habitat and/or quantity of spawning habitat (e.g., kilometers of stream etc.).
* **metrics**: A table for each metric (stressor variable). This table lists all the watershed metrics. Fields are included for the `metric_id` (primary key), `metric_name` (for display purposes), `units` (e.g., km/km2), `description` (describing what it is and why it is important) and `source` (a note on the source and ideally a description of the calculation method).
    * hide_in_app (TRUE/FALSE): A special flag to specify whether the metric should appear in the list of stressors to visualize on the map view.
    * default_threshold_moderate: The threshold value, on the original scale of the metric (e.g., km/km2), at which the general risk level is expected to change from low to moderate. For example, road density (km/km2) within a watershed is considered to be at low risk at values under 0.6 kilometers of road per km2 of watershed area. Customized thresholds can be defined for each stressor category relationship (described below) to supersede these default thresholds.
    * default_threshold_high: The threshold value, on the original scale of the metric (e.g., km/km2), at which the general risk level is expected to change from moderate to high. For example, road density (km/km2) within a watershed is considered to be at high risk at values above 1.2 kilometers of road per km2 of watershed area. Customized thresholds can be defined for each stressor category relationship (described below) to supersede these default thresholds.
* **watershed_metric**: (junction table) Containing the raw metric value for each watershed (watershed_id) and metric (metric_id). The `value` field is the raw value of metric X in watershed Y. A `note` (if specified) will appear in the watershed detailed view.
* **categories**: Unique stressor categories (e.g., channel habitat, sedimentation, and peak flows) specifying various impact pathways. id, name, and description fields can be modified as needed.
* **metric_category**: (junction table) Defining the relationships between each metric (metric_id) and stressor category (category_id). Each metric may be linked to one or more categories.
    * custom_threshold_moderate / custom_threshold_high: Fields specifying the custom threshold applied to metric_id X and category_id Y. Note that the metrics table already includes default threshold benchmarks. In many cases the default values can be applied globally to all categories and this field can be left blank. However, in certain circumstances benchmark stressors may have custom values for specific impact pathways. For example, road density threshold values may change depending on impact pathways for peak flows vs sedimentation. If a value is specified here, it will superspeed the threshold benchmark defined in the metrics table.
    * metric_weight_for_category: Custom weighting factor for each metric in each category. If summary calculations involve a roll-up or creation of an index, this field can be used to weight individual metrics within each category.
* **observations**: This table is intended to record observations, insights or other restoration projects. This table is not linked to the other tables; however, points appear on the map with latitude and longitude coordinates.
    * photo_url: The photo url specifies the file path to the photo resource. Photos are stored within `./data/photos/`. Photo urls should look like the following with no quotation marks `./data/photos/photo_1.jpg`. Photos can also be referenced from the internet (provided links are public). If a photo url begins with `http` the tool will reference photos from online sources. Example:`https://www.website.com/photo_1.jpeg`. File size of photos should be small and optimized for web viewing.

### Other inputs:

GIS file of watershed polygons: Watershed polygons must be available in the data directory of the app. The watersheds GIS file must contain polygon geometry data for each watershed. The file must be a geopackage and must be named `watersheds.gpkg`. It is also necessary that the file includes an attribute field for `watershed_id` of type integer (linking watersheds with a 1:1 relationship to the watershed table in the Excel file) and a field for the watershed name `name_gazette`. The GIS data must also meet the following additional criteria:
File format: geopackage
File name: `.\data\watersheds.gpkg`
Geometry type: polygon data (single layer)
Coordinate Reference System (CRS): EPSG:4326 - WGS 84 (Latitude/Longitude)
Mandatory fields:
watershed_id (of type integer, with no duplicate values).
name_gazette (string, values optional)
File size: Simplify geometry such that the file size is less than 7MB.
Ensure geometries are valid.
Ensure that watershed_id match values in the Excel workbook.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

<!-- CONTACT -->
## Contact

Matthew Bayly - mjbayly@mjbayly.com
Project Link: [https://github.com/essatech/ccira-watersheds](https://github.com/essatech/ccira-watersheds)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

TODO...

* []()
* []()
* []()

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[issues-shield]: https://img.shields.io/github/issues/essatech/ccira-watersheds.svg?style=for-the-badge
[issues-url]: https://github.com/essatech/ccira-watersheds/issues
[contributors-shield]: https://img.shields.io/github/contributors/essatech/ccira-watersheds.svg?style=for-the-badge
[contributors-url]: https://github.com/essatech/ccira-watersheds/graphs/contributors
[data-model]: www/img/data-model.png
[app-screenshot]: www/img/preview.png