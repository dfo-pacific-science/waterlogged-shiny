# ui.R
# Waterlogged Shiny Application

# ------------------------------------------------
# Setup UI function for Shiny app
ui <- dashboardPage(
  title = "waterlogged: Water Data Processing",
  skin = "blue",
  header = dashboardHeader(
    title = "waterlogged",
    titleWidth = 280
  ),

  # Sidebar page menu
  sidebar = page_sidebar_tab_ui("sidebar_navigation"),

  # Main body panel
  body = dashboardBody(
    # Import custom css files to html head tag
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css"),

      # Prevent scroll-to-top when fileInput browse is clicked or
      # when Shiny reactive outputs update and replace DOM nodes.
      tags$script(HTML("
        $(document).ready(function() {

          // ---- 1. Neutralise href='#' inside .btn-file anchors ----
          // Bootstrap 3 file buttons sometimes use <a href='#'> which
          // triggers a hash-navigation scroll-to-top.
          $(document).on('click', '.btn-file a[href=\"#\"]', function(e) {
            e.preventDefault();
          });

          // ---- 2. Freeze ALL scrollable containers during file dialog ----
          var wrapper = document.querySelector('.content-wrapper');
          var targets = [window, document.documentElement, document.body];
          if (wrapper) targets.push(wrapper);

          var scrollLocked = false;
          var savedPositions = [];

          function savePositions() {
            savedPositions = targets.map(function(el) {
              return (el === window) ? window.pageYOffset
                                     : (el.scrollTop || 0);
            });
          }

          function restorePositions() {
            targets.forEach(function(el, i) {
              if (el === window) {
                window.scrollTo(window.pageXOffset, savedPositions[i]);
              } else {
                el.scrollTop = savedPositions[i];
              }
            });
          }

          function lockScroll() {
            if (scrollLocked) return;
            savePositions();
            scrollLocked = true;

            // Use a fast interval to continuously restore positions
            // in case anything resets them during file dialog
            lockScroll._interval = setInterval(restorePositions, 10);
          }

          function unlockScroll() {
            if (!scrollLocked) return;
            clearInterval(lockScroll._interval);
            restorePositions();
            scrollLocked = false;
          }

          // Lock on browse button click
          $(document).on('click', '.btn-file', function(e) {
            lockScroll();
            // Unlock after 2 seconds (covers dialog open/cancel)
            setTimeout(unlockScroll, 2000);
          });

          // Lock again on file selection (change event)
          $(document).on('change', '.btn-file input[type=file]', function() {
            lockScroll();
            setTimeout(unlockScroll, 2000);
          });

          // Safety: unlock on user scroll gesture
          if (wrapper) {
            wrapper.addEventListener('wheel', unlockScroll, {passive: true});
            wrapper.addEventListener('touchstart', unlockScroll, {passive: true});
          }

          // ---- 3. Shiny output updates: save/restore scroll ----
          var shinyScroll = null;
          $(document).on('shiny:outputinvalidated', function() {
            if (wrapper) shinyScroll = wrapper.scrollTop;
          });
          $(document).on('shiny:value shiny:error', function() {
            if (shinyScroll !== null && wrapper) {
              requestAnimationFrame(function() {
                wrapper.scrollTop = shinyScroll;
                shinyScroll = null;
              });
            }
          });
        });
      "))
    ),

    # Import shinyjs
    shinyjs::useShinyjs(),

    # ---- Shared HOBOware export instructions modal (Bootstrap 3) ----
    tags$div(
      class = "modal fade", id = "hoboware_export_modal",
      tabindex = "-1", role = "dialog",
      `aria-labelledby` = "hobowareModalLabel",
      tags$div(
        class = "modal-dialog modal-lg", role = "document",
        tags$div(
          class = "modal-content",
          tags$div(
            class = "modal-header",
            tags$button(type = "button", class = "close",
                        `data-dismiss` = "modal", `aria-label` = "Close",
                        tags$span(`aria-hidden` = "true", HTML("&times;"))),
            tags$h3(class = "modal-title", id = "hobowareModalLabel",
                    "How to Download CSV Files in the Correct Format from HOBOware")
          ),
          tags$div(
            class = "modal-body",
            tags$p(
              "HOBOware is Onset's free software for reading data from HOBO loggers. ",
              "You can download it for free here: ",
              tags$a(
                href = "https://www.onsetcomp.com/support/help-center/software/hoboware",
                target = "_blank",
                style = "color: #1565c0; font-weight: bold;",
                "onsetcomp.com/support/help-center/software/hoboware"
              )
            ),
            tags$hr(),
            tags$h4("Step 1"),
            tags$p("First, open HOBOware, and make sure the units are set to metric (e.g., metres/Celsius)."),
            tags$img(src = "img/export_instructions_01.png",
                     style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
                     alt = "HOBOware units settings"),
            tags$h4("Step 2"),
            tags$p("Ensure that export settings are set to HOBOware default values. Go to File, and then Preferences..."),
            tags$img(src = "img/export_instructions_02.png",
                     style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
                     alt = "HOBOware File Preferences menu"),
            tags$h4("Step 3"),
            tags$p("Click 'General' and then ensure the datetime format is set to HOBOware defaults."),
            tags$img(src = "img/export_instructions_03.png",
                     style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
                     alt = "HOBOware General preferences datetime format"),
            tags$h4("Step 4"),
            tags$p("Load your data file and export as a CSV by clicking 'File' and then 'Export Table Data...'."),
            tags$img(src = "img/export_instructions_04.png",
                     style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
                     alt = "HOBOware Export Table Data menu")
          ),
          tags$div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-default",
                        `data-dismiss` = "modal", "Close")
          )
        )
      )
    ),

    # Individual pages - right side tab content
    tabItems(
      tabItem("t_welcome", module_welcome_ui("welcome")),

      # ---- Temperature (Water & Air) ----
      tabItem("t_temp_upload",    module_temp_upload_ui("temp_upload")),
      tabItem("t_temp_qc_water", module_temp_qc_water_ui("temp_qc_water")),
      tabItem("t_temp_qc_air",   module_temp_qc_air_ui("temp_qc_air")),

      # ---- Barometric Pressure ----
      tabItem("t_baro_upload", module_baro_upload_ui("baro_upload")),
      tabItem("t_baro_qc",    module_baro_qc_ui("baro_qc")),

      # ---- Water Level (Pressure) ----
      tabItem("t_wl_upload",  module_wl_upload_ui("wl_upload")),
      tabItem("t_wl_convert", module_wl_convert_ui("wl_convert")),
      tabItem("t_wl_qaqc",   module_wl_qaqc_ui("wl_qaqc")),
      tabItem("t_wl_datum",  module_wl_datum_ui("wl_datum")),

      # ---- Dissolved Oxygen ----
      tabItem("t_do_upload",  module_do_upload_ui("do_upload")),
      tabItem("t_do_convert", module_do_convert_ui("do_convert")),
      tabItem("t_do_qc",     module_do_qc_ui("do_qc")),

      # ---- Conductivity ----
      tabItem("t_cond_upload",  module_cond_upload_ui("cond_upload")),
      tabItem("t_cond_convert", module_cond_convert_ui("cond_convert")),
      tabItem("t_cond_qc",     module_cond_qc_ui("cond_qc"))
    )
  )
)
