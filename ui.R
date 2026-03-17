# ui.R
# Waterlogged Shiny Application

# ------------------------------------------------
# Setup UI function for Shiny app
ui <- dashboardPage(
  title = "Waterlogged: Water Data Processing",
  skin = "blue",
  header = dashboardHeader(
    title = "Waterlogged",
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

    # Individual pages - right side tab content
    tabItems(
      tabItem("t_welcome", module_welcome_ui("welcome")),

      # ---- Temperature ----
      tabItem("t_temp_upload",   module_temp_upload_ui("temp_upload")),
      tabItem("t_temp_snap",     module_temp_snap_ui("temp_snap")),
      tabItem("t_temp_baro",     module_temp_baro_ui("temp_baro")),
      tabItem("t_temp_qc_air",   module_temp_qc_air_ui("temp_qc_air")),
      tabItem("t_temp_qc_water", module_temp_qc_water_ui("temp_qc_water")),

      # ---- Barometric Pressure ----
      tabItem("t_baro_upload", module_baro_upload_ui("baro_upload")),
      tabItem("t_baro_qc",    module_baro_qc_ui("baro_qc")),

      # ---- Water Level (Pressure) ----
      tabItem("t_wl_upload",  module_wl_upload_ui("wl_upload")),
      tabItem("t_wl_convert", module_wl_convert_ui("wl_convert")),
      tabItem("t_wl_qaqc",   module_wl_qaqc_ui("wl_qaqc")),
      tabItem("t_wl_adjust",  module_wl_adjust_ui("wl_adjust")),

      # ---- Dissolved Oxygen ----
      tabItem("t_do_upload",  module_do_upload_ui("do_upload")),
      tabItem("t_do_baro",   module_do_baro_ui("do_baro")),
      tabItem("t_do_convert", module_do_convert_ui("do_convert")),
      tabItem("t_do_qc",     module_do_qc_ui("do_qc")),

      # ---- Conductivity ----
      tabItem("t_cond_upload",  module_cond_upload_ui("cond_upload")),
      tabItem("t_cond_convert", module_cond_convert_ui("cond_convert"))
    )
  )
)
