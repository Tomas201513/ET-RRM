source(here::here("data_pipeline_apps/03. data_cleaning/3.1 cleaning_log_generator/data_cleaning_log.R"))
source(here::here("data_pipeline_apps/03. data_cleaning/3.1 cleaning_log_generator/utility_cleaning_log.R"))
source(here::here("data_pipeline_apps/03. data_cleaning/3.2 cleaning_log_validator/check_cleaning_log.R"))
source(here::here("data_pipeline_apps/03. data_cleaning/3.3 data_cleaning/data_cleaning_module.R"))

data_cleaningModuleUI <- function(id) {
  ns <- NS(id)
  
  bslib::navset_card_tab(
    id = "data_quality_sub_tabs",
    
    # ---------- Sub-tab 1: About ----------
    bslib::nav_panel(
      title = "Info",
      icon = icon("info-circle"),
      p("This section provides tools for generating and validating cleaning logs, as well as performing data cleaning tasks. Use the tabs above to navigate through the different functionalities.")
    ),
    nav_panel(
      title = "Generate Cleaning Log",
      icon = icon("file-alt"),
      add_loading_js(),
      mod_cleaning_log_generator_ui("cleaning_log")
    ),
    nav_panel(
      title = "Validate Cleaning Log",
      icon = icon("file-alt"),
      add_loading_js(),
      mod_check_cleaning_log_ui("check_cleaning_log")
    ),
    
    # ==================== MAIN TAB 3: Data Cleaning Tools ====================
    
    nav_panel(
      title = "Data cleaning tools",
      icon = icon("file-alt"),
      add_loading_js(),
      dataCleaningUI("cleaning_module")
    )
  )
}

data_cleaningModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_cleaning_log_generator_server("cleaning_log")
    dataCleaningServer("cleaning_module")
    mod_check_cleaning_log_server("check_cleaning_log")
    
    
  })
}