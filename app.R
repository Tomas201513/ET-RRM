rm(list = ls())

# install and Load required impact libraries locally
install.packages("local_packages/cleaningtools-master.zip", repos = NULL, type = "source")
install.packages("local_packages/impactR4PHU-main.zip", repos = NULL, type = "source")
install.packages("local_packages/analysistools-main.zip", repos = NULL, type = "source")
install.packages("local_packages/presentresults-main.zip", repos = NULL, type = "source")
library(cleaningtools)
library(impactR4PHU)
library(analysistools)
library(presentresults)

# Load required libraries
library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(rhandsontable)
library(readxl)
library(writexl)
library(dplyr)
library(tibble)
library(stringr)
library(processx)
library(shinycssloaders)
library(jsonlite)
library(openxlsx)
library(purrr)
library(tidyr)
library(crayon)
library(qdapRegex)
library(tidyverse)
library(srvyr)
library(svDialogs)

source("data_pipeline_apps/01. kobo_xlsform_validator/init.R")
source("data_pipeline_apps/03. data_cleaning/init.R")
source("data_pipeline_apps/02. xlsform_to_paperform/init.R")
source("data_pipeline_apps/04. data_analysis/init.R")


# Application configuration
app_config <- get_config()

# Suppress warnings
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",
  # Navbar colors
  "navbar-bg"                      = "#2C3E50",
  # white background
  "navbar-fg"                      = "#2C3E50",
  # brand/title color
  "navbar-light-color"             = "#ffffff",
  # link color
  "navbar-light-hover-color"       = "#2E99C6",
  # link hover
  "navbar-light-active-color"      = "#2E99C6",
  # active link
  "navbar-light-brand-color"       = "#ffffff",
  # # brand text/icon
  # "navbar-light-brand-hover-color" = "#2E99C6",
  # brand hover
  "navbar-light-toggler-border-color" = "transparent",
  "btn-light-hover-bg"              = "#2E99C6",
  # button hover
  "bg-success" = "#0C7669"  # success messages
  
  
)
# UI Definition
ui <- bslib::page_navbar(
  title = "ET-RRM",
  theme = theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # ==================== MAIN TAB 1: XLS Validator ====================
  bslib::nav_panel(
    title = "XLS Validator",
    icon = icon("check-circle"),
    add_loading_js(),
    xlsform_validatorModuleUI("validator")
  ),
  
  
  # ==================== MAIN TAB 2: Paper Form Generator ====================
  bslib::nav_panel(
    title = "Paper Form Generator",
    icon = icon("file-alt"),
    add_loading_js(),
    mod_paper_form_generator_ui("paper_form")
  ),
  
  # ==================== MAIN TAB 3: Data Cleaning ====================
  bslib::nav_panel(
    title = "Data Quality",
    icon = icon("chart-bar"),
    add_loading_js(),
    data_cleaningModuleUI("cleaning_module")
  ),
  
  # ==================== MAIN TAB 4: Data Analysis ====================
  bslib::nav_panel(
    title = "Data Analysis",
    icon = icon("chart-line"),
    add_loading_js(),
    analysisModuleUI("analysis1")
  ),
  
  nav_spacer(),
  
  div(
    class = "logo-pos",
    img(src = "banner.png", height = "35px")
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Call module servers
  mod_xlsform_validator_server("validator") # Call the server function of the XLSForm Validator module
  
  mod_paper_form_generator_server("paper_form") # Call the server function of the Paper Form Generator module
  
  mod_cleaning_log_generator_server("cleaning_log") # Call the server function of the Cleaning Log Generator module
  mod_check_cleaning_log_server("check_cleaning_log") # Call the server function of the Cleaning Log Validator module
  dataCleaningServer("cleaning_module") # Call the server function of the Data Cleaning module
  
  analysisModuleServer("analysis1") # Call the server function of the Data Analysis module
  
}

# Run the application
shiny::shinyApp(ui = ui, server = server)