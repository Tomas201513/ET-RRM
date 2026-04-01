# XLS-Validator - ODK XLSForm Validation Platform
# Main Shiny Application Entry Point

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
library(janitor)
library(openxlsx)

# Source all R modules
source("Kobo_XLSForm_Validator/R/utils/config.R")
source("Kobo_XLSForm_Validator/R/utils/file_utils.R")
source("Kobo_XLSForm_Validator/R/schema/validation_schema.R")
source("Kobo_XLSForm_Validator/R/editor/xlsform_reader.R")
source("Kobo_XLSForm_Validator/R/editor/change_tracker.R")
source("Kobo_XLSForm_Validator/R/editor/xlsform_writer.R")
source("Kobo_XLSForm_Validator/R/custom_rules/rule_registry.R")
source("Kobo_XLSForm_Validator/R/custom_rules/expression_validator.R")
source("Kobo_XLSForm_Validator/R/validate.R")

source("Kobo_XLSForm_Validator/R/custom_rules/selected_validation.R")
source("Kobo_XLSForm_Validator/R/custom_rules/brackets_connectors.R")
source("Kobo_XLSForm_Validator/R/custom_rules/choice_list_validation.R")
source("Kobo_XLSForm_Validator/R/custom_rules/comparisons.R")
source("Kobo_XLSForm_Validator/R/custom_rules/cross_sheet_refs.R")
source("Kobo_XLSForm_Validator/R/custom_rules/no_spaces_inside.R")

# Source Shiny modules
source("Kobo_XLSForm_Validator/modules/mod_upload.R")
source("Kobo_XLSForm_Validator/modules/mod_issues_log.R")
source("Kobo_XLSForm_Validator/modules/mod_spreadsheet.R")
source("Kobo_XLSForm_Validator/modules/mod_export.R")
source("Kobo_XLSForm_Validator/modules/mod_rule_config.R")
source("Kobo_XLSForm_Validator/modules/mod_cleaning_panel.R")
source("KoboXLSForm_to_PaperForm/mod_paper_generator.R")

# Application configuration
app_config <- get_config()

# Suppress warnings
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# UI Definition
ui <- bslib::page_navbar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    bg = "#ffffff",
    fg = "#333333",
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto")
  ),
  
  # Custom CSS
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$style(HTML("
      body { 
        font-family: 'Segoe UI', Arial, sans-serif; 
        background-color: #f5f5f5;
      }
      
      /* Cards */
      .card {
        border-radius: 4px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        border: 1px solid #e0e0e0;
        margin-bottom: 20px;
      }
      
      .card-header {
        background-color: #f8f9fa;
        border-bottom: 1px solid #e0e0e0;
        font-weight: 500;
        padding: 8px 15px;
      }
      
      .card-body {
        padding: 20px;
      }
      
      /* Buttons */
      .btn-primary {
        background-color: #2c3e50;
        border-color: #2c3e50;
      }
      
      .btn-primary:hover {
        background-color: #1a252f;
        border-color: #1a252f;
      }
      
      .btn-success {
        background-color: #27ae60;
        border-color: #27ae60;
      }
      
      .btn-success:hover {
        background-color: #229954;
        border-color: #229954;
      }
      
      /* Status messages */
      .status-success { 
        background-color: #d4edda; 
        padding: 12px; 
        border-radius: 4px; 
        margin-bottom: 15px; 
        border-left: 4px solid #28a745;
        font-weight: 500;
      }
      
      .status-error { 
        background-color: #f8d7da; 
        padding: 12px; 
        border-radius: 4px; 
        margin-bottom: 15px; 
        border-left: 4px solid #dc3545;
        font-weight: 500;
      }
      
      .status-info { 
        background-color: #d1ecf1; 
        padding: 12px; 
        border-radius: 4px; 
        margin-bottom: 15px; 
        border-left: 4px solid #17a2b8;
        font-weight: 500;
      }
      
      /* Navbar */
      .navbar {
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      
      .navbar-brand {
        font-weight: 500;
        font-size: 1.2rem;
      }
      
      /* Tabs */
      .nav-tabs {
        border-bottom: 1px solid #dee2e6;
      }
      
      .nav-tabs .nav-link {
        color: #555;
        border: none;
        padding: 10px 15px;
        margin-right: 5px;
      }
      
      .nav-tabs .nav-link:hover {
        border-color: transparent;
        color: #2c3e50;
        background-color: #f8f9fa;
      }
      
      .nav-tabs .nav-link.active {
        color: #2c3e50;
        font-weight: 500;
        border-bottom: 2px solid #2c3e50;
        background-color: transparent;
      }
      
      /* Sub-tabs (nav-pills) */
      .nav-pills .nav-link {
        color: #555;
        border-radius: 4px;
        padding: 8px 15px;
        margin-right: 5px;
      }
      
      .nav-pills .nav-link:hover {
        background-color: #e9ecef;
        color: #2c3e50;
      }
      
      .nav-pills .nav-link.active {
        background-color: #2c3e50;
        color: white;
      }
      
      /* Tables */
      .dataTables_wrapper {
        margin-top: 10px;
      }
      
      table.dataTable {
        border-collapse: collapse;
        font-size: 13px;
      }
      
      table.dataTable thead th {
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6;
        font-weight: 500;
        padding: 10px;
      }
      
      table.dataTable tbody td {
        padding: 8px 10px;
        border-bottom: 1px solid #f0f0f0;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #f5f5f5;
      }
      
      /* Badges */
      .badge {
        padding: 4px 8px;
        border-radius: 3px;
        font-weight: 400;
      }
      
      .badge-success {
        background-color: #27ae60;
        color: white;
      }
      
      .badge-warning {
        background-color: #f39c12;
        color: white;
      }
      
      /* Iframe */
      .iframe-container {
        position: relative;
        overflow: hidden;
        width: 100%;
        padding-top: 56.25%;
      }
      
      .responsive-iframe {
        position: absolute;
        top: 0;
        left: 0;
        bottom: 0;
        right: 0;
        width: 100%;
        height: 100%;
        border: none;
      }
      
      .iframe-card-body {
        padding: 0 !important;
        height: calc(100vh - 200px);
        min-height: 600px;
      }
      
      .iframe-loading {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
        min-height: 400px;
        background-color: #f8f9fa;
        color: #6c757d;
      }
      
      /* Utilities */
      .mt-20 {
        margin-top: 20px;
      }
      
      .mb-20 {
        margin-bottom: 20px;
      }
    "))
  ),
  
  shinyjs::useShinyjs(),
  
  # ==================== MAIN TAB 1: XLS Validator ====================
  bslib::nav_panel(
    title = "XLS Validator",
    icon = icon("check-circle"),
    
    # Sub-tabs within XLS Validator
    bslib::navset_card_tab(
      id = "validator_subtabs",
      
      # ---------- Sub-tab 1: About ----------
      bslib::nav_panel(
        title = "About",
        icon = icon("info-circle"),
        fluidRow(
          column(
            width = 6,
            # div(class = "container-fluid", style = " margin: 0 auto; padding: 0px 0px;",
            div(class = "card",
                # div(class = "card-header", icon("info-circle"), " About XLS-Validator"),
                div(class = "card-body", 
                    style = "height: 60vh;",
                    
                    h6("What it does:"),
                    tags$ul(
                      tags$li(icon("check-circle"), " Upload XLSForm files for instant validation"),
                      tags$li(icon("check-circle"), " Navigate directly to issue rows in the spreadsheet"),
                      tags$li(icon("check-circle"), " Edit cells in-browser and apply fixes"),
                      tags$li(icon("check-circle"), " Export corrected forms ready for deployment")
                    ),
                    
                    
                    
                )
                # )
            )
            
            ),
          
          column(
            width = 6,
            div(class = "card",
                # div(class = "card-header", icon("info-circle"), "Custom Validation Rules (28 total):"),
                div(class = "card-body",
                    style = "height: 60vh;",
                    h6("Custom Validation Tool:"),
                    
                    h6("Steps to Use the Validator:"),
                    tags$ol(
                      tags$li(
                        strong("Step 1: Validate with ODK XLSForm Online Tool"),
                        br(),
                        "Upload your XLSForm in the ODK XLSForm Online tool and fix all structural or conversion issues there. ",
                        "You can access the tool from the next tab."
                      ),
                      tags$li(
                        strong("Step 2: Run Custom Validation & Cleaning"),
                        br(),
                        "After resolving all ODK validation issues, return here to run additional custom checks and clean your form."
                      )
                    )
                )
            )
          ))
      ),
      
      # In your UI, use a button or link to open in new tab
      bslib::nav_panel(
        title = "Step 1: XLSForm Online tool",
        icon = icon("book-open"),
        
        div(
          class = "container-fluid",
          style = "max-width: 100vw; margin: 0 auto; padding: 20px;
           display: flex; flex-direction: column; 
           align-items: center; justify-content: center; text-align: center;",
          

          br(),
          
          actionButton(
            inputId = "open_odk_link",
            label = "Open ODK XLSForm Tool in New Tab",
            icon = icon("external-link-alt"),
            class = "btn-primary btn-md",
            onclick = "window.open('https://getodk.org/xlsform/', '_blank')"
          ),
          
          br(), br(),
          
          p(
            "Alternatively, you can copy and paste this URL:",
            tags$code("https://getodk.org/xlsform/")
          )
        )
      )
      
      
      
      ,
      
      # ---------- Sub-tab 2: Step 2 (Upload, Issues, Cleaning) ----------
      bslib::nav_panel(
        title = "Step 2: Custom validation & cleaning tool",
        icon = icon("file-excel"),
        
        # Upload section at top
        div(class = "container-fluid", style = "max-width: 100vw; margin: 0 auto; padding: 0px 0px;",
            # div(class = "card",
            #     # div(class = "card-header", icon("upload"), " Upload XLSForm"),
            #     div(class = "card-body",
            fluidRow(
              column(
                width = 6,
                mod_upload_ui("upload")
              ),
              column(
                width = 6,
                #   
                #   # Status message
                div(class = "container-fluid", style = "max-width: 100vw; margin: 0 auto; padding: 0 0px; font-size: 0.75rem;",
                    uiOutput("status_message")
                )
                #   # helpText(
                #   #   icon("info-circle"),
                #   #   "Upload your XLSForm file to start validation.",
                #   #   br(),
                #   #   "Use the tabs below to review issues and clean your form."
                #   # )
              )
            )
            #     )
            # )
        ),
        
        
        
        # Issues and Cleaning tabs
        div(class = "container-fluid", style = "max-width: 100vw; margin: 0 0; padding: 0px 0px;",
            bslib::navset_card_tab(
              id = "validator_tabs",
              height = "auto",
              
              # Issues Tab
              bslib::nav_panel(
                title = "Issues",
                icon = icon("exclamation-triangle"),
                mod_issues_log_ui("issues")
              ),
              
              # Cleaning Tab
              bslib::nav_panel(
                title = "Cleaning",
                icon = icon("broom"),
                
                mod_cleaning_panel_ui("cleaning"),
                div(class = "card",
                    div(class = "card-header", icon("download"), " Export"),
                    div(class = "card-body",
                        mod_export_ui("export")
                    )
                )

              )
            )
        )
      )
    )
  ),
  
  # ==================== MAIN TAB 2: New Empty Tab ====================
  # In your main app.R file, update the "New Tab" section:
  
  # ==================== MAIN TAB 2: Paper Form Generator ====================
  bslib::nav_panel(
    title = "Paper Form Generator",
    icon = icon("file-alt"),
    add_loading_js(),
    mod_paper_form_generator_ui("paper_form")
  )

)

# Server Definition
server <- function(input, output, session) {
  
  # ==================== Paper Form Generator Module ====================
  mod_paper_form_generator_server("paper_form")
  
  # Reactive values for status
  values <- reactiveValues(
    warnings = character()
  )
  
  # Capture warnings
  capture_warning <- function(warning_msg) {
    values$warnings <- c(values$warnings, warning_msg)
  }
  
  # Reactive config
  config <- shiny::reactive({
    get_config()
  })
  
  # Issue status tracking (for marking fixed/ignored)
  issue_status <- shiny::reactiveVal(list())
  
  # Shared validation results - can be updated by upload or re-validation
  shared_validation_results <- shiny::reactiveVal(NULL)
  
  # Re-validation in progress flag
  revalidating <- shiny::reactiveVal(FALSE)
  
  # Working data - live edited version (separate from original upload)
  working_data <- shiny::reactiveVal(NULL)
  
  # Change tracker - shared across modules
  shared_change_tracker <- shiny::reactiveVal(create_change_tracker())
  
  # Status message
  output$status_message <- renderUI({
    if (is.null(shared_validation_results())) {
      return(div(class = "status-info",
                 icon("info-circle"),
                 " Upload an XLSForm file to start validation."))
    }
    
    results <- shared_validation_results()
    if (is.null(results)) return(NULL)
    
    errors <- results$summary$errors
    warnings <- results$summary$warnings
    
    if (errors > 0) {
      div(class = "status-error",
          icon("exclamation-triangle"),
          sprintf(" Validation complete: %d error(s), %d warning(s). Fix issues before exporting.", errors, warnings))
    } else if (warnings > 0) {
      div(class = "status-warning",
          icon("warning"),
          sprintf(" Validation complete: %d warning(s). Review before exporting.", warnings))
    } else {
      div(class = "status-success",
          icon("check-circle"),
          " Validation complete: No errors or warnings found! Ready to export.")
    }
  })
  
  # Upload module
  upload <- mod_upload_server("upload", config)
  
  # Initialize working_data when new file is uploaded
  shiny::observeEvent(upload$xlsform_data(), {
    data <- upload$xlsform_data()
    if (!is.null(data)) {
      working_data(data)
      # Reset change tracker for new file
      shared_change_tracker(create_change_tracker())
    }
  })
  
  # Sync upload results to shared reactive
  shiny::observeEvent(upload$validation_results(), {
    shared_validation_results(upload$validation_results())
    # Reset issue status when new file is uploaded
    issue_status(list())
  })
  
  # Issues log module - uses shared results
  issues <- mod_issues_log_server(
    "issues",
    validation_results = shared_validation_results,
    issue_status = issue_status,
    is_revalidating = revalidating
  )
  
  # Spreadsheet module - uses working_data for live edits
  spreadsheet <- mod_spreadsheet_server(
    "spreadsheet",
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    selected_issue = issues$selected_issue,
    validation_results = shared_validation_results,
    change_tracker = shared_change_tracker
  )
  
  # Sync spreadsheet changes back to shared change tracker
  shiny::observe({
    tracker <- spreadsheet$change_tracker()
    if (!is.null(tracker)) {
      shared_change_tracker(tracker)
    }
  })
  
  # Cleaning panel module - pass issues module for Skip button integration
  cleaning <- mod_cleaning_panel_server(
    "cleaning",
    selected_issue = issues$selected_issue,
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    change_tracker = shared_change_tracker,
    validation_results = shared_validation_results,
    issues_module = issues
  )
  
  # Export module - uses shared results
  export <- mod_export_server(
    "export",
    xlsform_data = upload$xlsform_data,
    working_data = working_data,
    change_tracker = shared_change_tracker,
    config = config,
    validation_results = shared_validation_results
  )
  
  # Rule configuration module
  rule_config <- mod_rule_config_server("rule_config")
  
  # Handle re-validation request
  shiny::observeEvent(issues$revalidate_trigger(), {
    # Get working data (with live edits)
    data <- working_data()
    tracker <- shared_change_tracker()
    
    if (is.null(data)) {
      # shiny::showNotification(
      #   "No form loaded. Please upload an XLSForm first.",
      #   type = "warning"
      # )
      return()
    }
    
    # Show loading state
    revalidating(TRUE)
    
    tryCatch({
      # Apply any pending changes to working data
      updated_data <- data
      if (!is.null(tracker) && count_total_operations(tracker) > 0) {
        updated_data <- apply_changes(tracker, data)
        # Update working_data with applied changes
        working_data(updated_data)
      }
      
      # Run validation on updated data
      cfg <- config()
      new_results <- revalidate_xlsform(updated_data, tracker, cfg)
      
      # Update shared results
      shared_validation_results(new_results)
      
      # Reset issue statuses for fresh results
      issue_status(list())
      
      # Show result notification
      shiny::showNotification(
        sprintf("Re-validation complete: %d error(s), %d warning(s)",
                new_results$summary$errors, new_results$summary$warnings),
        type = if (new_results$summary$errors > 0) "warning" else "message",
        duration = 4
      )
    }, error = function(e) {
      shiny::showNotification(
        paste("Re-validation failed:", e$message),
        type = "error",
        duration = 6
      )
    }, finally = {
      revalidating(FALSE)
    })
  })
  
  # Config status indicator
  output$config_status <- shiny::renderUI({
    cfg <- config()
    validation <- validate_config(cfg)
    
    if (validation$valid) {
      div(
        class = "status-success",
        style = "margin-top: 10px;",
        icon("check-circle"),
        " System ready"
      )
    } else {
      div(
        class = "status-error",
        style = "margin-top: 10px;",
        icon("exclamation-triangle"),
        " Setup required"
      )
    }
  })
  
}

# Run the application
shiny::shinyApp(ui = ui, server = server)