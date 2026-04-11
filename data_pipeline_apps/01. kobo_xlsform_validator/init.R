source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/utils/config.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/utils/file_utils.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/schema/validation_schema.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/editor/xlsform_reader.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/editor/change_tracker.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/editor/xlsform_writer.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/rule_registry.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/expression_validator.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/validate.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/selected_validation.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/brackets_connectors.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/choice_list_validation.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/comparisons.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/cross_sheet_refs.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/R/custom_rules/no_spaces_inside.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_upload.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_issues_log.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_spreadsheet.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_export.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_rule_config.R"))
source(here::here("data_pipeline_apps/01. kobo_xlsform_validator/modules/mod_cleaning_panel.R"))


xlsform_validatorModuleUI <- function(id) {
  ns <- NS(id)
  
  # Sub-tabs within XLS Validator
  bslib::navset_card_tab(
    id = "validator_subtabs",
    # ---------- Sub-tab 1: About ----------
    bslib::nav_panel(
      title = "Info",
      icon = icon("info-circle"),
      fluidRow(
        column(
          width = 6,
          style = "border-right: 1px solid #ccc; padding-left: 20px;", # vertical line
          div(
            class = "card-body",
            style = "height: auto;",
            h6("What it does:"),
            tags$ul(
              tags$li(icon("check-circle"), " Upload XLSForm files for instant validation"),
              tags$li(icon("check-circle"), " Navigate directly to issue rows in the spreadsheet"),
              tags$li(icon("check-circle"), " Edit cells in-browser and apply fixes"),
              tags$li(icon("check-circle"), " Export corrected forms ready for deployment")
            )
          )
        ),
        column(
          width = 6,
          div(
            class = "card-body",
            style = "height: auto;",
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
      )
    ),
    
    # ---------- Sub-tab 2: Step 1 (ODK XLSForm Online Tool) ----------
    bslib::nav_panel(
      title = "Step 1: XLSForm Online tool",
      icon = icon("book-open"),
      
      div(
        class = "container-fluid",
        style = "max-width: 100vw; margin: 0 auto; padding: 20px;
           display: flex; flex-direction: column;
           align-items: center; justify-content: center; text-align: center;",
        
        br(),
        br(),
        
        actionButton(
          inputId = "open_odk_link",
          label = "Open ODK XLSForm Tool in New Tab",
          icon = icon("external-link-alt"),
          class = "btn-primary btn-sm",
          onclick = "window.open('https://getodk.org/xlsform/', '_blank')"
        ),
        
        br(),
        
        p(
          "Alternatively, you can copy and paste this URL:",
          tags$code("https://getodk.org/xlsform/")
        )
      )
    )
    ,
    
    # ---------- Sub-tab 3: Step 2 (Upload, Issues, Cleaning) ----------
    bslib::nav_panel(
      title = "Step 2: Custom validation & cleaning tool",
      icon = icon("file-excel"),
      
          fluidRow(
            
            column(width = 6, 
              mod_upload_ui(ns("upload")), ), 
            
          column(width = 6,
            div(class = "container-fluid", 
                style = "margin-bottom: 20px; margin-top: 28px;
                font-size: 0.75rem;", 
                uiOutput(ns("status_message")))
          )), 
      
      # Issues and Cleaning tabs
        bslib::navset_card_tab(
          id = "validator_tabs",
          height = "auto",
          
          # Issues Tab
          bslib::nav_panel(
            title = "Issues",
            icon = icon("exclamation-triangle"),
            mod_issues_log_ui(ns("issues"))
          ),
          
          # Cleaning Tab
          bslib::nav_panel(
            title = "Cleaning",
            icon = icon("broom"),
            
            mod_cleaning_panel_ui(ns("cleaning")),
            div(
              class = "card",
              div(class = "card-header", icon("download"), " Export"),
              div(class = "card-body", mod_export_ui(ns("export")))
            )
          )
        )
    )
  )
  
}

mod_xlsform_validator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ================= STATE =================
    values <- reactiveValues(warnings = character())
    
    config <- reactive({
      get_config()
    })
    
    issue_status <- reactiveVal(list())
    shared_validation_results <- reactiveVal(NULL)
    revalidating <- reactiveVal(FALSE)
    working_data <- reactiveVal(NULL)
    shared_change_tracker <- reactiveVal(create_change_tracker())
    
    # ================= STATUS MESSAGE =================
    output$status_message <- renderUI({
      if (is.null(shared_validation_results())) {
        return(div(
          class = "status-info",
          icon("info-circle"),
          " Upload an XLSForm file to start validation."
        ))
      }
      
      results <- shared_validation_results()
      if (is.null(results))
        return(NULL)
      
      errors <- results$summary$errors
      warnings <- results$summary$warnings
      
      if (errors > 0) {
        div(
          class = "status-error",
          icon("exclamation-triangle"),
          sprintf(
            " Validation complete: %d error(s), %d warning(s).",
            errors,
            warnings
          )
        )
      } else if (warnings > 0) {
        div(
          class = "status-warning",
          icon("warning"),
          sprintf(" Validation complete: %d warning(s).", warnings)
        )
      } else {
        div(class = "status-success",
            icon("check-circle"),
            " Validation complete: No issues!")
      }
    })
    
    # ================= SUB MODULES =================
    
    upload <- mod_upload_server("upload", config)
    
    observeEvent(upload$xlsform_data(), {
      data <- upload$xlsform_data()
      if (!is.null(data)) {
        working_data(data)
        shared_change_tracker(create_change_tracker())
      }
    })
    
    observeEvent(upload$validation_results(), {
      shared_validation_results(upload$validation_results())
      issue_status(list())
    })
    
    issues <- mod_issues_log_server(
      "issues",
      validation_results = shared_validation_results,
      issue_status = issue_status,
      is_revalidating = revalidating
    )
    
    spreadsheet <- mod_spreadsheet_server(
      "spreadsheet",
      xlsform_data = upload$xlsform_data,
      working_data = working_data,
      selected_issue = issues$selected_issue,
      validation_results = shared_validation_results,
      change_tracker = shared_change_tracker
    )
    
    observe({
      tracker <- spreadsheet$change_tracker()
      if (!is.null(tracker)) {
        shared_change_tracker(tracker)
      }
    })
    
    cleaning <- mod_cleaning_panel_server(
      "cleaning",
      selected_issue = issues$selected_issue,
      xlsform_data = upload$xlsform_data,
      working_data = working_data,
      change_tracker = shared_change_tracker,
      validation_results = shared_validation_results,
      issues_module = issues
    )
    
    mod_export_server(
      "export",
      xlsform_data = upload$xlsform_data,
      working_data = working_data,
      change_tracker = shared_change_tracker,
      config = config,
      validation_results = shared_validation_results
    )
    
    # ================= REVALIDATION =================
    
    observeEvent(issues$revalidate_trigger(), {
      data <- working_data()
      tracker <- shared_change_tracker()
      
      if (is.null(data))
        return()
      
      revalidating(TRUE)
      
      tryCatch({
        updated_data <- data
        
        if (!is.null(tracker) &&
            count_total_operations(tracker) > 0) {
          updated_data <- apply_changes(tracker, data)
          working_data(updated_data)
        }
        
        cfg <- config()
        new_results <- revalidate_xlsform(updated_data, tracker, cfg)
        
        shared_validation_results(new_results)
        issue_status(list())
        
        showNotification(
          sprintf(
            "Re-validation: %d errors, %d warnings",
            new_results$summary$errors,
            new_results$summary$warnings
          ),
          type = if (new_results$summary$errors > 0)
            "warning"
          else
            "message"
        )
        
      }, error = function(e) {
        showNotification(paste("Re-validation failed:", e$message), type = "error")
      }, finally = {
        revalidating(FALSE)
      })
    })
    
  })
}