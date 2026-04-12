source(here::here("data_pipeline_apps/03. data_cleaning/3.2 cleaning_log_validator/util_check_cleaning_log.R"))

mod_check_cleaning_log_ui <- function(id) {
  ns <- NS(id)
  # Cleaning Log Reviewr UI
  bslib::nav_panel(
    title = "Cleaning Log Reviewer",
    icon = shiny::icon("check"),
    
    # =======================
    # TOP TOOLBAR (HORIZONTAL)
    # =======================
    fluidRow(
    
      bslib::card(
        bslib::card_header(
          # class = "bg-info text-white",
          icon("upload"),
          "Upload & Settings"
        ),

        bslib::card_body(

          bslib::layout_columns(
            col_widths = c(6, 6),
            
            fileInput(
              ns("kobo_file"),
              label = "Kobo Form (.xlsx)",
              # corrected spelling
              accept = c(".xlsx", ".xls", ".xlsm")
            ),
            
            fileInput(
              ns("cl_file"),
              label = "Cleaning Log (.xlsx)",
              # corrected spelling
              accept = c(".xlsx", ".xls", ".xlsm")
            )
            ,
           
          ),
          bslib::layout_columns(
            col_widths = c(6,6),
            div(
              style = "position: relative;",
              
              actionButton(
                ns("run_check"),
                "Run Validation",
                icon = icon("play"),
                class = "btn-success btn-sm flex-grow-1", 
                style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 3px;"),
              
              # Hidden spinner that appears during download
              tags$div(
                id = ns("runing_spinner"),
                style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;",
                icon("spinner", class = "fa-spin fa-2x")
              )
            ),
           
            div(
              style = "position: relative;",
              
              downloadButton(ns("download"), "Download Log",  class = "btn-primary btn-sm flex-grow-1", 
                             style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 3px;"),
              # Hidden spinner that appears during download
              tags$div(
                id = ns("runing_spinner"),
                style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;",
                icon("spinner", class = "fa-spin fa-2x")
              )
            ),
            
            
          )
        )
      )
    ),
    
    # =======================
    # STATUS + TABLE (BOTTOM)
    # =======================
    fluidRow(

      bslib::card(
        full_screen = TRUE,
  
        bslib::card_header(
          # class = "bg-info text-white",
          icon("clipboard-check"),
          "Logs"
        ),
        # bslib::card_header(
        #   # shiny::icon("clipboard-check"),
        #   # "Validation Results",
        #   uiOutput("status")
        # ),
        # 
        bslib::card_body(
          min_height = "400px",
          DTOutput(ns("log_table"))
        )
      )
    )
  )
}

mod_check_cleaning_log_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store data and results
    rv <- reactiveValues(
      survey_df = NULL,
      choices_df = NULL,
      cleaning_log_df = NULL,
      validation_results = NULL
    )
    
    shinyjs::disable("download")
    
    # Observe file uploads and read data
    observeEvent(input$kobo_file, {
      req(input$kobo_file)
      rv$survey_df  <- read_xlsx(input$kobo_file$datapath, sheet = "survey")
      rv$choices_df <- read_xlsx(input$kobo_file$datapath, sheet = "choices")
    })
    
    observeEvent(input$cl_file, {
      req(input$cl_file)
      print("Cleaning log uploaded tobrbbbbbb")
      
            rv$cleaning_log_df <-  read_xlsx(input$cl_file$datapath, sheet = "cleaning_log")
      print(nrow(rv$cleaning_log_df))
    })
    
    
    result <- reactiveVal(NULL)
    is_running <- reactiveVal(FALSE)
    # Run validation when button is clicked
    observeEvent(input$run_check, {
      req(rv$survey_df, rv$choices_df, rv$cleaning_log_df)
      is_running(TRUE)   # ⬅️ START loading

      
      shinyjs::disable("run_check")
      # shinyjs::show("runing_spinner")
      session$sendCustomMessage("showDownloadLoading", list(
        button_id = session$ns("run_check")
      ))
      
      print("Running validation...")
      tryCatch({
        rv$validation_results <- check_cleaning_log(
          survey_df = rv$survey_df,
          choices_df = rv$choices_df,
          cleaning_log_df = rv$cleaning_log_df
        )
        
        shinyjs::enable("run_check")
        shinyjs::enable("download")
        session$sendCustomMessage("hideDownloadLoading", list(button_id = ns("run_check")))
        
        print("Validation completed successfully.")
        output$status <- renderUI({
          tags$p("Validation successful! No issues found.", class = "text-success")
        })
      }, error = function(e) {
        rv$validation_results <- data.frame(Error = e$message)
        
        shinyjs::enable("run_check")
        shinyjs::enable("download")
        session$sendCustomMessage("hideDownloadLoading", list(button_id = ns("run_check")))
        
        
        output$status <- renderUI({
          tags$p("Validation failed. See log for details.", class = "text-danger")
        })
      })
    })
    
    
    
    # Download log
    output$download <- downloadHandler(
      
      filename = function() {
        paste0("cleaning_log_validation_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        
        shinyjs::enable("download")
        session$sendCustomMessage("showDownloadLoading", list(button_id = ns("download")))
        
        res <-  rv$validation_results
        if (is.null(res)) return(NULL)
        
        df <- res$log
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Validation Log")
        
        openxlsx::writeData(wb, "Validation Log", df)
        
        # Define colors per rule_id
        rule_colors <- c(
          CL_INVALID_CHANGE_TYPE        = "#F3BEBD", 
          CL_NEW_VALUE_NOT_ALLOWED      = "#F1F1F1", 
          CL_QUESTION_NOT_IN_SURVEY     = "#F6E3E3", 
          CL_INVALID_SLASH_USAGE        = "#E7F3F9",
          CL_SELECT_MULTIPLE_BAD_CHOICE = "#F4F0E8", 
          CL_DUPLICATE_ACTION           = "#DAD9D9", 
          CL_REMOVE_SURVEY_CONFLICT     = "#FFF0CC", 
          CL_QUESTION_MISSING           = "#EAF4EA", 
          CL_SELECT_ONE_BAD_CHOICE      = "#EDE7F6", 
          CL_NUMERIC_NOT_NUMBER         = "#E6DDCA"  
        )
        
        # Apply row styles
        for (rule in names(rule_colors)) {
          rows <- which(df$rule_id == rule) + 1  # +1 for header row
          
          if (length(rows) > 0) {
            style <- openxlsx::createStyle(
              fgFill = rule_colors[[rule]]
            )
            
            openxlsx::addStyle(
              wb,
              sheet = "Validation Log",
              style = style,
              rows = rows,
              cols = 1:ncol(df),
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }
        
        # Optional: make header bold
        header_style <- openxlsx::createStyle(textDecoration = "bold")
        openxlsx::addStyle(
          wb,
          "Validation Log",
          style = header_style,
          rows = 1,
          cols = 1:ncol(df),
          gridExpand = TRUE
        )
        
        openxlsx::setColWidths(
          wb,
          "Validation Log",
          cols = 1:ncol(df),
          widths = "auto"
        )
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        session$sendCustomMessage("hideDownloadLoading", list(button_id = ns("download")))
        
      }
    )
    
    
    
    # Render validation results table
    # Log table
    output$log_table <- renderDT({
      res <- rv$validation_results
      if (is.null(res)) return(NULL)
      
      print("Validation results exist, preparing data frame...")
      df <- res$log
      # Define colors per rule_id
      rule_colors <- c(
        
        CL_INVALID_CHANGE_TYPE        = "#F3BEBD", 
        CL_NEW_VALUE_NOT_ALLOWED      = "#F1F1F1", 
        CL_QUESTION_NOT_IN_SURVEY     = "#F6E3E3", 
        CL_INVALID_SLASH_USAGE        = "#E7F3F9",
        CL_SELECT_MULTIPLE_BAD_CHOICE = "#F4F0E8", 
        CL_DUPLICATE_ACTION           = "#DAD9D9", 
        CL_REMOVE_SURVEY_CONFLICT     = "#FFF0CC", 
        CL_QUESTION_MISSING           = "#EAF4EA", 
        CL_SELECT_ONE_BAD_CHOICE      = "#EDE7F6", 
        CL_NUMERIC_NOT_NUMBER         = "#E6DDCA"  
        
        
      )
      
      print("Applying styles to log table...")
      datatable(
        df,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          columnDefs = list(
            list(
              targets = which(names(df) == "rule_id") - 1,  # 0-based index
              visible = FALSE
            )
          )
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(df),          # apply to whole row
          valueColumns = "rule_id",     # still available for styling
          backgroundColor = styleEqual(
            names(rule_colors),
            unname(rule_colors)
          )
        )
      
    })
    
    # Download log functionality (to be implemented)
  })
}