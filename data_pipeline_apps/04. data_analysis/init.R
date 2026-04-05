analysisModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
  

        
    bslib::navset_card_tab(
      id = "data_quality_sub_tabs",
      
      # ---------- Sub-tab 1: About ----------
      bslib::nav_panel(
        title = "About",
        icon = icon("info-circle"),
        p("This section allows you to perform data analysis using your KoboToolbox survey structure, cleaned dataset, and LOA file. Upload the required files and click 'Run Analysis' to generate results. You can preview the results in the table below and download them as an Excel file.")
        
      ),
      nav_panel(
        title = "Run Analysis",
        fluidRow(
          column(
            4,
        fileInput(ns("kobo_tool"), "Upload Kobo Tool (.xlsx)", accept = ".xlsx", width = "100%")
          )
        ,
        column(
          4,
        fileInput(ns("dataset"), "Upload Cleaned Dataset (.xlsx)", accept = ".xlsx", width = "100%")
        ),
        column(
          4,
        fileInput(ns("loa"), "Upload LOA File (.xlsx)", accept = ".xlsx", width = "100%")
        )
          ),
        fluidRow(
          # LEFT SIDEBAR
          column(
            6,
        actionButton(ns("run"), "Run Analysis", class = "btn-primary btn-sm flex-grow-1",
                     style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 3px;")
        
          )
        ,
        column(
          6,
        downloadButton(ns("download_results"), "Download Results", class = "btn-primary btn-sm flex-grow-1",
                       style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 3px;")
        )
          
        
        
        )
        ,
        
        
        fluidRow(
          # LEFT SIDEBAR
          column(
            12,
            card(
              title = "Results Preview",
              
              full_screen = FALSE,
              bslib::card_header(
                # class = "bg-info text-white",
                icon("upload"), "Upload & Settings"
              ),
              bslib::card_body(
              
        tableOutput(ns("results_preview"))
            ))
      ))
      ))
      
  )
}

analysisModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    results_data <- eventReactive(input$run, {
      req(input$kobo_tool, input$dataset, input$loa)
      
      # Read Kobo
      tool_survey <- read_excel(input$kobo_tool$datapath, sheet = "survey", col_types = "text") %>% 
        rename_with(tolower) %>%
        filter(!is.na(name)) %>%
        filter(!(type %in% c("begin_group","end_group","beging_repeat","end_repeat","note")))
      
      tool_choices <- read_excel(input$kobo_tool$datapath, sheet = "choices", col_types = "text") %>%
        rename_with(tolower) %>%
        filter(!is.na(list_name)) %>% 
        select(list_name, name, `label::english`) %>% 
        distinct()
      
      # Review labels
      test <- presentresults::review_kobo_labels(
        kobo_survey_sheet = tool_survey,
        kobo_choices_sheet = tool_choices,
        label_column = "label::english",
        exclude_type = c("begin_group","end_group","beging_repeat","end_repeat","note"),
        results_table = NULL
      )
      
      dictionary <- presentresults::create_label_dictionary(
        kobo_survey_sheet = tool_survey %>% filter(!(`label::english` %in% test$`label::english`)),
        kobo_choices_sheet = tool_choices,
        label_column = "label::english"
      )
      
      # Read dataset
      data_main <- read_excel(input$dataset$datapath)
      
      # Remove all-NA columns
      only_nas <- data_main %>%
        summarise(across(everything(), ~ all(is.na(.)))) %>%
        pivot_longer(everything()) %>%
        filter(value) %>%
        pull(name)
      
      data_main <- data_main[, !names(data_main) %in% only_nas]
      
      # Read LOA
      my_loa <- read_excel(input$loa$datapath)
      
      
      # Survey design
      my_design <- srvyr::as_survey_design(data_main, strata = "admin1")
      
      # Analysis
      my_results <- create_analysis(
        my_design,
        loa = my_loa,
        group_var = NULL,
        sm_separator = "/"
      )
      
      my_results_table <- my_results$results_table%>%
        distinct(
          analysis_type,
          analysis_var,
          analysis_var_value,
          group_var_value,
          .keep_all = TRUE
        )
      
      # Add labels
      label_results <- add_label_columns_to_results_table(
        results_table = my_results_table,
        dictionary = dictionary
      )%>%
        distinct(
          analysis_type,
          analysis_var,
          analysis_var_value,
          group_var,
          group_var_value,
          .keep_all = TRUE
        )
      
      # # Final table
      # label_results_final <- label_results %>%
      #   create_table_variable_x_group(
      #     analysis_key = "label_analysis_key",
      #     value_columns = c("stat","stat_low","stat_upp","n","n_total")
      #   )
      
      return(label_results)
    })
    
    output$results_preview <- renderTable({
      head(results_data(), 5)
    })
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("analysis_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        write.xlsx(results_data(), file)
        # writexl::write_xlsx(results_data(), file)
      }
    )
    
  })
}
