source(here::here("data_pipeline_apps/03. data_cleaning/3.1 cleaning_log_generator/utility_cleaning_log.R"))

# Make sure to include all your helper functions in this file
# UI Module
mod_cleaning_log_generator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    tags$head(tags$style(
      HTML(
        "
    .sidebar-card {
      background: white;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      margin-bottom: 16px;
    }
    .step-badge {
      display: inline-block;
      background: #2c3e50;
      color: white;
      border-radius: 50%;
      width: 24px; height: 24px;
      text-align: center;
      line-height: 24px;
      font-size: 13px;
      font-weight: bold;
      margin-right: 6px;
    }
    .step-title { font-weight: 600; font-size: 15px; color: #2c3e50; }
    .log-box {
      background: #1e1e1e;
      border-radius: 6px;
      padding: 4px;
      height: 220px;
      overflow-y: auto;
    }
    .log-box pre {
      background: transparent;
      color: #d4d4d4;
      font-family: monospace;
      font-size: 12px;
      margin: 0;
      padding: 8px;
      border: none;
      white-space: pre-wrap;
      word-break: break-all;
      height: 100%;
      overflow-y: auto;
    }
    .summary-card {
      background: white; border-radius: 8px;
      padding: 16px; box-shadow: 0 2px 8px rgba(0,0,0,0.07);
      margin-bottom: 12px;
    }
    .metric-num { font-size: 32px; font-weight: 700; color: #2c3e50; }
    .metric-lbl { font-size: 13px; color: #7f8c8d; }
    .badge-check { background:#27ae60; color:white; padding:2px 8px; border-radius:12px; font-size:11px; }
    .badge-warn  { background:#e67e22; color:white; padding:2px 8px; border-radius:12px; font-size:11px; }
    .btn-run {
      background-color:#2c3e50; border-color:#2c3e50; color:white;
      width:100%; padding:12px; font-size:15px; font-weight:600;
      border-radius:6px; transition:all 0.2s;
    }
    .btn-run:hover:not(:disabled) { background-color:#1a252f; }
    .btn-run:disabled { opacity:0.55; cursor:not-allowed; }
    .btn-dl {
      background-color:#27ae60; border-color:#27ae60; color:white;
      width:100%; padding:10px; font-size:14px; font-weight:600;
      border-radius:6px; margin-top:6px;
    }
    .btn-dl:hover:not(:disabled) { background-color:#229954; }
    .btn-dl:disabled { opacity:0.55; cursor:not-allowed; }
    .tab-content { padding-top: 16px; }
  "
      )
    )),
    
    uiOutput(ns("summary_row")),
    fluidRow(
      # LEFT SIDEBAR
      column(
        4,
        div(
          class = "sidebar-card",
          div(
            span(class = "step-badge", "1"),
            span(class = "step-title", "Raw Dataset")
          ),
          hr(style = "margin:10px 0;"),
          fileInput(
            ns("raw_file"),
            NULL,
            accept = c(".xlsx", ".xls"),
            placeholder = "Single-sheet .xlsx",
            buttonLabel = "Browse"
          ),
          helpText(
            "Needs only one sheet with all raw data."
          ),
          uiOutput(ns("raw_info"))
        )),
      column(
        4,
        div(
          class = "sidebar-card",
          div(
            span(class = "step-badge", "2"),
            span(class = "step-title", "Logical Checks")
          ),
          hr(style = "margin:10px 0;"),
          fileInput(
            ns("checks_file"),
            NULL,
            accept = c(".xlsx", ".xls"),
            placeholder = "logical_checks.xlsx",
            buttonLabel = "Browse"
          ),
          helpText(
            "Needs sheets: 'checks' (select_one) and 'checks_sm' (select_multiple)"
          ),
          uiOutput(ns("checks_info"))
       ) ),
      column(
        4,
        div(
          class = "sidebar-card",
          div(
            span(class = "step-badge", "3"),
            span(class = "step-title", "KoBo Form")
          ),
          hr(style = "margin:10px 0;"),
          fileInput(
            ns("kobo_file"),
            NULL,
            accept = c(".xlsx", ".xls"),
            placeholder = "kobo_tool.xlsx",
            buttonLabel = "Browse"
          ),
          helpText("Needs sheets: 'survey' and 'choices'"),
          uiOutput(ns("kobo_info"))
        ))
      ),

    fluidRow(
      column(
        3,
        
        div(
          class = "sidebar-card",
          div(
            span(class = "step-badge", "4"),
            span(class = "step-title", "Options")
          ),
          hr(style = "margin:10px 0;"),
          textInput(ns("uuid_col"), "UUID column", value = "uuid"),
          textInput(ns("index_col"), "Index column", value = "index")
          )
        ),
      column(
        3,
          div(
            class = "sidebar-card",
            numericInput(
              ns("strongness"),
              "Outlier strongness factor",
              value = 3,
              min = 1,
              max = 10
            ),
          numericInput(
            ns("missing_factor"),
            "Missing % strongness factor",
            value = 2,
            min = 1,
            max = 10
          ),
          checkboxInput(ns("run_soft_dup"), "Run soft duplicate check", value = TRUE),
          checkboxInput(ns("run_others"), "Run other-responses check", value = TRUE))
    
      ),
      
      
      column(
        6,
        
        
        div(
          class = "sidebar-card",
          div(
            span(class = "step-badge", "5"),
            span(class = "step-title", "Output Folder")
          ),
          hr(style = "margin:10px 0;"),
          
          textInput(
            ns("info_cols"),
            "Extra info columns (comma-separated)",
            value = "index,admin1,admin2,admin3,enum_id,dc_modality,_submission_time",
            width = "100%"
          ),
          textInput(
            ns("out_dir"),
            NULL,
            value = normalizePath("~", mustWork = FALSE),
            placeholder = "Folder path for saved outputs",
            width = "100%"
          ),
          
          # actionButton(ns("browse_out"), "Browse Folder", icon = icon("folder-open"),
          #              style="width:100%; margin-bottom:8px;")
          actionButton(ns("run_btn"), "▶ Run Cleaning Checks", class = "btn-run"),
          br(),
          uiOutput(ns("dl_buttons"))
        )
        
        # ,
        #
        # div(class = "sidebar-card",
        #     actionButton(ns("run_btn"), "▶ Run Cleaning Checks", class = "btn-run"),
        #     br(),
        #     uiOutput(ns("dl_buttons"))
        # )
      )
    
    
    ),
    fluidRow(
      # MAIN PANEL
      column(
        12,
        # uiOutput(ns("summary_row")),
        tabsetPanel(
          id = ns("main_tabs"),
          tabPanel(
            "📋 Cleaning Log",
            div(
              class = "summary-card",
              fluidRow(column(6, uiOutput(
                ns("cl_filter_ui")
              )), column(
                6,
                div(style = "text-align:right; padding-top:22px;", uiOutput(ns("cl_count")))
              )),
              hr(style = "margin:10px 0;"),
              DTOutput(ns("cl_table"))
            )
          ),
          tabPanel("🔁 Other Responses", div(class = "summary-card", DTOutput(
            ns("others_table")
          ))),
          # tabPanel("📊 Check Summary",
          #          div(class="summary-card", DTOutput(ns("check_summary_table")))
          # ),
          tabPanel("🖥 Run Log", div(
            class = "summary-card",
            h5("Console output", style = "margin-top:0;"),
            div(class = "log-box", verbatimTextOutput(ns("log_output")))
          ))
        )
      )
    )
  )
}

# Server Module
mod_cleaning_log_generator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    rv <- reactiveValues(
      raw_path = NULL,
      raw_data = NULL,
      kobo_s = NULL,
      kobo_c = NULL,
      so_checks = NULL,
      sm_checks = NULL,
      other_db = NULL,
      cl = NULL,
      combined_obj = NULL,
      checked_dataset = NULL,
      others = NULL,
      other_df = NULL,
      check_sum = NULL,
      ready = FALSE,
      running = FALSE,
      log = character(0),
      cl_path = NULL,
      oth_path = NULL,
      cols_numeric = character(0)
    )
    
    # Helper functions (keep all your existing helper functions here)
    
    # logging helper
    log_msg <- function(msg, type = "info") {
      prefix <- switch(
        type,
        info = "ℹ ",
        success = "✅ ",
        warn = "⚠️ ",
        error = "❌ "
      )
      ts <- format(Sys.time(), "%H:%M:%S")
      rv$log <- c(rv$log, paste0("[", ts, "] ", prefix, msg))
    }
    
    # Your existing helper functions here:
    # - get_column_letter
    # - save_other_responses
    # - check_others_custom
    # - check_answer_in_list
    # - check_constraints
    # - get_ref_question
    # - get_name_from_label
    # - get_value_from_uuid
    # - add_choice
    # - remove_choice
    # - create_other_db_from_kobo
    # - read_raw_data_single
    # - save_other_responses_simple
    
    
    # File upload observers
    observeEvent(input$raw_file, {
      req(input$raw_file)
      tryCatch({
        rv$raw_path <- input$raw_file$datapath
        full_df <- read_excel(rv$raw_path, sheet = 1, col_types = "text")
        log_msg(paste0(
          "Raw data loaded: ",
          nrow(full_df),
          " rows, ",
          ncol(full_df),
          " columns"
        ),
        "success")
      }, error = function(e)
        log_msg(paste("Error reading raw data:", e$message), "error"))
    })
    
    observeEvent(input$checks_file, {
      req(input$checks_file)
      tryCatch({
        sheets <- excel_sheets(input$checks_file$datapath)
        if (!("checks" %in% sheets))
          log_msg("'checks' sheet missing!", "warn")
        if (!("checks_sm" %in% sheets))
          log_msg("'checks_sm' sheet missing!", "warn")
        rv$so_checks <- read_excel(input$checks_file$datapath,
                                   sheet = "checks",
                                   col_types = "text")
        rv$sm_checks <- read_excel(input$checks_file$datapath,
                                   sheet = "checks_sm",
                                   col_types = "text")
        log_msg(paste0(
          "Logical checks: ",
          nrow(rv$so_checks),
          " SO, ",
          nrow(rv$sm_checks),
          " SM"
        ),
        "success")
      }, error = function(e)
        log_msg(paste("Error reading checks:", e$message), "error"))
    })
    
    observeEvent(input$kobo_file, {
      log_msg(
        paste0(
          "-----------------------------------------------------\nLoading KoBo form..."
        ),
        "info"
      )
      
      req(input$kobo_file)
      tryCatch({
        kobo_s <- openxlsx::read.xlsx(input$kobo_file$datapath,
                                      sheet = "survey",
                                      colNames = TRUE) %>%
          rename_with(tolower) %>%
          filter(!is.na(name)) %>%
          mutate(
            q_type = sapply(type, function(x)
              strsplit(x, " ")[[1]][1]),
            list_name = sapply(type, function(x)
              strsplit(x, " ")[[1]][2])
          )
        
        kobo_c <- openxlsx::read.xlsx(input$kobo_file$datapath,
                                      sheet = "choices",
                                      colNames = TRUE) %>%
          rename_with(tolower) %>%
          filter(!is.na(list_name)) %>%
          select(list_name, name, `label::english`) %>%
          distinct()
        
        rv$kobo_s <- kobo_s
        rv$kobo_c <- kobo_c
        rv$cols_numeric <- filter(kobo_s, type %in% c("integer", "decimal")) %>% pull(name)
        
        log_msg(paste0(
          "KoBo loaded: ",
          nrow(kobo_s),
          " survey rows, ",
          nrow(kobo_c),
          " choices"
        ),
        "success")
        
        # Create other_db from kobo
        rv$other_db <<- create_other_db_from_kobo(kobo_s, kobo_c)
        
        if (nrow(rv$other_db) > 0) {
          log_msg(paste0(
            "Other DB created with ",
            nrow(rv$other_db),
            " 'other' questions"
          ),
          "success")
          log_msg(paste0(
            "Other DB sample:\n",
            paste(
              head(
                rv$other_db %>% select(name, ref_question, q_type, list_name),
                5
              ) %>% as.data.frame(),
              collapse = "\n"
            )
          ), "info")
          
        } else {
          log_msg("No 'other' questions found in the KoBo tool", "warn")
        }
        
        # # Constraint check
        # tryCatch({
        #   kobo_c_check <- kobo_c %>% select(list_name, name, label = `label::english`)
        #   issues <- check_constraints(kobo_s, kobo_c_check, kobo_s, kobo_c)
        #   if (length(issues) > 0)
        #     log_msg(paste("Constraint issues:", paste(issues, collapse = "; ")), "warn")
        #   else
        #     log_msg("KoBo constraints OK", "success")
        # }, error = function(e) log_msg(paste("Constraint check skipped:", e$message), "warn"))
        
      }, error = function(e)
        log_msg(paste("Error reading KoBo:", e$message), "error"))
    })
    
    # Browse output folder
    observeEvent(input$browse_out, {
      tryCatch({
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable()) {
          chosen <- rstudioapi::selectDirectory("Select output folder", path = input$out_dir)
          if (!is.null(chosen))
            updateTextInput(session, "out_dir", value = chosen)
        }
      }, error = function(e)
        NULL)
    })
    
    # File info UIs
    output$raw_info <- renderUI({
      req(rv$raw_path)
      n <- nrow(read_excel(rv$raw_path, sheet = 1, col_types = "text"))
      div(style = "font-size:12px; color:#27ae60;", icon("check"), sprintf(" %d rows loaded", n))
    })
    
    output$checks_info <- renderUI({
      req(rv$so_checks, rv$sm_checks)
      div(style = "font-size:12px; color:#27ae60;",
          icon("check"),
          sprintf(
            " %d SO + %d SM checks",
            nrow(rv$so_checks),
            nrow(rv$sm_checks)
          ))
    })
    
    output$kobo_info <- renderUI({
      req(rv$kobo_s)
      div(style = "font-size:12px; color:#27ae60;",
          icon("check"),
          sprintf(
            " %d questions, %d choices",
            nrow(rv$kobo_s),
            nrow(rv$kobo_c)
          ))
    })
    
    output$header_status <- renderUI({
      if (rv$running)
        return(span(style = "color:#f39c12; font-size:13px;", "⏳ Running checks..."))
      if (rv$ready)
        return(span(style = "color:#2ecc71; font-size:13px;", "✅ Cleaning log ready"))
      span(style = "color:#bdc3c7; font-size:13px;", "Upload files to begin")
    })
    
    # ── RUN button ──────────────────────────────────────────
    observeEvent(input$run_btn, {
      req(rv$raw_path,
          rv$so_checks,
          rv$sm_checks,
          rv$kobo_s,
          rv$kobo_c)
      if (rv$running)
        return()
      
      out_dir <- trimws(input$out_dir)
      if (!dir.exists(out_dir)) {
        showNotification("Output folder does not exist.",
                         type = "error",
                         duration = 4)
        return()
      }
      
      rv$running <- TRUE
      rv$ready   <- FALSE
      rv$cl      <- NULL
      rv$others  <- NULL
      rv$log     <- character(0)
      shinyjs::disable("run_btn")
      
      tryCatch({
        kobo_survey  <- rv$kobo_s
        kobo_choices <- rv$kobo_c
        uuid_col     <- trimws(input$uuid_col)
        info_cols    <- trimws(unlist(str_split(input$info_cols, ",")))
        
        # ── 1. Read raw data ─────────────────────────────────
        log_msg("Reading raw dataset...")
        raw <- read_raw_data_single(rv$raw_path, kobo_survey, rv$cols_numeric)
        log_msg(paste0("Raw data: ", nrow(raw), " rows"), "success")
        
        # Store raw data for other responses
        rv$raw_data <- raw
        
        # ── 2. Main cleaning checks ──────────────────────────
        log_msg("Running duplicate check...")
        cleaning_log <- raw %>%
          cleaningtools::check_duplicate(
            uuid_column      = uuid_col,
            columns_to_check = c(trimws(input$index_col)),
            log_name         = "duplicate_log_uuid"
          )
        
        if (isTRUE(input$run_soft_dup)) {
          log_msg("Running soft duplicate check...")
          cleaning_log <- cleaning_log %>%
            cleaningtools::check_soft_duplicates(
              kobo_survey      = kobo_survey,
              uuid_column      = uuid_col,
              idnk_value       = "dnk",
              sm_separator     = "/",
              log_name         = "soft_duplicate_log",
              return_all_results = FALSE
            )
        }
        
        log_msg("Running select_one logical checks...")
        so_list <- rv$so_checks
        if ("sheet" %in% names(so_list))
          so_list <- so_list %>% filter(sheet == "main") %>% select(-sheet, -any_of("order"))
        cleaning_log <- cleaning_log %>%
          cleaningtools::check_logical_with_list(
            uuid_column              = uuid_col,
            list_of_check            = so_list,
            check_id_column          = "check_id",
            check_to_perform_column  = "check_to_perform",
            columns_to_clean_column  = "columns_to_clean",
            description_column       = "description"
          )
        
        log_msg("Running value check (-999, 999, -888, 888)...")
        cleaning_log <- cleaning_log %>%
          cleaningtools::check_value(
            uuid_column  = uuid_col,
            element_name = "checked_dataset",
            values_to_look = c(-999, 999, -888, 888)
          )
        
        log_msg("Running outlier check...")
        cleaning_log <- cleaning_log %>%
          cleaningtools::check_outliers(
            uuid_column                      = uuid_col,
            element_name                     = "checked_dataset",
            kobo_survey                      = NULL,
            kobo_choices                     = NULL,
            cols_to_add_cleaning_log         = NULL,
            strongness_factor                = input$strongness,
            minimum_unique_value_of_variable = NULL,
            remove_choice_multiple           = TRUE,
            sm_separator                     = "/",
            columns_not_to_check             = NULL
          )
        
        # ── 3. Select_multiple logical checks ────────────────
        log_msg("Running select_multiple logical checks...")
        new_main      <- raw
        clean_names   <- gsub("/", "__", names(new_main))
        clean_names   <- make.unique(clean_names, sep = "__")
        names(new_main) <- clean_names
        
        sm_list <- rv$sm_checks
        cl_sm <- new_main %>%
          cleaningtools::check_logical_with_list(
            uuid_column              = uuid_col,
            list_of_check            = sm_list,
            check_id_column          = "check_id",
            check_to_perform_column  = "check_to_perform",
            columns_to_clean_column  = "columns_to_clean",
            description_column       = "description"
          )
        
        cleaning_log[["checked_dataset"]] <- bind_cols(cleaning_log[["checked_dataset"]], cl_sm[["checked_dataset"]] %>% select(starts_with("sm_check")))
        cleaning_log[["logical_all"]] <- bind_rows(cleaning_log[["logical_all"]], cl_sm[["logical_all"]])
        
        # ── 4. Missing percentage ────────────────────────────
        log_msg("Adding percentage missing...")
        cleaning_log$checked_dataset <- cleaning_log$checked_dataset %>%
          cleaningtools::add_percentage_missing(
            kobo_survey     = kobo_survey,
            type_to_include = c("integer", "select_one", "select_multiple")
          )
        cleaning_log <- cleaning_log %>%
          cleaningtools::check_percentage_missing(
            uuid_column      = uuid_col,
            column_to_check  = "percentage_missing",
            strongness_factor = input$missing_factor,
            log_name         = "percentage_missing_log"
          )
        
        # ── 5. Combine log ───────────────────────────────────
        log_msg("Combining cleaning log...")
        info_cols_present <- intersect(info_cols, names(cleaning_log[["checked_dataset"]]))
        
        combined <- cleaningtools::create_combined_log(cleaning_log, dataset_name = "checked_dataset") %>%
          cleaningtools::add_info_to_cleaning_log(
            dataset                   = "checked_dataset",
            cleaning_log              = "cleaning_log",
            dataset_uuid_column       = uuid_col,
            cleaning_log_uuid_column  = uuid_col,
            information_to_add        = info_cols_present
          )
        
        # ── 6. Enrich with kobo labels ───────────────────────
        log_msg("Adding question labels and choices...")
        combined$cleaning_log <- combined$cleaning_log %>%
          left_join(kobo_survey %>% select(name, `label::english`),
                    by = c("question" = "name")) %>%
          mutate(`FOLLOW-UP message` = NA_character_) %>%
          select(1:min(8, ncol(.)), `FOLLOW-UP message`, everything()) %>%
          select(1:min(3, ncol(.)), `label::english`, everything())
        
        survey_question <- kobo_survey %>%
          filter(grepl("^select_one", type)) %>%
          select(name, type, `label::english`) %>%
          mutate(list_name = gsub("^select_one\\s+", "", type)) %>%
          select(list_name, name)
        
        survey_choice <- kobo_choices %>%
          select(list_name, name, `label::english`) %>%
          filter(
            !str_detect(
              list_name,
              "^(l_admin|cluster_id|l_point_number|l_cluster_id|l_enum_id)"
            )
          )
        
        survey_choice$choice_string <- paste0("[",
                                              survey_choice$name,
                                              " : ",
                                              survey_choice$`label::english`,
                                              "]")
        
        grouped_choices <- survey_choice %>%
          group_by(list_name) %>%
          summarise(
            choices_joined = paste(choice_string, collapse = "  "),
            .groups = "drop"
          )
        
        survey_question <- survey_question %>%
          left_join(grouped_choices, by = "list_name") %>%
          select(name, choices_joined)
        
        combined$cleaning_log <- combined$cleaning_log %>%
          left_join(survey_question, by = c("question" = "name")) %>%
          rename(choices = choices_joined)
        # %>%
        #     mutate(question = ifelse(
        #         "id_most_similar_survey" %in% names(.) & !is.na(id_most_similar_survey),
        #         "soft_duplicate", question
        #     ))
        
        rv$cl           <- combined$cleaning_log
        rv$combined_obj <- combined
        log_msg(paste0("Cleaning log: ", nrow(rv$cl), " issues found"),
                "success")
        
        # ── 7. Check summaryc
        if ("issue_type" %in% names(rv$cl)) {
          rv$check_sum <- rv$cl %>%
            count(issue_type, name = "n_issues") %>%
            arrange(desc(n_issues))
        } else {
          rv$check_sum <- data.frame(note = "No issue_type column found in log")
        }
        # ── 7. Other responses ────────────────────────────────
        if (isTRUE(input$run_others)) {
          log_msg(
            paste0(
              "Checking other responses...",
              nrow(rv$other_db),
              "questions to check"
            ),
            "success"
          )
          log_msg(
            paste0(
              "2Checking other responses...",
              nrow(raw),
              "questions to check"
            ),
            "success"
          )
          
          # rv$other_df <- check_others_custom(raw, rv$other_db)
          
          
          
          
          data_sheet <- raw %>%
            distinct(uuid, .keep_all = TRUE)
          
          raw_data_sheet <- raw %>%
            distinct(uuid, .keep_all = TRUE)
          var_other_raw <- rv$other_db$name[rv$other_db$name %in% colnames(data_sheet)]
          other_responses_main <- data_sheet %>%
            select(c("uuid", all_of(var_other_raw))) %>%
            pivot_longer(
              cols = all_of(var_other_raw),
              names_to = "question_name",
              values_to = "response_eth"
            ) %>%
            filter(!is.na(response_eth)) %>%
            select(uuid, question_name, response_eth)
          other_responses <- other_responses_main %>%
            mutate(response_en = NA)
          
          df <- other_responses %>%
            left_join(
              select(
                data_sheet,
                uuid,
                `_submission_time`,
                admin1,
                admin2,
                admin3,
                enum_id
              ),
              by = "uuid"
            ) %>%
            mutate(`_submission_time` = format(as.Date(`_submission_time`), "%y:%m:%d")) %>%
            arrange(question_name, uuid) %>%
            left_join(
              select(
                rv$other_db,
                name,
                full_label,
                q_type,
                list_name,
                ref_question
              ),
              by = c("question_name" = "name")
            ) %>%
            select(
              uuid,
              `_submission_time`,
              admin1,
              admin2,
              admin3,
              enum_id,
              question_name,
              q_type,
              list_name,
              full_label,
              response_eth,
              response_en
            ) %>%
            mutate(
              "TRUE other (copy response_en or provide a better translation)" = NA,
              "EXISTING other 1 (select the most appropriate choice)" = NA,
              "EXISTING other 2 (select the most appropriate choice)" = NA,
              "EXISTING other 3 (select the most appropriate choice)" = NA,
              "INVALID other (select yes or leave blank)" = NA,
              "FOLLOW-UP message (what is unclear about this response?)" = NA,
              "Explanation" = NA,
              "_index" = NA,
              selected_choices = NA
              
            )
          if (nrow(df) > 0) {
            for (r in seq_len(nrow(df))) {
              ref_name <- rv$other_db$ref_question[rv$other_db$name == df$question_name[r]]
              q_type <- rv$other_db$q_type[rv$other_db$name == df$question_name[r]]
              
              if (!is.na(ref_name) && q_type == "select_multiple") {
                choices <- raw_data_sheet[[ref_name]][raw_data_sheet$uuid == df$uuid[r]]
                choices <- choices[1]
                paste0(choices)
                if (length(choices) > 0 && !is.na(choices[1])) {
                  choice_vec <- str_split(choices, " ")[[1]]
                  df$selected_choices[r] <- paste0(choice_vec, collapse = ";\n")
                }
              }
            }
          } else {
            cat(yellow("No other response log for the day:\n"))
            cat(green(submissions), "\n")
            cat(red("Please double check the dataset manually.\n"))
          }
          rv$other_df <- relocate(df, "selected_choices", .before = "response_eth")
          
          
          #############################################################################################################################################################
          log_msg("3Checking other responses...")
          if (!is.null(rv$other_df) && nrow(rv$other_df) > 0) {
            log_msg(paste0("Other responses: ", nrow(rv$other_df), " found"),
                    "success")
            rv$others <- rv$other_df
          } else {
            log_msg("No other responses found", "info")
            rv$others <- NULL
          }
        }
        
        rv$ready <- TRUE
        rv$running <- FALSE
        shinyjs::enable("run_btn")
        log_msg("Ready — click a download button to save outputs.",
                "success")
        showNotification(
          "✅ Checks complete! Use the download buttons to save.",
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        log_msg(paste("FATAL ERROR:", e$message), "error")
        rv$running <- FALSE
        shinyjs::enable("run_btn")
        showNotification(paste("Error:", e$message),
                         type = "error",
                         duration = 8)
      })
    })
    
    # Summary row
    output$summary_row <- renderUI({
      if (!rv$ready)
        return(NULL)
      cl <- rv$cl
      n_issues <- if (!is.null(cl))
        nrow(cl)
      else
        0
      n_oth <- if (!is.null(rv$others) &&
                   nrow(rv$others) > 0)
        nrow(rv$others)
      else
        0
      
      fluidRow(column(4, div(
        class = "summary-card",
        div(class = "metric-num", n_issues),
        div(class = "metric-lbl", "Total Issues")
      )), column(4, div(
        class = "summary-card",
        div(class = "metric-num", n_oth),
        div(class = "metric-lbl", "Other Responses")
      )), column(4, div(
        class = "summary-card",
        div(class = "metric-num", if (!is.null(cl) &&
                                      "uuid" %in% names(cl))
          n_distinct(cl$uuid)
          else
            "—"),
        div(class = "metric-lbl", "UUIDs Flagged")
      )))
    })
    
    # Download buttons
    output$dl_buttons <- renderUI({
      if (!rv$ready)
        return(NULL)
      tagList(
        div(
          style = "margin-top:10px;",
          actionButton(ns("save_cl"), "💾 Save & Open Cleaning Log", class = "btn-dl")
        ),
        if (!is.null(rv$others) && nrow(rv$others) > 0)
          div(
            style = "margin-top:6px;",
            actionButton(ns("save_oth"), "💾 Save & Open Other Responses", class = "btn-dl")
          )
      )
    })
    
    # Save cleaning log
    observeEvent(input$save_cl, {
      req(rv$combined_obj, rv$kobo_s, rv$kobo_c)
      out_dir <- trimws(input$out_dir)
      if (!dir.exists(out_dir)) {
        showNotification("Output folder does not exist.",
                         type = "error",
                         duration = 4)
        return()
      }
      out_date <- stringr::str_sub(stringr::str_remove_all(Sys.Date(), "-"), 3)
      cl_path  <- file.path(out_dir, paste0("cleaning_log_", out_date, ".xlsx"))
      log_msg("Saving cleaning log...")
      tryCatch({
        cleaningtools::create_xlsx_cleaning_log(
          rv$combined_obj,
          kobo_survey      = rv$kobo_s,
          kobo_choices     = rv$kobo_c,
          sm_dropdown_type = "logical",
          use_dropdown     = TRUE,
          output_path      = cl_path
        )
        log_msg(paste("Cleaning log saved:", cl_path), "success")
        
        # Try to open the file
        sys <- Sys.info()["sysname"]
        tryCatch({
          if (sys == "Windows")
            shell.exec(cl_path)
          else if (sys == "Darwin")
            system2("open", shQuote(cl_path))
          else
            system2("xdg-open", shQuote(cl_path))
        }, error = function(e)
          NULL)
        
        showNotification(paste("✅ Saved:", basename(cl_path)),
                         type = "message",
                         duration = 4)
      }, error = function(e) {
        log_msg(paste("Formatted save failed, using plain xlsx:", e$message),
                "warn")
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "cleaning_log")
        openxlsx::writeData(wb, "cleaning_log", rv$cl)
        if (!is.null(rv$others) && nrow(rv$others) > 0) {
          openxlsx::addWorksheet(wb, "other_responses")
          openxlsx::writeData(wb, "other_responses", rv$others)
        }
        openxlsx::saveWorkbook(wb, cl_path, overwrite = TRUE)
        log_msg(paste("Plain cleaning log saved:", cl_path), "success")
        showNotification(paste("✅ Saved:", basename(cl_path)),
                         type = "message",
                         duration = 4)
      })
      rv$cl_path <- cl_path
    })
    # Save other responses
    observeEvent(input$save_oth, {
      req(rv$others, rv$other_db)
      if (is.null(rv$others) || nrow(rv$others) == 0) {
        showNotification("No other responses to save.",
                         type = "warning",
                         duration = 3)
        return()
      }
      
      out_dir <- trimws(input$out_dir)
      if (!dir.exists(out_dir)) {
        showNotification("Output folder does not exist.",
                         type = "error",
                         duration = 4)
        return()
      }
      
      out_date <- str_sub(str_remove_all(Sys.Date(), "-"), 3)
      oth_path <- file.path(out_dir, paste0("other_responses_", out_date, ".xlsx"))
      
      tryCatch({
        ###################################################################################################################################################################
        log_msg("Saving other responses...")
        # save_other_responses(rv$other_df, oth_path)
        get_column_letter <- function(r) {
          return(ifelse(r <= 26, LETTERS[r], ifelse(
            r <= 52,
            paste0(LETTERS[1], LETTERS[r - 26]),
            paste0(LETTERS[2], LETTERS[r - 52])
          )))
        }
        
        # Save other responses
        wb <- createWorkbook()
        
        # Define the styles
        style.col.color <- createStyle(
          fgFill = "#DDDDDE",
          border = "TopBottomLeftRight",
          # sborderColour = "#000000",
          valign = "top",
          wrapText = T
        )
        style.col.color1 <- createStyle(
          fgFill = "#DBD5C6",
          border = "TopBottomLeftRight",
          borderColour = "#000000",
          valign = "top",
          wrapText = T
        )
        style.col.color2 <- createStyle(
          fgFill = "#F3F4F4",
          border = "TopBottomLeftRight",
          # borderColour = "#000000",
          valign = "top",
          wrapText = T
        )
        style.col.color.first <- createStyle(
          textDecoration = "bold",
          fgFill = "#F1797A",
          valign = "top",
          border = "TopBottomLeftRight",
          borderColour = "#000000",
          wrapText = T
        )
        log_msg("Styles defined, creating workbook...")
        # Add Worksheet
        df <- rv$other_df
        addWorksheet(wb, "cleaning_log")
        writeData(
          wb = wb,
          x = df,
          sheet = "cleaning_log",
          startRow = 1
        )
        
        # Apply the styles
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color.first,
          rows = 1:(nrow(df) + 1),
          cols = 1
        ) # First column color
        
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color2,
          rows = 1:(nrow(df) + 1),
          cols = 13
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color,
          rows = 1:(nrow(df) + 1),
          cols = 14
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color1,
          rows = 1:(nrow(df) + 1),
          cols = 15
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color1,
          rows = 1:(nrow(df) + 1),
          cols = 16
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color1,
          rows = 1:(nrow(df) + 1),
          cols = 17
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color,
          rows = 1:(nrow(df) + 1),
          cols = 18
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color2,
          rows = 1:(nrow(df) + 1),
          cols = 19
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color2,
          rows = 1:(nrow(df) + 1),
          cols = 20
        )
        log_msg("Styles applied, setting column widths and filters...")
        
        # Freeze the first row
        freezePane(wb, sheet = "cleaning_log", firstActiveRow = 2)
        
        # Add column filters
        addFilter(wb,
                  "cleaning_log",
                  rows = 1,
                  cols = 1:ncol(df))
        
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(wrapText = TRUE),
          rows = 1:19,
          cols = 1
        )
        
        
        log_msg("Adding dropdown values and data validation...")
        # Add Dropdown values worksheet and data validation
        addWorksheet(wb, "Dropdown_values")
        for (r in 1:nrow(rv$other_db)) {
          if (rv$other_db$q_type[r] != "text") {
            choices <- str_split(rv$other_db$choices[r], ";;")[[1]]
            writeData(wb,
                      sheet = "Dropdown_values",
                      x = choices,
                      startCol = r)
            uuids <- which(df$question_name == rv$other_db$name[r])
            if (length(uuids) > 0) {
              column_letter <- get_column_letter(r)
              values <- paste0(
                "'Dropdown_values'!$",
                column_letter,
                "$1:$",
                column_letter,
                "$",
                rv$other_db$num_choices[r]
              )
              dataValidation(
                wb,
                "cleaning_log",
                col = 15,
                rows = uuids + 1,
                type = "list",
                value = values
              )
              dataValidation(
                wb,
                "cleaning_log",
                col = 16,
                rows = uuids + 1,
                type = "list",
                value = values
              )
              dataValidation(
                wb,
                "cleaning_log",
                col = 17,
                rows = uuids + 1,
                type = "list",
                value = values
              )
              
            }
          }
        }
        # Find the column index
        target_col <- which(names(df) == "INVALID other (select yes or leave blank)")
        
        # Write dropdown values
        writeData(
          wb,
          sheet = "Dropdown_values",
          x = c("Yes"),
          startCol = r + 1
        )
        
        # Get Excel column letter
        column_letter <- get_column_letter(r + 1)
        
        # Create validation reference
        values <- paste0("'Dropdown_values'!$",
                         column_letter,
                         "$1:$",
                         column_letter,
                         "$1")
        log_msg("Dropdown values added, applying data validation...")
        # Apply data validation to the correct column
        dataValidation(
          wb,
          "cleaning_log",
          col = target_col,
          rows = 2:(nrow(df) + 1),
          type = "list",
          value = values
        )
        # Set column widths
        setColWidths(wb,
                     "cleaning_log",
                     cols = 1,
                     widths = 20)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 2,
                     widths = 18)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 3,
                     widths = 15)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 4,
                     widths = 15)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 5,
                     widths = 15)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 6,
                     widths = 13)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 7:10,
                     widths = 20)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 11,
                     widths = 35)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 12,
                     widths = 15)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 13,
                     widths = 35)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 14,
                     widths = 35)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 15:19,
                     widths = 30)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 19,
                     widths = 25)
        setColWidths(wb,
                     "cleaning_log",
                     cols = 20:26,
                     widths = 15)
        
        log_msg("Column widths set, applying text alignment and wrapping...")
        # Apply text alignment and wrapping
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 1
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 2
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 3
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 4
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 5
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 6
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(wrapText = T, valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 7
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(wrapText = T, valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 8
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(wrapText = T, valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 9
        )
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(wrapText = T, valign = "top"),
          rows = 1:(nrow(df) + 1),
          cols = 10
        )
        
        setRowHeights(wb,
                      "cleaning_log",
                      rows = 1,
                      heights = 15)
        
        # Bold first row
        addStyle(
          wb,
          "cleaning_log",
          style = createStyle(textDecoration = "bold"),
          rows = 1,
          cols = 1:ncol(df)
        )
        addStyle(
          wb,
          "cleaning_log",
          style = style.col.color.first,
          rows = 1,
          cols = 1:26
        )
        
        modifyBaseFont(
          wb,
          fontSize = 10,
          fontColour = "black",
          fontName = "Calibri"
        )
        
        log_msg("Styles applied, saving workbook...")
        
        # Define file name and save workbook
        # sub.filename = paste0(Sys.Date(), Region, "_other_responses.xlsx")
        saveWorkbook(wb, oth_path
                     , overwrite = T)
        log_msg(paste("Other responses saved:", oth_path), "success")
        ###################################################################################################################################################################
        
        showNotification(paste("✅ Saved:", basename(oth_path)),
                         type = "message",
                         duration = 4)
      }, error = function(e) {
        log_msg(paste("Error saving other responses:", e$message),
                "error")
        showNotification(paste("Error saving:", e$message),
                         type = "error",
                         duration = 4)
      })
    })
    
    # Tables
    output$cl_filter_ui <- renderUI({
      req(rv$cl, "issue_type" %in% names(rv$cl))
      types <- c("All", sort(unique(rv$cl$issue_type)))
      selectInput(ns("cl_filter"),
                  "Filter by issue type:",
                  choices = types,
                  width = "320px")
    })
    
    cl_filtered <- reactive({
      req(rv$cl)
      df <- rv$cl
      if (!is.null(input$cl_filter) &&
          input$cl_filter != "All" && "issue_type" %in% names(df))
        df <- df %>% filter(issue_type == input$cl_filter)
      df
    })
    
    output$cl_count <- renderUI({
      req(rv$cl)
      span(class = "badge-check", paste0(nrow(cl_filtered()), " rows"))
    })
    
    output$cl_table <- renderDT({
      req(rv$cl)
      datatable(
        cl_filtered(),
        filter = "top",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })
    
    output$others_table <- renderDT({
      req(rv$others)
      if (is.null(rv$others) || nrow(rv$others) == 0)
        return(datatable(data.frame(Note = "No other responses found.")))
      datatable(
        rv$others,
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })
    
    output$check_summary_table <- renderDT({
      req(rv$check_sum)
      datatable(
        rv$check_sum,
        rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })
    
    output$log_output <- renderText({
      if (length(rv$log) == 0)
        return("No log yet.")
      paste(rv$log, collapse = "\n")
    })
    
  })
}

# Function to add loading JS (include in your main app)
add_loading_js <- function() {
  tags$script(
    HTML(
      "
    $(document).on('shiny:connected', function() {
      var loading = $('<div id=\"loading-overlay\"><div class=\"spinner\"></div><p>Loading...</p></div>');
      $('body').append(loading);
      $(document).on('shiny:disconnected', function() {
        $('#loading-overlay').fadeOut();
      });
    });
  "
    )
  )
}