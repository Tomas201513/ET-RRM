source(here::here("data_pipeline_apps/03. data_cleaning/3.3 data_cleaning/utils_cleaning.R"))
# -------------------------------------------------------------------------------------------------------------------------------------------
# data_cleaning_module.R
# Data Cleaning Module for Dashboard

# UI Module
dataCleaningUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    add_loading_js(),
    fluidRow(
    bslib::card(
      full_screen = FALSE,
      bslib::card_header(
        # class = "bg-info text-white",
        icon("upload"), "Upload & Settings"
      ),
      bslib::card(
        style = "height: 100%;",
        
        bslib::card_body(
          style = "height: 100%;",
          
          fluidRow(
            # LEFT SIDEBAR
            column(4,
                   fileInput(ns("kobo_file"), "Kobo Form (.xlsx)", 
                             accept = c(".xlsx", ".xls"))
            ),
            
            column(4,
                   fileInput(ns("raw_file"), "Raw Data (.xlsx)", 
                             accept = c(".xlsx", ".xls"))
            ),
            
            column(4,
                   fileInput(ns("fu_log_file"), "Cleaning Log (.xlsx)", 
                             accept = c(".xlsx", ".xls"))
            )
          ),
          
          fluidRow(
            # LEFT SIDEBAR
            
            column(12,
                   actionButton(ns("run_cleaning"), "Run Cleaning", 
                                class = "btn-primary", 
                                icon = icon("play"),
                                width = "100%"),
                   
                   hr(),
                   
                   conditionalPanel(
                     condition = paste0("output['", ns("cleaned_data_ready"), "']"),
                     downloadButton(ns("download_cleaned"), "Download Cleaned Data", 
                                    class = "btn-success",
                                    icon = icon("download"),
                                    width = "100%")
                   )
            )
          )
        )
      ))
          ),
          
          fluidRow(
            # LEFT SIDEBAR
            column(12,
                   
                   tabsetPanel(
                     tabPanel("Preview", 
                              h4("Cleaned Data Preview"),
                              DTOutput(ns("preview"))),
                     
                     tabPanel("Status",
                              h4("Processing Status"),
                              verbatimTextOutput(ns("status")),
                              hr(),
                              h4("Cleaning Summary"),
                              verbatimTextOutput(ns("summary"))),
                     
                     tabPanel("Raw Data Preview",
                              h4("Raw Data (First 100 rows)"),
                              DTOutput(ns("raw_preview"))),
                     
                     tabPanel("Cleaning Logs",
                              h4("FU Cleaning Log"),
                              DTOutput(ns("fu_log_preview")),
                              hr(),
                              h4("Other Cleaning Log"),
                              DTOutput(ns("other_log_preview")))
                   )
            )
          )
      )
          
      
      }

# Server Module
dataCleaningServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for status
    status_text <- reactiveVal("Waiting for file uploads...")
    cleaned_result_reactive <- reactiveVal(NULL)
    
    # Store global variables for the module
    kobo_choices_local <- NULL
    other_db_local <- NULL
    
    # Helper functions (defined inside module to avoid conflicts)
    get_value_from_uuid <- function(uuid, column, raw_dataset) {
      value <- raw_dataset[[column]][raw_dataset$uuid == uuid]
      if (length(value) == 0) return(NA)
      return(value)
    }
    
    add_choice <- function(concat_value, choice) {
      if (is.null(concat_value) || is.na(concat_value) || concat_value == "") {
        return(choice)
      }
      l <- str_split(concat_value, " ")[[1]]
      l <- l[l != "" & !is.na(l)]
      l <- sort(unique(c(l, choice)))
      l <- l[l != ""]
      if (length(l) == 0) return(NA_character_)
      return(paste(l, collapse = " "))
    }
    
    remove_choice <- function(concat_value, choice) {
      if (is.null(concat_value) || is.na(concat_value) || concat_value == "") {
        return(NA_character_)
      }
      l <- str_split(concat_value, " ")[[1]]
      l <- l[l != choice & l != "" & !is.na(l)]
      if (length(l) == 0) return(NA_character_)
      return(paste(l, collapse = " "))
    }
    
    get_name_from_label <- function(list_name, label, kobo_choices) {
      if (is.null(kobo_choices)) return(NA)
      result <- kobo_choices$name[kobo_choices$list_name == list_name & kobo_choices$`label::english` == label]
      if (length(result) == 0) return(NA)
      return(result[1])
    }
    
    get_name_from_labell <- function(list_name, label, kobo_choices) {
      if (is.null(kobo_choices)) return(NA)
      normalize <- function(x) {
        x <- tolower(x)
        x <- gsub("[^a-z0-9]+", "_", x)
        x <- gsub("^_|_$", "", x)
        x <- trimws(x)
        return(x)
      }
      
      label_norm <- normalize(label)
      choices_norm <- normalize(kobo_choices$`label::english`)
      
      result <- kobo_choices$name[
        kobo_choices$list_name == list_name & 
          choices_norm == label_norm
      ]
      
      if (length(result) == 0) return(NA)
      if (length(result) > 1) {
        warning(paste("Multiple matches found for", list_name, label))
        return(result[1])
      }
      return(result)
    }
    
    select_multiple_add_remove <- function(el, to_remove, to_add = c(), raw_dataset, kobo_choices) {
      if (is.null(el) || is.null(el$ref_name) || is.null(el$uuid)) {
        return(data.frame())
      }
      
      cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(el$ref_name, "/"))]
      if (length(cols) == 0) {
        return(data.frame())
      }
      
      cl <- data.frame()
      old_concat <- get_value_from_uuid(el$uuid, el$ref_name, raw_dataset)
      
      if (is.na(old_concat) || old_concat == "") {
        new_concat <- ""
      } else {
        new_concat <- old_concat
      }
      
      if (length(to_remove) > 0 && !is.na(to_remove) && to_remove != "") {
        for (choice in to_remove) {
          if (!is.na(choice) && choice != "") {
            cl <- rbind(cl, data.frame(
              uuid = el$uuid, 
              question = paste0(el$ref_name, "/", choice),
              old_value = "1", 
              new_value = "0",
              stringsAsFactors = FALSE
            ))
            new_concat <- remove_choice(new_concat, choice)
          }
        }
      }
      
      if (length(to_add) > 0 && !is.na(to_add)) {
        for (choice in to_add) {
          if (!is.na(choice) && choice != "") {
            current_val <- get_value_from_uuid(el$uuid, paste0(el$ref_name, "/", choice), raw_dataset)
            cl <- rbind(cl, data.frame(
              uuid = el$uuid, 
              question = paste0(el$ref_name, "/", choice),
              old_value = ifelse(is.na(current_val), "0", current_val), 
              new_value = "1",
              stringsAsFactors = FALSE
            ))
            new_concat <- add_choice(new_concat, choice)
          }
        }
      }
      
      if (!is.na(new_concat) && new_concat != old_concat) {
        cl <- rbind(cl, data.frame(
          uuid = el$uuid, 
          question = el$ref_name, 
          old_value = old_concat, 
          new_value = trimws(new_concat),
          stringsAsFactors = FALSE
        ))
      }
      
      if (nrow(cl) > 0) {
        cl$change_type <- ifelse(is.null(el$change_type), "Recoding other response", el$change_type)
      }
      
      return(cl)
    }
    
    select_multiple_add_removee <- function(el, to_remove, to_add = c(), raw_dataset, kobo_choices) {
      if (is.null(el) || is.null(el$ref_name) || is.null(el$uuid)) {
        return(data.frame())
      }
      
      cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(el$ref_name, "/"))]
      if (length(cols) == 0) {
        return(data.frame())
      }
      
      cl <- data.frame()
      old_concat <- get_value_from_uuid(el$uuid, el$ref_name, raw_dataset)
      
      if (is.na(old_concat) || old_concat == "") {
        new_concat <- ""
      } else {
        new_concat <- old_concat
      }
      
      if (length(to_remove) > 0 && !is.na(to_remove) && to_remove != "") {
        for (choice in to_remove) {
          if (!is.na(choice) && choice != "") {
            cl <- rbind(cl, data.frame(
              uuid = el$uuid, 
              question = paste0(el$ref_name, "/", choice),
              old_value = "1", 
              new_value = "0",
              stringsAsFactors = FALSE
            ))
            new_concat <- remove_choice(new_concat, choice)
          }
        }
      }
      
      if (length(to_add) > 0 && !is.na(to_add)) {
        for (choice in to_add) {
          if (!is.na(choice) && choice != "") {
            cl <- rbind(cl, data.frame(
              uuid = el$uuid, 
              question = paste0(el$ref_name, "/", choice),
              old_value = NA, 
              new_value = "1",
              stringsAsFactors = FALSE
            ))
            new_concat <- add_choice(new_concat, choice)
          }
        }
      }
      
      if (!is.na(new_concat) && new_concat != old_concat) {
        cl <- rbind(cl, data.frame(
          uuid = el$uuid, 
          question = el$ref_name, 
          old_value = old_concat, 
          new_value = trimws(new_concat),
          stringsAsFactors = FALSE
        ))
      }
      
      if (nrow(cl) > 0) {
        cl$change_type <- ifelse(is.null(el$change_type), "Recoding other response", el$change_type)
      }
      
      return(cl)
    }
    
    add_to_cleaning_log_other_remove <- function(x, raw_dataset, other_db, kobo_choices) {
      if (is.null(other_db)) return(data.frame())
      
      change_type <- "Removing other response"
      option_other <- other_db$option_other[other_db$name == x$name]
      if (length(option_other) == 0 || is.na(option_other)) {
        return(data.frame())
      }
      
      var_option_other <- paste0(x$ref_name, "/", option_other)
      cl <- data.frame()
      
      cl <- rbind(cl, data.frame(
        uuid = x$uuid, question = x$name, change_type = change_type, 
        old_value = x$response_eth, new_value = NA, stringsAsFactors = FALSE
      ))
      
      if (!is.na(x$ref_type) && x$ref_type == "select_one") {
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$ref_name, change_type = change_type, 
          old_value = option_other, new_value = NA, stringsAsFactors = FALSE
        ))
      } else if (!is.na(x$ref_type) && x$ref_type == "select_multiple") {
        old_concat_value <- get_value_from_uuid(x$uuid, x$ref_name, raw_dataset)
        if (length(old_concat_value) == 0) old_concat_value <- NA
        old_concat_value <- as.character(old_concat_value)
        
        if (is.na(old_concat_value) || old_concat_value == "") {
          new_concat_value <- NA
        } else {
          new_concat_value <- remove_choice(old_concat_value, option_other)
        }
        
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$ref_name, change_type = change_type,
          old_value = old_concat_value, new_value = new_concat_value, stringsAsFactors = FALSE
        ))
        
        if (is.na(new_concat_value)) {
          cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(x$ref_name, "/"))]
          if (length(cols) > 0) {
            old_values <- raw_dataset[raw_dataset$uuid == x$uuid, cols, drop = FALSE]
            old_values <- as.character(unlist(old_values))
            for (i in 1:length(cols)) {
              cl <- rbind(cl, data.frame(
                uuid = x$uuid, question = cols[i], change_type = change_type, 
                old_value = old_values[i], new_value = NA, stringsAsFactors = FALSE
              ))
            }
          }
        } else {
          cl <- rbind(cl, data.frame(
            uuid = x$uuid, question = var_option_other, change_type = change_type,
            old_value = "1", new_value = "0", stringsAsFactors = FALSE
          ))
        }
      }
      
      return(cl)
    }
    
    add_to_cleaning_log_other_recode <- function(x, raw_dataset, other_db, kobo_choices) {
      if (is.null(other_db)) return(data.frame())
      
      if (!is.na(x$ref_type) && x$ref_type == "select_one") {
        change_type <- "Recoding other response"
        cl <- data.frame()
        
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$name, change_type = change_type, 
          old_value = x$response_eth, new_value = NA, stringsAsFactors = FALSE
        ))
        
        new_value <- get_name_from_labell(x$list_name, x$existing_other, kobo_choices)
        if (length(new_value) == 1 && !is.na(new_value)) {
          cl <- rbind(cl, data.frame(
            uuid = x$uuid, question = x$ref_name, change_type = change_type,
            old_value = get_value_from_uuid(x$uuid, x$ref_name, raw_dataset), 
            new_value = new_value, stringsAsFactors = FALSE
          ))
        }
        return(cl)
        
      } else if (!is.na(x$ref_type) && x$ref_type == "select_multiple") {
        change_type <- "Recoding other response"
        option_other <- other_db$option_other[other_db$name == x$name]
        if (length(option_other) == 0) return(data.frame())
        
        cl <- data.frame()
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$name, change_type = change_type, 
          old_value = x$response_eth, new_value = NA, stringsAsFactors = FALSE
        ))
        
        existing_other_clean <- x$existing_other
        if (!is.na(existing_other_clean)) {
          choices <- unlist(lapply(str_split(existing_other_clean, ";")[[1]], 
                                   function(c) get_name_from_label(x$list_name, trimws(c), kobo_choices)))
          choices <- choices[!is.na(choices)]
          
          if (length(choices) > 0) {
            el <- list(uuid = x$uuid, ref_name = other_db$ref_question[other_db$name == x$name][1], change_type = change_type)
            if (!is.null(el$ref_name) && !is.na(el$ref_name)) {
              additional_cl <- select_multiple_add_remove(el, to_remove = c(option_other), to_add = choices, raw_dataset, kobo_choices)
              if (nrow(additional_cl) > 0) {
                cl <- rbind(cl, additional_cl)
              }
            }
          }
        }
        return(cl)
      }
      return(data.frame())
    }
    
    add_to_cleaning_log_new_choice <- function(x, raw_dataset, other_db, kobo_choices) {
      if (is.null(other_db)) return(data.frame())
      
      if (!is.na(x$ref_type) && x$ref_type == "select_one") {
        change_type <- "Recoding other response"
        cl <- data.frame()
        
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$name, change_type = change_type, 
          old_value = x$response_eth, new_value = NA, stringsAsFactors = FALSE
        ))
        
        new_value <- x$col_to_add
        if (length(new_value) == 1 && !is.na(new_value)) {
          cl <- rbind(cl, data.frame(
            uuid = x$uuid, question = x$ref_name, change_type = change_type,
            old_value = "other", new_value = new_value, stringsAsFactors = FALSE
          ))
        }
        return(cl)
        
      } else if (!is.na(x$ref_type) && x$ref_type == "select_multiple") {
        change_type <- "Recoding other response"
        option_other <- x$col_to_add
        if (is.na(option_other)) return(data.frame())
        
        cl <- data.frame()
        cl <- rbind(cl, data.frame(
          uuid = x$uuid, question = x$name, change_type = change_type, 
          old_value = x$response_eth, new_value = NA, stringsAsFactors = FALSE
        ))
        
        true_other_clean <- x$true_other
        if (!is.na(true_other_clean)) {
          choices <- unlist(lapply(str_split(true_other_clean, ";")[[1]],
                                   function(c) get_name_from_label(x$list_name, trimws(c), kobo_choices)))
          choices <- choices[!is.na(choices)]
          
          el <- list(uuid = x$uuid, ref_name = other_db$ref_question[other_db$name == x$name][1], change_type = change_type)
          if (!is.null(el$ref_name) && !is.na(el$ref_name)) {
            additional_cl <- select_multiple_add_removee(el, to_remove = "other", to_add = option_other, raw_dataset, kobo_choices)
            if (nrow(additional_cl) > 0) {
              cl <- rbind(cl, additional_cl)
            }
          }
        }
        return(cl)
      }
      return(data.frame())
    }
    
    create_clean_data <- function(raw_dataset, raw_data_uuid_column, cleaning_log,
                                  cleaning_log_uuid_column, cleaning_log_question_column,
                                  cleaning_log_new_value_column, cleaning_log_change_type_column,
                                  change_response_value, NA_response_value, no_change_value,
                                  remove_survey_value) {
      
      cleaned_data <- raw_dataset
      
      for (i in 1:nrow(cleaning_log)) {
        log_entry <- cleaning_log[i, ]
        
        if (log_entry[[cleaning_log_change_type_column]] == change_response_value) {
          row_idx <- which(cleaned_data[[raw_data_uuid_column]] == log_entry[[cleaning_log_uuid_column]])
          col_idx <- which(names(cleaned_data) == log_entry[[cleaning_log_question_column]])
          
          if (length(row_idx) > 0 && length(col_idx) > 0) {
            cleaned_data[row_idx, col_idx] <- log_entry[[cleaning_log_new_value_column]]
          }
        } else if (log_entry[[cleaning_log_change_type_column]] == NA_response_value) {
          row_idx <- which(cleaned_data[[raw_data_uuid_column]] == log_entry[[cleaning_log_uuid_column]])
          col_idx <- which(names(cleaned_data) == log_entry[[cleaning_log_question_column]])
          
          if (length(row_idx) > 0 && length(col_idx) > 0) {
            cleaned_data[row_idx, col_idx] <- NA
          }
        } else if (log_entry[[cleaning_log_change_type_column]] == remove_survey_value) {
          cleaned_data <- cleaned_data[cleaned_data[[raw_data_uuid_column]] != log_entry[[cleaning_log_uuid_column]], ]
        }
      }
      
      return(cleaned_data)
    }
    
    get_ref_question <- function(x) {
      x.1 <- str_split(x, "\\{")[[1]][2]
      return(str_split(x.1, "\\}")[[1]][1])
    }
    
    create_other_db_from_kobo <- function(kobo_survey, kobo_choices) {
      kobo_survey <- kobo_survey %>% 
        mutate(
          q_type = stringr::word(type, 1),
          list_name = stringr::word(type, 2)
        )
      
      other_labels <- kobo_survey %>% 
        filter(grepl("^other_", name) | grepl("_other$", name)) %>%
        filter(type == "text" & !name %in% c("pho_enumerator", "mail_enumerator")) %>%
        mutate(ref_question = as.character(lapply(relevant, get_ref_question))) %>%
        mutate(ref_question = ifelse(is.na(ref_question), name, ref_question)) %>%
        select(name, ref_question) %>%
        left_join(select(kobo_survey, ref_question = name, full_label = `label::english`), by = "ref_question")
      
      other_db_result <- other_labels %>% 
        left_join(select(kobo_survey, name, q_type, list_name), by = c("ref_question" = "name")) %>% 
        left_join(select(kobo_survey, name, relevant), by = "name") %>% 
        mutate(option_other = str_replace_all(str_extract(relevant, "\'.*\'"), "'", "")) %>% 
        select(-relevant)
      
      kobo_choices_sub <- filter(kobo_choices, list_name %in% other_db_result$list_name)
      for (r in 1:nrow(other_db_result)) {
        if (!is.na(other_db_result$option_other[r])) {
          kobo_choices_sub <- kobo_choices_sub %>% 
            filter(!(list_name == other_db_result$list_name[r] & name == other_db_result$option_other[r]))
        }
      }
      
      other_db_result <- other_db_result %>% 
        left_join(
          select(kobo_choices_sub, list_name, label = "label::english") %>% 
            group_by(list_name) %>% 
            summarise(num_choices = n(), choices = paste0(label, collapse = ";;")),
          by = "list_name"
        )
      
      return(other_db_result)
    }
    
    apply_others_cleaning_log <- function(dataset, uuid_col = "uuid",
                                          or_remove, or_recode, or_true, 
                                          other_db, kobo_choices) {
      
      all_log_entries <- data.frame()
      
      # Process true other to get new column names
      or_true_filtered <- or_true %>% filter(uuid %in% dataset$uuid)
      
      if (nrow(or_true_filtered) > 0) {
        res <- or_true_filtered %>% 
          filter(!is.na(true_other)) %>%
          filter(q_type == "select_multiple") %>%
          group_by(list_name) %>% 
          mutate(n_list = n()) %>% 
          group_by(list_name, true_other) %>% 
          summarise(n_list = n_list[1], n_option = n(), .groups = 'drop') %>%
          arrange(-n_list, -n_option) %>% 
          select(-n_list) %>%
          mutate(
            col_to_add = paste0(
              list_name, "/", 
              str_replace_all(
                tolower(true_other), "[^a-z0-9 /]", ""
              ) %>%
                str_replace_all("/", "_or_") %>%
                str_replace_all(" \\(.*", "") %>%
                str_replace_all(" ", "_")
            ))
        
        new_col_names <- unique(res$col_to_add)
        
        for (col in new_col_names) {
          if (!col %in% names(dataset)) {
            dataset[[col]] <- NA
          }
        }
        
        ress <- res %>%
          mutate(list_name_other = paste0(list_name, "_other"))
        
        reordered_cols <- c()
        for (col in names(dataset)) {
          reordered_cols <- c(reordered_cols, col)
          if (col %in% ress$list_name_other) {
            new_col <- ress$col_to_add[ress$list_name_other == col]
            if (length(new_col) > 0 && !is.na(new_col)) {
              reordered_cols <- c(reordered_cols, new_col)
            }
          }
        }
        
        reordered_cols <- reordered_cols[reordered_cols %in% names(dataset)]
        dataset <- dataset %>% select(all_of(reordered_cols))
      }
      
      or_remove_sheet <- or_remove
      or_recode_sheet <- or_recode
      or_true_sheet <- or_true %>%
        mutate(
          col_to_add = paste0(str_replace_all(tolower(true_other), "[^a-z0-9 ]", "") %>% str_replace_all(" ", "_"))
        )
      
      # Handle remove
      if (nrow(or_remove_sheet) > 0) {
        for (r in 1:nrow(or_remove_sheet)) {
          remove_log <- add_to_cleaning_log_other_remove(or_remove_sheet[r, ], dataset, other_db, kobo_choices)
          if (nrow(remove_log) > 0) {
            all_log_entries <- dplyr::bind_rows(all_log_entries, remove_log)
          }
        }
      }
      
      # Handle recoding
      if (nrow(or_recode_sheet) > 0) {
        for (r in 1:nrow(or_recode_sheet)) {
          recode_log <- add_to_cleaning_log_other_recode(or_recode_sheet[r, ], dataset, other_db, kobo_choices)
          if (nrow(recode_log) > 0) {
            all_log_entries <- dplyr::bind_rows(all_log_entries, recode_log)
          }
        }
      }
      
      # Handle true other
      if (nrow(or_true_sheet) > 0) {
        for (r in 1:nrow(or_true_sheet)) {
          true_log <- add_to_cleaning_log_new_choice(or_true_sheet[r, ], dataset, other_db, kobo_choices)
          if (nrow(true_log) > 0) {
            all_log_entries <- dplyr::bind_rows(all_log_entries, true_log)
          }
        }
      }
      
      if (nrow(all_log_entries) > 0) {
        all_log_entries <- all_log_entries %>% distinct()
      }
      
      # Apply changes to dataset
      for (r in 1:nrow(all_log_entries)) {
        col_name <- all_log_entries$question[r]
        uuid_val <- all_log_entries[[uuid_col]][r]
        new_val <- all_log_entries$new_value[r]
        
        if (col_name %in% names(dataset)) {
          row_idx <- which(dataset[[uuid_col]] == uuid_val)
          if (length(row_idx) > 0) {
            dataset[[col_name]][row_idx] <- new_val
          }
        }
      }
      
      return(list(cleaned_data = dataset, cleaning_log_other = all_log_entries))
    }
    
    # Preview raw data
    output$raw_preview <- renderDT({
      req(input$raw_file)
      raw <- read_xlsx(input$raw_file$datapath)
      datatable(head(raw, 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    
    # Preview FU log
    output$fu_log_preview <- renderDT({
      req(input$fu_log_file)
      fu <- readxl::read_xlsx(input$fu_log_file$datapath, sheet = "cleaning_log")
      datatable(head(fu, 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    
    # Preview other log
    output$other_log_preview <- renderDT({
      req(input$fu_log_file)
      other <- readxl::read_xlsx(input$fu_log_file$datapath, sheet = "cleaning_log_other")
      datatable(head(other, 100), options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    
    # Run cleaning when button is clicked
    observeEvent(input$run_cleaning, {
      req(input$kobo_file, input$raw_file, input$fu_log_file)
      
      status_text("Starting cleaning process...")
      
      tryCatch({
        # Read Kobo files
        status_text("Reading Kobo form...")
        kobo_survey_local <- read_xlsx(input$kobo_file$datapath, sheet = "survey") %>%
          rename_with(tolower) %>%
          filter(!is.na(name)) %>%
          mutate(
            q_type = sapply(type, function(x) strsplit(x, " ")[[1]][1]),
            list_name = sapply(type, function(x) strsplit(x, " ")[[1]][2])
          )
        
        kobo_choices_local <- read_xlsx(input$kobo_file$datapath, sheet = "choices") %>%
          rename_with(tolower) %>%
          filter(!is.na(list_name)) %>%
          select(list_name, name, `label::english`) %>%
          distinct()
        
        # Read data files
        status_text("Reading data files...")
        raw_data_local <- read_xlsx(input$raw_file$datapath)
        fu_log_local <- readxl::read_xlsx(input$fu_log_file$datapath, sheet = "cleaning_log")
        other_log_local <- readxl::read_xlsx(input$fu_log_file$datapath, sheet = "cleaning_log_other")
        
        # Step 1: Apply FU cleaning
        status_text("Applying FU cleaning log...")
        clean_data <- create_clean_data(
          raw_dataset = raw_data_local,
          raw_data_uuid_column = "uuid",
          cleaning_log = fu_log_local,
          cleaning_log_uuid_column = "uuid",
          cleaning_log_question_column = "question",
          cleaning_log_new_value_column = "new_value",
          cleaning_log_change_type_column = "change_type",
          change_response_value = "change_response",
          NA_response_value = "blank_response",
          no_change_value = "no_action",
          remove_survey_value = "remove_survey"
        )
        
        status_text(paste("FU cleaning complete. Rows:", nrow(clean_data)))
        
        # Step 2: Create other DB from Kobo
        status_text("Creating other database from Kobo form...")
        other_db_local <- create_other_db_from_kobo(kobo_survey_local, kobo_choices_local)
        
        # Step 3: Process other log
        status_text("Processing other responses log...")
        or <- other_log_local
        
        # Rename columns
        colnames(or)[str_starts(colnames(or), "TRUE")] <- "true_other"
        colnames(or)[str_starts(colnames(or), "EXISTING other 1")] <- "existing_other_1"
        colnames(or)[str_starts(colnames(or), "EXISTING other 2")] <- "existing_other_2"
        colnames(or)[str_starts(colnames(or), "EXISTING other 3")] <- "existing_other_3"
        colnames(or)[str_starts(colnames(or), "INVALID")] <- "invalid_other"
        colnames(or)[str_starts(colnames(or), "FOLLOW")] <- "fu_message"
        
        # Prepare three types of recoding
        or <- or %>% 
          unite(existing_other, c(existing_other_1, existing_other_2, existing_other_3), 
                sep = ";", remove = TRUE, na.rm = TRUE) %>% 
          mutate(existing_other = ifelse(existing_other == "", NA, existing_other)) %>% 
          left_join(select(other_db_local, name, ref_question, ref_type = q_type), 
                    by = c("question_name" = "name")) %>% 
          rename(name = "question_name", ref_name = "ref_question")
        
        # Get removed surveys
        removed_survey <- fu_log_local %>%
          filter(change_type == "remove_survey")
        
        # Filter to only include UUIDs that were NOT removed
        or <- or %>% filter(!uuid %in% removed_survey$uuid)
        
        or_recode_all <- filter(or, !is.na(existing_other))
        or_remove_all <- filter(or, !is.na(invalid_other))
        or_true_all <- filter(or, !is.na(true_other))
        
        status_text(paste(
          "Split results - Recode:", nrow(or_recode_all),
          "Remove:", nrow(or_remove_all),
          "True other:", nrow(or_true_all)
        ))
        
        # Apply other cleaning
        status_text("Applying other responses cleaning...")
        result <- apply_others_cleaning_log(
          dataset = clean_data,
          uuid_col = "uuid",
          or_remove = or_remove_all,
          or_recode = or_recode_all,
          or_true = or_true_all,
          other_db = other_db_local,
          kobo_choices = kobo_choices_local
        )
        
        cleaned_result_reactive(result)
        
        status_text(paste(
          "✓ Cleaning complete! Final rows:", nrow(result$cleaned_data),
          "Cleaning log entries:", nrow(result$cleaning_log_other)
        ))
        
        # Update summary
        output$summary <- renderPrint({
          cat("CLEANING SUMMARY\n")
          cat(strrep("=", 50), "\n\n")
          cat("Original data rows:", nrow(raw_data_local), "\n")
          cat("Cleaned data rows:", nrow(result$cleaned_data), "\n")
          cat("Rows removed:", nrow(raw_data_local) - nrow(result$cleaned_data), "\n\n")
          
          if (nrow(result$cleaning_log_other) > 0) {
            cat("Cleaning actions:\n")
            print(table(result$cleaning_log_other$change_type))
          }
        })
        
      }, error = function(e) {
        status_text(paste("✗ Error:", e$message))
        cleaned_result_reactive(NULL)
      })
    })
    
    # Preview cleaned data
    output$preview <- renderDT({
      req(cleaned_result_reactive())
      datatable(head(cleaned_result_reactive()$cleaned_data, 100), 
                options = list(scrollX = TRUE, pageLength = 10),
                rownames = FALSE)
    })
    
    # Show status
    output$status <- renderPrint({
      cat(status_text())
    })
    
    # Check if cleaned data is ready for download
    output$cleaned_data_ready <- reactive({
      !is.null(cleaned_result_reactive()) && !is.null(cleaned_result_reactive()$cleaned_data)
    })
    outputOptions(output, "cleaned_data_ready", suspendWhenHidden = FALSE)
    
    # Download handler
    output$download_cleaned <- downloadHandler(
      filename = function() {
        paste0("cleaned_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(cleaned_result_reactive())
        openxlsx::write.xlsx(
          list(
            cleaned_data = cleaned_result_reactive()$cleaned_data,
            cleaning_log_other = cleaned_result_reactive()$cleaning_log_other
          ),
          file
        )
      }
    )
  })
}
