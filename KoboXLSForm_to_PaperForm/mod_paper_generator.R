# Kobo Paper Form Generator Module
# This module generates printable paper forms from XLSForm files

mod_paper_form_generator_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid",
    style = "max-width: 100vw; margin: 0 auto; padding: 0px;",
    
    fluidRow(
      column(
        width = 6,
        bslib::card(
          full_screen = FALSE,
          bslib::card_header(
            # class = "bg-info text-white",
            icon("upload"), "Upload & Settings"
          ),
          bslib::card_body(
            # File uploads
            fluidRow(
              column(12, fileInput(ns("xlsform"), "📄 Kobo XLSForm (*.xlsx)", 
                                   width = "100%",
                                   accept = ".xlsx", buttonLabel = "Browse..."),),
              
              
              # Generate button with loading state
              div(style = "margin: 8px 0px;",
                  actionButton(ns("generate"), "Generate Paper Form", 
                               class = "btn-success btn-", width = "100%",
                               style = "font-size: 16px; padding: 12px 5px;") 
              )
            ),
          )
        )
      ),
      
      column(
        width = 6,
        bslib::card(
          full_screen = FALSE,
          bslib::card_header(
            # class = "bg-success text-white",
            icon("download"), "Output"
          ),
          bslib::card(
            full_screen = FALSE,
            # bslib::card_header(class = "bg-light", "Status"),
            verbatimTextOutput(ns("status"), placeholder = TRUE)
          ),
          bslib::card_body(
            
            
            fluidRow(
              column(6, 
                     # Download button with loading state
                     div(
                       style = "position: relative;",
                       downloadButton(ns("download"), "Download Excel Form", 
                                      class = "btn-primary btn-lg", 
                                      style = "width: 100%; margin-bottom: 15px; font-size: 16px; padding: 12px 5px;"),
                       # Hidden spinner that appears during download
                       tags$div(
                         id = ns("download_spinner"),
                         style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;",
                         icon("spinner", class = "fa-spin fa-2x")
                       )
                     )
              ),
              column(6, 
                     downloadButton(ns("download_admin"), "Download Admin List", 
                                    class = "btn-primary btn-lg", 
                                    style = "width: 100%; margin-bottom: 15px; font-size: 16px; padding: 12px 5px;") 
                     
              )
            ),
            
            # Progress bar for download
            fluidRow(
              column(12,
                     tags$div(
                       id = ns("progress_container"),
                       style = "display: none; margin-top: 10px;",
                       div(
                         class = "progress",
                         div(
                           id = ns("progress_bar"),
                           class = "progress-bar progress-bar-striped active",
                           role = "progressbar",
                           style = "width: 0%;",
                           "0%"
                         )
                       )
                     )
              )
            )
            
          )
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        width = 12,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            # class = "bg-secondary text-white",
            icon("eye"), 
            "Form Preview"
          ),
          bslib::card_body(
            DT::dataTableOutput(ns("preview_table"))
          )
        )
      )
    )
  )
}

mod_paper_form_generator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    values <- reactiveValues(
      wb = NULL, 
      paperForm = NULL, 
      admin_df = NULL, 
      generated = FALSE,
      form_title = "No form loaded",
      month_year = format(Sys.Date(), "%B %Y"),
      download_in_progress = FALSE
    )
    
    # Function to get banner image path from www folder
    get_banner_path <- function() {
      # Check for banner.png in www folder
      banner_path <- file.path("../www", "banner.png")
      if (file.exists(banner_path)) {
        return(banner_path)
      }
      
      # Try other common formats as fallback
      other_formats <- c("banner.jpg", "banner.jpeg", "logo.png", "logo.jpg")
      for (format in other_formats) {
        alt_path <- file.path("www", format)
        if (file.exists(alt_path)) {
          return(alt_path)
        }
      }
      
      return(NULL)
    }
    
    # Process XLSForm when uploaded
    observeEvent(input$xlsform, {
      req(input$xlsform$datapath)
      
      withProgress(message = "Processing XLSForm...", value = 0, {
        incProgress(0.2, detail = "Reading settings...")
        
        # Read settings sheet if it exists
        tryCatch({
          sheets <- readxl::excel_sheets(input$xlsform$datapath)
          if ("settings" %in% tolower(sheets)) {
            settings_sheet <- sheets[tolower(sheets) == "settings"][1]
            settings_data <- readxl::read_excel(input$xlsform$datapath, sheet = settings_sheet)
            
            # Look for form_title in settings
            if ("form_title" %in% names(settings_data) && nrow(settings_data) > 0) {
              title_value <- settings_data$form_title[1]
              if (!is.na(title_value) && title_value != "") {
                values$form_title <- title_value
              }
            }
          }
        }, error = function(e) {
          # If settings sheet doesn't exist or can't be read, use default
          showNotification(
            "No settings sheet found. Using default title.",
            type = "warning",
            duration = 3
          )
        })
        
        incProgress(0.3, detail = "Reading survey data...")
        
        # Read survey and choices sheets
        koboQues <- readxl::read_excel(input$xlsform$datapath, sheet = "survey", col_types = "text")
        koboChoices <- readxl::read_excel(input$xlsform$datapath, sheet = "choices", col_types = "text")
        
        incProgress(0.4, detail = "Creating admin hierarchy...")
        values$admin_df <- create_admin_hierarchy(koboChoices)
        
        incProgress(0.5, detail = "Processing questions...")
        
        paperForm <- koboQues %>%
          dplyr::select(type, name, `relevant_message::English`, `label::English`, 
                        `hint::English`, `constraint_message::English`) %>%
          dplyr::filter(!is.na(`label::English`)) %>%
          dplyr::filter(!`label::English` %in% c("start","end","today","deviceid","audit")) %>%
          dplyr::rename(TypeRaw = type, q_name = name, Name = `label::English`,
                        Hint = `hint::English`, Constraint = `constraint_message::English`, 
                        Relevancy = `relevant_message::English`) %>%
          dplyr::mutate(
            Type = stringr::str_replace(TypeRaw, "^(select_one|select_multiple) ", ""),
            TypeReadable = dplyr::case_when(
              stringr::str_detect(TypeRaw, "^select_one") ~ "Select one",
              stringr::str_detect(TypeRaw, "^select_multiple") ~ "Select multiple",
              stringr::str_detect(tolower(TypeRaw), "rank") ~ "Rank",
              TypeRaw == "text" ~ "Text",
              TypeRaw == "integer" ~ "Integer",
              TypeRaw == "decimal" ~ "Decimal number",
              TypeRaw == "date" ~ "Date",
              TypeRaw == "note" ~ "Note",
              TypeRaw %in% c("begin group", "begin_group") ~ "Section",
              TRUE ~ TypeRaw
            )
          ) %>%
          dplyr::left_join(
            koboChoices %>%
              dplyr::filter(!is.na(list_name)) %>%
              dplyr::group_by(list_name) %>%
              dplyr::summarise(
                ChoicesLabels = paste(" ☐ ", na.omit(`label::English`), collapse = "\n"),
                ChoicesNames = paste(na.omit(name), collapse = "\n"),
                .groups = "drop"
              ) %>%
              dplyr::rename(Type = list_name),
            by = "Type"
          ) %>%
          dplyr::mutate(
            ChoicesLabels = dplyr::coalesce(ChoicesLabels, ""),
            ChoicesNames = dplyr::coalesce(ChoicesNames, "")
          ) %>%
          dplyr::filter(Type != "end_group")
        
        values$paperForm <- paperForm
      })
      
      # Show success notification
      showNotification(
        paste("XLSForm loaded successfully:", nrow(paperForm), "questions detected"),
        type = "message",
        duration = 3
      )
    })
    
    # Load admin data
    observeEvent(input$admin, {
      req(input$admin$datapath)
      
      tryCatch({
        admin_df_temp <- readxl::read_excel(input$admin$datapath)
        
        # Check if there are admin columns
        admin_cols <- grep("admin", names(admin_df_temp), ignore.case = TRUE, value = TRUE)
        if (length(admin_cols) > 0) {
          values$admin_df <- admin_df_temp %>%
            dplyr::select(dplyr::all_of(admin_cols)) %>%
            janitor::clean_names()
          
          showNotification(
            paste("Admin list loaded successfully. Found", length(admin_cols), "admin columns."),
            type = "message",
            duration = 3
          )
        } else {
          showNotification(
            "Warning: No admin columns found in the uploaded file.",
            type = "warning",
            duration = 4
          )
          values$admin_df <- NULL
        }
        
      }, error = function(e) {
        showNotification(
          paste("Error loading admin list:", e$message),
          type = "error",
          duration = 4
        )
        values$admin_df <- NULL
      })
    })
    
    # Display reactive values
    output$title_display <- renderText({
      values$form_title
    })
    
    output$month_display <- renderText({
      values$month_year
    })
    
    output$banner_display <- renderText({
      banner_path <- get_banner_path()
      if (!is.null(banner_path)) {
        "banner.png (auto-loaded)"
      } else {
        "No banner found"
      }
    })
    
    output$width_display <- renderText({
      "6.6 inches (default)"
    })
    
    # Preview table
    output$preview_table <- DT::renderDataTable({
      req(values$paperForm)
      values$paperForm %>%
        dplyr::select(Name, TypeReadable, ChoicesLabels) %>%
        dplyr::mutate(
          ChoicesLabels = stringr::str_trunc(ChoicesLabels, 80, ellipsis = "..."),
          Name = stringr::str_trunc(Name, 60, ellipsis = "...")
        ) %>%
        DT::datatable(
          rownames = FALSE, 
          escape = FALSE, 
          options = list(
            pageLength = 15, 
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = '300px', targets = 0),
              list(width = '150px', targets = 1),
              list(width = '400px', targets = 2)
            )
          ),
          class = 'display compact stripe hover'
        ) %>%
        DT::formatStyle(
          columns = 1:3,
          fontSize = '85%'
        )
    }, server = FALSE)
    
    # Generate button handler with withProgress
    observeEvent(input$generate, {
      req(values$paperForm)
      
      # Disable generate button
      shinyjs::disable("generate")
      
      # Use withProgress for generation progress
      withProgress(message = "Generating Paper Form", value = 0, {
        
        tryCatch({
          titleQues <- paste0(values$form_title, " ", values$month_year)
          
          # Update progress: 5%
          incProgress(0.05, detail = "Creating workbook structure...")
          
          # Create styles using openxlsx:: prefix
          titleStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 18, textDecoration = "bold",
                                              fgFill = "#ddd9c4", halign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
          sectionStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 16, textDecoration = "bold",
                                                fgFill = "#bfbfbf", halign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
          noteStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 12, textDecoration = "bold",
                                             fgFill = "#dce6f1", halign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
          questionTitleStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 13, textDecoration = "bold",
                                                      fgFill = "#e6e6e6", wrapText = TRUE, border = "TopBottomLeftRight")
          choicesStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 11, wrapText = TRUE, border = "TopBottomLeftRight")
          blankRowStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 10)
          metaBGStyle <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 11, wrapText = TRUE,
                                               fgFill = "#ebf1de", border = "TopBottomLeftRight")
          
          # Annex styles
          annexTableHeader <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 12, textDecoration = "bold",
                                                    fgFill = "#dce6f1", halign = "center", wrapText = TRUE, border = "TopBottomLeftRight")
          annexTableBody <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 11, wrapText = TRUE, border = "TopBottomLeftRight")
          
          # Workbook
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "paperEnglish")
          openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Arial Narrow")
          
          # Update progress: 10%
          incProgress(0.05, detail = "Adding banner and title...")
          
          # Banner from www folder
          banner_path <- get_banner_path()
          if (!is.null(banner_path) && file.exists(banner_path)) {
            tryCatch({
              openxlsx::insertImage(wb, "paperEnglish", banner_path, startRow = 1, startCol = 1,
                                    width = 6.6, height = 1.0, units = "in")
              openxlsx::setRowHeights(wb, "paperEnglish", rows = 1, heights = 75)
            }, error = function(e) {
              # Silently fail for banner
            })
          }
          
          # Title
          openxlsx::mergeCells(wb, "paperEnglish", rows = 3, cols = 1)
          openxlsx::writeData(wb, "paperEnglish", titleQues, startRow = 3, startCol = 1)
          openxlsx::addStyle(wb, "paperEnglish", titleStyle, rows = 3, cols = 1)
          openxlsx::setRowHeights(wb, "paperEnglish", rows = 3, heights = 45)
          
          currentRow <- 5
          skip_next_other_specify <- FALSE
          admin_types <- c("admin1", "admin2", "admin3", "admin4")
          
          # Progress tracking for large forms
          total_questions <- nrow(values$paperForm)
          progress_start <- 0.15  # Start at 15%
          progress_range <- 0.75  # 75% of total progress for questions
          
          # Update progress: 15%
          incProgress(0.05, detail = paste("Processing", total_questions, "questions..."))
          
          # Main loop - generate form content
          for (i in 1:total_questions) {
            # Update progress percentage every few questions
            if (i %% 5 == 0 || i == total_questions) {
              progress_value <- progress_start + (i / total_questions) * progress_range
              setProgress(value = progress_value, 
                          detail = paste("Processing question", i, "of", total_questions))
            }
            
            q <- values$paperForm[i, ]
            qname <- tolower(ifelse(is.na(q$q_name), "", q$q_name))
            
            if (skip_next_other_specify && stringr::str_detect(qname, "_other$")) {
              skip_next_other_specify <- FALSE
              next
            }
            
            # SECTION
            if (q$Type %in% c("begin group", "begin_group")) {
              section_text <- q$Name
              if (!is.na(q$Relevancy) && q$Relevancy != "") {
                section_text <- paste0("[", q$Relevancy, "] \n", section_text)
              }
              openxlsx::writeData(wb, "paperEnglish", section_text, startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", sectionStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 2
              next
            }
            
            # NOTE
            if (q$Type == "note") {
              note_text <- q$Name
              if (!is.na(q$Relevancy) && q$Relevancy != "") {
                note_text <- paste0("[", q$Relevancy, "] \n", note_text)
              }
              openxlsx::writeData(wb, "paperEnglish", note_text, startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", noteStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 2
              next
            }
            
            # QUESTION TITLE
            openxlsx::writeData(wb, "paperEnglish", q$Name, startRow = currentRow, startCol = 1)
            openxlsx::addStyle(wb, "paperEnglish", questionTitleStyle, rows = currentRow, cols = 1)
            currentRow <- currentRow + 1
            
            # QUESTION TYPE
            openxlsx::writeData(wb, "paperEnglish", paste0("📝 Question type: ", q$TypeReadable), 
                                startRow = currentRow, startCol = 1)
            openxlsx::addStyle(wb, "paperEnglish", metaBGStyle, rows = currentRow, cols = 1)
            currentRow <- currentRow + 1
            
            # RELEVANCY, HINT, CONSTRAINT
            if (!is.na(q$Relevancy) && q$Relevancy != "") {
              openxlsx::writeData(wb, "paperEnglish", paste0("⚡ Relevancy: ", q$Relevancy), startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", metaBGStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
            }
            if (!is.na(q$Hint) && q$Hint != "") {
              openxlsx::writeData(wb, "paperEnglish", paste0("💡 Hint: ", q$Hint), startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", metaBGStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
            }
            if (!is.na(q$Constraint) && q$Constraint != "") {
              openxlsx::writeData(wb, "paperEnglish", paste0("⛔ Constraint: ", q$Constraint), startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", metaBGStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
            }
            
            # ADMIN FIELDS
            if (q$Type %in% admin_types) {
              openxlsx::writeData(wb, "paperEnglish", "Choices:", startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
              openxlsx::writeData(wb, "paperEnglish", paste0("Please specify ", q$Type, " name and pcode based on the annex admin list."), 
                                  startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 2
              openxlsx::writeData(wb, "paperEnglish", "", startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", blankRowStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
              next
            }
            
            # RANK QUESTIONS
            if (!is.na(q$TypeRaw) && stringr::str_detect(q$TypeRaw, "^rank")) {
              openxlsx::writeData(wb, "paperEnglish", "Choices:", startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
              for (rank_num in 1:3) {
                openxlsx::writeData(wb, "paperEnglish", paste0(rank_num, ") ________________________________"), 
                                    startRow = currentRow, startCol = 1)
                openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
                currentRow <- currentRow + 1
              }
              currentRow <- currentRow + 1
              openxlsx::writeData(wb, "paperEnglish", "", startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", blankRowStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
              next
            }
            
            # CHOICES
            if (!is.na(q$ChoicesLabels) && q$ChoicesLabels != "") {
              label_lines <- stringr::str_split(q$ChoicesLabels, "\n")[[1]]
              name_lines <- stringr::str_split(q$ChoicesNames, "\n")[[1]]
              n <- min(length(label_lines), length(name_lines))
              
              openxlsx::writeData(wb, "paperEnglish", "Choices:", startRow = currentRow, startCol = 1)
              openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
              currentRow <- currentRow + 1
              
              wrote_inline_specify <- FALSE
              for (j in seq_len(n)) {
                lbl <- label_lines[j]
                nm <- tolower(name_lines[j])
                
                openxlsx::writeData(wb, "paperEnglish", lbl, startRow = currentRow, startCol = 1)
                openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
                currentRow <- currentRow + 1
                
                if (nm == "other") {
                  openxlsx::writeData(wb, "paperEnglish", "    • Other (please specify): __________________________", 
                                      startRow = currentRow, startCol = 1)
                  openxlsx::addStyle(wb, "paperEnglish", choicesStyle, rows = currentRow, cols = 1)
                  currentRow <- currentRow + 1
                  wrote_inline_specify <- TRUE
                }
              }
              if (wrote_inline_specify) skip_next_other_specify <- TRUE
            }
            
            # Spacer
            openxlsx::writeData(wb, "paperEnglish", "", startRow = currentRow, startCol = 1)
            openxlsx::addStyle(wb, "paperEnglish", blankRowStyle, rows = currentRow, cols = 1)
            currentRow <- currentRow + 1
          }
          
          # Update progress: 90%
          setProgress(value = 0.92, detail = "Formatting columns...")
          
          # Column width
          openxlsx::setColWidths(wb, "paperEnglish", cols = 1, widths = 87)
          
          # Update progress: 94%
          setProgress(value = 0.94, detail = "Adding admin annex...")
          
          # ANNEX SHEET
          if (!is.null(values$admin_df) && nrow(values$admin_df) > 0) {
            openxlsx::addWorksheet(wb, "ANNEX_Admin_List")
            openxlsx::writeData(wb, "ANNEX_Admin_List", values$admin_df, startRow = 1, startCol = 1)
            openxlsx::addStyle(wb, "ANNEX_Admin_List", annexTableHeader, rows = 1, cols = 1:ncol(values$admin_df), gridExpand = TRUE)
            openxlsx::addStyle(wb, "ANNEX_Admin_List", annexTableBody, rows = 2:(nrow(values$admin_df) + 1), 
                               cols = 1:ncol(values$admin_df), gridExpand = TRUE)
            
            # Set column widths - limit to first 5 columns if many
            cols_to_format <- min(5, ncol(values$admin_df))
            if (cols_to_format > 0) {
              openxlsx::setColWidths(wb, "ANNEX_Admin_List", cols = 1:cols_to_format, widths = rep(30, cols_to_format))
            }
            
            openxlsx::pageSetup(wb, "ANNEX_Admin_List", orientation = "landscape", fitToWidth = 1, fitToHeight = 0)
          }
          
          # Update progress: 98%
          setProgress(value = 0.98, detail = "Finalizing...")
          
          values$wb <- wb
          values$generated <- TRUE
          
          # Complete
          setProgress(value = 1, detail = "Complete!")
          
          showNotification(
            paste("✅ SUCCESS! Form generated with", nrow(values$paperForm), "questions.\n📄 Ready for download."),
            type = "message",
            duration = 5
          )
          
          output$status <- renderText({
            paste0("✅ SUCCESS! Form generated with ", nrow(values$paperForm), " questions.\n📄 Ready for download.")
          })
          
        }, error = function(e) {
          showNotification(
            paste("Error generating form:", e$message),
            type = "error",
            duration = 5
          )
          output$status <- renderText({
            paste0("❌ ERROR: ", e$message)
          })
        })
      })
      
      # Re-enable generate button after completion
      shinyjs::enable("generate")
    })
    
    # Download handler with loading state
    output$download <- downloadHandler(
      filename = function() {
        safe_title <- gsub("[^A-Za-z0-9]", "_", values$form_title)
        safe_month <- gsub("[^A-Za-z0-9]", "_", values$month_year)
        paste0(safe_title, "_", safe_month, ".xlsx")
      },
      content = function(file) {
        req(values$wb)
        
        # Show loading state on button
        session$sendCustomMessage("showDownloadLoading", list(button_id = ns("download")))
        
        tryCatch({
          # Simulate progress for download (optional)
          for (i in seq(0, 90, by = 10)) {
            Sys.sleep(0.02)  # Minimal delay for UI update
            session$sendCustomMessage("updateDownloadProgress", list(percent = i))
          }
          
          # Actually save the file
          openxlsx::saveWorkbook(values$wb, file, overwrite = TRUE)
          
          # Complete progress
          session$sendCustomMessage("updateDownloadProgress", list(percent = 100))
          Sys.sleep(0.3)
          
          showNotification("File downloaded successfully!", type = "message", duration = 3)
          
        }, error = function(e) {
          showNotification(
            paste("Error saving file:", e$message),
            type = "error",
            duration = 5
          )
        }, finally = {
          # Hide loading state
          session$sendCustomMessage("hideDownloadLoading", list(button_id = ns("download")))
          session$sendCustomMessage("hideDownloadProgress", list())
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    output$download_admin <- downloadHandler(
      filename = function() {
        paste0("admin_list.xlsx")
      },
      content = function(file) {
        if (is.null(values$admin_df) || nrow(values$admin_df) == 0) {
          showNotification("No admin data available.", type = "error")
          return(NULL)
        }
        
        session$sendCustomMessage("showDownloadLoading", list(button_id = ns("download_admin")))
        
        tryCatch({
          
          # Create workbook
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Admin_List")
          
          # Write as Excel table
          openxlsx::writeDataTable(
            wb,
            sheet = "Admin_List",
            x = values$admin_df,
            startRow = 1,
            startCol = 1,
            tableStyle = "TableStyleMedium9"  # nice built-in style
          )
          
          # Auto column width
          openxlsx::setColWidths(
            wb,
            sheet = "Admin_List",
            cols = 1:ncol(values$admin_df),
            widths = "auto"
          )
          
          # Freeze header row
          openxlsx::freezePane(wb, "Admin_List", firstRow = TRUE)
          
          # Save workbook
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          
          showNotification("Admin list downloaded successfully!", type = "message", duration = 3)
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 3)
        }, finally = {
          session$sendCustomMessage("hideDownloadLoading", list(button_id = ns("download_admin")))
        })
      }
    )
    # Return reactive values if needed for parent module
    return(list(
      generated = reactive({ values$generated }),
      paperForm = reactive({ values$paperForm })
    ))
    
  })
}

create_admin_hierarchy <- function(choices_df) {
  
  admin1 <- choices_df %>%
    dplyr::filter(list_name == "admin1") %>%
    dplyr::transmute(
      admin1Name = `label::English`,
      admin1Pcod = name
    )
  
  admin2 <- choices_df %>%
    dplyr::filter(list_name == "admin2") %>%
    dplyr::mutate(admin1Pcod = substr(name, 1, 4)) %>%  # ET01
    dplyr::transmute(
      admin2Name = `label::English`,
      admin2Pcod = name,
      admin1Pcod
    )
  
  admin3 <- choices_df %>%
    dplyr::filter(list_name == "admin3") %>%
    dplyr::mutate(admin2Pcod = substr(name, 1, 6)) %>%  # ET0101
    dplyr::transmute(
      admin3Name = `label::English`,
      admin3Pcod = name,
      admin2Pcod
    )
  
  # Join hierarchy
  admin_full <- admin3 %>%
    dplyr::left_join(admin2, by = "admin2Pcod") %>%
    dplyr::left_join(admin1, by = "admin1Pcod") %>%
    dplyr::select(
      admin1Name, admin1Pcod,
      admin2Name, admin2Pcod,
      admin3Name, admin3Pcod
    )
  
  return(admin_full)
}

# Add JavaScript for loading states
# Add this to your app's UI or in a separate JavaScript file
add_loading_js <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('showDownloadLoading', function(message) {
      var buttonId = message.button_id;
      var button = $('#' + buttonId);
      var originalText = button.html();
      button.data('original-text', originalText);
      button.html('<i class=\\'fa fa-spinner fa-spin\\'></i> Saving...');
      button.prop('disabled', true);
    });
    
    Shiny.addCustomMessageHandler('hideDownloadLoading', function(message) {
      var buttonId = message.button_id;
      var button = $('#' + buttonId);
      var originalText = button.data('original-text');
      if (originalText) {
        button.html(originalText);
      } else {
        button.html('Download Excel Form');
      }
      button.prop('disabled', false);
    });
    
    Shiny.addCustomMessageHandler('updateDownloadProgress', function(message) {
      var percent = message.percent;
      $('#progress_bar').css('width', percent + '%').attr('aria-valuenow', percent);
      $('#progress_bar').html(Math.round(percent) + '%');
      if (percent === 0 || percent === 100) {
        setTimeout(function() {
          $('#progress_container').hide();
        }, 1000);
      } else {
        $('#progress_container').show();
      }
    });
    
    Shiny.addCustomMessageHandler('hideDownloadProgress', function(message) {
      $('#progress_container').hide();
      $('#progress_bar').css('width', '0%').html('0%');
    });
  "))
}