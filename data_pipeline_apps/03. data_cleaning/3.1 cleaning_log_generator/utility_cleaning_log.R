
save_other_responses <- function(df, output_path, other_db2) {

  get_column_letter <- function(r) {
    return(ifelse(r <= 26, LETTERS[r],
                  ifelse(r <= 52, paste0(LETTERS[1], LETTERS[r - 26]),
                         paste0(LETTERS[2], LETTERS[r - 52]))))
  }

  # Save other responses
  wb <- createWorkbook()

  # Define the styles
  style.col.color <- createStyle(fgFill = "#DDDDDE", border = "TopBottomLeftRight",
                                 # sborderColour = "#000000",
                                 valign = "top", wrapText = T)
  style.col.color1 <- createStyle(fgFill = "#DBD5C6", border = "TopBottomLeftRight", borderColour = "#000000",
                                  valign = "top", wrapText = T)
  style.col.color2 <- createStyle(fgFill = "#F3F4F4", border = "TopBottomLeftRight",
                                  # borderColour = "#000000",
                                  valign = "top", wrapText = T)
  style.col.color.first <- createStyle(textDecoration = "bold", fgFill = "#F1797A", valign = "top",
                                       border = "TopBottomLeftRight", borderColour = "#000000", wrapText = T)

  # Add Worksheet
  addWorksheet(wb, "cleaning_log")
  writeData(wb = wb, x = df, sheet = "cleaning_log", startRow = 1)

  # Apply the styles
  addStyle(wb, "cleaning_log", style = style.col.color.first, rows = 1:(nrow(df) + 1), cols = 1) # First column color

  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 13)
  addStyle(wb, "cleaning_log", style = style.col.color, rows = 1:(nrow(df) + 1), cols = 14)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 15)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 16)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 17)
  addStyle(wb, "cleaning_log", style = style.col.color, rows = 1:(nrow(df) + 1), cols = 18)
  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 19)
  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 20)


  # Freeze the first row
  freezePane(wb, sheet = "cleaning_log", firstActiveRow = 2)

  # Add column filters
  addFilter(wb, "cleaning_log", rows = 1, cols = 1:ncol(df))

  addStyle(wb, "cleaning_log", style = createStyle(wrapText = TRUE), rows = 1:19, cols = 1)



  # Add Dropdown values worksheet and data validation
  addWorksheet(wb, "Dropdown_values")
  for (r in 1:nrow(other_db2)) {
    if (other_db2$q_type[r] != "text") {
      choices <- str_split(other_db2$choices[r], ";;")[[1]]
      writeData(wb, sheet = "Dropdown_values", x = choices, startCol = r)
      uuids <- which(df$question_name == other_db2$name[r])
      if (length(uuids) > 0) {
        column_letter <- get_column_letter(r)
        values <- paste0("'Dropdown_values'!$", column_letter, "$1:$", column_letter, "$", other_db$num_choices[r])
        dataValidation(wb, "cleaning_log", col = 15, rows = uuids + 1, type = "list", value = values)
        dataValidation(wb, "cleaning_log", col = 16, rows = uuids + 1, type = "list", value = values)
        dataValidation(wb, "cleaning_log", col = 17, rows = uuids + 1, type = "list", value = values)

      }
    }
  }
  # Find the column index
  target_col <- which(names(df) == "INVALID other (select yes or leave blank)")

  # Write dropdown values
  writeData(wb, sheet = "Dropdown_values", x = c("Yes"), startCol = r + 1)

  # Get Excel column letter
  column_letter <- get_column_letter(r + 1)

  # Create validation reference
  values <- paste0("'Dropdown_values'!$", column_letter, "$1:$", column_letter, "$1")

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
  setColWidths(wb, "cleaning_log", cols = 1, widths = 20)
  setColWidths(wb, "cleaning_log", cols = 2, widths = 18)
  setColWidths(wb, "cleaning_log", cols = 3, widths = 15)
  setColWidths(wb, "cleaning_log", cols = 4, widths = 15)
  setColWidths(wb, "cleaning_log", cols = 5, widths = 15)
  setColWidths(wb, "cleaning_log", cols = 6, widths = 13)
  setColWidths(wb, "cleaning_log", cols = 7:10, widths = 20)
  setColWidths(wb, "cleaning_log", cols = 11, widths = 35)
  setColWidths(wb, "cleaning_log", cols = 12, widths = 15)
  setColWidths(wb, "cleaning_log", cols = 13, widths = 35)
  setColWidths(wb, "cleaning_log", cols = 14, widths = 35)
  setColWidths(wb, "cleaning_log", cols = 15:19, widths = 30)
  setColWidths(wb, "cleaning_log", cols = 19, widths = 25)
  setColWidths(wb, "cleaning_log", cols = 20:26, widths = 15)


  # Apply text alignment and wrapping
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 1)
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 2)
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 3)
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 4)
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 5)
  addStyle(wb, "cleaning_log", style = createStyle(valign = "top"), rows = 1:(nrow(df) + 1), cols = 6)
  addStyle(wb, "cleaning_log", style = createStyle(wrapText = T, valign = "top"), rows = 1:(nrow(df) + 1), cols = 7)
  addStyle(wb, "cleaning_log", style = createStyle(wrapText = T, valign = "top"), rows = 1:(nrow(df) + 1), cols = 8)
  addStyle(wb, "cleaning_log", style = createStyle(wrapText = T, valign = "top"), rows = 1:(nrow(df) + 1), cols = 9)
  addStyle(wb, "cleaning_log", style = createStyle(wrapText = T, valign = "top"), rows = 1:(nrow(df) + 1), cols = 10)

  setRowHeights(wb, "cleaning_log", rows = 1, heights = 15)

  # Bold first row
  addStyle(wb, "cleaning_log", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1:ncol(df))
  addStyle(wb, "cleaning_log", style = style.col.color.first, rows = 1, cols = 1:26)

  modifyBaseFont(wb, fontSize = 10, fontColour = "black", fontName = "Calibri")



  # Define file name and save workbook
  # sub.filename = paste0(Sys.Date(), Region, "_other_responses.xlsx")
  saveWorkbook(wb,output_path, overwrite = T)
}



check_others_custom <- function(raw, other_db) {
  log_msg("Starting processing inside my_helper_function")
  cat(green("Processing other responses...\n"))
  print(head(raw))
  # For MAIN sheet
  data_sheet <- raw %>%
    distinct(uuid, .keep_all = TRUE)

  raw_data_sheet <- raw %>%
    distinct(uuid, .keep_all = TRUE)
  var_other_raw <- other_db$name[other_db$name %in% colnames(data_sheet)]

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
    left_join(select(data_sheet, uuid, `_submission_time`,  admin1, admin2, admin3, enum_id), by = "uuid") %>%
    mutate(`_submission_time` = format(as.Date(`_submission_time`), "%y:%m:%d")) %>%
    arrange(question_name, uuid) %>%
    left_join(select(other_db, name, full_label, q_type, list_name, ref_question),
              by = c("question_name" = "name")) %>%
    select(
      uuid, `_submission_time`,  admin1, admin2, admin3,enum_id,
      question_name, q_type, list_name, full_label,
      response_eth, response_en
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
      ref_name <- other_db$ref_question[other_db$name == df$question_name[r]]
      q_type <- other_db$q_type[other_db$name == df$question_name[r]]

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

  df <- relocate(df, "selected_choices", .before = "response_eth")
  # df <- relocate(df, "sheet", .before = "`_submission_time`")
  # df <- relocate(df, "_index", .before = "sheet")

  return(df)

}


# ---------- check_kobo.R helpers ----------------------------
check_answer_in_list <- function(questions, choices, constraint) {
  if (!str_detect(constraint, ",")) return(TRUE)
  question_regex <- "\\{([^()]+)\\}"
  answer_regex   <- "\\'([^()]+)\\'"
  question <- gsub(question_regex, "\\1", str_extract_all(constraint, question_regex)[[1]])
  answer   <- gsub(answer_regex,   "\\1", str_extract_all(constraint, answer_regex)[[1]])
  question_type <- questions %>%
    filter(name == question) %>%
    filter(!grepl("^(begin|end)\\s+group$", type)) %>%
    pull(type)
  if (length(question_type) == 0) return(FALSE)
  if (question_type == "calculate") return(TRUE)
  listname     <- gsub("^.*\\s", "", question_type)
  choices_list <- choices %>% filter(list_name == listname) %>% pull(name)
  answer %in% choices_list
}

check_constraints <- function(questions, choices, kobo_survey, kobo_choices) {
  if (!verify_valid_survey(kobo_survey))  stop("Survey sheet invalid.")
  if (!verify_valid_choices(kobo_choices)) stop("Choices sheet invalid.")
  questions <- mutate_at(questions, c("name", "type"), ~str_trim(.))
  choices   <- mutate_at(choices,   c("list_name", "name"), ~str_trim(.))
  all_constraints <- questions %>% filter(grepl("selected", relevant)) %>% pull(relevant)
  all_constraints <- gsub('"', "'", all_constraints)
  rs_list <- map(all_constraints,
                 ~map_lgl(unlist(ex_default(.x, pattern = "selected\\s*\\([^\\)]*\\)")),
                          ~check_answer_in_list(questions, choices, .)))
  map2(rs_list, seq_along(rs_list),
       ~if (length(which(!.x)) != 0) {
         return(unlist(ex_default(all_constraints[.y],
                                  pattern = "selected\\s*\\([^\\)]*\\)"))[which(!.x)])
       }) %>% unlist() %>% unique()
}

# ---------- util helpers ------------------------------------
get_ref_question <- function(x) {
  x.1 <- str_split(x, "\\{")[[1]][2]
  str_split(x.1, "\\}")[[1]][1]
}

get_name_from_label <- function(list_name, label, kobo_choices) {
  kobo_choices$name[kobo_choices$list_name == list_name &
                      kobo_choices$`label::english` == label]
}

get_value_from_uuid <- function(uuid, column, raw_dataset) {
  raw_dataset[[column]][raw_dataset$uuid == uuid]
}

add_choice <- function(concat_value, choice) {
  l <- str_split(concat_value, " ")[[1]]
  l <- sort(unique(c(l, choice)))
  l <- l[l != ""]
  paste(l, collapse = " ")
}

remove_choice <- function(concat_value, choice) {
  l <- str_split(concat_value, " ")[[1]]
  l <- l[l != choice]
  paste(l, collapse = " ")
}

# ---------- Function to create other_db from kobo survey -----
create_other_db_from_kobo <- function(kobo_survey, kobo_choices) {



  other_labels <- kobo_survey %>%
    filter(grepl("^other_", name) | grepl("_other$", name)) %>%
    filter((type =="text") &
             !name %in% c("pho_enumerator", "mail_enumerator")) %>%
    mutate(ref_question=as.character(lapply(relevant, get_ref_question))) %>%
    mutate(ref_question = ifelse(is.na(ref_question), name, ref_question)) %>%
    select(name, ref_question) %>%
    left_join(select(kobo_survey, ref_question=name, full_label=`label::english`), by = "ref_question")

  other_db <- other_labels %>%
    left_join(select(kobo_survey, name, q_type, list_name), by=c("ref_question"="name")) %>%
    left_join(select(kobo_survey, name, relevant), by="name") %>%
    mutate(option_other=str_replace_all(str_extract(relevant, "\'.*\'"), "'", "")) %>%
    select(-relevant)

  # remove all option_other from choices
  kobo_choices_sub <- filter(kobo_choices, list_name %in% other_db$list_name)
  for (r in 1:nrow(other_db)){
    if (!is.na(other_db$option_other[r])){
      kobo_choices_sub <- kobo_choices_sub %>%
        filter(!(list_name==other_db$list_name[r] & name==other_db$option_other[r]))
    }
  }
  # add list of available choices
  other_db <- other_db %>%
    left_join(select(kobo_choices_sub, list_name, label="label::english") %>%
                group_by(list_name) %>%
                summarise(num_choices=n(), choices=paste0(label, collapse=";;")),
              by="list_name")

  return(other_db)
}
# Add this function to check for other columns in your data
debug_other_columns <- function(raw_data, other_db) {
  cat("\n========== DEBUG: Other Columns Check ==========\n")

  # Check for any columns starting with "other_" in raw data
  other_cols_in_raw <- names(raw_data)[grepl("^other_", names(raw_data))]
  cat("Columns starting with 'other_' in raw data:", length(other_cols_in_raw), "\n")
  if (length(other_cols_in_raw) > 0) {
    cat("First 10:", paste(head(other_cols_in_raw, 10), collapse = ", "), "\n")
  }

  # Check other_db if provided
  if (!is.null(other_db) && nrow(other_db) > 0) {
    cat("\nOther DB has", nrow(other_db), "expected 'other' columns\n")
    cat("Expected columns:", paste(head(other_db$name, 10), collapse = ", "), "\n")

    # Check which expected columns exist in raw data
    existing <- other_db$name[other_db$name %in% names(raw_data)]
    missing <- other_db$name[!other_db$name %in% names(raw_data)]

    cat("\nExisting in raw data:", length(existing), "\n")
    if (length(existing) > 0) {
      cat("Existing:", paste(head(existing, 10), collapse = ", "), "\n")

      # Check if they have non-NA values
      for (col in existing[1:min(5, length(existing))]) {
        non_empty <- sum(!is.na(raw_data[[col]]) & raw_data[[col]] != "" & raw_data[[col]] != "NA")
        cat("  Column '", col, "' has", non_empty, "non-empty values\n", sep="")
      }
    }

    if (length(missing) > 0) {
      cat("Missing from raw data:", paste(head(missing, 10), collapse = ", "), "\n")
    }
  } else {
    cat("\nother_db is NULL or empty\n")
  }

  cat("================================================\n\n")
}
# ---------- check_others_custom Shiny version ----------------
# Updated check_others_custom_shiny that handles both prefixes and suffixes
check_others_custom_shiny <- function(raw_dataset, kobo_survey, kobo_choices, other_db, not_other = c()) {

  # Identify other columns from other_db
  var_other_raw <- other_db$name[other_db$name %in% names(raw_dataset)]
  var_other_raw <- var_other_raw[!var_other_raw %in% not_other]

  if (length(var_other_raw) == 0) {
    cat(yellow("No 'other' columns found in dataset\n"))
    return(data.frame())
  }

  # Process other responses for main sheet
  other_responses_main <- raw_dataset %>%
    select(uuid, all_of(var_other_raw)) %>%
    tidyr::pivot_longer(
      cols = all_of(var_other_raw),
      names_to = "question_name",
      values_to = "response_eth"
    ) %>%
    filter(!is.na(response_eth), response_eth != "", response_eth != "NA") %>%
    select(uuid, question_name, response_eth) %>%
    mutate(response_en = NA_character_)

  if (nrow(other_responses_main) == 0) {
    cat(yellow("No other responses found\n"))
    return(data.frame())
  }

  # Add metadata from other_db
  df <- other_responses_main %>%
    left_join(
      select(other_db, name, full_label, q_type, list_name, ref_question),
      by = c("question_name" = "name")
    ) %>%
    mutate(
      `TRUE other (copy response_en or provide a better translation)` = NA_character_,
      `EXISTING other 1 (select the most appropriate choice)` = NA_character_,
      `EXISTING other 2 (select the most appropriate choice)` = NA_character_,
      `EXISTING other 3 (select the most appropriate choice)` = NA_character_,
      `INVALID other (select yes or leave blank)` = NA_character_,
      `FOLLOW-UP message (what is unclear about this response?)` = NA_character_,
      Explanation = NA_character_,
      selected_choices = NA_character_
    )

  # Add submission time and admin info if available
  if ("_submission_time" %in% names(raw_dataset)) {
    df <- df %>%
      left_join(
        select(raw_dataset, uuid, `_submission_time`, any_of(c("admin1", "admin2", "admin3", "enum_id"))),
        by = "uuid"
      ) %>%
      mutate(`_submission_time` = format(as.Date(`_submission_time`), "%Y:%m:%d"))
  } else {
    df$`_submission_time` <- NA
    df$admin1 <- NA
    df$admin2 <- NA
    df$admin3 <- NA
    df$enum_id <- NA
  }

  # For select_multiple questions, get the selected choices
  if (nrow(df) > 0) {
    for (r in seq_len(nrow(df))) {
      ref_name <- df$ref_question[r]
      q_type <- df$q_type[r]

      if (!is.na(ref_name) && q_type == "select_multiple" && ref_name %in% names(raw_dataset)) {
        choices <- raw_dataset[[ref_name]][raw_dataset$uuid == df$uuid[r]]

        if (length(choices) > 0 && !is.na(choices) && choices != "") {
          choice_vec <- str_split(choices, " ")[[1]]

          # Get labels for choices if available
          if (!is.na(df$list_name[r]) && nrow(kobo_choices) > 0) {
            choice_labels <- kobo_choices %>%
              filter(list_name == df$list_name[r], name %in% choice_vec) %>%
              pull(`label::english`)

            if (length(choice_labels) > 0) {
              df$selected_choices[r] <- paste(paste0(choice_vec, ": ", choice_labels), collapse = ";\n")
            } else {
              df$selected_choices[r] <- paste(choice_vec, collapse = "; ")
            }
          } else {
            df$selected_choices[r] <- paste(choice_vec, collapse = "; ")
          }
        }
      }
    }
  }

  # Arrange columns in a sensible order
  df <- df %>%
    select(
      uuid, `_submission_time`, any_of(c("admin1", "admin2", "admin3", "enum_id")),
      question_name, q_type, list_name, full_label,
      response_eth, response_en, selected_choices,
      `TRUE other (copy response_en or provide a better translation)`,
      `EXISTING other 1 (select the most appropriate choice)`,
      `EXISTING other 2 (select the most appropriate choice)`,
      `EXISTING other 3 (select the most appropriate choice)`,
      `INVALID other (select yes or leave blank)`,
      `FOLLOW-UP message (what is unclear about this response?)`,
      Explanation,
      everything()
    )

  cat(green(paste0("Found ", nrow(df), " other responses\n")))
  return(df)
}

# ---------- read raw data (single sheet) --------------------
read_raw_data_single <- function(filename, kobo_survey, cols_numeric) {
  df <- read_excel(filename, sheet = 1, col_types = "text")

  # Convert date / datetime columns
  cols_date <- c(kobo_survey$name[kobo_survey$type %in% c("today", "date", "date_survey")], "submission_date")
  submission_col <- intersect(c("_submission_time", "_submission__submission_time","date_survey"), colnames(df))

  if (length(submission_col) > 0) {
    df <- df %>% mutate(`_submission_time` = .[[submission_col[1]]])
  } else {
    df$`_submission_time` <- NA
  }

  datetime_cols <- intersect(c("start", "end", "today", submission_col[1]), colnames(df))
  if (length(datetime_cols) > 0) {
    df <- df %>%
      mutate_at(datetime_cols,
                ~ifelse(is.na(.), NA,
                        as.character(openxlsx::convertToDateTime(as.numeric(.)))))
  }

  date_cols_present <- intersect(cols_date, colnames(df))
  if (length(date_cols_present) > 0) {
    df <- df %>%
      mutate_at(date_cols_present,
                ~ifelse(is.na(.), NA,
                        as.character(as.Date(openxlsx::convertToDate(as.numeric(.))))))
  }

  # Rename _id / _uuid
  if ("_uuid" %in% colnames(df)) df <- df %>% rename(uuid = `_uuid`)
  if ("_id"   %in% colnames(df)) df <- df %>% rename(id   = `_id`)

  # Numeric conversions
  num_cols <- intersect(cols_numeric, colnames(df))
  if (length(num_cols) > 0) df <- df %>% mutate_at(num_cols, as.numeric)

  df
}

# # ---------- Save other responses simple ----------------------
# save_other_responses_simple <- function(df, output_path) {
#   if (is.null(df) || nrow(df) == 0) {
#     return(FALSE)
#   }
# 
#   wb <- openxlsx::createWorkbook()
#   addWorksheet(wb, "other_responses")
# 
#   # Style for header
#   header_style <- createStyle(fgFill = "#2c3e50", textDecoration = "bold",
#                               fontColour = "white", halign = "center")
# 
#   writeData(wb, "other_responses", df)
#   addStyle(wb, "other_responses", header_style, rows = 1, cols = 1:ncol(df))
# 
#   # Auto-filter
#   addFilter(wb, "other_responses", rows = 1, cols = 1:ncol(df))
# 
#   # Freeze header row
#   freezePane(wb, "other_responses", firstActiveRow = 2)
# 
#   # Auto-size columns (with max width)
#   for (i in 1:ncol(df)) {
#     setColWidths(wb, "other_responses", cols = i, widths = min(50, max(15, nchar(names(df)[i]) + 5)))
#   }
# 
#   saveWorkbook(wb, output_path, overwrite = TRUE)
#   return(TRUE)
# }