################################################################################
# READ DATASET SINGLE SHEET
################################################################################

read_raw_data <- function(filename){
  
  # fix dates
  df <- read_excel(filename, sheet=1, col_types="text")
  cat(green(paste0("--> RAW --> ", filename, " --> ", nrow(df), " entries, ", ncol(df), " columns \n")))
  cat(green("--> convert dates \n"))
  cols_date <- c(kobo_survey$name[kobo_survey$type %in% c("today", "date")], "submission_date")
  df <- df %>%
    mutate(submission_date = `_submission_time`) %>%
    mutate_at(cols_date,
              ~ifelse(is.na(.), NA, as.character(as.Date(convertToDate(as.numeric(.)))))) %>%
    mutate_at(c("start", "end", "_submission_time"),
              ~ifelse(is.na(.), NA, as.character(convertToDateTime(as.numeric(.)))))
  
  #---------------------------------------------------------------------------
  cat(green("--> rename id/uuid and add submission time \n"))
  df <- df %>%
    rename(id="_id", uuid="_uuid", `_submission_time` = "_submission_time")
  #-----------------------------------------------------------------------------
  # convert numeric columns
  cat(green("--> convert integer columns to numeric \n"))
  df <- df %>% mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  return(df)
}

################################################################################
# READ DATASET MULTIPLE SHEET
################################################################################

read_multisheet_raw_data <- function(filename) {
  
  # Get all sheet names
  sheets <- excel_sheets(filename)
  
  # Read and process each sheet
  sheet_data <- lapply(sheets, function(sheet_name) {
    
    # Read sheet
    df <- read_excel(filename, sheet = sheet_name, col_types = "text")
    
    cat(green(paste0("--> RAW --> ", filename, " --> Sheet: ", sheet_name, " --> ", 
                     nrow(df), " entries, ", ncol(df), " columns \n")))
    
    cat(green("--> Convert dates \n"))
    
    # Check for submission time column
    submission_col <- intersect(c("_submission_time", "_submission__submission_time"), colnames(df))
    
    if (length(submission_col) > 0) {
      df <- df %>% mutate(`_submission_time` = .[[submission_col[1]]])
    } else {
      df$`_submission_time` <- NA
      cat(red("--> WARNING: No submission time column found! \n"))
    }
    
    datetime_cols <- intersect(c("start", "end","today", submission_col[1]), colnames(df))
    
    if (length(datetime_cols) > 0) {
      df <- df %>%
        mutate_at(datetime_cols, ~ifelse(is.na(.), NA, as.character(convertToDateTime(as.numeric(.)))))
    }
    
    
    
    # Rename columns only if they exist
    rename_cols <- intersect(c("_id", "_uuid"), colnames(df))
    rename_map <- c("_id" = "id", "_uuid" = "uuid")
    
    if (length(rename_cols) > 0) {
      df <- df %>% rename_at(rename_cols, ~rename_map[.])
    }
    
    # Convert numeric columns only if they exist
    numeric_cols <- intersect(cols_numeric, colnames(df))
    
    if (length(numeric_cols) > 0) {
      df <- df %>% mutate_at(numeric_cols, as.numeric)
    }
    
    return(df)
  })
  
  sheets[1] <- "main"  # rename the first sheet to "main"
  # Return a named list of dataframes (each sheet is a dataframe)
  names(sheet_data) <- sheets
  return(sheet_data)
}



#############################################################################################################
# Check Soft Duplicate - Troubleshooting
#############################################################################################################

view_soft_duplicates <- function(df, uuids, only_differences=F){
  
  cols_to_remove <- kobo_survey %>% 
    filter(type %in% c("note", "calculate")) %>% pull(name) %>% 
    intersect(colnames(df))
  
  check <- df %>% 
    select(-all_of(cols_to_remove)) %>% 
    filter(uuid %in% uuids) %>% t() %>% as.data.frame()
  check$num_unique <- unlist(lapply(1:nrow(check), function(r)
    length(unique(as.character(check[r, colnames(check)])))))
  check <- check[!(rownames(check) %in% "_index"),]
  if (only_differences){
    check <- check %>% filter(num_unique!=1) %>% select(-num_unique)
  } else{
    check <- check %>% arrange(-num_unique)
  }
  return(check)
}

#############################################################################################################
# OTHER RESPONSES
#############################################################################################################

get_other_labels <- function(){
  
  other_labels <- kobo_survey %>% 
    filter((type =="text") &
             !name %in% c("pho_enumerator", "mail_enumerator")) %>%
    mutate(ref_question=as.character(lapply(relevant, get_ref_question))) %>%
    mutate(ref_question = ifelse(is.na(ref_question), name, ref_question)) %>%
    select(name, ref_question) %>%
    left_join(select(kobo_survey, ref_question=name, full_label=`label::english`), by = "ref_question")
  
  # check if any "text" type questions are "open response" type and not "other" options 
  # to be added manually to "labels_questions_others.xlsx" file 
  t <- filter(kobo_survey, type=="text" &
                !name %in% c("pho_enumerator", "mail_enumerator")) %>% pull(name)
  if (any(!(t %in% other_labels$name)) | any(!(other_labels$name %in% t))){
    cat(red(" - Some (text) type questions are not included in other_labels! \n"))
    cat(green(" - Please check kobo tool and add missing questions to labels_questions_others.xlsx if needed! \n"))
    cat(green(" - save updated excel file as labels_questions_others_edited.xlsx \n"))
  }
  
  cat(green(" - SAVING (./resources/labels_questions_others.xlsx) ... \n"))
  write.xlsx(other_labels, "resources/labels_questions_others.xlsx", overwrite=T)
  return(other_labels)
}
################################################################################

get_other_db <- function(){
  # generate other_db
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
################################################################################
add_to_cleaning_log_other_remove <- function(x, raw_dataset){
  raw_dataset <- raw_dataset
  
  change_type <- "Removing other response"
  
  # load option other
  option_other <- other_db$option_other[other_db$name==x$name]
  var_option_other <- paste0(x$ref_name, "/", option_other)
  
  # remove text of the response
  df <- data.frame(uuid=x$uuid, question=x$name, change_type=change_type, 
                   old_value=x$response_eth, new_value=NA)
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # remove relative entries
  if (x$ref_type=="select_one"){
    df <- data.frame(uuid=x$uuid, question=x$ref_name, change_type=change_type, 
                     old_value=option_other, new_value=NA)
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  } else if (x$ref_type=="select_multiple"){
    old_concat_value <- get_value_from_uuid(x$uuid, x$ref_name, raw_dataset)
    new_concat_value <- remove_choice(old_concat_value, option_other)
    new_concat_value <- ifelse(new_concat_value=="", NA, new_concat_value)
    df <- data.frame(uuid=x$uuid, question=x$ref_name, change_type=change_type,
                     old_value=old_concat_value, new_value=new_concat_value)
    cleaning_log_other <<- rbind(cleaning_log_other, df)
    
    if (is.na(new_concat_value)){
      # set all options to NA
      cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(x$ref_name, "/"))]
      old_values <- raw_dataset[raw_dataset$uuid==x$uuid, all_of(cols)]
      
      old_values <- as.character(old_values)
      if (length(cols)!=length(old_values)) stop("cols and old_values have different lengths")
      for (i in 1:length(cols)){
        df <- data.frame(uuid=x$uuid, question=cols[i], change_type=change_type, 
                         old_value=old_values[i], new_value=NA)
        cleaning_log_other <<- rbind(cleaning_log_other, df)
      }
    } else{
      # set var_option_other to "0"
      df <- data.frame(uuid=x$uuid, question=var_option_other, change_type=change_type,
                       old_value="1", new_value="0")
      cleaning_log_other <<- rbind(cleaning_log_other, df)
    }
  } else if (x$ref_type=="text" & x$name !="ki_id_other"){
    df <- data.frame(uuid=x$uuid, question=x$ref_name, change_type=change_type, 
                     old_value=x$response_eth , new_value=NA)
    cleaning_log_other <<- rbind(cleaning_log_other, df)
    
  }
}
################################################################################
add_to_cleaning_log_other_recode <- function(x, raw_dataset){
  raw_dataset<-raw_dataset
  if (x$ref_type[1]=="select_one") add_to_cleaning_log_other_recode_one(x,raw_dataset)
  if (x$ref_type[1]=="select_multiple") add_to_cleaning_log_other_recode_multiple(x, raw_dataset)
}
################################################################################
add_to_cleaning_log_other_recode_one <- function(x, raw_dataset){
  change_type <- "Recoding other response"
  raw_dataset<-raw_dataset
  
  # remove text of the response
  df <- data.frame(uuid=x$uuid, question=x$name, change_type=change_type, 
                   old_value=x$response_eth, new_value=NA)
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # recode choice
  new_value <- get_name_from_labell(x$list_name, x$existing_other)
  if (length(new_value)!=1){
    stop(paste0("Choice is not in the list: ", x$uuid, "; ", x$list_name, "; ", x$existing_other))
  } else{
    df <- data.frame(uuid=x$uuid, question=x$ref_name, change_type=change_type,
                     old_value=get_value_from_uuid(x$uuid, x$ref_name, raw_dataset), new_value=new_value)
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  }
}
################################################################################
add_to_cleaning_log_other_recode_multiple <- function(x, raw_dataset){
  change_type <- "Recoding other response"
  raw_dataset<-raw_dataset
  # get option other
  option_other <- other_db$option_other[other_db$name==x$name]
  var_option_other <- paste0(x$ref_name, "/", option_other)
  
  # remove text of the response
  df <- data.frame(uuid=x$uuid, question=x$name, change_type=change_type, 
                   old_value=x$response_eth, new_value=NA)
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # set option other to "0" and selected choices to "1" (if not already "1")
  choices <- unlist(lapply(str_split(x$existing_other, ";")[[1]], 
                           function(c) get_name_from_label(x$list_name, c)))
  if (option_other %in% choices) warning(paste0(x$name, ": adding again the other option"))
  el <- list(uuid=x$uuid, ref_name=other_db$ref_question[other_db$name==x$name], change_type=change_type)
  cleaning_log_other <<- rbind(cleaning_log_other, 
                               select_multiple_add_remove(el, to_remove=c(option_other), to_add=choices, raw_dataset))
}
################################################################################
select_multiple_add_remove <- function(el, to_remove, to_add=c(), raw_dataset){
  raw_dataset<-raw_dataset
  # load exclusive options
  exclusive_options <- c()
  # get column names
  cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(el$ref_name, "/"))]
  # generate cleaning log
  cl <- data.frame()
  if (is.na(get_value_from_uuid(el$uuid, el$ref_name,raw_dataset )) & length(to_remove)>0) stop()
  if (is.na(get_value_from_uuid(el$uuid, el$ref_name, raw_dataset))){
    #---------------------------------------------------------------------------
    # CASE 1) old value is NA
    if (length(exclusive_options)>0 & any(exclusive_options %in% to_add)) stop("To be implemented if needed")
    concat <- ""
    for (col in cols){
      choice <- str_split(col, "/")[[1]][2]
      if (choice %in% to_add) new_value <- "1"
      else new_value <- "0"
      cl <- rbind(cl, data.frame(uuid=el$uuid, question=col, 
                                 old_value=NA, new_value=new_value))
      if (choice %in% to_add) concat <- add_choice(concat, choice)
    }
    if (concat=="") stop()
    cl <- rbind(cl, data.frame(uuid=el$uuid, question=el$ref_name, 
                               old_value=NA, new_value=trimws(concat)))
  } else{
    #---------------------------------------------------------------------------
    # CASE 2) old value is not NA
    old_concat <- get_value_from_uuid(el$uuid, el$ref_name, raw_dataset)
    if (is.na(old_concat) | old_concat=="") stop()
    new_concat <- old_concat
    # remove options
    if (length(to_remove)>0){
      for (choice in to_remove){
        cl <- rbind(cl, data.frame(uuid=el$uuid, question=paste0(el$ref_name, "/", choice),
                                   old_value="1", new_value="0"))
        new_concat <- remove_choice(new_concat, choice)
      }
    }
    # add options
    if (length(to_add)>0){
      if (any(exclusive_options %in% to_add)){
        print(paste0("Recoding exclusive option: ", el$uuid, " --> ", el$ref_name, " --> ", to_add[1]))
        if (length(to_add)>1) stop("Cannot select an exclusive option with other options")
        cl <- data.frame()
        for (col in cols){
          option <- str_split(col, "/")[[1]][2]
          old_value <- get_value_from_uuid(el$uuid, col, raw_dataset)
          if (option==to_add[1]){
            cl <- rbind(cl, data.frame(uuid=el$uuid, question=col, 
                                       old_value="0", new_value="1"))
            new_concat <- to_add[1]
          } else if (old_value=="1"){
            cl <- rbind(cl, data.frame(uuid=el$uuid, question=col, 
                                       old_value="1", new_value="0"))
          }
        }
      } else{
        for (choice in to_add){
          old_value <- get_value_from_uuid(el$uuid, paste0(el$ref_name, "/", choice), raw_dataset)
          if (old_value=="0"){
            cl <- rbind(cl, data.frame(uuid=el$uuid, question=paste0(el$ref_name, "/", choice),
                                       old_value="0", new_value="1"))
            new_concat <- add_choice(new_concat, choice)
          }
        }
      }
    }
    #---------------------------------------------------------------------------
    # either update the concatenate column or set all to NA if new_concat is empty
    if (new_concat!="" & new_concat!=old_concat){
      cl <- rbind(cl, data.frame(uuid=el$uuid, question=el$ref_name, 
                                 old_value=old_concat, new_value=trimws(new_concat)))
    } else if (new_concat==""){
      cl <- data.frame()
      for (col in cols){
        cl <- rbind(cl, data.frame(uuid=el$uuid, question=col, 
                                   old_value=get_value_from_uuid(el$uuid, col, raw_dataset), 
                                   new_value=NA))
      }
      cl <- rbind(cl, data.frame(uuid=el$uuid, question=el$ref_name, 
                                 old_value=old_concat, new_value=NA))
    }
  }
  return(cl %>% mutate(change_type=el$change_type))
}

###############################################################################
################################################################################
################################################################################
###############################################################################

select_multiple_add_removee <- function(el, to_remove, to_add=c(),raw_dataset){
  raw_dataset<-raw_dataset
  # get column names
  cols <- colnames(raw_dataset)[str_starts(colnames(raw_dataset), paste0(el$ref_name, "/"))]
  # generate cleaning log
  cl <- data.frame()
  
  old_concat <- get_value_from_uuid(el$uuid, el$ref_name, raw_dataset)
  
  # If old value is NA, initialize new value
  if (is.na(old_concat) || old_concat == ""){
    new_concat <- ""
  } else {
    new_concat <- old_concat
  }
  
  # Remove options if needed
  if (length(to_remove) > 0){
    for (choice in to_remove){
      cl <- rbind(cl, data.frame(uuid=el$uuid, question=paste0(el$ref_name, "/", choice),
                                 old_value="1", new_value="0"))
      new_concat <- remove_choice(new_concat, choice)
    }
  }
  
  # Add options (always add without condition)
  if (length(to_add) > 0){
    for (choice in to_add){
      cl <- rbind(cl, data.frame(uuid=el$uuid, question=paste0(el$ref_name, "/", choice),
                                 old_value=NA, new_value="1"))
      new_concat <- add_choice(new_concat, choice)
    }
  }
  
  # Update concatenate column
  if (new_concat != old_concat){
    cl <- rbind(cl, data.frame(uuid=el$uuid, question=el$ref_name, 
                               old_value=old_concat, new_value=trimws(new_concat)))
  }
  
  return(cl %>% mutate(change_type=el$change_type))
}

################################################################################
add_to_cleaning_log_new_choice <- function(x, raw_dataset){
  raw_dataset<-raw_dataset
  if (x$ref_type[1]=="select_one") add_to_cleaning_log_new_choice_one(x)
  if (x$ref_type[1]=="select_multiple") add_to_cleaning_log_new_choice_multiple(x, raw_dataset)
}
################################################################################
add_to_cleaning_log_new_choice_one <- function(x){
  change_type <- "Recoding other response"
  
  # remove text of the response
  df <- data.frame(uuid=x$uuid, question=x$name, change_type=change_type, 
                   old_value=x$response_eth, new_value=NA)
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # recode choice
  new_value <- x$col_to_add
  if (length(new_value)!=1){
    stop(paste0("Choice is not in the list: ", x$uuid, "; ", x$list_name, "; ", x$true_other))
  } else{
    df <- data.frame(uuid=x$uuid, question=x$ref_name, change_type=change_type,
                     old_value="other", new_value=new_value)
    
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  }
}
################################################################################
add_to_cleaning_log_new_choice_multiple <- function(x,raw_dataset){
  raw_dataset<-raw_dataset
  change_type <- "Recoding other response"
  
  # get option other
  option_other <- x$col_to_add
  var_option_other <- paste0(x$ref_name, "/", option_other)
  
  # remove text of the response
  df <- data.frame(uuid=x$uuid, question=x$name, change_type=change_type, 
                   old_value=x$response_eth, new_value=NA)
  
  
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # set option other to "0" and selected choices to "1" (if not already "1")
  choices <- unlist(lapply(str_split(x$true_other, ";")[[1]],
                           function(c) get_name_from_label(x$list_name, c)))
  
  if (option_other %in% choices) warning(paste0(x$name, ": adding again the other option"))
  el <- list(uuid=x$uuid, ref_name=other_db$ref_question[other_db$name==x$name], change_type=change_type)
  cleaning_log_other <<- rbind(cleaning_log_other, 
                               select_multiple_add_removee(el, to_remove="other", to_add=option_other, raw_dataset))
}




################################################################################
################################################################################
################################################################################

save_other_responses <- function(df, ref_date = "",output_path) {
  
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
  
  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 19)
  addStyle(wb, "cleaning_log", style = style.col.color, rows = 1:(nrow(df) + 1), cols = 20)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 21)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 22)
  addStyle(wb, "cleaning_log", style = style.col.color1, rows = 1:(nrow(df) + 1), cols = 23)
  addStyle(wb, "cleaning_log", style = style.col.color, rows = 1:(nrow(df) + 1), cols = 24)
  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 25)
  addStyle(wb, "cleaning_log", style = style.col.color2, rows = 1:(nrow(df) + 1), cols = 26)
  
  
  # Freeze the first row
  freezePane(wb, sheet = "cleaning_log", firstActiveRow = 2)
  
  # Add column filters
  addFilter(wb, "cleaning_log", rows = 1, cols = 1:ncol(df))
  
  addStyle(wb, "cleaning_log", style = createStyle(wrapText = TRUE), rows = 1:19, cols = 1)
  
  
  
  # Add Dropdown values worksheet and data validation
  addWorksheet(wb, "Dropdown_values")
  for (r in 1:nrow(other_db)) {
    if (other_db$q_type[r] != "text") {
      choices <- str_split(other_db$choices[r], ";;")[[1]]
      writeData(wb, sheet = "Dropdown_values", x = choices, startCol = r)
      uuids <- which(df$question_name == other_db$name[r])
      if (length(uuids) > 0) {
        column_letter <- get_column_letter(r)
        values <- paste0("'Dropdown_values'!$", column_letter, "$1:$", column_letter, "$", other_db$num_choices[r])
        dataValidation(wb, "cleaning_log", col = 19, rows = uuids + 1, type = "list", value = values)
        dataValidation(wb, "cleaning_log", col = 20, rows = uuids + 1, type = "list", value = values)
        dataValidation(wb, "cleaning_log", col = 21, rows = uuids + 1, type = "list", value = values)
        
      }
    }
  }
  writeData(wb, sheet = "Dropdown_values", x = c("Yes"), startCol = r + 1)
  column_letter <- get_column_letter(r + 1)
  values <- paste0("'Dropdown_values'!$", column_letter, "$1:$", column_letter, "$1")
  dataValidation(wb, "cleaning_log", col = 19, rows = 2:(nrow(df) + 1), type = "list", value = values)
  
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

#############################################################################################################
# UTILS
#############################################################################################################
get_name_from_label <- function(list_name, label){
  return(kobo_choices$name[kobo_choices$list_name==list_name & kobo_choices$`label::english`==label])
}

get_name_from_labell <- function(list_name, label){
  normalize <- function(x){
    x <- tolower(x)                          # make lowercase
    x <- gsub("[^a-z0-9]+", "_", x)          # replace special characters with "_"
    x <- gsub("^_|_$", "", x)                # remove leading/trailing "_"
    x <- trimws(x)                           # trim whitespace
    return(x)
  }
  
  label_norm <- normalize(label)
  choices_norm <- normalize(kobo_choices$`label::english`)
  
  result <- kobo_choices$name[
    kobo_choices$list_name == list_name & 
      choices_norm == label_norm
  ]
  
  if (length(result) == 0) {
    return(NA)   # nothing found
  } else if (length(result) > 1) {
    stop(paste0("Multiple matches found for list_name = ", list_name, 
                " and label = ", label))
  } else {
    return(result)
  }
}

################################################################################
get_label_from_name <- function(list.name, name){
  return(kobo_choices$`label::english`[kobo_choices$list_name==list.name & kobo_choices$name==name])
}
################################################################################
get_value_from_uuid <- function(uuid, column, raw_dataset){
  raw_dataset <- raw_dataset
  value <- raw_dataset[[column]][raw_dataset$uuid==uuid]
  return(value)
}
################################################################################
add_choice <- function(concat_value, choice){
  l <- str_split(concat_value, " ")[[1]]
  l <- sort(unique(c(l, choice)))
  l <- l[l!=""]
  return(paste(l, collapse=" "))
}
################################################################################
remove_choice <- function(concat_value, choice){
  l <- str_split(concat_value, " ")[[1]]
  l <- l[l!=choice]
  return(paste(l, collapse=" "))
}
################################################################################
get_ref_question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}
################################################################################


# Define the formatting and saving function
format_and_save_excel <- function(data, file_name_prefix = "formatted_file", sheet_name = "Formatted Data") {
  # Base Colors
  base_colors <- c("#E4DFD4", "#F6F4F0", "#DDDEDF", "#BCBCBD", "#E8E9E9", "#C7C8CA")
  
  # Function to map unique values to colors
  get_color <- function(unique_values) {
    colors <- rep(base_colors, length.out = length(unique_values))
    setNames(colors, unique_values)
  }
  
  # Generate the file name with timestamp
  file_name <- paste0(file_name_prefix, "_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".xlsx")
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, sheetName = sheet_name)
  
  # Write the data to the worksheet
  writeData(wb, sheet_name, data)
  
  # Get unique values of `uuid` and assign colors
  unique_uuid <- unique(data$uuid)
  color_mapping <- get_color(unique_uuid)
  
  # Apply styles for each row
  for (i in seq_len(nrow(data))) {
    row_color <- color_mapping[data$uuid[i]]
    
    if (!(row_color %in% base_colors)) {
      stop("Invalid color value detected: ", row_color)
    }
    
    # Create style with background color only
    row_style <- createStyle(fgFill = row_color)
    
    # Apply background color style to the entire row
    addStyle(
      wb,
      sheet = sheet_name,
      style = row_style,
      rows = i + 1,  # +1 for the header row
      cols = 1:ncol(data),
      gridExpand = TRUE
    )
  }
  
  # Set column widths dynamically
  setColWidths(wb, sheet = sheet_name, cols = seq_len(ncol(data)), widths = "auto")
  
  # Define header style
  header_style <- createStyle(
    fontSize = 9,
    fontColour = "black",
    halign = "left",
    fgFill = "#EE5859",
    border = "TopBottomLeftRight",
    borderColour = "black",
    borderStyle = "thick",
    textDecoration = "bold"
  )
  
  # Apply header style
  addStyle(
    wb,
    sheet = sheet_name,
    style = header_style,
    rows = 1,  # Header row
    cols = seq_len(ncol(data)),
    gridExpand = TRUE
  )
  
  # Freeze the header row
  freezePane(wb, sheet = sheet_name, firstActiveRow = 2)
  
  # Add column filters
  addFilter(wb, sheet = sheet_name, rows = 1, cols = seq_len(ncol(data)))
  
  # Save the workbook
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  # Print success message
  cat(green("Workbook saved successfully as: "), file_name, "\n")
}

########################################################################################################
download_audit_files <- function(df, uuid_column = "_uuid", audit_dir, usr, pass){
  if (!"httr" %in% installed.packages()) 
    stop("The package is httr is required!")
  
  if (is.na(audit_dir) || audit_dir == "") 
    stop("The path for storing audit files can't be empty!")
  
  if (is.na(usr) || usr == "") 
    stop("Username can't be empty!")
  
  if (is.na(pass) || pass == "") 
    stop("Password can't be empty!")
  
  # checking if the output directory is already available
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir)
    if (dir.exists(audit_dir)) {
      cat("Attention: The audit file directory was created in", audit_dir,"\n")
    }
  }
  
  # checking if creating output directory was successful
  if (!dir.exists(audit_dir))
    stop("download_audit_fils was not able to create the output directory!")
  # checking if uuid column exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  # checking if column audit_URL exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  if (!"audit_URL" %in% names(df))
    stop("Error: the column audit_URL is not available in data set.")
  
  # getting the list of uuids that are already downloaded
  available_audits <- dir(audit_dir)
  
  # excluding uuids that their audit files are already downloaded
  df <- df[!df[[uuid_column]] %in% available_audits,]
  
  audits_endpoint_link <- df[["audit_URL"]]
  names(audits_endpoint_link) <- df[[uuid_column]]
  audits_endpoint_link <- na.omit(audits_endpoint_link)
  
  if (length(audits_endpoint_link) > 0) {
    # iterating over each audit endpoint from data
    for (i in 1:length(audits_endpoint_link)) {
      uuid = names(audits_endpoint_link[i])
      endpoint_link_i <- audits_endpoint_link[i]
      cat("Downloading audit file for", uuid, "\n")
      
      # requesting data
      audit_file <- content(GET(endpoint_link_i,
                                authenticate(usr, pass),
                                timeout(1000),
                                progress()), "text", encoding = "UTF-8")
      
      if (!is.na(audit_file)) {
        if (length(audit_file) > 2) {
          dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
          write.csv(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F)
        }else if(!audit_file == "Attachment not found"){
          if (grepl("[eventnodestartend]", audit_file)) {
            dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
            write.table(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F, col.names = FALSE, quote = F)
          } else{
            cat("Error: Downloading audit was unsucessful!\n")
          }
        }
      } else{
        cat("Error: Downloading audit was unsucessful!\n")
      }
    }
  } else{
    cat("Attention: All audit files for given data set is downloaded!")
  }
}




zip_audit_files <- function(audit_dir, zip_name = "audit.zip") {
  if (dir.exists(audit_dir)) {
    # Ensure the zip file is saved inside the audit directory
    zip_path <- file.path(audit_dir, zip_name)
    zip(zipfile = zip_path, files = list.files(audit_dir, full.names = TRUE, recursive = TRUE))
    cat("All audit files have been zipped into:", zip_path, "\n")
    
    cat(green("\033[31mAll audit folders and files have been removed, except the zip file.\033[0m\n"))
  } else {
    cat("Error: Audit directory does not exist. No files to zip.\n")
  }
}


##########################################################
audit_files_exist <- function(uuid, audit_dir) {
  audit_folder <- file.path(audit_dir, uuid)
  dir.exists(audit_folder) && length(list.files(audit_folder)) > 0}





####################################################### create other response ###############################################################################
check_others_custom <- function(sheet_name, not_other) {
  if (sheet_name == "main") {
    # For MAIN sheet
    data_sheet <- filtered_data[[sheet_name]] %>%
      distinct(uuid, .keep_all = TRUE)
    
    raw_data_sheet <- raw_data_all[["main"]] %>%
      distinct(uuid, .keep_all = TRUE)
    var_other_raw <- other_db$name[other_db$name %in% colnames(data_sheet)]
    var_other_raw <- var_other_raw[!var_other_raw %in% not_other]
    
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
      left_join(select(data_sheet, uuid, `_submission_time`,  admin1Name, admin2Name, admin3Name, admin4Name, cluster_id,point_number, team_leader,enum_id), by = "uuid") %>%
      mutate(`_submission_time` = format(as.Date(`_submission_time`), "%y:%m:%d")) %>%
      arrange(question_name, uuid) %>%
      left_join(select(other_db, name, full_label, q_type, list_name, ref_question),
                by = c("question_name" = "name")) %>%
      select(
        uuid, `_submission_time`, admin1Name, admin2Name, admin3Name, admin4Name, cluster_id,point_number, team_leader,enum_id,
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
        selected_choices = NA,
        sheet = sheet_name
      )
    
    if (nrow(df) > 0) {
      for (r in seq_len(nrow(df))) {
        ref_name <- other_db$ref_question[other_db$name == df$question_name[r]]
        q_type <- other_db$q_type[other_db$name == df$question_name[r]]
        
        if (!is.na(ref_name) && q_type == "select_multiple") {
          choices <- raw_data_sheet[[ref_name]][raw_data_sheet$uuid == df$uuid[r]]
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
    df <- relocate(df, "_index", .before = "sheet")
    
    return(df)
    
  } else {
    # For non-main sheets
    data_sheet <- filtered_data[[sheet_name]]
    raw_data_sheet <- raw_data_all[[sheet_name]]
    
    var_other_raw <- other_db$name[other_db$name %in% colnames(data_sheet)]
    var_other_raw <- var_other_raw[!var_other_raw %in% c("income_expenditure", "distance_reason", "income_expenditure")]
    
    # Add check here
    if (length(var_other_raw) == 0) {
      cat(yellow(paste0("No 'other' columns found for sheet: ", sheet_name, "\n")))
      return(tibble())
    }
    
    other_responses_main <- data_sheet %>%
      select(c("_index", "_submission__uuid", all_of(var_other_raw))) %>%
      pivot_longer(
        cols = all_of(var_other_raw),
        names_to = "question_name",
        values_to = "response_eth"
      ) %>%
      filter(!is.na(response_eth)) %>%
      select("_index", "_submission__uuid", question_name, response_eth) %>%
      rename(uuid = "_submission__uuid")
    
    other_responses <- other_responses_main %>%
      mutate(response_en = NA)
    
    df <- other_responses %>%
      left_join(select(filtered_data$main, uuid, `_submission_time`, admin1Name, admin2Name, admin3Name, admin4Name, cluster_id,point_number, team_leader,enum_id), by = "uuid") %>%
      mutate(`_submission_time` = format(as.Date(`_submission_time`), "%y:%m:%d")) %>%
      arrange(question_name, uuid) %>%
      left_join(select(other_db, name, full_label, q_type, list_name, ref_question), by = c("question_name" = "name")) %>%
      # FIXED: include ref_question so it can be used in the loop
      select(
        uuid, "_index", `_submission_time`,  admin1Name, admin2Name, admin3Name, admin4Name, cluster_id,point_number, team_leader,enum_id,
        question_name, q_type, list_name, full_label, ref_question,
        response_eth, response_en
      )%>%
      mutate(
        "TRUE other (copy response_en or provide a better translation)" = NA,
        "EXISTING other 1 (select the most appropriate choice)" = NA,
        "EXISTING other 2 (select the most appropriate choice)" = NA,
        "EXISTING other 3 (select the most appropriate choice)" = NA,
        "INVALID other (select yes or leave blank)" = NA,
        "FOLLOW-UP message (what is unclear about this response?)" = NA,
        "Explanation" = NA,
        selected_choices = NA,
        sheet = sheet_name
      )
    
    if (nrow(df) > 0) {
      for (r in seq_len(nrow(df))) {
        ref_name <- df$ref_question[r]
        q_type <- df$q_type[r]
        
        if (!is.na(ref_name) && q_type == "select_multiple") {
          choices <- raw_data_sheet[[ref_name]][raw_data_sheet$`_index` == df$`_index`[r]]
          if (!is.na(choices)) {
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
    
    df <- select(df, -ref_question)
    return(df)
  }
}
#################################################### create_combined_sheet_log ##################################################################################

create_combined_sheet_log <- function(sheet_name, log_list) {
  
  if (length(log_list) == 0 || all(sapply(log_list, is.null))) {
    warning("Log list is empty. Returning an empty combined log.")
    return(list(cleaning_log = data.frame(), message = "No logs to combine."))
  }
  # Create the combined log
  combined_log <- create_combined_log(list_of_log = log_list, dataset_name = "checked_dataset")
  
  # Add submission UUID by matching index
  combined_log$cleaning_log$`_submission__uuid` <- raw_data_all[[sheet_name]]$`_submission__uuid`[
    match(combined_log$cleaning_log$`uuid`, raw_data_all[[sheet_name]]$`_index`)
  ]
  
  # Set sheet name
  combined_log$cleaning_log$sheet <- sheet_name
  
  # Move _submission__uuid to uuid and update _index
  combined_log$cleaning_log$`_index` <- combined_log$cleaning_log$`uuid`
  combined_log$cleaning_log$`uuid` <- combined_log$cleaning_log$`_submission__uuid`
  
  # Drop helper column
  combined_log$cleaning_log <- combined_log$cleaning_log %>% select(-`_submission__uuid`)
  
  # # Add admin info from main sheet
  combined_log <- add_info_to_cleaning_log(
    dataset = raw_data_all$main,
    cleaning_log = combined_log$cleaning_log,
    dataset_uuid_column = "uuid",
    cleaning_log_uuid_column = "uuid",
    information_to_add = c( "admin1Name", "admin2Name", "admin3Name", "admin4Name", "cluster_id","cluster_type","point_number", "team_leader","enum_id", "_submission_time")
  )
  
  return(combined_log)
}

##################################### filter_data_by_submission_date ###########################################
filter_data_by_submission_date <- function(submissions, raw_data) {
  cat(green("Cleaning log for dates:", paste(as.Date(submissions), collapse = ", "), "\n"))
  
  # Step 1: Filter the main sheet
  main_sheet <- raw_data$main
  
  # Ensure ``_submission_time`` is converted to Date format
  main_sheet <- main_sheet %>%
    mutate(`_submission_time` = as.Date(`_submission_time`, format = "%Y-%m-%d"))
  
  # Filter the main sheet by submissions
  filtered_main_sheet <- main_sheet %>%
    filter(`_submission_time` %in% submissions)
  
  # Extract valid UUIDs from the filtered main sheet
  valid_uuids <- filtered_main_sheet$uuid
  
  # Step 2: Filtering function for other sheets
  clean_sheet_data <- function(df, sheet_name) {
    if (sheet_name == "main") {
      return(filtered_main_sheet)
    } else if ("_submission__submission_time" %in% colnames(df)) {
      # Ensure `_submission__submission_time` is converted to Date format
      df <- df %>%
        mutate(`_submission__submission_time` = as.Date(`_submission__submission_time`, format = "%Y-%m-%d"))
      
      # Filter by submissions and valid UUIDs
      return(df %>%
               filter(`_submission__submission_time` %in% submissions & 
                        `_submission__uuid` %in% valid_uuids))
    } else {
      cat(yellow("Skipping sheet:", sheet_name, "- No `_submission__submission_time` column found.\n"))
      return(NULL)
    }
  }
  
  # Step 3: Apply the filtering function to each sheet in raw_data
  filtered_data <- purrr::map2(
    raw_data, 
    names(raw_data), 
    clean_sheet_data
  )
  
  # Remove NULL results
  filtered_data <- purrr::compact(filtered_data)
  
  return(filtered_data)
}
################################################# filter_multisheet_by_uuid ###################################################################
filter_multisheet_by_uuid <- function(raw_data_all, uuid_list) {
  # Step 1: Filter main sheet by _uuid
  filtered_main <- raw_data_all$main %>%
    filter(`uuid` %in% uuid_list)
  
  # Step 2: Extract matching UUIDs
  valid_uuids <- filtered_main$`uuid`
  
  # Step 3: Define function to filter each sheet
  filter_sheet <- function(df, sheet_name) {
    if (sheet_name == "main") {
      return(filtered_main)
    } else if ("_submission__uuid" %in% names(df)) {
      return(df %>% filter(`_submission__uuid` %in% valid_uuids))
    } else {
      return(df)  # return unfiltered sheet if no UUID column
    }
  }
  
  # Step 4: Apply to all sheets
  filtered_data <- mapply(
    filter_sheet,
    raw_data_all,
    names(raw_data_all),
    SIMPLIFY = FALSE
  )
  
  return(filtered_data)
}

################################################## save_anonymised_data  ################################################## 

save_anonymised_data <- function(output_file, PII_list, pii_sheet_to_remove, raw_data_all) {
  # Create output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Internal function to drop PII columns from each sheet
  remove_pii_from_sheets <- function(sheet_data, pii_cols) {
    lapply(sheet_data, function(df) {
      df %>% select(-any_of(pii_cols))
    })
  }
  
  # Apply PII removal
  raw_data_anonymised <- remove_pii_from_sheets(raw_data_all, PII_list)
  
  # Remove the pii_sheet_to_remove only if it exists and is not empty
  if (pii_sheet_to_remove %in% names(raw_data_anonymised)) {
    sheet_data <- raw_data_anonymised[[pii_sheet_to_remove]]
    if (nrow(sheet_data) > 0 || ncol(sheet_data) > 0) {
      raw_data_anonymised[[pii_sheet_to_remove]] <- NULL
      message(paste("Removed sheet:", pii_sheet_to_remove))
    }
  }
  # Write to Excel
  write.xlsx(raw_data_anonymised, output_file)
  cat(green("Anonymized data saved to:", output_file, "\n"))
  
  return(raw_data_anonymised)
}
################################################## save_anonymised_data  ################################################## 

anonymize_data <- function(raw_data_all, PII_list, pii_sheet_to_remove) {
  
  # Internal function to drop PII columns from each sheet
  remove_pii_from_sheets <- function(sheet_data, pii_cols) {
    lapply(sheet_data, function(df) {
      df %>% select(-any_of(pii_cols))
    })
  }
  
  # Apply PII removal
  raw_data_anonymised <- remove_pii_from_sheets(raw_data_all, PII_list)
  
  # Remove the pii_sheet_to_remove only if it exists and is not empty
  if (pii_sheet_to_remove %in% names(raw_data_anonymised)) {
    sheet_data <- raw_data_anonymised[[pii_sheet_to_remove]]
    if (nrow(sheet_data) > 0 || ncol(sheet_data) > 0) {
      raw_data_anonymised[[pii_sheet_to_remove]] <- NULL
      message(paste("Removed sheet:", pii_sheet_to_remove))
    }
  }
  
  cat(green("Data anonymized"))
  
  return(raw_data_anonymised)
}

################################################## add_info_to_cleaning_log  ##################################################

restore_uuid_column <- function(cleaning_log, clean_data, sheet_name) {
  if (sheet_name != "main") {
    clean_data <- clean_data %>%
      mutate(`_index` = as.character(`_index`))   # force character
    
    out <- cleaning_log %>%
      rename(`_index` = uuid) %>%
      mutate(
        issue = "Other response - to be checked",
        feedback = "",
        `changed (select yes/ no)` = ifelse(change_type=="no_action", "No", "Yes"),
        `_index` = as.character(`_index`)
      ) %>%
      left_join(clean_data %>% select(`_index`, uuid), by = "_index") %>%
      mutate(sheet = sheet_name)
    
    
  } else {
    out <- cleaning_log %>%
      mutate(
        issue = "Other response - to be checked",
        feedback = "",
        `changed (select yes/ no)` = ifelse(change_type=="no_action", "No", "Yes"),
        sheet = sheet_name,
        `_index` = ""
      )
    
  }
  
  return(out)
}



##################### Functions to create cleaning log for others #####################

apply_others_cleaning_log <- function(dataset, sheet_name, uuid_col = "uuid",
                                      or_remove, or_recode, or_true) {
  
  
  
  or_true <- or_true %>% filter(uuid %in% dataset$uuid)
  # View(or)
  res <- or_true %>% 
    filter(!is.na(true_other) ) %>%
    filter(q_type=="select_multiple") %>%
    group_by(list_name) %>% 
    mutate(n_list=n()) %>% 
    group_by(list_name, true_other) %>% 
    summarise(n_list=n_list[1], n_option=n()) %>%
    arrange(-n_list, -n_option) %>% 
    select(-n_list) %>%
    mutate(
      col_to_add = paste0(
        list_name, "/", 
        str_replace_all(
          tolower(true_other), "[^a-z0-9 /]", ""
        ) %>%
          str_replace_all("/", "_or_") %>%         # Replace "/" with "_or_"
          str_replace_all(" \\(.*", "") %>%        # Remove everything from "(" onwards (including "(")
          str_replace_all(" ", "_")
      ))
  
  
  
  new_col_names <- unique(res$col_to_add)
  
  # Add newlly added columns to dataset, initializing them with NA
  for (col in new_col_names) {
    dataset[[col]] <- NA
  }
  
  
  ress <- res %>%
    mutate(list_name_other = paste0(list_name, "_other"))
  
  
  # Reorder columns: Insert new columns after the respective list_name columns
  reordered_cols <- c()
  for (col in names(dataset)) {
    reordered_cols <- c(reordered_cols, col)
    
    # If col is in list_name, add its corresponding col_to_add after it
    if (col %in% ress$list_name_other) {
      new_col <- ress$col_to_add[ress$list_name_other == col]
      reordered_cols <- c(reordered_cols, new_col)
    }
  }
  
  # Reorder dataset
  dataset <- dataset %>%
    select(all_of(reordered_cols))
  
  
  
  
  
  
  cleaning_log_other <<- data.frame()
  # Filter by sheet
  or_remove_sheet <- or_remove
  or_recode_sheet <- or_recode
  or_true_sheet   <- or_true %>%
    mutate(
      col_to_add = paste0(str_replace_all(tolower(true_other), "[^a-z0-9 ]", "") %>% str_replace_all(" ", "_"))
    )
  
  # Initialize cleaning log
  
  # 1) handle remove
  if (nrow(or_remove_sheet) > 0) {
    cat("Number of responses to be removed:", nrow(or_remove_sheet), "\n")
    
    cleaning_log_remove <- list()
    
    for (r in 1:nrow(or_remove_sheet)) {
      remove_log <- add_to_cleaning_log_other_remove(or_remove_sheet[r, ], raw_dataset = dataset)
      cleaning_log_remove[[r]] <- remove_log
    }
    cleaning_log_remove <- dplyr::bind_rows(cleaning_log_remove)
    
    # write.csv(cleaning_log_remove, paste0("cleaning_log_other_remove_", sheet_name, ".csv"), row.names = FALSE)
  }  else {
    cleaning_log_remove <- data.frame()
  }
  
  
  
  # 2) handle recoding
  cat("Number of responses to be recoded:", nrow(or_recode_sheet), "\n")
  
  if (nrow(or_recode_sheet) > 0) {
    cleaning_log_recode <- list()
    
    for (r in 1:nrow(or_recode_sheet)) {
      recode_log <- add_to_cleaning_log_other_recode(or_recode_sheet[r, ], raw_dataset = dataset)
      cleaning_log_recode[[r]] <- recode_log
    }
    
    cleaning_log_recode <- dplyr::bind_rows(cleaning_log_recode)
    
    # write.csv(cleaning_log_recode, paste0("cleaning_log_other_recode_", sheet_name, ".csv"), row.names = FALSE)
  } else {
    cleaning_log_recode <- data.frame()
  }
  
  
  # 3) handle true other
  if (nrow(or_true_sheet) > 0) {
    cat("Number of true other:", nrow(or_true_sheet), "\n")
    
    cleaning_log_true <- list()
    
    for (r in 1:nrow(or_true_sheet)) {
      recode_log <- add_to_cleaning_log_new_choice(or_true_sheet[r, ], raw_dataset = dataset)
      cleaning_log_true[[r]] <- recode_log
    }
    
    cleaning_log_true <- dplyr::bind_rows(cleaning_log_true)
    # cl_or_true <- or_true_sheet %>%
    #   select(uuid = all_of(uuid_col), name, response_eth, true_other) %>%
    #   rename(question = name, old_value = response_eth, new_value = true_other) %>%
    #   mutate(change_type = "Translation of true other")
    
    add_to_cleaning_log_new_choice
  } else {
    cleaning_log_true <- data.frame()
  }
  
  # put all logs in a list
  logs_list <- list(
    cleaning_log_recode,
    cleaning_log_remove,
    cleaning_log_true
  )
  
  # drop NULL objects
  logs_list <- logs_list[!sapply(logs_list, is.null)]
  
  # bind safely
  cleaning_log_other <- dplyr::bind_rows(logs_list) %>%
    distinct()
  
  
  # 4) Apply changes to dataset
  for (r in 1:nrow(cleaning_log_other)) {
    col_name <- cleaning_log_other$question[r]
    uuid_val <- cleaning_log_other[[uuid_col]][r]
    new_val  <- cleaning_log_other$new_value[r]
    
    old_val <- dataset[[col_name]][dataset[[uuid_col]] == uuid_val]
    dataset[[col_name]][dataset[[uuid_col]] == uuid_val] <- new_val
    updated_val <- dataset[[col_name]][dataset[[uuid_col]] == uuid_val]
    
    cat(r, "-", uuid_val, ":", paste(old_val, collapse = ", "), " --> ", paste(updated_val, collapse = ", "), "\n")
  }
  
  return(list(cleaned_data = dataset, cleaning_log_other = cleaning_log_other))
}












##################### Review cleaning process function #####################


review_cleaning_process <- function(filled_fu_log, cleaning_log_other = NULL, raw_data, clean_data, sheet_name="review_log", file_path,
                                    raw_dataset_uuid_column = "uuid", 
                                    clean_dataset_uuid_column = "uuid",
                                    cleaning_log_uuid_column = "uuid"
) {
  
  # Create deletion log
  deletion_log <- filled_fu_log %>%
    filter(change_type == "remove_survey") %>%
    rename(feedback_deletion = `FOLLOW-UP message`)  # Rename to avoid duplicates
  
  filled_log_no_deletion <- filled_fu_log %>%
    filter(change_type != "remove_survey") %>%
    filter(!.data[[cleaning_log_uuid_column]] %in% deletion_log[[cleaning_log_uuid_column]]) %>%
    rename(feedback_clean = `FOLLOW-UP message`)  # Rename to avoid duplicates
  
  
  # handle optional cleaning_log_other
  if (!is.null(cleaning_log_other) && nrow(cleaning_log_other) > 0) {
    cleaning_log_other <- cleaning_log_other %>%
      mutate(issue = "Other response - to be checked")
    
    combined_cleaning_log <- filled_log_no_deletion %>%
      select(all_of(c(cleaning_log_uuid_column, "question", "issue", "change_type", "old_value", "new_value"))) %>%
      rbind(cleaning_log_other)
    
  } else {
    combined_cleaning_log <- filled_log_no_deletion %>%
      select(cleaning_log_uuid_column, question, issue, change_type,  old_value, new_value)
  }
  
  # Run review_cleaning
  review_of_cleaning <- review_cleaning(
    raw_dataset = raw_data,
    raw_dataset_uuid_column = raw_dataset_uuid_column,
    clean_dataset = clean_data,
    clean_dataset_uuid_column = clean_dataset_uuid_column,
    cleaning_log = combined_cleaning_log,
    cleaning_log_uuid_column = cleaning_log_uuid_column,
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_change_type_column = "change_type",
    cleaning_log_old_value_column = "old_value",
    deletion_log = deletion_log,
    deletion_log_uuid_column = cleaning_log_uuid_column,
    check_for_deletion_log = TRUE
  )
  
  # Handle results
  if (nrow(review_of_cleaning) == 0) {
    cat(green(paste0("✅ Well done, Good cleaning on sheet: ", sheet_name, " \n")))
    return(review_of_cleaning)
  } else {
    cat(red(paste0("⚠️ Review cleaning needed on sheet: ", sheet_name, " \n")))
    
    wb <- createWorkbook()
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, review_of_cleaning)
    
    # Save the file
    saveWorkbook(wb, file_path, overwrite = TRUE)
    
    return(review_of_cleaning)
  }
}





##################### format and save dataset

custom_save <- function(multi_df, header_color = "#4F81BD", file_name = "styled_output.xlsx") {
  # Create workbook
  wb <- createWorkbook()
  
  # Define header style
  header_style <- createStyle(
    fontColour = "#FFFFFF",       # white text
    fgFill = header_color,        # custom color
    halign = "CENTER",
    textDecoration = "Bold",
    valign = "CENTER",
    border = "Bottom"
  )
  
  # Loop through each sheet
  for (sheet_name in names(multi_df)) {
    safe_name <- gsub("[\\*\\?/\\[\\]:]", "_", substr(sheet_name, 1, 31))  # Excel-safe name
    addWorksheet(wb, safe_name)
    
    df <- as.data.frame(multi_df[[sheet_name]])
    
    # Write data with filter
    writeData(wb, sheet = safe_name, x = df, withFilter = TRUE)
    
    # Apply style to first row
    if (ncol(df) > 0) {
      addStyle(
        wb, sheet = safe_name,
        style = header_style,
        rows = 1, cols = 1:ncol(df),
        gridExpand = TRUE
      )
    }
    
    # Freeze top row
    freezePane(wb, sheet = safe_name, firstRow = TRUE)
    
    # Auto-fit columns
    setColWidths(wb, sheet = safe_name, cols = 1:ncol(df), widths = "auto")
  }
  
  # Save workbook safely
  tryCatch({
    saveWorkbook(wb, file_name, overwrite = TRUE)
    message("Excel file saved as: ", file_name)
  }, error = function(e) {
    message("❌ Error saving Excel file: ", e$message)
  })
}

#############################################################################################################

compare_sampling_vs_survey <- function(clean_data_main, sample_file,
                                       sheet_summary = "sampling summary",
                                       sheet_clusters = "samples_all",
                                       excluded_clusters = NULL,
                                       rem_clusters = NULL) {
  
  # ------------------------------------------------------------------
  # 1. Survey summary by admin2 (non-quota)
  data_summary <- clean_data_main %>%
    filter(cluster_type != "quota", !(cluster_id %in% rem_clusters)) %>%
    group_by(admin2) %>%
    summarise(
      survey_count = n(),
      unique_clusters = n_distinct(cluster_id),
      .groups = "drop"
    ) %>%
    mutate(row_num = row_number()) %>%
    select(row_num, everything())
  
  # 1b. Quota summary by admin2
  quota_summary <- clean_data_main %>%
    filter(cluster_type == "quota") %>%
    group_by(admin2) %>%
    summarise(
      survey_collected = n(),
      target_with_buffer = 90,
      target_without_buffer = 85,
      .groups = "drop"
    ) %>%
    mutate(
      status = ifelse(survey_collected < target_without_buffer, "UNDER", "GOOD")
    )
  
  # ------------------------------------------------------------------
  # 2. Sample summary (from Excel)
  sample_summary <- readxl::read_xlsx(sample_file, sheet = sheet_summary) %>%
    select(admin2, Clusters_to_assess, Samples)
  
  # 3. Comparison (admin2 level)
  comparison_admin2 <- data_summary %>%
    left_join(sample_summary, by = "admin2") %>%
    mutate(
      survey_collected = ifelse(is.na(survey_count), 0, survey_count),
      Samples_target = ifelse(is.na(Samples), 0, Samples),
      Sample_without_buffer = round(0.9 * Samples_target),
      difference = survey_count - Samples_target,
      status = case_when(
        survey_collected < 0.9 * Samples_target ~ "UNDER",
        survey_collected >= 0.9 * Samples_target ~ "GOOD"
      ),
      unique_clusters_without_buffer = round(0.9 * Clusters_to_assess),
      cluster_difference = unique_clusters - unique_clusters_without_buffer,
      status_cluster = case_when(
        unique_clusters < 0.9 * Clusters_to_assess ~ "UNDER",
        unique_clusters >= 0.9 * Clusters_to_assess ~ "GOOD"
      )
    )
  
  # Add quota summary to admin2 comparison
  comparison_admin2 <- bind_rows(comparison_admin2, quota_summary %>%
                                   rename(
                                     Clusters_to_assess = target_with_buffer,
                                     Samples_target = target_with_buffer
                                   ) %>%
                                   mutate(Sample_without_buffer = target_without_buffer))
  
  # ------------------------------------------------------------------
  # 4. Cluster-level comparison (non-quota)
  sample_all <- readxl::read_xlsx(sample_file, sheet = sheet_clusters) %>%
    select(cluster_id, samples_targeted = samples)
  
  comparison_clusters <- clean_data_main %>%
    filter(cluster_type != "quota") %>%
    group_by(admin2, cluster_id) %>%
    summarise(survey_collected = n(), .groups = "drop") %>%
    left_join(sample_all, by = "cluster_id") %>%
    mutate(
      status = case_when(
        survey_collected < samples_targeted ~ "UNDER",
        survey_collected >= samples_targeted ~ "GOOD"
      )
    ) %>%
    filter(!(cluster_id %in% excluded_clusters))
  
  # ------------------------------------------------------------------
  # 5. Unique clusters comparison summary
  cluster_summary <- clean_data_main %>%
    group_by(admin2) %>%
    summarise(
      unique_clusters_collected = n_distinct(cluster_id),
      unique_quota_clusters = sum(cluster_type == "quota"),
      unique_nonquota_clusters = sum(cluster_type != "quota"),
      .groups = "drop"
    ) %>%
    left_join(sample_summary %>%
                select(admin2, Clusters_to_assess), by = "admin2") %>%
    mutate(
      unique_clusters_targeted = Clusters_to_assess,
      cluster_difference = unique_clusters_collected - unique_clusters_targeted,
      status = ifelse(unique_clusters_collected < 0.9 * unique_clusters_targeted, "UNDER", "GOOD")
    ) %>%
    select(admin2, unique_clusters_collected, unique_clusters_targeted,
           cluster_difference, status)
  
  # ------------------------------------------------------------------
  return(list(
    data_summary = data_summary,
    quota_summary = quota_summary,
    comparison_admin2 = comparison_admin2,
    comparison_clusters = comparison_clusters,
    cluster_summary = cluster_summary
  ))
}
###########################################################################################################

check_hh_size <- function(main_df, roster_df, 
                          uuid_col_main = "uuid", 
                          uuid_col_roster = "uuid", 
                          hh_col = "hh_size") {
  
  # ensure uuid are characters
  main_df <- main_df %>% mutate(!!uuid_col_main := as.character(.data[[uuid_col_main]]))
  roster_df <- roster_df %>% mutate(!!uuid_col_roster := as.character(.data[[uuid_col_roster]]))
  
  # reduce main to uuid + hh_size
  main_df <- main_df %>% select(all_of(c(uuid_col_main, hh_col)))
  
  # count members per uuid in roster
  roster_count <- roster_df %>%
    group_by(.data[[uuid_col_roster]]) %>%
    summarise(hh_size_r = n(), .groups = "drop") %>%
    rename(!!uuid_col_main := all_of(uuid_col_roster))
  
  # join and compare
  result <- main_df %>%
    left_join(roster_count, by = uuid_col_main) %>%
    mutate(
      hh_size_r    = ifelse(is.na(hh_size_r), 0, hh_size_r),
      hh_size_diff = .data[[hh_col]] - hh_size_r
    )
  
  # return mismatches if any
  mismatches <- result %>% filter(hh_size_diff != 0)
  
  if (nrow(mismatches) > 0) {
    return(mismatches)
  } else {
    message("✅ No mismatches found.\n")
    cat("Total records checked: ", nrow(result), "\n")
    cat("Displaying first 6 rows of the full comparison:\n")
    print(knitr::kable(result %>% head()))
    return(invisible(result))
  }
}

# ------------------------------------------------------------------
# Function to check for non-empty "other_" responses
check_other_responses <- function(df, uuid_col = "uuid") {
  
  # check if uuid column exists
  if (!(uuid_col %in% names(df))) {
    message("⚠️ No column named '", uuid_col, "' found. Proceeding without it.")
    uuid_col <- NULL
  }
  
  # select uuid (if available) + other_ columns
  selected_df <- df %>%
    select(any_of(uuid_col), starts_with("other_")) %>%
    # keep only rows with at least one non-NA and non-empty
    filter(if_any(starts_with("other_"), ~ !is.na(.) & . != "")) %>%
    # drop columns that are entirely empty
    select(where(~ any(!is.na(.) & . != "")))
  
  # check result
  if (nrow(selected_df) > 0) {
    print(kable(selected_df))
    View(selected_df)
  } else {
    message("✅ All good: no non-empty 'other_' responses found. ")
  }
}






