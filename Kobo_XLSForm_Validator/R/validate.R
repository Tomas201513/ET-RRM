# Main Validation Orchestrator (CUSTOM ONLY)

validate_xlsform <- function(
    xlsform_path,
    run_custom = TRUE,
    config = get_config()
) {
  results <- list(
    success = TRUE,
    xlsform_path = xlsform_path,
    xlsform_data = NULL,
    issues = create_empty_results(),
    summary = list(),
    timestamp = Sys.time()
  )
  
  # Read raw sheets for custom rules
  kobo_survey <- read_xlsx(xlsform_path, sheet = "survey") 
  kobo_choices <- read_xlsx(xlsform_path, sheet = "choices")
  kobo_form <- list(survey = kobo_survey, choices = kobo_choices)
  
  # Step 1: Validate file
  file_check <- validate_upload(xlsform_path, config)
  if (!file_check$valid) {
    results$success <- FALSE
    results$issues <- create_issue(
      id = 1,
      source = "custom",
      level = "error",
      message = file_check$message,
      rule_id = "file_validation"
    )
    return(results)
  }
  
  # Step 2: Read XLSForm (structured)
  tryCatch({
    results$xlsform_data <- read_xlsform(xlsform_path, config)
  }, error = function(e) {
    results$success <- FALSE
    results$issues <- create_issue(
      id = 1,
      source = "custom",
      level = "error",
      message = paste("Failed to read XLSForm:", e$message),
      rule_id = "file_read"
    )
    return(results)
  })
  
  if (is.null(results$xlsform_data)) {
    return(results)
  }
  
  # Step 3: Basic structure check
  structure_check <- is_valid_xlsform(xlsform_path)
  if (!structure_check$valid) {
    results$success <- FALSE
    results$issues <- dplyr::bind_rows(
      results$issues,
      create_issue(
        id = 1,
        source = "custom",
        level = "error",
        message = structure_check$message,
        rule_id = "xlsform_structure"
      )
    )
    return(results)
  }
  
  all_issues <- create_empty_results()
  
  # Step 4: Run CUSTOM validation rules
  if (run_custom) {
    
    custom_issues1 <- check_brackets_connectors_rule(kobo_form)
    custom_issues2 <- check_choice_list_validation(kobo_form)
    custom_issues3 <- check_comparisons_rule(kobo_form)
    custom_issues4 <- check_cross_sheet_refs_rule(kobo_form)
    custom_issues5 <- check_no_spaces_inside_rule(kobo_form)
    
    issues_relevant1 <- validate_selected_for_field(kobo_form, field = "relevant")
    issues_relevant2 <- validate_selected_for_field(kobo_form, field = "constraint")
    issues_relevant3 <- validate_selected_for_field(kobo_form, field = "calculation")
    issues_relevant4 <- validate_selected_for_field(kobo_form, field = "choice_filter")
    
    custom_issues <- unique(rbind(
      custom_issues1,
      custom_issues2,
      custom_issues3,
      custom_issues4,
      custom_issues5,
      issues_relevant1,
      issues_relevant2,
      issues_relevant3,
      issues_relevant4
    ))
    
    if (nrow(custom_issues) > 0) {
      all_issues <- dplyr::bind_rows(all_issues, custom_issues)
    }
  }
  
  

  # Step 5: Enhance issues with row numbers
  # if (nrow(all_issues) > 0) {
  #   all_issues <- enhance_issues_with_rows(all_issues, results$xlsform_data)
  # }
  
  # Step 6: Combine results
  results$issues <- combine_results(all_issues)
  
  # Step 7: Summary
  results$summary <- summarize_results(results$issues)
  
  # Final status
  results$success <- results$summary$errors == 0
  
  results
}





#' Re-validate after corrections (full validation)
#' @param xlsform_data Updated XLSForm data (with changes applied)
#' @param tracker Change tracker
#' @param config Configuration
#' @return Validation results
revalidate_xlsform <- function(xlsform_data, tracker, config = get_config()) {
  
  # Create temp file with changes
  download_info <- prepare_download(xlsform_data, tracker, config)
  
  # Run validation on temp file
  validate_xlsform(download_info$path, config = config)
}



#' Incremental re-validation (fast path for custom rules only)
#' Always runs custom rules in-memory; only runs ODK validation when structural changes exist
#' @param xlsform_data Updated XLSForm data (with changes applied)
#' @param tracker Change tracker
#' @param config Configuration
#' @param force_odk Force ODK validation even without structural changes
#' @param previous_results Previous validation results (for merging ODK issues)
#' @return Validation results
revalidate_incremental <- function(xlsform_data, tracker, config = get_config(), 
                                   force_odk = FALSE, previous_results = NULL) {
  results <- list(
    success = TRUE,
    xlsform_path = xlsform_data$file_path,
    xlsform_data = xlsform_data,
    issues = create_empty_results(),
    odk_output = list(),
    summary = list(),
    timestamp = Sys.time(),
    validation_mode = "incremental"
  )
  
  all_issues <- create_empty_results()
  
  # Determine if we need full ODK validation
  needs_odk <- force_odk || has_structural_changes(tracker)
  
  if (needs_odk) {
    # Full validation path - write temp file and run pyxform + ODK Validate
    results$validation_mode <- "full"
    download_info <- prepare_download(xlsform_data, tracker, config)
    
    odk_issues <- run_odk_validation(download_info$path, config)
    results$odk_output <- odk_issues$output
    
    if (nrow(odk_issues$issues) > 0) {
      all_issues <- dplyr::bind_rows(all_issues, odk_issues$issues)
    }
  } else if (!is.null(previous_results) && !is.null(previous_results$odk_output)) {
    # Reuse previous ODK results (only cell edits, structure unchanged)
    results$odk_output <- previous_results$odk_output
    
    # Filter out ODK issues that were in changed cells
    if (!is.null(previous_results$issues)) {
      changed_cells <- get_changed_cells_from_tracker(tracker)
      odk_issues <- previous_results$issues[previous_results$issues$source == "odk", ]
      
      # Keep ODK issues that are NOT in changed cells (user might have fixed them)
      for (i in seq_len(nrow(odk_issues))) {
        cell_key <- paste(odk_issues$sheet[i], odk_issues$row[i], odk_issues$field[i], sep = ":")
        if (!cell_key %in% changed_cells) {
          all_issues <- dplyr::bind_rows(all_issues, odk_issues[i, ])
        }
      }
    }
  }
  
  # Always run custom rules (fast, in-memory)
  custom_issues <- run_custom_rules(xlsform_data$sheets)
  
  if (nrow(custom_issues) > 0) {
    all_issues <- dplyr::bind_rows(all_issues, custom_issues)
  }
  
  # Enhance issues with row numbers
  if (nrow(all_issues) > 0) {
    all_issues <- enhance_issues_with_rows(all_issues, xlsform_data)
  }
  
  # Combine and sort results
  results$issues <- combine_results(all_issues)
  
  # Generate summary
  results$summary <- summarize_results(results$issues)
  
  # Determine overall success
  results$success <- results$summary$errors == 0
  
  results
}

#' Check if tracker has structural changes (row operations)
#' Structural changes require full ODK re-validation
#' @param tracker Change tracker
#' @return TRUE if there are structural changes
has_structural_changes <- function(tracker) {
  if (is.null(tracker)) return(FALSE)
  
  # Row operations always require full re-validation
  if (!is.null(tracker$row_operations) && nrow(tracker$row_operations) > 0) {
    return(TRUE)
  }
  
  # Check for changes to structural columns that affect validation
  if (!is.null(tracker$changes) && nrow(tracker$changes) > 0) {
    structural_columns <- c("type", "name", "list_name", "calculation", "relevant", 
                            "constraint", "required", "repeat_count", "choice_filter")
    changed_columns <- unique(tracker$changes$column)
    if (any(changed_columns %in% structural_columns)) {
      return(TRUE)
    }
  }
  
  FALSE
}

#' Get changed cells from tracker as keys for filtering
#' @param tracker Change tracker
#' @return Character vector of "sheet:row:field" keys
get_changed_cells_from_tracker <- function(tracker) {
  if (is.null(tracker) || is.null(tracker$changes) || nrow(tracker$changes) == 0) {
    return(character())
  }
  
  paste(tracker$changes$sheet, tracker$changes$row, tracker$changes$column, sep = ":")
}

#' Run custom rules only (fastest validation path)
#' For quick feedback during editing without waiting for ODK tools
#' @param xlsform_data XLSForm data with sheets
#' @return List with issues and summary
validate_custom_only <- function(xlsform_data) {
  results <- list(
    success = TRUE,
    issues = create_empty_results(),
    summary = list(),
    timestamp = Sys.time(),
    validation_mode = "custom_only"
  )
  
  # Run custom validation rules
  custom_issues <- run_custom_rules(xlsform_data$sheets)
  
  # Enhance issues with row numbers
  if (nrow(custom_issues) > 0) {
    custom_issues <- enhance_issues_with_rows(custom_issues, xlsform_data)
  }
  
  results$issues <- custom_issues
  results$summary <- summarize_results(results$issues)
  results$success <- results$summary$errors == 0
  
  results
}
