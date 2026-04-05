# Configuration Management for XLS-Validator (Custom Rules Only)

#' Get application configuration
#' @return List of configuration values
get_config <- function() {
  list(
    # Temp directory for processing
    temp_dir = file.path(tempdir(), "xls_validator"),
    
    # Validation settings
    max_file_size_mb = 50,
    allowed_extensions = c("xls", "xlsx"),
    
    # Sheets to validate
    xlsform_sheets = c("survey", "choices", "settings"),
    
    # Required columns per sheet
    required_columns = list(
      survey = c("type", "name"),
      choices = c("list_name", "name"),
      settings = c()
    )
  )
}

#' Validate configuration
#' @param config Configuration list from get_config()
#' @return List with valid (logical) and messages (character vector)
validate_config <- function(config = get_config()) {
  
  messages <- character()
  
  # Basic checks only (no external tools anymore)
  
  if (config$max_file_size_mb <= 0) {
    messages <- c(messages, "max_file_size_mb must be greater than 0")
  }
  
  if (length(config$allowed_extensions) == 0) {
    messages <- c(messages, "No allowed file extensions defined")
  }
  
  list(
    valid = length(messages) == 0,
    messages = messages
  )
}

#' Ensure temp directory exists
#' @param config Configuration list
#' @return Path to temp directory
ensure_temp_dir <- function(config = get_config()) {
  if (!dir.exists(config$temp_dir)) {
    dir.create(config$temp_dir, recursive = TRUE)
  }
  config$temp_dir
}