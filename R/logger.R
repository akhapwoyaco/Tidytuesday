### Logger.R - R6 Logger Class

#' @title Advanced R6 Logger for Shiny Applications
#' @description An extensive R6 class for logging in Shiny applications, with 
#' performance tracking, 
#' memory analysis
#'  and color-coded console output
Logger <- R6::R6Class(
  "Logger",
  public = list(
    initialize = function(app_name = "TuitionApp", log_to_console = TRUE, log_to_file = FALSE, log_file = NULL) {
      private$app_name <- app_name
      private$log_to_console <- log_to_console
      private$log_to_file <- log_to_file
      
      if (log_to_file && is.null(log_file)) {
        private$log_file <- file.path(tempdir(), paste0(app_name, "_", format(Sys.Date(), "%Y%m%d"), ".log"))
      } else {
        private$log_file <- log_file
      }
      
      private$session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(10000:99999, 1))
      
      # Initialize memory usage
      private$initial_memory <- pryr::mem_used()
      
      self$info("Logger initialized", location = "System")
      self$info(paste("Session ID:", private$session_id), location = "System")
      self$debug(paste("Initial memory usage:", format(private$initial_memory, units = "auto", standard = "SI")), location = "System")
    },
    
    debug = function(message, location = "App", module = NULL, component = NULL, additional_data = NULL) {
      self$log(message, "DEBUG", location, module, component, additional_data)
    },
    
    info = function(message, location = "App", module = NULL, component = NULL, additional_data = NULL) {
      self$log(message, "INFO", location, module, component, additional_data)
    },
    
    warn = function(message, location = "App", module = NULL, component = NULL, additional_data = NULL) {
      self$log(message, "WARNING", location, module, component, additional_data)
    },
    
    error = function(message, location = "App", module = NULL, component = NULL, additional_data = NULL) {
      self$log(message, "ERROR", location, module, component, additional_data)
    },
    
    performance = function(operation, execution_time, location = "App", module = NULL, component = NULL) {
      perf_data <- list(
        execution_time_ms = round(execution_time * 1000, 2),
        execution_time_readable = private$format_time(execution_time)
      )
      
      message <- paste0("Performance [", operation, "]: ", perf_data$execution_time_readable)
      self$log(message, "PERFORMANCE", location, module, component, perf_data)
      
      # Add to performance metrics for trending analysis
      private$performance_metrics[[operation]] <- c(
        private$performance_metrics[[operation]], 
        execution_time
      )
      
      return(invisible(self))
    },
    
    memory = function(location = "App", module = NULL, component = NULL) {
      current_memory <- pryr::mem_used()
      mem_diff <- current_memory - private$last_memory_check
      
      mem_data <- list(
        current = format(current_memory, units = "auto", standard = "SI"),
        diff = format(mem_diff, units = "auto", standard = "SI"),
        diff_numeric = mem_diff
      )
      
      message <- paste0("Memory: Current=", mem_data$current, ", Change=", mem_data$diff)
      self$log(message, "MEMORY", location, module, component, mem_data)
      
      private$last_memory_check <- current_memory
      
      # Track memory metrics for trending
      private$memory_metrics <- c(private$memory_metrics, current_memory)
      
      return(invisible(self))
    },
    
    log = function(message, level = "INFO", location = "App", module = NULL, component = NULL, additional_data = NULL) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Build location string
      location_str <- location
      if (!is.null(module)) location_str <- paste0(location_str, ":", module)
      if (!is.null(component)) location_str <- paste0(location_str, ":", component)
      
      # Build log entry
      log_entry <- list(
        timestamp = timestamp,
        level = level,
        location = location_str,
        message = message,
        additional_data = additional_data,
        session_id = private$session_id
      )
      
      # Add to log history
      private$log_history <- c(private$log_history, list(log_entry))
      
      # Console output with color
      if (private$log_to_console) {
        color <- private$log_colors[[level]]
        
        # If no color defined, default to white
        if (is.null(color)) color <- private$log_colors[["INFO"]]
        
        colored_level <- paste0(color, sprintf("%-9s", level), private$log_colors[["RESET"]])
        console_message <- paste0(
          timestamp, " | ",
          colored_level, " | ",
          sprintf("%-30s", location_str), " | ",
          message
        )
        
        cat(console_message, "\n")
      }
      
      # File output
      if (private$log_to_file && !is.null(private$log_file)) {
        file_message <- paste0(
          timestamp, " | ",
          sprintf("%-9s", level), " | ",
          sprintf("%-30s", location_str), " | ",
          message
        )
        
        cat(file_message, "\n", file = private$log_file, append = TRUE)
      }
      
      return(invisible(self))
    },
    
    get_logs = function(n = 100, level = NULL, location = NULL, module = NULL) {
      # Filter logs based on criteria
      filtered_logs <- private$log_history
      
      if (!is.null(level)) {
        filtered_logs <- filtered_logs[sapply(filtered_logs, function(x) x$level == level)]
      }
      
      if (!is.null(location)) {
        filtered_logs <- filtered_logs[sapply(filtered_logs, function(x) grepl(location, x$location))]
      }
      
      if (!is.null(module)) {
        filtered_logs <- filtered_logs[sapply(filtered_logs, function(x) grepl(module, x$location))]
      }
      
      # Get the last n logs
      result <- tail(filtered_logs, n)
      
      # Convert to data frame
      logs_df <- data.frame(
        timestamp = sapply(result, function(x) x$timestamp),
        level = sapply(result, function(x) x$level),
        location = sapply(result, function(x) x$location),
        message = sapply(result, function(x) x$message),
        stringsAsFactors = FALSE
      )
      
      return(logs_df)
    },
    
    get_performance_summary = function() {
      result <- list()
      
      for (op_name in names(private$performance_metrics)) {
        metrics <- private$performance_metrics[[op_name]]
        
        if (length(metrics) > 0) {
          result[[op_name]] <- list(
            min = min(metrics),
            max = max(metrics),
            mean = mean(metrics),
            median = median(metrics),
            p95 = quantile(metrics, 0.95),
            count = length(metrics)
          )
        }
      }
      
      return(result)
    },
    
    get_memory_trend = function() {
      if (length(private$memory_metrics) > 0) {
        return(data.frame(
          index = seq_along(private$memory_metrics),
          memory = private$memory_metrics
        ))
      } else {
        return(data.frame(index = integer(0), memory = numeric(0)))
      }
    },
    
    reset = function() {
      private$log_history <- list()
      private$performance_metrics <- list()
      private$memory_metrics <- numeric(0)
      private$initial_memory <- pryr::mem_used()
      private$last_memory_check <- private$initial_memory
      
      self$info("Logger reset", location = "System")
      
      return(invisible(self))
    }
  ),
  
  private = list(
    app_name = NULL,
    session_id = NULL,
    log_to_console = TRUE,
    log_to_file = FALSE,
    log_file = NULL,
    log_history = list(),
    performance_metrics = list(),
    memory_metrics = numeric(0),
    initial_memory = NULL,
    last_memory_check = NULL,
    
    log_colors = list(
      "DEBUG" = "\033[36m",      # Cyan
      "INFO" = "\033[32m",       # Green
      "WARNING" = "\033[33m",    # Yellow
      "ERROR" = "\033[31m",      # Red
      "PERFORMANCE" = "\033[35m", # Magenta
      "MEMORY" = "\033[34m",     # Blue
      "RESET" = "\033[0m"        # Reset
    ),
    
    format_time = function(time_in_seconds) {
      if (time_in_seconds < 0.001) {
        return(paste0(round(time_in_seconds * 1000000), " µs"))
      } else if (time_in_seconds < 1) {
        return(paste0(round(time_in_seconds * 1000, 2), " ms"))
      } else {
        return(paste0(round(time_in_seconds, 2), " s"))
      }
    }
  )
)

# Create global logger instance
appLogger <- Logger$new("TuitionApp")
