#' Test reporter: logs errors.
#' 
#' This report logs its output to a file.
#' 
#' \code{\link{test_dir}} and \code{\link{test_file}}.
#' 
#' @name LogReporter
#' @export
#' @keywords debugging
NULL

LogReporter$do({
  logfile <- "test.log"
  
  self$start_context <- function(desc) {
    cat(desc, "-- ", file = logfile, append = TRUE)
  }
  
  self$end_context <- function() {
    cat("\n", file = logfile, append = TRUE)
  }
  
  self$start_reporter <- function() {
    self$failures <- list()
    self$n <- 0
  }
  
  self$add_result <- function(result) {
    if (result$passed) {
      cat(" . ", file = logfile, append = TRUE)
    } else {
      self$failed <- TRUE
      self$n <- self$n + 1
      
      result$test <- self$test
      self$failures[[self$n]] <- result
    }
    
  }

               
  self$end_reporter <- function() {
    if (self$n == 0) {
      cat("\n", file = logfile, append = TRUE)
    } else {
      type <- ifelse(sapply(self$failures, "[[", "error"), "Error", "Failure")
      tests <- sapply(self$failures, "[[", "test")
      
      header <- str_c(type, ": ", tests, " ")
      
      message <- sapply(self$failures, "[[", "message")
      
      cat(str_c(header, ", ", 
                message, ", ", collapse = ";"),
          file = logfile, append = TRUE)      
    }
  }
  
})
