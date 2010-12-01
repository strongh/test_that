#' Run all of the tests in a directory.  
#' 
#' Test files start with \code{test} and are executed in alphabetical order 
#' (but they shouldn't have dependencies). Helper files start with 
#' \code{helper} and loaded before any tests are run.
#'
#' @param path path to tests
#' @param env environment in which to execute test suite. Defaults to new
#    environment inheriting from the global environment.
#' @param reporter reporter to use
#' @param chdir whether to change directories for sourcing
#' @export
test_dir <- function(path, reporter = "summary", env = NULL, chdir = TRUE) {
  reporter <- find_reporter(reporter)
  if (is.null(env)) {
    env <- new.env(parent = globalenv())    
  }
  
  source_dir(path, "^helper.*\\.[rR]$", env = env, chdir = chdir)
  with_reporter(reporter, {
    source_dir(path, "^test.*\\.[rR]$", env = env, chdir = chdir)    
  })
}

#' Load all source files in a directory.
#' 
#' The expectation is that the files can be sourced in alphabetical order.
#'
#' @param path path to tests
#' @param pattern regular expression used to filter files
#' @param env environment in which to execute test suite. Defaults to new
#    environment inheriting from the global environment.
#' @param chdir change working directory to path?
#' @keywords internal
#' @export
#' @usage source_dir(path, pattern="\\\\.[rR]$", env = NULL, chdir=TRUE)
source_dir <- function(path, pattern = "\\.[rR]$", env = NULL, chdir = TRUE) {
  files <- sort(dir(path, pattern, full.names = TRUE))
  if (is.null(env)) {
    env <- new.env(parent = globalenv())
  }
  
  lapply(files, sys.source, chdir = chdir, envir = env)
}

#' Run all tests in specified file.
#' 
#' @param path path to file
#' @param reporter reporter to use
#' @param chdir whether to change directories for sourcing
#' @export
test_file <- function(path, reporter = "summary", env = NULL, chdir = TRUE) {    
  reporter <- find_reporter(reporter)
  if (is.null(env)) {
    env <- new.env(parent = globalenv())
  }

  with_reporter(reporter, 
    sys.source(path, env, chdir))
}


#' Run all tests on whatever.
#'
#' Test using either test_file or test_dir, depending on whether path is a directory
#'
#' @param path path to file
#' @param reporter reporter to use
#' @param chdir whether to change directories for sourcing
#' @export
test_whatever <- function(path, reporter = "summary", env = NULL, chdir = TRUE) {
  (if (file.info(path)$isdir) {
    test_dir
  } else {
    test_file
  })(path, reporter, env, chdir)
}

#' Run a test suite on each object in a named list.
#'
#' To be used when a list of objects should be
#' tested with the same suite of tests. 
#'
#' So the suite should be testing, in the default case, the 
#' object \code{Obj}. In its verbosity, the test suite may
#' use \code{context(Name)} or something similar.
#'
#' @param things a list of objects to test
#' @param suite path to a file or folder of tests
#' @param aliases generic names for the object and the object's name
#' @param parentenv the parent environment for the test environments
#' @param ... to test_whatever
#' @export
test_each <- function(things, suite,
                      aliases = list(name="Name", obj="Obj"),
                      parentenv = globalenv(), ...) {

  if (is.null(names(things)))
    names(things) <- length(things)
  
  mapply(function(obj, name) {
    env <- new.env(parent = parentenv)
    assign(aliases$name, name, envir = env)
    assign(aliases$obj, obj, envir = env)
    test_whatever(suite, env = env, ...)
  },
         things,
         names(things),
         SIMPLIFY = FALSE) 
}
