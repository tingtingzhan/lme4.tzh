

# @importFrom parallel detectCores
.onLoad <- function(libname, pkgname = 'lme4.tzh') {
  
  Sys.setenv('_R_CHECK_LIMIT_CORES_' = 'false') 
  # otherwise ?parallel:::.check_ncores causes error when ?devtools::check
  
  Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
  # https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time
  # to disable "checking for future file timestamps" in R Check
  
  #options(mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()))
  
}

