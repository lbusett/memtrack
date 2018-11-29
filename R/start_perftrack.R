#' @title start_perftrack
#' @description starts collection of information about memory, disk and CPU
#'  usage of rsession and other processes
#' @param timestep `integer`, timestep used for recording performance, Default: 1
#' @param trackmem `logical`, If TRUE, monitor memory usage, Default: TRUE
#' @param trackCPU `logical`, If TRUE, monitor CPU usage, Default: TRUE
#' @param trackdisk `logical`, If TRUE, monitor disk usage, Default: TRUE
#' @param add_processes `character array` containg the names of other processes
#'  to be monitored, Default: NULL
#' @param outfile `character`, Full path of a file used to store the csv file used to
#'  store performance data. Defaults to a temporary file: tempfile(fileext = ".txt")
#' @return The function is called for its side effects. It starts monitoring
#'  performances and strore useful information in baseenv.
#' @details On Windows, the function calls `typeperf`. On Linux, it calls `???`
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname start_perftrack
#' @export
#' @author Lorenzo Busetto, phD (2018) <lbusett@gmail.com>
#' @importFrom glue glue
#' @importFrom sys exec_background
#'
start_perftrack <- function(timestep      = 1,
                            trackmem      = TRUE,
                            trackCPU      = TRUE,
                            trackdisk     = TRUE,
                            gc_onstart    = TRUE,
                            add_processes = "",
                            outfile       = tempfile(fileext = ".txt")) {

  stopifnot(is.logical(gc_onstart))
  stopifnot(is.numeric(timestep))
  stopifnot(is.character(outfile))
  stopifnot(dir.exists(dirname(outfile)))
  stopifnot(all(is.character(add_processes)))

  if (Sys.info()["sysname"] == "Windows") {

    counters <- NULL
    countnames <- NULL
    processes <- "rsession"
    if (!all(add_processes == "")) {
      processes <- c(processes, add_processes)
    }
    if (trackmem) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\Working Set\""))
    }
    if (trackCPU) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\% Processor Time\""))
    }
    if (trackdisk) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\IO Read Bytes/sec\""))
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\IO Write Bytes/sec\""))
    }

    call_string <- glue::glue("typeperf {glue::glue_collapse(counters)} -si {timestep} -sc 5000 -o {outfile}")

    if (gc_onstart) gc()

    currperfmon <- sys::exec_background(call_string)
    Sys.sleep(timestep)
    options(".currperfmon"          = currperfmon)
    options(".currperfmon_file"      = outfile)
    options(".currperfmon_processes" = processes)
    options(".currperfmon_timestep"  = timestep)

  }
}