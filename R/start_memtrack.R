#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param timestep PARAM_DESCRIPTION, Default: 1
#' @param trackmem PARAM_DESCRIPTION, Default: TRUE
#' @param trackCPU PARAM_DESCRIPTION, Default: FALSE
#' @param add_processes PARAM_DESCRIPTION, Default: ''
#' @param outfile PARAM_DESCRIPTION, Default: tempfile(fileext = ".txt")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname start_memtrack
#' @export
#' @author Lorenzo Busetto, phD (2018) <lbusett@gmail.com>
#' @importFrom glue glue
#' @importFrom sys exec_background
#'
start_memtrack <- function(timestep = 1,
                           trackmem = TRUE,
                           trackCPU = FALSE,
                           add_processes = "",
                           outfile = tempfile(fileext = ".txt")) {

  stopifnot(is.numeric(timestep))
  stopifnot(is.character(outfile))
  stopifnot(dir.exists(dirname(outfile)))
  stopifnot(is.character(add_processes))

  if (Sys.info()["sysname"] == "Windows") {

    counters <- NULL
    countnames <- NULL
    processes <- "rsession"
    if (!(add_processes == "")) {
      processes <- c(processes, add_processes)
    }
    if (trackmem) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\Working Set\""))
      countnames <- c(countnames, glue::glue("Memory_{processes}"))
    }
    if (trackCPU) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\% Processor Time\""))
      countnames <- c(countnames, glue::glue("CPU_perc_{processes}"))
    }
    call_string <- glue::glue("typeperf {counters} -si {timestep} -sc 5000 -o {outfile}")
    currperfmon <- sys::exec_background(call_string)
    assign(".currperfmon", currperfmon, envir=baseenv())
    assign(".currperfmon_file", outfile, envir=baseenv())
    assign(".currperfmon_processes", processes, envir=baseenv())
    assign(".currperfmon_counters", countnames, envir=baseenv())
  }
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname start_memtrack
#' @export
#' @author Lorenzo Busetto, phD (2018) <lbusett@gmail.com>
#' @importFrom tools pskill
#' @importFrom anytime anytime
stop_memtrack <- function() {
  gc()
  currperfmon   <- get(".currperfmon", envir = baseenv())
  currperffile  <- get(".currperfmon_file", envir = baseenv())
  currperfnames <- get(".currperfmon_counters", envir = baseenv())
  currperfprocesses <- get(".currperfmon_processes", envir = baseenv())
  tools::pskill(currperfmon)

  perfdata <- read.delim(currperffile, sep = ",", stringsAsFactors = FALSE)
  names(perfdata) <- c("Time", currperfnames)
  perfdata[["Time"]] <- anytime::anytime(perfdata[["Time"]])
  starttime <- min(perfdata[["Time"]])
  perfdata[["Time"]] <- perfdata[["Time"]] - starttime

  perfdata <- perfdata %>% tidyr::gather(variable, value, -Time) %>%
     dplyr::mutate(counter = stringr::str_split_fixed(variable, "_", 2)[,1],
                   process = stringr::str_split_fixed(variable, "_", 2)[,2]) %>%
    dplyr::group_by(counter, process) %>%
    dplyr::mutate(lvar = dplyr::lag(value, 1)) %>%
    dplyr::mutate(value = value - lvar) %>%
    dplyr::ungroup()

  perfdata[["variable"]] <- NULL
  perfdata[["lvar"]] <- NULL

  perfdata
}
