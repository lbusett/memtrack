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
#' @rdname start_perftrack
#' @export
#' @author Lorenzo Busetto, phD (2018) <lbusett@gmail.com>
#' @importFrom glue glue
#' @importFrom sys exec_background
#'
start_perftrack <- function(timestep  = 1,
                            trackmem  = TRUE,
                            trackCPU  = TRUE,
                            trackdisk = TRUE,
                            gc_onstart = TRUE,
                            add_processes = "",
                            outfile = tempfile(fileext = ".txt")) {

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
      countnames <- c(countnames, glue::glue("Memory_{processes}"))
    }
    if (trackCPU) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\% CPU Usage\""))
      countnames <- c(countnames, glue::glue("CPU_perc_{processes}"))
    }
    if (trackdisk) {
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\IO Read Bytes/sec\""))
      counters <- paste(counters, glue::glue("\"\\Process({processes})\\IO Write Bytes/sec\""))
      countnames <- c(countnames, glue::glue("diskread_{processes}"))
      countnames <- c(countnames, glue::glue("diskwrite_{processes}"))
    }

    call_string <- glue::glue("typeperf {glue::glue_collapse(counters)} -si {timestep} -sc 5000 -o {outfile}")

    if (gc_onstart) gc()

    currperfmon <- sys::exec_background(call_string)
    Sys.sleep(timestep)
    assign(".currperfmon", currperfmon, envir = baseenv())
    assign(".currperfmon_file", outfile, envir = baseenv())
    assign(".currperfmon_processes", processes, envir = baseenv())
    assign(".currperfmon_counters", countnames, envir = baseenv())
    assign(".currperfmon_timestep", timestep, envir = baseenv())

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
#' @rdname stop_perftrack
#' @export
#' @author Lorenzo Busetto, phD (2018) <lbusett@gmail.com>
#' @importFrom tools pskill
#' @importFrom anytime anytime
stop_perftrack <- function(gc_onexit = TRUE) {

  Sys.sleep(get(".currperfmon_timestep", envir = baseenv()))
  currperfmon       <- get(".currperfmon", envir = baseenv())
  currperffile      <- get(".currperfmon_file", envir = baseenv())
  currperfnames     <- get(".currperfmon_counters", envir = baseenv())
  currperfprocesses <- get(".currperfmon_processes", envir = baseenv())

  if (gc_onexit) {gc(verbose = FALSE)}
  tools::pskill(currperfmon)

  perfdata           <- utils::read.delim(currperffile, sep = ",", stringsAsFactors = FALSE)
  names(perfdata)[1] <- "Time"
  perfdata["Time"]   <- anytime::anytime(perfdata[, "Time"])
  starttime          <- min(perfdata[, "Time"])
  perfdata["Time"]   <- perfdata[, "Time"] - starttime

  # Find "memory" counters ----
  mem_cols <- grep( "Working.Set", names(perfdata))
browser()
  if (length(mem_cols) != 0) {
    mem_data <- round(perfdata[mem_cols] / 1e06, 3)
    mem_data <- sapply(mem_data, FUN = function(x) x - x[1])
    mem_data <- cbind(perfdata["Time"], mem_data)
    names(mem_data) <- c("Time", currperfprocesses)
    mem_data <- reshape(mem_data, idvar = "Time", varying = list(2:3),
                        times = currperfprocesses, direction = "long",
                        timevar = "process", v.names = "memory")
    row.names(mem_data) <- NULL
  }

  # Find "cpu" counters ----
  cpu_cols <- grep( "Processor.Time", names(perfdata))
  if (length(cpu_cols) != 0) {
    cpu_data <- perfdata[cpu_cols]
    cpu_data <- cbind(perfdata["Time"], cpu_data)
    names(cpu_data) <- c("Time", currperfprocesses)
    cpu_data <- reshape(cpu_data, idvar="Time", varying = list(2:3),
                        times = currperfprocesses, direction="long",
                        timevar = "process", v.names = "CPU%")
    row.names(cpu_data) <- NULL
  }

  # Find "diskread" counters ----
  read_cols <- grep( "IO.Read.Bytes.sec", names(perfdata))
  if (length(read_cols) != 0) {
    read_data <- perfdata[read_cols] / 1e06
    read_data <- cbind(perfdata["Time"], read_data)
    names(read_data) <- c("Time", currperfprocesses)
    read_data <- reshape(read_data, idvar="Time", varying = list(2:3),
                        times = currperfprocesses, direction="long",
                        timevar = "process", v.names = "disk_read")
    row.names(read_data) <- NULL
  }

  # Find "diskwrite" counters ----
  write_cols <- grep( "IO.Write.Bytes.sec", names(perfdata))
  if (length(write_cols) != 0) {
    write_data <- perfdata[write_cols] / 1e06
    write_data <- cbind(perfdata["Time"], write_data)
    names(write_data) <- c("Time", currperfprocesses)
    write_data <- reshape(write_data, idvar = "Time", varying = list(2:3),
                         times = currperfprocesses, direction = "long",
                         timevar = "process", v.names = "disk_write")
    row.names(write_data) <- NULL
  }
  out <- cbind(mem_data,
               cpu_data["CPU%"],
               read_data["disk_read"],
               write_data["disk_write"])
  out
}
