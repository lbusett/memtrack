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
 browser()
  Sys.sleep(options(".currperfmon_timestep")[[1]])
  currperfmon       <- options(".currperfmon")[[1]]
  currperffile      <- options(".currperfmon_file")[[1]]
  currperfnames     <- options(".currperfmon_counters")[[1]]
  currperfprocesses <- options(".currperfmon_processes")[[1]]

  if (gc_onexit) {gc(verbose = FALSE)}
  tools::pskill(currperfmon)

  perfdata           <- utils::read.delim(currperffile, sep = ",",
                                          stringsAsFactors = FALSE)
  #Remove first row - used for ramp-up
  browser()
  perfdata <- perfdata[, -1]


  names(perfdata)[1] <- "Time"
  perfdata["Time"]   <- anytime::anytime(perfdata[, "Time"])
  starttime          <- min(perfdata[, "Time"])
  perfdata["Time"]   <- perfdata[, "Time"] - starttime

  # Find "memory" counters ----
  mem_cols <- grep( "Working.Set", names(perfdata))

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
  class(out) <- c("perftrack", "data.frame")
  # Cleanup options before returning
  options(".currperfmon"           = NULL)
  options(".currperfmon_file"      = NULL)
  options(".currperfmon_processes" = NULL)
  options(".currperfmon_timestep"  = NULL)

  return(out)
}

summary.perftrack <- function(x) {
  stopifnot(inherits(x, "perftrack"))
  browser()
  processes <- unique(x["process"])

  memquants   <- tapply(x$memory,     x$process, quantile, na.rm = TRUE)
  cpuquants   <- tapply(x$`CPU%`,     x$process, quantile, na.rm = TRUE)
  readquants  <- tapply(x$disk_read,  x$process, quantile, na.rm = TRUE)
  writequants <- tapply(x$disk_write, x$process, quantile, na.rm = TRUE)


  if (length(processes) == 1) {
    quants <- quantile(x[,"memory"], na.rm = TRUE)

    sprintf("perftrack summary")
    sprintf("=================")
    sprintf("Monitored processes: `rsession`")
    sprintf("")
    glue::glue("Memory (MB) --> Median: {round(memquants$rsession[3], 3)} \\
                  Max: {round(memquants$rsession[2],  3)} \\
                  25th: {round(memquants$rsession[4], 3)} \\
                  75th: {round(memquants$rsession[5], 3)}")

  } else {



  }

  cat("\t\n",
      sprintf("Unique Words (Length): %s\n", length(x)),
      sprintf("Total Words: %s", sum(sapply(x, length))))
}