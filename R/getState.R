#########################################################################
#
#' Create a state field from \code{source} for the \code{data.frame}
#' 
#' \code{Pdata}, where \code{Pdata} is a \code{data.frame} from
#' PacFIN.
#' \subsection{Workflow}{
#' \code{getState} is run by \code{\link{cleanPacFIN}}.
#' }
#' 
#' @export
#'
#' @template Pdata
#' @param source The column name where state information is located in
#'   \code{Pdata}. Typical options are \code{'SOURCE_AGID'},
#'   \code{'PSMFC_ARID'}, or \code{'SAMPLE_AGENCY'}.
#' @param CLEAN A logical value indicating if records for which state
#'   cannot be assigned are removed or if just summary stats on what would
#'   have been removed are generated.
#' @param keepPW A logival value indicating if records for Pacific Whiting
#'   should be removed or kept in as their own state.
#' @template verbose

getState <- function (Pdata, source = "SOURCE_AGID", CLEAN = TRUE,
  keepPW = FALSE, verbose = FALSE) {

  if (verbose) {
    cat("\nGetting state information from", source, "\n\n")
  }
  sources = c("SOURCE_AGID", "PSMFC_ARID", "SAMPLE_AGENCY")

  if (!source %in% sources) {
    stop(paste("Legitimate sources for getState are:", sources))
  } # End if

  if (verbose) {
    cat("\nOriginal data: \n\n")
    print(summary(as.factor(eval(parse(text=paste0("Pdata$", source))))))
  }


  if (!CLEAN) {
    if (verbose){
      cat("\nGenerating data report only. No data will be removed.\n\n")
    }
    Original_data = Pdata

  }

  Pdata$state = as.character(Pdata[, source])

  if ( source == "PSMFC_ARID" ) {

    Pdata$state[Pdata$state %in% c("3A","3B","3S")] = "WA"
    Pdata$state[Pdata$state %in% c("2A","2B","2C")] = "OR"
    Pdata$state[Pdata$state %in% c("1A","1B","1C","CAL")] = "CA"

  } else {

    Pdata$state[Pdata$state == "C"] = "CA"
    Pdata$state[Pdata$state == "O"] = "OR"
    Pdata$state[Pdata$state == "W"] = "WA"
    Pdata$state[Pdata$state == "W"] = "WA"

  } # End if

  Pdata$state[Pdata$SOURCE_AGID == "CalCOM"] = "CA"

  # Remove stateless data

  states = c("OR","CA","WA")

  # PW is Pacific Whiting

  if ( keepPW ) { states = c(states, "PW") }

  nostate = sum(! Pdata$state %in% states )

  if (CLEAN) {

    Pdata = Pdata[Pdata$state %in% states,]

    if (verbose) {
      cat("\n\nData retained: \n\n")
      print(summary(as.factor(eval(parse(text="Pdata$state")))))
      cat("\n\n", nostate,
        " records were removed because no state id could be assigned\n\n")
    }

  } else {
    if (verbose){
      cat("There are ", nostate, " records for which no state id could be assigned\n")
      cat("\nReturning original data because CLEAN=FALSE\n\n")
    }
  } # End if CLEAN

  return(Pdata)

} # end getState
