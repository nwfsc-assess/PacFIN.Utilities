###########################################################################
#
#' Aggregate composition data by length, age, or age-at-length according to the
#' given stratification.
#' 
#' \subsection{Workflow}{
#' \code{getComps} is run subsequently to \code{\link{getExpansion_2}}.
#' }
#' 
#' @export
#'
#' @details The aggregation is of the \code{Pdata$Final_Sample_Size} column value,
#'   which should be set to the desired expansion:
#'   
#'   \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1} 
#'   
#'   or
#'   
#'   \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2}
#'   
#'   The default stratification is by fleet, fishyr, and season.
#'   Columns \code{lengthcm}, \code{age} or both are added
#'   depending on the \code{Comps} argument.
#'   
#' @template Pdata
#' @template strat
#' @param Comps The type of composition data to create. Options are length
#'   (\code{'LEN'}, age (\code{'AGE'}), or conditional age-at-length (\code{'AAL'}).
#' @param defaults The default stratification columns
#'   which will typically be left at their default value of
#'   \code{c('fleet', 'fishyr', 'season')}.
#' @template verbose
#' @param ... Pass additional arguments to \code{getcomps_long}, such as
#' \code{dropmissing = FALSE} where the default behaviour is 
#' \code{dropmissing = TRUE}
#' @return A dataframe with composition data specific to the type specified
#'   in \code{Comps} for males, females, and unsexed records.
#' @author Andi Stephens, Kelli Faye Johnson
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#' 
############################################################################


getComps = function( Pdata, strat = NULL, Comps = "AAL",
  defaults = c("fleet", "fishyr", "season"), verbose = FALSE, ...) {

  # Check for expansion factor

  if (length(Pdata$Final_Sample_Size) == 0) {
    
    stop(paste("\ngetComps relies on the column labeled 'Final_Sample_Size'\n",
      "please make sure this column (the expansion factor) has a value.\n\n",
      "Example: Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1",
      "* Pdata$Expansion_Factor_2\n"))
    
  } # End if

  # Set up stratification

  usualSuspects <- defaults

  # Avoid duplication
  strat = strat[!strat %in% usualSuspects]
  Comps <- toupper(substr(Comps, 1, 3))
  TowStrat <- c(strat, switch(Comps, 
    LEN = usualSuspects,
    AGE = usualSuspects,
    c(usualSuspects, "age")))
  usualSuspects <- switch(Comps,
    LEN = c(usualSuspects, "lengthcm"),
    AGE = c(usualSuspects, "age"),
    c(usualSuspects, "lengthcm", "age"))

  if (verbose) {
    cat("\nAggregating, stratification is by", paste(c(strat, usualSuspects), collapse=", "), "\n\n")
    flush.console()
  }

  ageComps <- getcomps_long(data = Pdata,
    towstrat = TowStrat, type = tail(usualSuspects, 1), ...)
  invisible(ageComps)

} # End function getComps

#' Create a Long Database to Prepare Compositions
#' @param data A data frame with columns defined by the following
#' arguments: towstrat, type, towid, and weightid
#' @param towstrat A vector of character values providing the column
#' names for which you want compositions for.
#' @param type A character value specifying which category to
#' summarize by, i.e., \code{"length"} or \code{"age"}.
#' @param towid A vector of character values providing the column
#' names that generate a unique id for each sample.
#' @param weightid A character value giving the column name that
#' holds the value to be summed for each type and strata.
#' @param dropmissing A logical value supplied to the
#' \code{drop} argument in \code{stats::aggregate}
#' that specifies whether or not to keep all levels in the data
#' even if there are no values to report for summaries. 
#' @author Kelli Faye Johnson
#' @return A data frame in long form with a weight for each
#' category included in the lengths or ages of interest by
#' stratification. Stratifications are normally year and fishery
#' and sex will always be included. If no sex is provided then it
#' is assumed all are unsexed and will be returned as such.
#'
getcomps_long <- function(data, towstrat, type,
  towid = "SAMPLE_NO", weightid = "Final_Sample_Size",
  dropmissing = TRUE) {

  if (!all(towstrat %in% colnames(data))) stop("Not all towstrat are available.")
  if (!type %in% colnames(data)) stop("'type' must be a column in data",
    " i.e., 'lengthcm' or 'age'")

  # Create a unique id for each sample
  data[, "uniqueid"] <- apply(data[, towid, drop = FALSE],
    1, paste, collapse = "_")
  towid <- "uniqueid"
  # Find which column contains the sex data or create unsexed
  sexn <- grep("sex", colnames(data), ignore.case = TRUE, value = TRUE)
  if (length(sexn) == 0) {
    sexn <- "SEX"
    data[, sexn] <- "U"
    warning("SEX was missing from the data and set to 'U' for unsexed fish")
  }
  if (is.character(data[, sexn])) {
    data[, sexn] <- factor(data[, sexn], levels = c("F", "M", "U"))
  }

  # FREQ... stores the number of fish that sum to the weightid
  freqn <- grep("freq", colnames(data), ignore.case = TRUE, value = TRUE)
  if (length(freqn) == 0) stop("FREQ is missing from the data.")

  tstratwsex <- c(towstrat, sexn)
  Cstrat <- c(towstrat, type)
  cstratwsex <- c(Cstrat, sexn)

  # Find which samples only have unsexed fish
  data[, "Uonly"] <- getunsexedsamps(data[, towid], data[, sexn])

  comp <- merge(by = tstratwsex, all = TRUE,
    stats::aggregate(
      data[, c(weightid, freqn)],
      by = data[, cstratwsex, drop = FALSE],
      sum, na.rm = TRUE, drop = dropmissing),
    stats::aggregate(
      list("tows" = data[, towid], "ONLY_U_TOWS" = data[, c("Uonly")]),
      by = data[, tstratwsex, drop = FALSE],
      lenique, drop = dropmissing))
  comp <- merge(
    stats::reshape(comp, timevar = "SEX", idvar = Cstrat, direction = "wide"),
    stats::aggregate(
      list("alltows" = data[, towid]),
      by = data[, towstrat, drop = FALSE],
      lenique, drop = dropmissing),
    by = towstrat, all.x = TRUE)
  comp <- comp[, -grep("ONLY_U_TOWS.F|ONLY_U_TOWS.M", colnames(comp))]
  colnames(comp) <- gsub("(.+)\\.([A-Z])", "\\L\\2\\1", colnames(comp),
    perl = TRUE)
  colnames(comp) <- gsub("freq|freq.+", "samps", colnames(comp),
    ignore.case = TRUE)
  colnames(comp) <- gsub("uonl.+", "ONLY_U_TOWS", colnames(comp),
    ignore.case = TRUE)
  colnames(comp) <- gsub(paste0("([a-z])", weightid), "\\1", colnames(comp),
    ignore.case = TRUE)
  colnames(comp) <- gsub("^f$", "female", colnames(comp))
  colnames(comp) <- gsub("^m$", "male", colnames(comp))
  colnames(comp) <- gsub("^u$", "unsexed", colnames(comp))
  # todo: remove legacy code of needing fishyr
  colnames(comp) <- gsub("^YEAR$", "fishyr", colnames(comp),
    ignore.case = TRUE)
  comp[is.na(comp)] <- 0
  return(comp)
}

#' Return Sample IDs That Did Not Sex Samples
#'
#' Identifiers that have female and male samples will be returned 
#' as NA and only identifiers that had unsexed fish will be provided.
#' @param identifier Unique IDs for the samples
#' @param sex A vector of the same length as \code{identifier} providing
#' the sex of the sampled fish.
#' @param good A character value supplying the \code{sex} value you
#' want to keep.
#'
#' @author Kelli Faye Johnson
#' @return A vector of identifiers that only had the sex given in
#' the \code{good} argument. The returned vector will be of the same
#' length as the supplied vectors.
#'
getunsexedsamps <- function(identifier, sex, good = "U") {
  if (class(sex) == "factor") sex <- as.character(sex)
  ff <- function(x) paste(unique(x), collapse = "")
  keep <- ave(sex, identifier, FUN = ff)
  return(ifelse(keep == good, identifier, NA))
}

#' Number of Unique Entries
#' A helper function that returns the number of unique
#' entries in a vector. Usefull for apply functions.
#' @param x A vector of character or numeric values.
#' @author Kelli Faye Johnson
#' @return An integer value specifying the number of unique entries.
lenique <- function(x) { return(length(unique(x))) }
