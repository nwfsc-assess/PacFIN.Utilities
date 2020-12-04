#' Format Catches from Long to Wide
#' 
#' Transform a long data frame of catches to a wide data frame.
#' The colnames of the wide data frame will be in the format
#' needed for the statification of stage-2 expansions of
#' composition data, i.e., \code{getExpansion_2}.
#' 
#' @param catch A data frame with at least a column specifying
#' the year the catches took place (e.g., Year), a column for
#' each of the variables specified in strat, and a column that holds
#' the measured catches specified using the \code{valuename} argument.
#' @template strat
#' @param valuename The column name that contains the 
#' catch values in \code{catch}
#' 
formatCatch <- function(catch, strat, valuename = "catch") {
  yearn <- grep("^Year|^Yr", colnames(catch), ignore.case = TRUE, value = TRUE)
  colnames(catch) <- gsub(valuename, "catch", colnames(catch))
  out <- catch[, c(yearn, strat, "catch")]
  out$merged <- apply(out[, strat, drop = FALSE], 1, paste, collapse = ".")
  if (any(duplicated(paste(out[, "merged"], out[, yearn])))) {
    out <- aggregate(out[, "catch", drop = FALSE], 
      out[, c(yearn, "merged"), drop = FALSE], FUN = sum, na.rm = TRUE)
  }
  out <- reshape(out, direction = "wide", 
    timevar = "merged", idvar = yearn)
  out <- out[, grep(paste0("catch|", yearn), colnames(out))]
  out[is.na(out)] <- 0
  colnames(out) <- gsub("catch\\.", "", colnames(out))
  return(out)
}
