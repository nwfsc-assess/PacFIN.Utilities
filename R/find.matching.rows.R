#' Function find.matching.rows
#' 
#' @description
#'   Takes two tables with a shared primary key, and
#'   returns the rows of the second table for which the
#'   keys match those of the first.
#'
#' @seealso A sub-function of \code{\link{getExpansion_1}}
#' @param file Rows to be matched from \code{table}
#' @param table Superset of \code{file}
#' @param findex columns of \code{file} to use to match values in \code{tindex}
#' @param tindex columns of \code{table} to use to match values in \code{findex}
#' @param tcol  Still mysterious.
#' @param round. if values are numeric, round.  Default:  TRUE.
#' @author John R. Wallace (John.Wallace@@noaa.gov), (revised) Andi Stephens, 2010.
#' 
#' @details
#'  NOTE:  The way this is written assumes that the second table is a
#'         superset of the first (i.e., that each value is matched).
#'
#   Changes:
#
#        Changed name from original "match.f" to "find.matching.rows".
#
#        Removed sub-function 'paste.col' and made it standalone.
#
#        The matching function no longer modifies it's inputs, just
#        returns the values to be 'cbound' in the calling function.
#
#  Using the primary keys in columns named 'findex' and 'tindex', finds the
#  matching values for 'file' in 'table' and returns 'table' column(s) 'tcol'.
#
#  Note that no test is made to determine if there are unmatched rows.
#
#############################################################################

find.matching.rows <- function(file, table, findex = 1, tindex = 1, tcol = 2, round. = T) {

  # Coerce a vector argument into a matrix

  if (is.null(dim(file))) {  dim(file) <- c(length(file), 1) }

  # If the primary keys are numeric, round them.

  if (round.) {

    if (is.numeric(file[, findex])) { file[, findex] <- round(file[, findex]) }

    if (is.numeric(table[, tindex])) { table[, tindex] <- round(table[, tindex]) }

  } # End if round.

  # Convert the indices to character strings for comparison, and get the
  # positions of the 'file' values in the 'table' values.

  matched.rows = match(paste.col(file[, findex]), paste.col(table[, tindex]))

  # Return the 'tcol' values in the rows of the 'table' that matched.

  return(table[matched.rows, tcol, drop = FALSE])

} # End function find.matching.rows

