#' Filter samples for appropriate ages and agemethods.
#'
#' \code{cleanAges} is now defunct and and users now only need to
#' run \code{\link{cleanPacFIN}()} rather than both functions.
#'
#' @export
#'
#' @param Pdata A PacFIN dataset
#' @param keep_age_methods  By default, methods "B","S" and "" are 
#' retained.  
#' @param minAge  Minimum age to retain; default is 1.  
#' @param maxAge Maximum age to retain; default (NULL) returns all ages 
#' greater than \code{minAge}.
#' @param CLEAN  Default is TRUE.  If FALSE, return the input data unchanged.
#' @return The input data filtered for age methods and minimum and maximum ages
#' specified, with added column \code{age}.  A brief report on filtering is
#' output to the console.
#'   
#' @details
#' 
#' Age methods "1" and "2" are equivalent to "B" and "S" respectively,
#' and are reset to those values.
#' 
#' No ages less than one occur in the commercial dataset.  Zero ages are
#' equivalent to NA.
#' 
#' @seealso \code{\link{cleanPacFIN}}, \code{\link{sink}}
#'
#' @author Andi Stephens
#
#############################################################################


cleanAges = function( Pdata, keep_age_methods=c("B","S",""), 
                      minAge=1, maxAge=NULL, CLEAN=TRUE ) {

  .Defunct("cleanPacFIN", package = "PacFIN.Utilities",
    msg = "Functionality was moved to clean cleanPacFIN.")

} # End cleanAges
