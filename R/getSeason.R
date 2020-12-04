########################################################################
#
#' Add a column to \code{Pdata} for season.
#' 
#' \subsection{Workflow}{
#' Most assessments won't require \code{getSeason}. It is run by \code{\link{cleanPacFIN}}
#' and users shouldn't need to worry about it.  If a specialized season structure is required,
#' \code{getSeason} should be run immediately after \code{\link{cleanPacFIN}}
#' }
#'
#' Several seasonal schemes are available, including the Petrale seasons
#' (1 = winter months, 2 else).
#' Contact the function author for more schemes if needed.
#'
#' @export
#'
#' @template Pdata
#' @param season_type Specify a \code{numeric} value for season type.
#'   If negative then all months will be assigned to season \code{1}.
#'   If \code{0} then seasons will be assinged from \code{Pdata$SAMPLE_MONTH},
#'   where each month is a unique season.
#'   If \code{1} then seasons are assigned according to methods used for Petrale,
#'   where winter months (\code{c(11:12, 1:2)}) are season \code{1} and
#'   the remaining months (summer) are assigned to season \code{2}.
#'   Please contact the function author if you wish to include an
#'   additional seasonal scheme.
#' @param yearUp Used to provide a list of months (i.e., \code{1:12})
#'   for which to adjust the year (\code{Pdata$fishyr}) up. For example,
#'   if winter months belong to the following year then use \code{yearUp = 11:12}.
#' @param yearDown Used to provide a list of months (i.e., \code{1:12})
#'   for which to adjust the year (\code{Pdata$fishyr}) down. For example,
#'   if winter months belong to the previous year then use \code{yearUp = 1:2}.
#' @param plotResults A logical value specifying if plots should or should not
#'   be created and shown in the console.
#' @template verbose
#' @return An additional column \code{season} is added to \code{Pdata}.
#'   No columns are modified.
#' @author Andi Stephens

getSeason = function ( Pdata, season_type=-1, yearUp=NULL, yearDown=NULL, plotResults=F,
  verbose = FALSE) {

  if (verbose){
    cat( "\nDefault season = 1\n\n")
  }

  Pdata$season = 1

  if ( season_type == 0 ) {

    if (verbose){
      cat( "Assigning season from SAMPLE_MONTH\n\n")
    }

    Pdata$season = as.numeric(Pdata$SAMPLE_MONTH)

  } # End if

  # Petrale seasons

  if ( season_type == 1 ) {

    if (verbose){
      cat("Assigning season ala Petrale; winter is season 1, summer is 2.\n\n")
    }

    Pdata$season = 2
    Pdata$season[Pdata$SAMPLE_MONTH %in%  c(11,12,1,2)] = 1

  } # End if Petrale

  if (! is.null(yearUp)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] + 1

    if (verbose){
      cat("Incremented fishyr for months", yearUp, "to the next year.\n\n")
    }

  } # End if yearUp

  if (! is.null(yearDown)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] - 1

    if (verbose){
      cat("Decremented fishyr for months", yearDown, "to the previous year.\n\n")
    }

  } # End if yearDown

  if ( plotResults ) {

    nSeas = length(unique(Pdata$season))
    tmp = xtabs(Pdata$FREQ ~ Pdata$season + Pdata$fishyr)
    barplot(tmp, col=rainbow(nSeas), legend.text=paste("Season", rownames(tmp)),
            main=unique(Pdata$SPID), xlab="Year", ylab="Samples")

  } # End if

  return( Pdata )

} # End getSeason
