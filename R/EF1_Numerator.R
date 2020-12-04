#########################################################################
#'
#' Calculate the numerator for the first level expansion factor.
#'
#' \subsection{Workflow}{
#' \code{EF1_Numerator} is not run by the user.  It is a sub-function of 
#' \code{\link{getExpansion_1}}
#' }
#' 
#' @details \code{Trip_Sampled_Lbs} is calculated differently for each state:
#' \itemize{
#' \item {California}{ = \code{Species_Percent_Sampled} *
#'   \code{TOTAL_WGT};}
#' \item {Oregon}{ = \code{Pdata$EXP_WT}.  Where missing,
#'   use \code{Species_Percent_Sampled} * \code{TOTAL_WGT}, as for CA;}
#' \item {Washington}{ = \code{Pdata$RWT_LBS}, \code{Pdata$TOTAL_WGT},
#'   median(RWT_LBS), or median(\code{Pdata$TOTAL_WGT});}
#' \item {if all else fails}{ = use per-year, state-specific medians.}
#' }
#' @return A \code{Pdata} with additional columns, where
#'   \code{Use_acs} is \code{all_cluster_sum} with NAs replaced by year-specific,
#'   state-specific medians;
#'   \code{Trip_Sampled_Lbs} is the sample weight in pounds;
#'
#' @template Pdata
#' @template verbose
#' @template plot
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#' @author Andi Stephens

EF1_Numerator = function(Pdata, verbose = FALSE, plot = FALSE) {

  # Start clean

  Pdata$Use_acs        = NA
  Pdata$Trip_Sampled_Lbs = NA

  tows = Pdata[!duplicated(Pdata$SAMPLE_NO),]

  ############################################################################
  #
  # First the all_cluster_sums, replacing NA with medians.
  #
  #############################################################################
  tows$Use_acs = tows$all_cluster_sum

  tows$Use_acs[tows$Use_acs == 0] = NA

  # Use median from year, state, gear, grade if total landed weight is missing
  # medians are calculated with all groups first then eliminating from the 
  # right, where the last ditch effort is the median by year across all
  # grades, gears, and states
  tows$median <- getMed(tows$Use_acs, 
    tows$SAMPLE_YEAR, tows$state, tows$geargroup, tows$GRADE)$median
  tows$Use_acs[is.na(tows$Use_acs)] <- tows$median[is.na(tows$Use_acs)]
  Pdata$Use_acs = tows$Use_acs[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]
  # KFJ(2019-03-29): todo - print information to the screen 
  # to let users know how many data points are being interpolated
  # rather than calculated from raw information

  ############################################################################
  #
  # Calculate Species_Percent_Sampled.  (Actually, fraction sampled).
  #
  ############################################################################
  if (!"Wt_Sampled" %in% colnames(Pdata)) stop("The column Wt_Sampled",
    " must be in your dataframe, run EF1_Denominator.")
  tows$Species_Percent_Sampled = tows$Wt_Sampled/tows$Use_acs
  tows$Use_Percent <- tows$TOTAL_WGT * 
    ifelse(tows$Species_Percent_Sampled > 1,  1, tows$Species_Percent_Sampled)
  # KFJ(2019-03-29): Determine if when percent should be multiplied by
  # round weight rather than total weight.
  # I looked at a few fish tickets and it appears as though Round Weight for 
  # California is the species-specific sampled weight.
  # For CA TOTAL_WGT is the sum of the landed weight for species that were
  # sampled. That is, if you landed boccacio, sablefish, and yellowtail in 
  # a tow but only sampled boccacio and sablefish, then TOTAL_WGT would 
  # be the landed weight of sablefish and bocaccio. For this example, 
  # RWT_WT would be the sablefish landed weight if sablefish was the species
  # that you cared about.

  ############################################################################
  #
  # Get total weight per SAMPLE_NO, calculated differently for each state.
  # Replace NAs with state/fishyr specific medians.
  #
  ############################################################################

  # Default
  tows$Trip_Sampled_Lbs[tows$state=="CA"] = tows$Use_Percent[tows$state=="CA"]
  tows$Trip_Sampled_Lbs[tows$state=="OR"] = tows$EXP_WT[tows$state=="OR"]
  tows$Trip_Sampled_Lbs[tows$state=="WA"] = tows$RWT_LBS[tows$state=="WA"]
  if (any(!unique(tows$state) %in% c("CA", "OR", "WA"))) warning("The state(s) '",
    paste(unique(tows$state)[!unique(tows$state) %in% c("CA", "OR", "WA")], 
      collapse = ", "), "' do not have methods for Trip_Sampled_Lbs")
  tows$Trip_Sampled_Lbs[tows$Trip_Sampled_Lbs == 0] = NA

  # California uses Percent of TOTAL_WGT.
  # Find percent of TOTAL_WGT attributable to the spp of interest based 
  # on the percent of all sampled fish that are your species. 

  # Oregon uses EXP_WT by preference.
  # EXP_WT will be the species-specific landing weight for the sample and will
  # account for dressed landings.
  # OR data is sometimes missing EXP_WT earlier than 1973
  # so use Species_Percent_Sampled, as for CA.

  indices = which(tows$state=="OR" & is.na(tows$Trip_Sampled_Lbs))
  tows$Trip_Sampled_Lbs[indices] = tows$Use_Percent[indices]
  # KFJ(2019-03-29): Using the percent of the total weight is more in
  # error than just using the total weight for Oregon because fish
  # might have a condition other than round when sampled.
  # Oregon samples are more species-specific, so the amount of
  # contamination of other species in the sample might be less of 
  # an error than just using the total weight. 
  # TOTAL_WGT can be the amount of the clusters rather than the landing weight
  # for OR. This is not the best way forward, but keeping it for now.
  # todo - check with Ali for how to proceed when EXP_WT is not available. 

  # Washington uses RWT_LBS as default.
  tows$Trip_Sampled_Lbs <- ifelse(
    tows$state == "WA" & 
    is.na(tows$Trip_Sampled_Lbs),
    tows$TOTAL_WGT, tows$Trip_Sampled_Lbs)
  # KFJ(2019-03-29): Is it better to use TOTAL_WGT rather than a median of 
  # roundweight? Need to ask Theresa best practice here.
  # Many fish that were sampled using alternate or fork length are 
  # included here.
  # Before the median of trip_sampled_lbs for those that were found 
  # was used before median round weight 

  # Get row-by-row alignment with tows for each median.
  # Fill CA and OR annual median Trip_Sampled_Lbs.
  tows$Trip_Sampled_Lbs <- ifelse(
    is.na(tows$Trip_Sampled_Lbs) & tows$state %in% c("OR", "CA"),
    getMed(tows$Trip_Sampled_Lbs, 
      tows$state, tows$SAMPLE_YEAR, tows$geargroup, tows$GRADE)$median, 
    tows$Trip_Sampled_Lbs)
  # Same for WA, but the preferred value is first RWT_LBS, then TOTAL_WGT.
  tows$Trip_Sampled_Lbs <- ifelse(
    is.na(tows$Trip_Sampled_Lbs) & tows$state=="WA",
    getMed(tows$RWT_LBS, 
      tows$state, tows$SAMPLE_YEAR, tows$geargroup, tows$GRADE)$median, 
    tows$Trip_Sampled_Lbs)
  tows$Trip_Sampled_Lbs <- ifelse(
    is.na(tows$Trip_Sampled_Lbs) & tows$state=="WA",
    getMed(tows$TOTAL_WGT, 
      tows$state, tows$SAMPLE_YEAR, tows$geargroup, tows$GRADE)$median, 
    tows$Trip_Sampled_Lbs)

  # Match Trip_Sampled_Lbs to the larger dataset.

  Pdata$Trip_Sampled_Lbs = tows$Trip_Sampled_Lbs[match(Pdata$SAMPLE_NO, tows$SAMPLE_NO)]

  if (verbose){
    cat("\nSampled pounds per trip:\n\n")
    print(summary(Pdata$Trip_Sampled_Lbs))
  }

  if (plot != FALSE) {
    numstate <- length(unique(Pdata$state))
    if (is.character(plot)) png(plot)
    par(mgp = c(2.5, 0.5, 0), mfrow = c(numstate, 1), mar = rep(0, 4),
      oma = c(4, 5, 3, 0.5))
    for (st in unique(Pdata$state)) {
      plotdata <- subset(Pdata, state == st)
      if (all(is.na(plotdata$Trip_Sampled_Lbs))) next
      boxplot(plotdata$Trip_Sampled_Lbs ~ plotdata$fishyr,
        ylab = "", xlab = "", xaxt = "n",
        at = unique(plotdata$fishyr), xlim = range(Pdata$fishyr))
      legend("topleft", legend = st, bty = "n")
    }
    axis(1)
    mtext(side = 1, "Year", outer = TRUE, line = 2)
    mtext(side = 3, "Expansion factor 1 numerator", outer = TRUE, line = 1)
    mtext(side = 2, "Sample weight per trip (lbs)", outer = TRUE, line = 2)

    if (is.character(plot)) dev.off()
  }

  return(Pdata)

} # End function EF1_Numerator

