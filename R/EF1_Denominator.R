###########################################################################
#
#' Calculate the denominator for the level-1 expansion factor.
#' 
#' \subsection{Workflow}{
#' \code{EF1_Denominator} is not run by the user.  It is a sub-function of 
#' \code{\link{getExpansion_1}}
#' }
#' 
#'
#' The denominator of the level-1 expansion factor is the weight of the sampled
#' fish in a tow. The calcuation is done one in three ways, all of which
#' are returned, though the column used in subsequent analyses is based on
#' the hierarchical structure of:
#' \enumerate{
#' \item{\code{sum(Pdata$FEMALES_WGT + Pdata$MALES_WGT)} per unique \code{SAMPLE_NO}}
#' \item{\code{sum(Pdata$SPECIES_WGT)} across all clusters in a sample, where the
#'   species weight is determined from the \code{cluster_wt}}
#' \item{Calculate weights of males and females given the weight length relationship,
#' if the length of a fish does not exist, then the median weight of all
#' weighed fish in the sample is used.}
#' }
#'
#' @template Pdata
#' @template Indiv_Wgts
#' @param calcWL A logical value specifying whether or not to calculate the
#' weight-length parameters from the supplied data.
#' @template weightlengthparams
#' @template verbose
#' @param plot Create plots.  Default:  FALSE
#' @return Additional columns are added to \code{Pdata}:
#' \itemize{
#' \item Wt_Sampled_1: the sum of weights for male and female fish within the
#' sample, this will ignore unsexed fish or hermaphrodites. 
#' \item Wt_Sampled_2: the species-specific sample weight only provided by
#'   California because the cluster weight could include multiple species.
#' \item LW_Calc_Wt: individual weights predicted from the specified length-weight relationships.
#' \item Wt_Sampled_3: The sum of empirical weights, for those fish within a 
#' sample where this information is available, and weights calculated from the
#' length-weight relationship. This uses the empirical data if available and 
#' fills in with the expected weight.
#' \item Wt_Sampled: the sample weight that will be used in subsequent analyses,
#'   where this is preferentially the empirical weights; all NA values are 
#'   subsequently filled in using Wt_Sampled_1, 
#'   with NAs replaced with values from Wt_Sampled_2,
#'   and NAs remaining replaced with values from Wt_Sampled_3.
#' \item Wt_Method: a \code{numeric} value starting with zero for empirical weights
#'   and then denoting which method was used for \code{Wt_Sampled}.
#' }
#' @author Andi Stephens
#' @seealso \code{\link{EF1_Numerator}}, \code{\link{getExpansion_1}}, \code{\link{getExpansion_2}}
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#
#
###########################################################################

EF1_Denominator = function( Pdata, Indiv_Wgts=TRUE, calcWL = FALSE,
                            fa = 2e-06, fb = 3.5, 
                            ma = 2e-06, mb = 3.5, 
                            ua = 2e-06, ub = 3.5, 
                            verbose = FALSE,
                            plot = FALSE) 
{
  if (!Indiv_Wgts) stop("EF1_Denominator no longer works without ",
    " parameters for the weight-length relationship.")

  sumNA <- function(x) {
    out <- sum(x, na.rm = TRUE)
    ifelse(out == 0, NA, out)
  }
  bweight <- function(data) {
    out <- ifelse(is.na(data[, "FISH_WEIGHT"]),
      ifelse(is.na(data[, "LW_Calc_Wt"]),
        data[, "meanfishweight"], data[, "LW_Calc_Wt"]),
      data[, "FISH_WEIGHT"])
    return(tapply(out, data$SEX, sum))
  }

# Clean up.  Muddled results if this function has been previously run.
  Pdata$Wt_Sampled_1   = NA
  Pdata$Wt_Sampled_2   = NA
  Pdata$LW_Calc_Wt     = NA
  Pdata$Wt_Sampled_3   = NA
  Pdata$Wt_Sampled     = NA
  Pdata$Wt_Method      = NA

  if ( Indiv_Wgts == TRUE & verbose) {

    cat("\nIndividual weights will be generated from the following values:\n\n")
    cat(" Females:", fa,fb, "\n",
        "Males:",  ma,mb, "\n",
        "Unknowns and hermaphrodites:",  ua,ub, "\n\n")

  } # End if

  # Assign 'state' if it's not already there.

  if (length(which(names(Pdata) == "state")) == 0) {

    if (verbose){
      cat("State variable was not assigned, getting state.\n\n")
    }
    Pdata = getState(Pdata, CLEAN=T, verbose = verbose)

  }

  if (!"UNK_NUM" %in% colnames(Pdata)) {
    Pdata$UNK_NUM <- ave(Pdata$SEX, Pdata$SAMPLE_NO,
    FUN = function(x) sumNA(x %in% c("U", "H")))
  }  
  if (!"UNK_WT" %in% colnames(Pdata)) stop("Must get a newer ",
    "version of the bds data to work with this version of PacFIN.Utilities.")
  if (!"UNK_WGT" %in% colnames(Pdata)) Pdata$UNK_WGT <- Pdata$UNK_WT
  if (!"FEMALE_NUM" %in% colnames(Pdata)) Pdata$FEMALE_NUM <- Pdata$FEMALES_NUM
  if (!"MALE_NUM" %in% colnames(Pdata))   Pdata$MALE_NUM <- Pdata$MALES_NUM

  # Everything is calculated in terms of unique samples.
  # Calculate the sampled weight based on weights of individual fish
  # If there are any fish that are not weighed in the sample then 
  # Wt_Sampled will be NA and you need to estimate the weight later.
  Pdata$Wt_Sampled <- ave(Pdata$FISH_WEIGHT,
    Pdata$SAMPLE_NO, FUN = sum)
  Wt_Sampled_L <- ave(ifelse(is.na(Pdata$length), NA, Pdata$FISH_WEIGHT),
    Pdata$SAMPLE_NO, FUN = sum)
  Wt_Sampled_A <- ave(ifelse(Pdata$age == -1, NA, Pdata$FISH_WEIGHT),
    Pdata$SAMPLE_NO, FUN = sum)

  check <- TRUE
  if (check) {
    # Check OR
    numsex <- aggregate(factor(SEX) ~ SAMPLE_NO, data = Pdata, table)
    test <- data.frame(Pdata, numsex[match(Pdata$SAMPLE_NO, numsex$SAMPLE_NO), "factor(SEX)"])
    test_OR <- test[test$state == "OR", ]
      if (dim(test_OR)[1] != 0) {
        if (any(test_OR$F != ifelse(is.na(test_OR$FEMALE_NUM), 0, test_OR$FEMALE_NUM))) stop("Some OR data",
          " don't have the proper number of females assigned to FEMALE_NUM.")
        if (any(test_OR$M != ifelse(is.na(test_OR$MALE_NUM), 0, test_OR$MALE_NUM))) stop("Some OR data",
          " don't have the proper number of males assigned to MALE_NUM.")
        if (any(test_OR$U != ifelse(is.na(test_OR$UNK_NUM), 0, test_OR$UNK_NUM))) stop("Some OR data",
          " don't have the proper number of unsexed fish assigned to UNK_NUM.")
      }
    # Check CA
    if (dim(test[test$state == "CA", ])[1] > 0) {
      if (dim(Pdata[is.na(Pdata$SPECIES_WGT) & Pdata$state == "CA", ])[1] != 0) stop("Some CA data", 
        " don't have a 'SPECIES_WGT' for a given cluster.")
    }
  }

  #### Washington b/c there is no other method to find the sample weight.
  # Only if there are individual weight factor and coefficients available

  if (Indiv_Wgts) {

    ############################################################################
    #
    # Create a predicted fish weight based on sex and length
    # (use mm for fitted regression coefficients!)
    # these will be summed to give the sample weight
    #
    ############################################################################

    # if (calcWL) wlfish <- getWLpars(Pdata, verbose = verbose)
    Pdata$LW_Calc_Wt <- getweight(Pdata$length, Pdata$SEX, 
      pars = data.frame(
        "A" = c("females" = fa, "males" = ma, "all" = ua),
        "B" = c("females" = fb, "males" = mb, "all" = ub)))
    # todo: create a switch to turn on use of fishery weight length relationship
    # pars could also equal wlfish if no parameters and Indiv_Wfts is TRUE

    bestweight <- ifelse(is.na(Pdata$FISH_WEIGHT),
      Pdata$LW_Calc_Wt, Pdata$FISH_WEIGHT)
    Pdata$meanfishweight <- ave(bestweight, Pdata$SAMPLE_NO,
      FUN = function(x) mean(x, na.rm = TRUE))
    bestweight[is.na(bestweight)] <- Pdata$meanfishweight[is.na(bestweight)]
    Pdata$Wt_Sampled_3 <- ave(bestweight, Pdata$SAMPLE_NO,
      FUN = sumNA)
    Wt_Sampled_3_L <- ave(ifelse(!is.na(Pdata$length), bestweight, 0),
      Pdata$SAMPLE_NO, FUN = sumNA)
    Wt_Sampled_3_A <- ave(ifelse(Pdata$age != -1, bestweight, 0),
      Pdata$SAMPLE_NO, FUN = sumNA)
    
    if (any(is.na(Pdata$Wt_Sampled_3[Pdata$state == "WA" & !is.na(Pdata$length)]))) {
      warning("Some fish",
          " from WA don't have an empirical or estimated weight,\n",
          "and thus, they are not included in the first expansion.")
    }
  } else {

    # Need for summary and boxplot
    Pdata$LW_Calc_Wt = NA
    Pdata$Wt_Sampled_3 = NA
    Pdata$meanfishweight = NA

  } # End if-else Indiv_Wgts
  # todo: calculate mean fish weight from sum of UNK_WT + FEMALES_WGT + MALE_WGT
  # or SPECIES_WGT by finding the number of fish that went into that number
  # and dividing.

  # Determine if any fish within a sample were not aged or lengthed
  identifier <- ifelse(Pdata[, "state"] == "CA",
    apply(Pdata[, c("SAMPLE_NO", "CLUSTER_NO")], 1, paste, collapse = ""),
    Pdata[, "SAMPLE_NO"])
  for (ii in 1:nrow(Pdata)) {
    if (!is.na(Pdata$length[ii]) & Pdata$age[ii] != -1) next
    if (Pdata$state[ii] == "WA") next
    if (is.na(Pdata$length[ii]) & Pdata$age[ii] == -1) {
      if (Pdata$state[ii] == "CA") {
        change <- which(identifier == 
          paste(Pdata[ii, c("SAMPLE_NO", "CLUSTER_NO"), drop = TRUE], 
            collapse = ""))
        if (all(c(is.na(Pdata$length[change]), Pdata$age[change] == -1))) {
          Pdata[change, "SPECIES_WGT"] <- NA
          next
        }
        Pdata[change, "SPECIES_WGT"] <- Pdata[ii, "SPECIES_WGT"] - bestweight[ii]
      }
      if (Pdata$state[ii] == "OR") {
        change <- which(identifier == 
          Pdata[ii, c("SAMPLE_NO"), drop = TRUE])
        if (all(c(is.na(Pdata$length[change]), Pdata$age[change] == -1))) {
          Pdata[change, c("UNK_WGT", "FEMALES_WGT", "MALES_WGT")] <- NA
          Pdata[change, c("UNK_NUM", "FEMALES_NUM", "MALES_NUM")] <- NA
          next
        }
        if (Pdata$SEX[ii] == "U") {
          Pdata[change, "UNK_WGT"] <- Pdata[ii, "UNK_WGT"] - bestweight[ii]
          Pdata[change, "UNK_NUM"] <- Pdata[ii, "UNK_NUM"] - 1
        }
        if (Pdata$SEX[ii] == "M") {
          Pdata[change, "MALES_WGT"] <- Pdata[ii, "MALES_WGT"] - bestweight[ii]
          Pdata[change, "MALES_NUM"] <- Pdata[ii, "MALES_NUM"] - 1
        }
        if (Pdata$SEX[ii] == "F") {
          Pdata[change, "FEMALES_WGT"] <- Pdata[ii, "FEMALES_WGT"] - bestweight[ii]
          Pdata[change, "FEMALES_NUM"] <- Pdata[ii, "FEMALES_NUM"] - 1
        }
      }
    }
  }
  if (any(Pdata$SPECIES_WGT < 0, na.rm = TRUE)) stop("Some California samples were",
    " reduced to below zero 'SPECIES_WGT', which is not plausible.")
  SPECIES_WGT_L <- SPECIES_WGT_A <- Pdata[, "SPECIES_WGT"]
  UNK_WGT_L <- UNK_WGT_A <- Pdata[, "UNK_WGT"]
  MALES_WGT_L <- MALES_WGT_A <- Pdata[, "MALES_WGT"]
  FEMALES_WGT_L <- FEMALES_WGT_A <- Pdata[, "FEMALES_WGT"]
  for (ii in 1:nrow(Pdata)) {
    # Good lengths Good ages
    if (!is.na(Pdata$length[ii]) & Pdata$age[ii] != -1) next
    if (is.na(Pdata$length[ii]) & Pdata$age[ii] == -1) next
    if (Pdata$state[ii] %in% ("WA")) next
    if (Pdata$state[ii] == "CA") {
      change <- which(identifier == 
        paste(Pdata[ii, c("SAMPLE_NO", "CLUSTER_NO"), drop = TRUE], 
          collapse = ""))
    }
    if (Pdata$state[ii] %in% ("OR")) {
      change <- which(identifier == 
        Pdata[ii, c("SAMPLE_NO"), drop = TRUE])
    }
    # Good lengths Bad ages
    if (!is.na(Pdata$length[ii]) & Pdata$age[ii] == -1) {
      SPECIES_WGT_A[change] <- SPECIES_WGT_A[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "U") UNK_WGT_A[change] <- UNK_WGT_A[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "M") MALES_WGT_A[change] <- MALES_WGT_A[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "F") FEMALES_WGT_A[change] <- FEMALES_WGT_A[change] - bestweight[ii]
    }
    # Bad lengths Good ages
    if (is.na(Pdata$length[ii]) & Pdata$age[ii] != -1) {
      SPECIES_WGT_L[change] <- SPECIES_WGT_L[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "U") UNK_WGT_L[change] <- UNK_WGT_L[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "M") MALES_WGT_L[change] <- MALES_WGT_L[change] - bestweight[ii]
      if (Pdata$SEX[ii] == "F") FEMALES_WGT_L[change] <- FEMALES_WGT_L[change] - bestweight[ii]
    }
  }

  #### Oregon - MALES_WGT and FEMALES_WGT is only available from Oregon.
  # Allow sum to be calculated when there are no males or no females
  # because weights are NA in those instances rather than a value of zero.
  # todo: Get JW to include UNK_NUM and UNK_WGT
  Pdata$Wt_Sampled_1 <- apply(Pdata[, c("UNK_WGT", "MALES_WGT", "FEMALES_WGT")], 
    1, sumNA)
  Wt_Sampled_1_L <- apply(data.frame(UNK_WGT_L, MALES_WGT_L, FEMALES_WGT_L),
      1, sumNA)
  Wt_Sampled_1_A <- apply(data.frame(UNK_WGT_A, MALES_WGT_A, FEMALES_WGT_A),
      1, sumNA)

  #### California - multiple species can be sampled in one sample number
  # SPECIES_WGT is specific to a cluster, so sum the species weight across clusters
  # within a given sample
  Pdata$Wt_Sampled_2 <- unsplit(
    # For every sample_no get the species_wgt for a given cluster
    lapply(split(Pdata, list(Pdata$SAMPLE_NO)), 
      function(x) sum(x[!duplicated(x$CLUSTER_NO), "SPECIES_WGT"])), 
    # Assign each answer to entries with that sample_no in Pdata
    Pdata$SAMPLE_NO)
  Wt_Sampled_2_L <- unsplit(
    lapply(split(data.frame(Pdata, SPECIES_WGT_L), list(Pdata$SAMPLE_NO)),
      function(x) sum(x[!duplicated(x$CLUSTER_NO), "SPECIES_WGT_L"])),
    Pdata$SAMPLE_NO)
  Wt_Sampled_2_A <- unsplit(
    lapply(split(data.frame(Pdata, SPECIES_WGT_A), list(Pdata$SAMPLE_NO)),
      function(x) sum(x[!duplicated(x$CLUSTER_NO), "SPECIES_WGT_A"])),
    Pdata$SAMPLE_NO)


  ############################################################################
  #
  # Use calculated weights for Wt_Sampled.
  #
  ############################################################################

  Pdata$Wt_Method <- 0
  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = 1
  Pdata$Wt_Sampled[Pdata$Wt_Method == 1] = Pdata$Wt_Sampled_1[Pdata$Wt_Method == 1]
  Wt_Sampled_L[is.na(Wt_Sampled_L)] <- Wt_Sampled_1_L[is.na(Wt_Sampled_L)]
  Wt_Sampled_A[is.na(Wt_Sampled_A)] <- Wt_Sampled_1_A[is.na(Wt_Sampled_A)]

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = 2
  Pdata$Wt_Sampled[is.na(Pdata$Wt_Sampled)] = Pdata$Wt_Sampled_2[is.na(Pdata$Wt_Sampled)]
  Wt_Sampled_L[is.na(Wt_Sampled_L)] <- Wt_Sampled_2_L[is.na(Wt_Sampled_L)]
  Wt_Sampled_A[is.na(Wt_Sampled_A)] <- Wt_Sampled_2_A[is.na(Wt_Sampled_A)]
  Wt_Sampled_L[Wt_Sampled_L == 0] <- NA
  Wt_Sampled_A[Wt_Sampled_A == 0] <- NA

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = 3
  Pdata$Wt_Sampled[is.na(Pdata$Wt_Sampled)] = Pdata$Wt_Sampled_3[is.na(Pdata$Wt_Sampled)]
  Wt_Sampled_L[is.na(Wt_Sampled_L)] <- Wt_Sampled_3_L[is.na(Wt_Sampled_L)]
  Wt_Sampled_A[is.na(Wt_Sampled_A)] <- Wt_Sampled_3_A[is.na(Wt_Sampled_A)]
  Wt_Sampled_L[Wt_Sampled_L == 0] <- NA
  Wt_Sampled_A[Wt_Sampled_A == 0] <- NA

  Pdata$Wt_Method[is.na(Pdata$Wt_Sampled)] = NA
  Pdata$Wt_Sampled_L <- Wt_Sampled_L
  Pdata$Wt_Sampled_A <- Wt_Sampled_A

  # Summary and boxplot

  printemp = data.frame(cbind(Pdata$Wt_Sampled_1, Pdata$Wt_Sampled_2,
                              Pdata$Wt_Sampled_3, Pdata$Wt_Sampled))

  names(printemp) = c("M+F","SPECIES_WT","L-W","Final Wt_Sampled")

  if (verbose) {
    cat("\nDone calculating sample weights\n\n")
    print(summary(printemp))
    cat("\nWt_Methods:\n\n")
    print(summary(as.factor(Pdata$Wt_Method)))
  }

  NA_Wt_Sampled <- Pdata[is.na(Pdata$Wt_Sampled), ]
  nNA <- NROW(NA_Wt_Sampled)

  if (plot != FALSE){
    if (is.character(plot)) png(plot)
    par(mfrow = c(1, ifelse(nNA > 0, 2, 1)), mgp = c(2.5, 0.5, 0))
    boxplot(as.data.frame(printemp),
      names = c("M+F", "species", "pred. w/ L-W","final"),
      ylab = "Sample weight (lbs)", xlab = "Expansion factor 1 denominator")
    if (nNA > 0) {
      barplot(xtabs(NA_Wt_Sampled$FREQ ~ NA_Wt_Sampled$state + NA_Wt_Sampled$fishyr),
        col = rainbow(length(unique(NA_Wt_Sampled$state))),
        legend.text = TRUE, xlab = "Year",
        ylab = "Num samples w/ denominator = NA",
        args.legend = list(x = "topleft", bty = "n"))
    }
    if (is.character(plot)) dev.off()
  }
  if (nNA == 0 & verbose) cat("\nSample Wts found for all samples.\n\n")

  return(Pdata)

} # End EF1_Denominator

