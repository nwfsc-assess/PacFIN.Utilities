############################################################################
#'
#' Plot cleaned fishery age or length composition data.
#' 
#' \subsection{Workflow}{
#' Can be used at any time after \code{\link{cleanPacFIN}}, has been run.
#' }
#'
#' @param Pdata A matrix of age or length data returned from
#' \code{\link{cleanPacFIN}}.
#' @param length A logical value defining whether or not \code{Pdata} is length-
#'   or age-composition data.
#' @return A plot of either 3 x 2 or 2 x 2 depending on whether or not unique
#'   gear groups exist. Left column are plots of sample size by type and right
#'   column is boxplots of either length or age and depth by year.
#' @author Andi Stephens, Kelli Faye Johnson
#' 
#' @export
#'
#' @seealso \code{\link{cleanPacFIN}}, \code{\link{Stratify}},
#' \code{\link{getGearGroup}}.
#' 
#############################################################################

plotCleaned <- function (Pdata, length = TRUE) {
  SPID = sort(unique(Pdata$SPID))

  geargroups <- ifelse(length(Pdata$geargroup) > 0, TRUE, FALSE)

  par(mfrow = c(ifelse(geargroups, 3, 2), 2),
    oma = c(1, 1, 3, 0.25), mar = c(0.5, 3.25, 0, 0),
    mgp = c(1.5, 0.5, 0))

  # Number of lengths by state per year
  if (length) {
    Pdata <- Pdata[Pdata$length > -1, ]
    x <- Pdata$lengthcm
    ylim2 <- c(0, max(Pdata$lengthcm, na.rm = TRUE))
  } else {
    x <- Pdata$age
    ylim2 <- c(0, max(Pdata$age, na.rm = TRUE))
  }
  barplot(xtabs(Pdata$FREQ ~ Pdata$state + Pdata$fishyr),
    col = rainbow(length(unique(Pdata$state))),
    legend.text = TRUE, xaxt = "n", xlab = "", ylab = "Samples per state",
    args.legend = list(x = "topleft", bty = "n"))
  mtext(side = 3, outer = TRUE, line = 1,
    paste(paste(SPID, collapse = ","), "Fishery", ifelse(length, "length", "age"),
    "compositions"))

  boxplot(x ~ Pdata$fishyr, ylab = ifelse(length, "Length (cm)", "Age"),
    xaxt = "n", frame.plot = FALSE, ylim = ylim2)

  nGRID <- length(unique(Pdata$GRID))

  barplot(xtabs(Pdata$FREQ ~ Pdata$GRID + Pdata$fishyr), col = rainbow(nGRID),
    legend.text = TRUE, xaxt = ifelse(geargroups, "n", "t"),
    ylab = "Samples per gear",
    args.legend = list(x = "topleft", bty = "n", ncol = ceiling(nGRID / 4)))

  boxplot(Pdata$DEPTH_AVG ~ Pdata$fishyr, ylab = expression(bar(Depth)),
    frame.plot = FALSE, ylim = c(0, max(Pdata$DEPTH_AVG, na.rm = TRUE)))

  if (geargroups) {

    barplot(xtabs(Pdata$FREQ ~ Pdata$geargroup + Pdata$fishyr),
      col = rainbow(length(unique(Pdata$geargroup))),
      legend.text = T, xlab = "", ylab = "Samples per gear group",
      args.legend = list(x = "topleft", bty = "n"))

  } # End if

} # End function plotCleaned
