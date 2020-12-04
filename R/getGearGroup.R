#' Create column for gears according to PacFIN gears
#'
#' \subsection{Workflow}{
#' \code{getGearGroup} is run by \code{\link{cleanPacFIN}} and users shouldn't need
#' to worry about it.
#' }
#' Data from the PacFIN
#' [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' is used to create a column in \code{Pdata} called \code{geargroup}, where
#' \code{Pdata$GRID} is recoded to \code{geargroup} according to the gear table.
#'
#' @template Pdata
#' @template spp
#' @template verbose
#' 
#' @export
#'
#' @details
#' 
#'   GRIDS according to the table taken from
#'   [gear table](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt)
#' 
#' @return A modified \code{data.frame} where an additional column labeled
#'   \code{geargroup} is added to \code{Pdata}. No original columns are modified
#'   in the process.
#' @author Andi Stephens
#' 
############################################################################

getGearGroup = function (Pdata, spp = NULL, verbose = FALSE) {

  if (verbose) {
    cat("\n\nGear groupings reflect those in the table at",
       "https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt\n\n")
  }
  local <- GearTable
  if (is.factor(Pdata[, "GRID"])) Pdata[, "GRID"] <- as.character(Pdata[, "GRID"])
  if (!is.null(spp)) {  
  	if (tolower(spp) %in% c("sablefish")) {
  	  msc <- Pdata[Pdata$GRID %in% c("DNT", "LJ", "JIG", "MDT", "MPT"), ]
      if (nrow(msc) > 0) {
        if (all(c("TOTAL_WGT", "SAMPLE_YEAR", "SOURCE_AGID", "GRID") %in% colnames(Pdata))) {
          message("The following samples were assigned to the gear group 'MSC':")
          print(aggregate(TOTAL_WGT ~ SAMPLE_YEAR + SOURCE_AGID + GRID, 
            data = msc, sum, na.rm = TRUE))
        }
        # Danish/Scottish Seine trawl
        local$GROUP[local$GRID == "DNT"] <- "MSC"
        # Midwater trawl
        local$GROUP[local$GRID %in% c("MDT", "MPT")] <- "MSC"
        # Handline jigger
        local$GROUP[local$GRID == "JIG"] <- "MSC"
      }
    }
  }

  Pdata$geargroup <- local$GROUP[match(Pdata$GRID, local$GRID)]
  indices <- which(is.na(Pdata$geargroup))
  Pdata$geargroup[indices] <- Pdata$GRID[indices]

  return(Pdata)

} # End function getGearGroup

