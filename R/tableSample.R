#' Table of Sample Size By Fleet and Year
#'
#' Create a table of samples by fleet and year to
#' be included in an assessment document.
#'
#' @template Pdata
#' @template fname
#' @template strat
#' @param comps Specify whether to calculate the length or age samples.
#' The default is to calculate the number of length samples.
#' @param remove_yrs A vector of years to remove from the data before
#' summarizing sample sizes. The default of \code{NULL} leads to no
#' sub setting of the data.
#' 
#' @return
#' A table is written to a csv file as specified in \code{fname} and the data frame
#' is also returned as an object invisibly.
#' @examples
#' data("XMPL.BDS", package = "PacFIN.Utilities")
#' test <- tableSample(XMPL.BDS)
#' all(test[, "Year"] %in% 2000:2015)
#' \dontshow{
#'   testthat::expect_identical(colnames(test),
#'     c("Year", "C.tows", "C.fish", "O.tows", "O.fish", "W.tows", "W.fish"))
#'   testthat::expect_equal(
#'     sum(as.numeric(test[, grepl("\\.fish", colnames(test))])),
#'     NROW(XMPL.BDS[!is.na(XMPL.BDS$FISH_LENGTH), ]))
#' }
#' @author Chantel Wetzel
#' @export

tableSample <- function(Pdata,
  fname = paste0("fishery_", comps, "_samples.csv"),
  strat = "SOURCE_AGID",
  comps = c("LEN", "AGE"), remove_yrs = NULL) {

  Pdata$strat <- apply(Pdata[, strat, drop = FALSE], 1, paste0, collapse = ".")
  comps <- match.arg(comps, several.ok = FALSE)
	if (comps == "LEN"){
		temp = Pdata[!is.na(Pdata$FISH_LENGTH),]
	}
	
	if (comps == "AGE"){
		temp = Pdata[Pdata$age != -1,]
	}

	if(!is.null(remove_yrs)){
		temp = temp[!temp$SAMPLE_YEAR %in% remove_yrs, ]
	}

	Ntow  = table(temp$SAMPLE_YEAR, temp$strat, !duplicated(as.character(temp$SAMPLE_NO)))[,,"TRUE"]
  Nfish = table(temp$SAMPLE_YEAR, temp$strat)

	samples = rownames(Ntow); names = "Year"
	for (a in colnames(Ntow)){
		get = cbind(Ntow[,a], Nfish[,a])
		samples = cbind(samples, get)
		names = c(names, paste0(a, ".tows"), paste0(a, ".fish"))
	}
	colnames(samples) = names	

  write.csv(samples, file = fname, row.names=FALSE)
  return(invisible(samples))
}