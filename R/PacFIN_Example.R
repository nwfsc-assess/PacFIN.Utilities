#' Sample PacFIN Run
#'
#' Cut and paste each of the following commands in turn to
#' see the workflow for processing PacFIN data.
#'
#' The data files for species "XMPL" are supplied with this
#' package, and can be used to explore the options of
#' the various functions.
#'
#' @section Getting Started:
#'
#'  \code{Pdata = cleanPacFIN(XMPL.BDS)}
#'
#'  \code{head(Catch.XMPL)}
#'
#' @section Stratification:
#'
#'  \code{table(Pdata$geargroup)}
#'
#'  \code{Pdata$mygear = Pdata$geargroup}
#'
#'  \code{Pdata$mygear[ Pdata$mygear != c("HKL")] = "TWL"}
#'
#'  \code{Pdata$stratification = paste(Pdata$state,Pdata$mygear, sep=".")}
#'
#'  \code{table(Pdata$stratification)}
#'
#' @section Expansions:
#'
#'  \code{Pdata = getExpansion_1(Pdata)}
#'
#'  \code{Pdata = getExpansion_2(Pdata, Catch.XMPL, Convert=T)}
#'
#'  \code{Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2}
#'
#' @section Length Comps:
#'
#'  \code{Lcomps = getComps(Pdata, Comps="LEN")}
#'
#'  \code{Lcomps = doSexRatio(Lcomps)}
#'
#'  \code{writeComps(Lcomps, fname = "~/LenComps.csv")}
#'
#' @section Ages:
#'
#'  \code{Adata = getExpantion_1(Pdata)}
#'
#'  \code{Adata = getExpansion_2(Adata, Catch.XMPL, Convert=T)}
#'
#'  \code{Adata$Final_Sample_Size = Adata$Expansion_Factor_1 * Adata$Expansion_Factor_2}
#'
#'  \code{Acomps = getComps(Adata, Comps="AGE")}
#'
#'  \code{Acomps = doSexRatio(Acomps)}
#'
#'  \code{writeComps(Acomps, fname= "~/Age.comps.csv")}
#'
#' @section Age-at-Length:
#'
#'  \code{Adata$Final_Sample_Size = 1}
#'
#'  \code{ALcomps = getComps(Adata, Comps="AAL")}
#'
#'  \code{ALcomps = doSexRatio(ALcomps)}
#'
#'  \code{writeComps(ALcomps, fname = "~/AAL.Comps.csv")}
#'
#' @name PacFIN_Example
#' @docType package
#'
#' @examples
#' data(XMPL.BDS)
#' data(Catch.XMPL)
#' catch <- Catch.XMPL
#' unique(gsub("[A-Z]+\\.", "", colnames(catch)[-1]))
#' # Don't clean it so you have ages and lengths
#' Pdata <- cleanPacFIN(XMPL.BDS,
#'   keep_gears = c("HKL", "TWL"),
#'   keep_length_type = unique(XMPL.BDS[, "FISH_LENGTH_TYPE"]),
#'   CLEAN = FALSE)
#' NROW(XMPL.BDS) - NROW(Pdata)
#' table(Pdata[, c("geargroup", "age")])
#' data <- getExpansion_1(Pdata)
#' data[, "stratification"] <- paste(data[, "state"], data[, "geargroup"], sep = ".")
#' data <- data[!is.na(data[, "lengthcm"]) & data[, "SAMPLE_YEAR"] %in% catch[, "Year"], ]
#' data <- data[data[, "stratification"] %in% colnames(catch), ]
#' test <- getExpansion_2(data, catch)
#' test$Final_Sample_Size <- capValues(test$Expansion_Factor_1_L * test$Expansion_Factor_2)
#' comps <- getComps(test[!is.na(test$lengthcm), ], Comps = "LEN")
#' comps <- doSexRatio(comps)

NULL
