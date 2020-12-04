#########################################################################################
#
#' PacFIN.Utilities: Functions for working up PacFIN biological (BDS) data.
#'
#' The PacFIN.Utilites package provides functions for filtering, summarizing, expanding, and compiling 
#' composition data, and writing out the final products:  length- age- and age-at-length
#' compositions.
#' 
#' Many of the functions described below write summary output to the console, and several create plots for 
#' visualizing data at various steps in processing.  The information written to the console
#' can be captured using the \code{\link{sink}} function to encapsulate your workflow:
#' 
#' \code{sink("myfilename.txt", split=TRUE)}
#' 
#' ... do work ...
#' 
#' \code{sink()}
#' 
#' The second call closes the file.
#' 
#' @section Workflow:
#' There are 15 functions in this package, and they are sensitive to the order in which they
#' are run.
#' 
#' @section Example Runthrough:
#' There is a complete runthrough of example data (species:  XMPL) available so
#' that you can experiment with the package.  Use \code{help(PacFIN_Example)}
#' to see how to filter, stratify and expand the data, and create length- age- 
#' and age-at-length-compositions.
#' 
#' 
#' @section Filtering functions:
#' \tabular{lcl}{
#' \code{\link{cleanPacFIN}}\tab\tab Filters raw PacFIN data\cr
#' }
#' 
#' @section Summary functions:
#' \tabular{lcl}{
#' \code{\link{plotRawData}}\tab\tab Plots commonly viewed raw PacFIN data\cr
#' \code{\link{plotCleaned}}\tab\tab Plots the product of \code{cleanPacFIN} similarly.\cr
#' }
#' 
#' In addition, the \code{\link{getExpansion_1}} function has a "plot" argument to create
#' plots documenting the expansions it creates.  It also writes summary output to the
#' console.
#' 
#' @section Optional functions:
#' \tabular{lcl}{
#' \code{\link{combineCalCOM}}\tab\tab For combinining PacFIN and CalCOM data\cr
#' \code{\link{getSeason}}\tab\tab For treating data from seasonal fisheries, such as Petrale.\cr
#' \code{\link{Stratify}}\tab\tab Reduces the values in a data column to an encoded or named subset.\cr
#' }
#' 
#' @section Expansion functions:
#' \tabular{lcl}{
#' \code{\link{getExpansion_1}}\tab\tab creates the values for the sample to tow expansion.\cr
#' \code{\link{getExpansion_2}}\tab\tab computes the expansion values from the tow upwards to a\cr
#' \tab\tab user-specified stratification
#' }
#' 
#' @section Expansion caveats:
#' There is one manual step in the workflow.
#' After running the expansion functions, data columns Expansion_Factor_1 and 
#' Expansion_Factor_2 are available to use in manually setting the Final_Expansion_Factor. 
#' \itemize{
#' \item{Age data are expanded separately from lengths}.
#' \item{WA fish are generally only expanded using Expansion_Factor_2.}
#' \item{Other expansions are the product of Expansion_Factor_1 * Expansion_Factor_2}
#' \item{For age-at-length comps, set Final_Expansion_Factor to 1.  Each fish represents only itself.}
#' }
#' 
#' @section Compostition functions:
#' \tabular{lcl}{
#' \code{\link{getComps}}\tab\tab uses the Final_Sample_Size and the user-specified 
#' stratification to create comps.\cr
#' \code{\link{doSexRatio}}\tab\tab separates the unsexed fish into males and females\cr
#' \code{\link{writeComps}}\tab\tab formats the composition data for SS3 and writes it to a file.\cr
#' }
#' 
#' @section Infrequentily used functions:
#' You can run these manually to reset the inital values.
#' \tabular{lcl}{
#' \code{\link{getState}}\tab\tab Run by \code{cleanPacFIN} to initialized the \code{state} column.\cr
#' \code{\link{getGearGroup}}\tab\tab Run by \code{cleanPacFIN} to initialize \code{geargroups}.\cr
#' \code{\link{capValues}}\tab\tab Used to limit the maximum value in a vector.\cr
#' }
#'  
#' @docType package
#' @name PacFIN.Utilities
NULL
