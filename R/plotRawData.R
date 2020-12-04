#############################################################################
#'
#' Diagnostic plots and summaries for a raw PacFIN dataset pre-filtering.
#' 
#' \code{plotRawData} creates a set of diagnostic plots and summaries, writing 
#' pdfs and a text file in addition to plotting onscreen and console.
#' 
#' \subsection{Workflow}{
#' Run \code{plotRawData} to visualize and summarize the PacFIN data prior to
#' running \code{\link{cleanPacFIN}}.
#' }
#'
#' @param rawData an unfiltered PacFIN dataset
#' @template fname
#'
#' @export
#'
#' @details
#' Will create a filename from the species ID if one is not provided.
#' 
##############################################################################

plotRawData = function( rawData, fname=NULL ) {

  cat( "\nRunning diagnostics\n\n" )

  if ( is.null(fname) ) {

    # Set up filenames for txt, pdf

    species = sort(unique(rawData$SPID))
    pdffile = paste( "Diags.", species, ".pdf", sep="")
    txtfile = paste( "Diags.", species, ".txt", sep="")

  } else {
    # Remove the extension
    fname <- gsub("\\.[a-zA-Z]{3}$", "", fname)
    pdffile = paste(fname, ".pdf", sep="")
    txtfile = paste(fname, ".txt", sep="")
    

  } # End ifelse

  cat( "Plots will be written to", pdffile, "\n" )
  cat( "Summaries will be written to", txtfile, "\n" )
  
  sink(file=txtfile,split=T, append=T)

  # Develop statistics of interest

  len = rawData[!is.na(rawData$FISH_LENGTH),]
  len$len = floor(len$FISH_LENGTH/10)
  len$depth_mid = (len$DEPTH_MIN+len$DEPTH_MAX)/2
  ltows = len[!duplicated(len$SAMPLE_NO),]

  meanLen.yr = tapply(len$len,list(len$SAMPLE_YEAR),mean)
  meanLen = tapply(len$len,list(len$SAMPLE_NO,len$SAMPLE_YEAR),mean)

  age = rawData[!is.na(rawData$FISH_AGE_YEARS_FINAL),]
  age$age = age$FISH_AGE_YEARS_FINAL
  atows = age[!duplicated(age$SAMPLE_NO),]
  meanAge = tapply(age$age,list(age$SAMPLE_NO,age$SAMPLE_YEAR),mean)

  # Print tables

  #cat("Lengths for which FISH_LENGTH_TYPE is T:  ")
  #print(len[len$FISH_LENGTH_TYPE=="T",])
  #cat("\n\n")

  cat("Records per SAMPLE_YEAR\n\n")
  print(table(rawData$SAMPLE_YEAR,useNA="ifany"))
  cat("\n\n")

  cat("SOURCE_AGID vs. SAMPLE_AGENCY\n")
  print(table(rawData$SOURCE_AGID,rawData$SAMPLE_AGENCY,useNA="ifany"))
  cat("\n\n")

  cat("FISH_LENGTH_TYPE\n")
  print(table(rawData$FISH_LENGTH_TYPE,useNA="ifany"))
  cat("\n\n")

  cat("FISH_LENGTH\n")
  print(table(rawData$FISH_LENGTH,useNA="ifany"))
  cat("\n\n")

  cat("GEAR vs GRID\n")
  print(table(len$GEAR,len$GRID))
  cat("\n\n")

  cat("FISH_LENGTH for lengthed fish\n")
  print(table(len$FISH_LENGTH_TYPE,useNA="ifany"))
  cat("\n\n")

  cat("SAMPLE_YEAR vs SOURCE_AGID for lengthed fish\n")
  print(table(len$SAMPLE_YEAR,len$SOURCE_AGID))
  cat("\n\n")

  cat("Difference between FISH_LENGTH and floor(FISH_LENGTH)\n")
  print(table(len$FISH_LENGTH-floor(len$FISH_LENGTH)))
  cat("\n\n")

  cat("Difference between FISH_LENGTH/10 and floor(FISH_LENGTH/10)\n")
  print(table(round(len$FISH_LENGTH/10-floor(len$FISH_LENGTH/10),1)))
  cat("\n\n")

  cat("DEPTH_AVG for lengthed fish\n")
  print(table(is.na(len$DEPTH_AVG)))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for SAMPLE_NOs with lengthed fish\n")
  print(table(ltows$SAMPLE_YEAR,ltows$SOURCE_AGID))
  cat("\n\n")

  cat("DEPTH_AVG for SAMPLE_NOs with lengthed fish\n")
  print(table(is.na(ltows$DEPTH_AVG),useNA="ifany"))
  cat("\n\n")

  cat("Number of aged fish\n")
  print(nrow(age))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for SAMPLE_NOs with aged fish\n")
  print(table(atows$SAMPLE_YEAR,atows$SOURCE_AGID))
  cat("\n\n")

  cat("SAMPLE_YEAR vs. SOURCE_AGID for aged fish\n")
  print(table(age$SAMPLE_YEAR,age$SOURCE_AGID))
  cat("\n\n")

  cat("age2 vs. age3 for aged fish\n")
  print(table(age$age2,age$age3,useNA="ifany"))
  cat("\n\n")

  cat("age1 vs. age2 for aged fish\n")
  print(table(age$age1,age$age2,useNA="ifany"))
  cat("\n\n")
  
  sink()

  # Plots

  # Fingers crossed, works the same for Mac and PC

  # Set up device for pdf

  graphics.off()

  pdf(pdffile)

  par(mfrow=c(2,2))

  hist(len$len,nclass=30, xlab="", main="FISH_LENGTH")

  barplot(table(10*round(len$FISH_LENGTH/10-floor(len$FISH_LENGTH/10),1)),
          xlab="Difference in rounded and floored lengths")

  plot(len$FISH_LENGTH,len$FORK_LENGTH,pch=16, xlab="FISH_LENGTH", ylab="FORK_LENGTH")

  plot(len$DEPTH_AVG,len$depth_mid,xlim=c(0,400),ylim=c(0,400), xlab="DEPTH_AVG", ylab="Depth_mid")
  abline(a=0,b=1)

  hist(ltows$DEPTH_AVG, xlab="", main="DEPTH_AVG")

  hist(age$age,nclass=30, xlab="", main="Age")

  par(mfrow=c(2,1))
  boxplot(as.list(as.data.frame(meanLen)),varwidth=T,main="Mean length")
  boxplot(as.list(as.data.frame(meanAge)),varwidth=T,main="Mean age")

  dev.off()

} # End plotRawData
