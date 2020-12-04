##########################################################################
#'
#' Write out composition data formatted for Stock Synthesis.
#' 
#' @description
#' Write out composition data to a file, binning the data as specified.
#' 
#' \subsection{Workflow}{
#' PacFIN data should first be stratified using \code{\link{getComps}},
#' then sexes should be assigned as males and females by \code{\link{doSexRatio}}
#' before using \code{writeComps}.
#' 
#' \strong{Failure to use \code{\link{doSexRatio}} will result in all unsexed fish being discarded.}
#' }
#' 
#' @export
#'   
#' @param inComps A dataframe generated as described in Workflow, above.
#' 
#' @template fname
#'   
#' @param abins  Bins to use for ages.  Default is the data bins.
#' @param lbins  Bins to use for lengths.  Default is the data bins.
#' 
#' @param maxAge A numeric value specifying the maximum age of fish that
#'   should be included in the composition data, unless \code{dummybins = TRUE},
#'   then those fish will be included in a plus group which you can investigate
#'   later. Note that \code{maxAge} is only used if \code{abins = NULL}, otherwise
#'   fish are binned according to user specified bins irregardless of \code{maxAge}.
#'   
#' @param partition  Used by Stock Synthesis for length- or age-composition data
#' where 0 = retained + discarded, 1= discarded, and 2 = retained fish.
#' The default is to assume that these fish are retained only.
#' The default was changed in 2020 from a value of 0,
#' and code should be updated accordingly if you really want 0.
#' @param ageErr     Defaults to 1.
#' 
#' @param returns  One of ("FthenM", "Fout", "Mout", or "Uout").  Choose return value
#' of the function.  Default: FthenM
#' 
#' @param dummybins A logcial value specifying whether data outside of the
#'   lower and upper \code{abins} or \code{lbins} should be added to dummy bins,
#'   or be placed in the specified bins. Default is \code{TRUE}.  Dummy
#'   bins are useful for determining whether the current bin structure
#'   properly captures the data.
#'
#' @param sum1 A logical value specifying whether to rescale the compositions
#'   to sum to 1 (they will also be rounded to 4 decimal places)
#'
#' @param digits Integer indicating the number of decimal places to round
#'   value to (value is passed to \code{round()}). NULL will skip rounding.
#'
#' @param overwrite A logical value specifying whether to overwrite an existing
#'   file if the file associated with the input \code{fname} already exists.
#' 
#' @template verbose
#'   
#' @details
#' 
#'   The structure of the input dataframe determines whether
#'   \code{writeComps} produces age-, length-, or conditional-age-at-length-
#'   composition data.
#'   
#'   Four sets of composition data are written to a single file specified
#'   by \code{fname}.  These are:
#'   \itemize{
#'   \item{females followed by males}
#'   \item{male data only}
#'   \item{female data only}
#'   \item{males and females recombined as unsexed fish}
#'   }
#'   
#'   The output file is appended to, rather than overwritten, so you may
#'   want to specify a new filename each time you generate a different
#'   stratification or bin structure, e.g., "out.1.csv", "out.2.csv".
#'   
#'   Composition data are raw weights rather than proportions. Stock Synthesis
#'   internally converts these to proportions.  The raw weights should be
#'   examined for anomalies.
#'   
#'   To create proportions use \code{prop.table} on the columns
#'   containing composition data.
#' 
#'   The columns in the output preceeding 'lengthcm' or 'age' are those that
#'   were used in stratifying the data.
#'   
#' \subsection{Reality Checks}{
#' \itemize{
#' \item{Set \code{verbose = TRUE} to follow progress}
#' \item{Use \code{dummybins} to evaluate bin structure}
#' \item{Examine the raw-weight output for anomalies}
#' }}
#'#' @return Appends data to the file given in \code{fname}.
#'  
#'   Conditionally returns a dataframe if the \code{out} argument
#'   specifies the type of composition to return.
#'   
#' @author Andi Stephens
#' @seealso \code{\link{getComps}}, \code{\link{doSexRatio}}
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
##############################################################################
writeComps = function(inComps, fname="out.csv", abins=NULL, lbins=NULL,
                      maxAge=Inf, partition = 2, ageErr=0, returns = "FthenM",
                      dummybins = FALSE, sum1 = FALSE, digits = 4,
                      overwrite = TRUE, verbose = FALSE) {

  if (verbose){
    cat(paste("Writing comps to file", fname, "\n"))
    cat(paste("\nNote that if you didn't run doSexRatio,",
      "all unsexed fish disappear at this point.\n\n"))
    flush.console()
  }
  # check for existence of the file before writing anything
  if(file.exists(fname)){
    if(overwrite){
      warning("The file ", fname,
              "\n  exists, and overwrite = TRUE, ",
              "so deleting the file before writing new tables.")
      file.remove(fname)
    }else{
      stop("The file ", fname, "\n  exists and overwrite = FALSE.")
    }
    flush.console()
  }

  # Adding columns in case a sex is not represented in inComps
  if(length(  inComps$male) == 0) {
    inComps$male = inComps$msamps = inComps$mtows = 0 }
  if(length(inComps$female) == 0) {
    inComps$female = inComps$fsamps = inComps$ftows = 0}

  # Unsexed fish should have been assigned sex with doSexRatio.  
  # Re-using those columns to represent males + females
  inComps$unsexed = inComps$male + inComps$female
  inComps$usamps = inComps$msamps + inComps$fsamps
  inComps$utows = inComps$alltows

  # Which comps are we doing?

  Names = names(inComps)
  AGE = which(Names == "age")
  LEN = which(Names == "lengthcm")

  # Fix length bins

  if ( !is.null(inComps$lengthcm) ) {

    if ( is.null(lbins) ) {

      if (verbose) {
        cat("\nNo length bins provided, using data as-is\n\n")
      }

      lbins = sort(unique(inComps$lengthcm))

    } # End if for lbins

    # Re-code actual lengths to be lbins
    # Note that the last bin is a dummy bin,
    # created because of how findInterval works. It is stripped later.
    if (dummybins) {
      # KFJ(2015-06-15): Only add zero to the left tail if the user
      # does not want this data included in the compositions and
      # wants to investigate dummy bins, else just include it in the
      # smallest group because it is a pain to have to add back later.
      if (min(lbins) > 0) {
        lbins <- c(0, lbins)
        LbinLo <- lbins
      }
      # KFJ(2015-06-15): Add a bin at the end of the userspecified
      # bins that is of the same bin width to capture the plus group
      # if people want to look at this separately and add back later
      # The Inf bin will work better than 999 b/c you theoretically
      # can have a fish with length larger than 999, but not Inf.
      lbins <- c(lbins, max(lbins) + diff(tail(lbins, 2)), Inf)
    } else {
      lbins <- c(lbins, Inf)
      LbinLo <- c(0, lbins[-1])
    }
    LbinHi = c(lbins[-1] - 1, Inf)

    inComps$lbin = findInterval(inComps$lengthcm, lbins, all.inside=T)
    
    
    # if (verbose) {
    #  cat(paste("Bins:\n\n", LbinLo, "\n",
    #           LbinHi, "\n\n",
    #           "Note that last bin is a dummy bin needed for internal purposes.\n\n"))
    # }
    
    

  } # End if

    # Fix age bins

  if ( !is.null(inComps$age) ) {

    if ( is.null(abins) ) {

      if (verbose){
        cat("\nNo age bins provided, using data as-is\n\n")
      }

      abins = sort(unique(inComps$age))

      abins = abins[abins < maxAge]

    } # End if for abins

    # Re-code actual ages to be abins
    
    if (dummybins) {
      
      if (min(abins) > 0) {
        
        abins = c(0, abins)
      }
      
      abins <- c(abins, max(abins) + diff(tail(abins, 2)), Inf)
      
    } else {
      
      abins <- c(abins, Inf)
      
    } # End if-else dummybins

    # add extra, dummy bin because all.inside=T

    inComps$abin = findInterval(inComps$age, abins, all.inside=T)

    # if (verbose) {
    #  cat(paste("Abins:\n\n", abins, "\n\n",
    #    "Note that last bin is a dummy bin needed for internal purposes.\n\n"))
    # }
    
  } # End if inComps$ages

  AAL = FALSE

  if ( length(AGE) > 0 ) {

    target = "abin"

    STRAT = AGE-1

    KeyNames = c(Names[1:STRAT])
    inComps$key = paste.col(inComps[,KeyNames])

    # matrix will be Ages, Ntows, Nsamps.
    # it gets re-ordered later.

    NCOLS = 2 + length(abins)
    OutNames = c(paste("A",abins, sep=""), "Ntows","Nsamps")

    if ( length(LEN) > 0 ) {

      AAL = TRUE

      STRAT = AGE-2

      KeyNames = c(Names[1:STRAT], "lbin")
      inComps$key = paste.col(inComps[,KeyNames])

      # matrix will be Ages, LbinLo, LbinHi, Ntows, Nsamps.
      # it gets re-ordered later.

      NCOLS =  4 + length(abins)
      OutNames = c(paste("A",abins,sep=""), "lbin","Ntows","Nsamps")

    } # End if

  } else {

    target = "lbin"

    STRAT = LEN-1

    KeyNames = c(Names[1:STRAT])
    inComps$key = paste.col(inComps[,KeyNames])

    # matrix will have Lbins, Ntows, Nsamps
    # it gets re-ordered later.

    NCOLS = 2 + length(lbins)
    OutNames = c(paste0("L",lbins), "Ntows","Nsamps")

  } # End if-else

  # Rename columns to be used below

  names(inComps)[which(names(inComps) == "female")] = "f"
  names(inComps)[which(names(inComps) == "male")] = "m"
  names(inComps)[which(names(inComps) == "unsexed")] = "u"

  # We'll work key by key

  uKeys = inComps$key[!duplicated(inComps$key)]

  # Save the matching stratification columns

  uStrat = inComps[!duplicated(inComps$key), 1:STRAT]

  if (verbose) {
    cat(length(uKeys), "unique keys for", nrow(inComps), "records\n\n")
    head(inComps)
    cat("\n\n")
    flush.console()
  }
  # For each sex in turn

  for ( g in c("m","f","u")) {

    myname = g
    if (myname == "u") { myname = "b" }
    if (verbose) {
      cat(paste("Assembling, sex is:", myname, "\n"))
      flush.console()
    }
    tows = which(names(inComps) == paste(g,"tows",sep=""))
    samps = which(names(inComps) == paste(g,"samps",sep=""))

    # Create output matrix

    output = data.frame(matrix(nrow=length(uKeys), ncol=NCOLS, 0))

    names(output) = OutNames

    for ( k in 1:length(uKeys) ) {

      # Get the matching records

      slice = inComps[inComps$key == uKeys[k],]

      if ( AAL ) {

        output$lbin[k] = slice$lbin[1]

      } # End if

      output$Nsamps[k] = sum(slice[,samps], na.rm=T)

      # Use max here to take care of spurious NA problem that arises in getComps
      # for lengths where there are unsexed fish and no sexed fish.

      output$Ntows[k] = max(slice[,tows], na.rm=T)

      for ( s in 1:length(slice[,target]) ) {

          index = slice[s,target]

          #KFJ(2015-05-01) TODO:
          # BUG ... If there is more than one row for a given index or bin
          # then the value will overwrite the previous value instead of summing

          # It would actually be a bug if there were more than one row for a given
          # index or bin!  The output from getComps shouldn't have any way of producing
          # an extra.  And we have no way here of detecting that, since we're accessing
          # the target vector sequentially, without checking the corresponding length/age.

          # But summing the bin doesn't hurt anything.

          output[k,index] = slice[s,g] + output[k, index]

      } # End for s

    } # End for k

    # Save and identify

    output[is.na(output)] = 0

    if ( AAL ) {

      output$LbinLo = LbinLo[output$lbin]
      output$LbinHi = LbinHi[output$lbin]

    } # End if

    # assign to a name like 'mComps' or 'fComps'
    assign(paste(g, "Comps", sep=""), output)

  } # End for g


  # Now assemble everything and write to a file
  # Note that we are stripping the last, dummy bin.

  NCOLS = ifelse(AAL, NCOLS-5, NCOLS-3)
  # KFJ(2015-06-16): Can also use the following b/c
  # with Inf as the last dummy column it will not be
  # selected as a number, and if other columns are later
  # added for some reason then the code will not break.
  # NCOLS <- max(grep("[A-Z]{1}[0-9]+", colnames(output)))

  blanks = mComps[1:NCOLS]
  blanks[,] = 0

  if (!"fishyr" %in% colnames(uStrat)) stop("fishyr should be a column")
  if (!"fleet" %in% colnames(uStrat)) stop("fleet should be a column")
  uStrat <- data.frame(uStrat[, c("fishyr", "fleet"), drop = FALSE],
    uStrat[, !colnames(uStrat) %in% c("fishyr", "fleet"), drop = FALSE])

  # Fill the rest of the values

  uStrat$sex = NA
  uStrat$partition = partition
  uStrat$ageErr = ageErr

  if ( AAL ) {

    # Note that until empty rows are removed, the LbinLo and LbinHi columns
    # are the same in each dataset

    uStrat$LbinLo = fComps$LbinLo
    uStrat$LbinHi = fComps$LbinHi

  }


  Nsamps = rowSums(cbind(fComps$Nsamps, mComps$Nsamps), na.rm=T)

  Ninput = round(ifelse( 
                 round(Nsamps / uComps$Ntows, 0) < 44, 
                 uComps$Ntows + 0.138 * Nsamps, 7.06 * uComps$Ntows), 0 )

  if(!AAL){
    FthenM = cbind(uStrat, uComps$Ntows, Nsamps, Ninput, fComps[,1:NCOLS], mComps[,1:NCOLS])
  } else {
    FthenM = cbind(uStrat, uComps$Ntows, Nsamps, fComps[,1:NCOLS], mComps[,1:NCOLS])
  }


  Ninput = round(ifelse( 
                 round(mComps$Nsamps / mComps$Ntows, 0) < 44, 
                 mComps$Ntows + 0.138 * mComps$Nsamps, 7.06 * mComps$Ntows), 0 )
  if(!AAL){
    Mout = cbind(uStrat, mComps$Ntows, mComps$Nsamps, Ninput, blanks, mComps[1:NCOLS])
  } else {
    Mout = cbind(uStrat, mComps$Ntows, mComps$Nsamps, blanks, mComps[1:NCOLS])
  }

  Ninput = round(ifelse( 
                 round(fComps$Nsamps / fComps$Ntows, 0) < 44, 
                 fComps$Ntows + 0.138 * fComps$Nsamps, 7.06 * fComps$Ntows), 0 )

  if(!AAL){
    Fout = cbind(uStrat, fComps$Ntows, fComps$Nsamps, Ninput, fComps[1:NCOLS], blanks)
  } else {
    Fout = cbind(uStrat, fComps$Ntows, fComps$Nsamps, fComps[1:NCOLS], blanks)
  } 

  Ninput = round(ifelse( 
                 round(uComps$Nsamps / uComps$Ntows, 0) < 44, 
                 uComps$Ntows + 0.138 * uComps$Nsamps, 7.06 * uComps$Ntows), 0 )

  if(!AAL){
    Uout = cbind(uStrat, uComps$Ntows, uComps$Nsamps, Ninput, uComps[1:NCOLS], blanks)
  } else {
    Uout = cbind(uStrat, uComps$Ntows, uComps$Nsamps, uComps[1:NCOLS], blanks)
  } 
  # Make it pretty

  index = which(names(Fout) == "fComps$Ntows")

  names(Mout)[index] = "Ntows"
  names(Fout)[index] = "Ntows"
  names(Uout)[index] = "Ntows"
  names(FthenM)[index] = "Ntows"

  names(Mout)[index + 1] = "Nsamps"
  names(Fout)[index + 1] = "Nsamps"
  names(Uout)[index + 1] = "Nsamps"
  names(FthenM)[index + 1] = "Nsamps"

  if(!AAL){
    names(Mout)[index + 2] = "InputN"
    names(Fout)[index + 2] = "InputN"
    names(Uout)[index + 2] = "InputN"
    names(FthenM)[index + 2] = "InputN"    
  }


  # Remove empty rows
  Fout = Fout[Fout$Nsamps > 0,]
  Mout = Mout[Mout$Nsamps > 0,]

  if(dim(Uout)[1] != 0) { Uout$sex = 0}
  if(dim(Fout)[1] != 0) { Fout$sex = 1}
  if(dim(Mout)[1] != 0) { Mout$sex = 2}
  if(dim(FthenM)[1] != 0) {FthenM$sex = 3 }

  # function to rescale comps to sum to 1
  # IGT(2019-04-25): I tried using an apply function but kept messing up,
  # so fell back on a simple loop over the rows
  rescale.comps <- function(out){
    value.names <- grep("^[alfmALFM][0-9]+", colnames(out), value = TRUE)
    for(irow in 1:nrow(out)){
      out[irow, names(out) %in% value.names] <-
        out[irow, names(out) %in% value.names] /
          sum(out[irow, names(out) %in% value.names])
    }
    return(out)
  }
  # function to round comps
  round.comps <- function(out, digits){
    value.names <- grep("^[alfmALFM][0-9]+", colnames(out), value = TRUE)
    out[,names(out) %in% value.names] <-
      round(out[,names(out) %in% value.names], digits = digits)
    return(out)
  }

  
  # optionally rescale to sum to 1
  # this needs to happen after combining FthenM rather than to
  # the sex-specific parts
  if(sum1){
    if(verbose){
      message("rescaling comps to sum to 1")
      if( dim(Uout)[1] != 0)   { Uout   <- rescale.comps(Uout) }
      if( dim(Fout)[1] != 0)   { Fout   <- rescale.comps(Fout) }
      if( dim(Mout)[1] != 0)   { Mout   <- rescale.comps(Mout) }
      if( dim(FthenM)[1] != 0) { FthenM <- rescale.comps(FthenM) }
    }
  }
  # optionally round off to chosen value
  if(!is.null(digits)){
    if(verbose){
      message("rounding values to ", digits, " digits")
    }
    if( dim(Uout)[1] != 0)   { Uout   <- round.comps(Uout, digits = digits) }
    if( dim(Fout)[1] != 0)   { Fout   <- round.comps(Fout, digits = digits) }
    if( dim(Mout)[1] != 0)   { Mout   <- round.comps(Mout, digits = digits) }
    if( dim(FthenM)[1] != 0) { FthenM <- round.comps(FthenM, digits = digits) }
  }
  
  # Print the whole shebang out to a file.

  ## # Turn off warnings about "appending column names to file"
  oldwarn = options("warn")
  options("warn" = -1)

  if (verbose) {
    cat("Writing F only, dimensions:", dim(Fout), "\n")
  }

  IDstring = paste("\n\n", "Females only")
  cat(file=fname, IDstring, "\n", append=FALSE)
  
  write.table(file=fname, Fout, sep=",", col.names=T, row.names=F, append=T)
  
  if (verbose) {
    cat("Writing M only, dimensions:", dim(Mout), "\n")
  }
  
  IDstring = paste("\n\n",  "Males only")
  cat(file=fname, IDstring, "\n", append=T)
  
  write.table(file=fname, Mout, sep=",", col.names=T, row.names=F, append=T)
  
  if (verbose) {
    cat("Writing combined sexes as females, dimensions:", dim(Uout), " \n")
  }
  
  IDstring = paste("\n\n", "Sexes combined")
  cat(file=fname, IDstring, "\n", append=T)
  
  write.table(file=fname, Uout, sep=",", col.names=T, row.names=F, append=T)
  
  if (verbose) {
    cat("Writing FthenM, dimensions:", dim(FthenM), "\n")
  }
  
  IDstring = paste("\n\n", "Females then males")
  cat(file=fname, IDstring, "\n", append = TRUE)
  
  write.table(file=fname, FthenM, sep=",", col.names=T, row.names=F, append=T)
  
  # Reset warnings
  options("warn" = oldwarn[[1]])
  
  invisible(eval(parse(text = returns)))
  
} # End function writeComps
