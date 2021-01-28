#' PacFIN Catch Extraction
#'
#' Extract commercial fisheries catch data from fish tickets supplied by fish processing plants when the fish were delivered. 
#' This data is archived in the Comprehensive_FT table contained within the PacFIN (https://pacfin.psmfc.org/) database.
#'
#' @param PACFIN_SPECIES_CODE A character vector of PacFIN species code or codes. For species with a nominal category use, e.g.: PACFIN_SPECIES_CODE = "('PTRL', 'PTR1')".
#' Otherwise, for major species that are always separated out in the catch use: e.g.: PACFIN_SPECIES_CODE = "'SABL'". 
#' @param PacFIN_Common_Name A character vector of PacFIN species common name used only for checking for a nominal species ID. The function will stop after the species codes are printed.
#' @param UID A character vector of for the PacFIN login. The default is 'PacFIN.Login', which can be set prior to running this function.
#' @param PWD A character vector of for the PacFIN password. The default is 'PacFIN.PW', which can be set prior to running this function.
#' @param minYr The minimum year for which data is to be extracted.
#' @param maxYr The maximum year for which data is to be extracted.
#' @param verbose A logical object. When TRUE, verbose output will be printed. When FALSE, only differences and ratios of INPFC and PSMFC areas will be printed. The default is TRUE. 
#' @param addColsWithLegacyNames When TRUE, the main three data frames (CompFT, PacFIN.INPFC.Summary.Catch, PacFIN.PSMFC.Summary.Catch) will have their historically used columns 
#' copied and given legacy names, mostly from the 'vdrfd' PacFIN table. The default is currently TRUE. 
#'
#' @author John R. Wallace
#' @export
#' @return An R list with the following data frames: 'CompFT' contains the comprehensive Fish Ticket information; PacFIN.INPFC.Summary.Catch' is catch summed over INPFC areas by year, agency, and port;
#' 'PacFIN.PSMFC.Summary.Catch' is catch summed over PSMFC areas by year, agency, and port; Catch.mt.by.Agency.Year.Fleet is catch summarized by agency, year, and fleet, 'Tribal.Catch.mt.by.Year.Gear'
#' is tribal catch data summarized by year and gear type.
#' @examples
#' PacFIN.Login <- "jonesj" 
#' PacFIN.PW <- "????????"   
#' PacFIN.PTRL.Catch <- PacFIN.Catch.Extraction("('PTRL', 'PTR1')")
#' PacFIN.SABL.Catch <- PacFIN.Catch.Extraction("'SABL'")
#'
PacFIN.Catch.Extraction <- function(PACFIN_SPECIES_CODE = "('CNRY','CNR1')", PacFIN_Common_Name = NULL, UID = PacFIN.Login, PWD = PacFIN.PW, minYr = 1900, maxYr = 2100, verbose = TRUE, addColsWithLegacyNames = TRUE) {

    # -------- Import utility Functions --------
    sourceFunctionURL <- function (URL) {
        " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
        require(httr)
        File.ASCII <- tempfile()
        on.exit(file.remove(File.ASCII))
        getTMP <- httr::GET(URL)
        write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
        source(File.ASCII)
    }
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/printf.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/catf.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/import.sql.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/agg.table.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ino.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/PacFIN-Data-Extraction/master/R/nameConvertVdrfdToCompFT.R")
    
    require(Hmisc)
    require(survival)
    require(Formula)
    require(ggplot2)
    
    
   #  -------- Check species info  --------
   
   
   if(!is.null(PacFIN_Common_Name) & UID == "wallacej") {
      sp <- import.sql("Select * from pacfin.sp", dsn = "PacFIN", uid = UID,  pwd = PWD)
      cat("\n\n")
      printf(sp[grep(casefold(PacFIN_Common_Name, upper = TRUE),  sp$CNAME), 1:7])
      stop("\n--- Just checking for a nominal species ID using the PacFIN common name. ---\n\n")
   }   
   
   if(verbose & UID == "wallacej") {
      if(verbose) {
         catf("\nChecking for nominal species names using the first 3 letters of the SPID listed first in the PACFIN_SPECIES_CODE argument\n")
         catf("\nIn rare cases this doesn't work to find the nominal species ID.\n\n")
      }    
      sp <- import.sql("Select * from pacfin.sp", dsn = "PacFIN", uid = UID,  pwd = PWD)
      if(nchar(PACFIN_SPECIES_CODE) <= 6)
        printf(sp[grep(substr(PACFIN_SPECIES_CODE, 2, 4),  sp$SPID), 1:7])
      else
        printf(sp[grep(substr(PACFIN_SPECIES_CODE, 3, 5),  sp$SPID), 1:7])
   }
   
   # -------- Data from the Comprehensive_FT table --------
   
   # Gear table
   # gr <- import.sql("Select * from pacfin.gr", dsn="PacFIN", uid=UID, pwd=PWD)
   
   
   # COUNCIL_CODE = 'P'; with research catch included
   # For species with a nominal category use, e.g.: PACFIN_SPECIES_CODE = "('PTRL', 'PTR1')" (which gives the code: < PACFIN_SPECIES_CODE = any ('PTRL', 'PTR1') > below), otherwise use: PACFIN_SPECIES_CODE = "'SABL'"
   if(verbose) 
      catf("\n\nImporting data from PacFIN\n")
      
   CompFT <- import.sql(
      "Select COUNCIL_CODE, AGENCY_CODE, DAHL_GROUNDFISH_CODE, INPFC_AREA_TYPE_CODE, LANDING_YEAR, LANDING_MONTH, LANDING_DATE, FTID, PARTICIPATION_GROUP_CODE, 
             PACFIN_CATCH_AREA_CODE, ORIG_PACFIN_CATCH_AREA_CODE, PACFIN_PORT_CODE, FLEET_CODE, VESSEL_ID, PACFIN_GEAR_CODE, IS_IFQ_LANDING, REMOVAL_TYPE_CODE, 
             CONDITION_CODE, DISPOSITION_CODE, EXVESSEL_REVENUE, PACFIN_SPECIES_CODE, NOMINAL_TO_ACTUAL_PACFIN_SPECIES_CODE, IS_SPECIES_COMP_USED, GRADE_CODE, GRADE_NAME, 
             PACFIN_GROUP_GEAR_CODE, ROUND_WEIGHT_MTONS, LANDED_WEIGHT_MTONS                         
        from pacfin_marts.Comprehensive_FT 
       where PACFIN_SPECIES_CODE = any &sp 
         and COUNCIL_CODE = 'P' 
         and AGENCY_CODE in ('W','O','C')
         and landing_year between &beginyr and &endyr",
    c("&sp", "&beginyr", "&endyr"), c(PACFIN_SPECIES_CODE, minYr, maxYr), dsn="PacFIN", uid=UID, pwd=PWD)
  
  
   # Create W_O_C_Port_Groups
   CompFT$W_O_C_Port_Groups <- CompFT$AGENCY_CODE
   CompFT$W_O_C_Port_Groups[CompFT$AGENCY_CODE %in% 'W'] <- "AWA"
   CompFT$W_O_C_Port_Groups[CompFT$AGENCY_CODE %in% 'O'] <- "AOR"
   CompFT$W_O_C_Port_Groups[CompFT$AGENCY_CODE %in% 'C'] <- "ACA"
   
   # Fleet type data frame
   Catch.mt.by.Agency.Year.Fleet <- aggregate(list(ROUND_WEIGHT_MTONS = CompFT$ROUND_WEIGHT_MTONS) , CompFT[, c('FLEET_CODE', 'LANDING_YEAR', 'AGENCY_CODE')], sum, na.rm = TRUE)
     
   # Tribal catch by gear ID
   Tribal.Summary.Catch <- CompFT[CompFT$FLEET_CODE %in% 'TI', ]
   if(nrow(Tribal.Summary.Catch) > 0)
      Tribal.Catch.mt.by.Year.Gear <- aggregate(list(ROUND_WEIGHT_MTONS = Tribal.Summary.Catch$ROUND_WEIGHT_MTONS), Tribal.Summary.Catch[ ,c('PACFIN_GEAR_CODE', 'LANDING_YEAR')], sum, na.rm = TRUE)   
   else
      Tribal.Catch.mt.by.Year.Gear <- data.frame(PACFIN_GEAR_CODE = character(0), LANDING_YEAR = character(0), ROUND_WEIGHT_MTONS = character(0))
    
   if(verbose) {
   
     print(Table(CompFT$INPFC_AREA_TYPE_CODE, CompFT$PACFIN_CATCH_AREA_CODE)); cat("\n\n")
     
     print(Table(CompFT$PACFIN_SPECIES_CODE, CompFT$W_O_C_Port_Groups)); cat("\n\n")
   
     print(Table(CompFT$PACFIN_SPECIES_CODE, CompFT$LANDING_YEAR)); cat("\n\n")
   
     print(Table(CompFT$PACFIN_SPECIES_CODE, CompFT$LANDING_YEAR, CompFT$AGENCY_CODE)); cat("\n\n")
     
     agg.table(aggregate(list(ROUND_WEIGHT_MTONS = CompFT$ROUND_WEIGHT_MTONS) , CompFT[, c('LANDING_YEAR', 'FLEET_CODE')], sum, na.rm = TRUE)) # Print = TRUE by default
     cat("\n\n")
     
     print(r(head(Catch.mt.by.Agency.Year.Fleet, 4), 2)); cat("\n\n")
     
     print(r(head(Tribal.Catch.mt.by.Year.Gear, 4), 2)); cat("\n\n")
     
     # PACFIN_CATCH_AREA_CODE by LANDING_YEAR by AGENCY_CODE - shows where the differences in the INPFC and PSMFC areas are.
     print(Table(CompFT$PACFIN_CATCH_AREA_CODE, CompFT$LANDING_YEAR, CompFT$AGENCY_CODE)); cat("\n\n")
   
     # Research catch by year and removal type - compare with FLEET removal
     print(r(agg.table(aggregate(CompFT$ROUND_WEIGHT_MTONS, CompFT[, c('LANDING_YEAR', 'REMOVAL_TYPE_CODE')], sum, na.rm = TRUE), Print = FALSE), 3)); cat("\n\n")
     
     # Here is how 'Fleet' compares to 'Removal type' 
       # Fleet type: limited entry 'LE', open access 'OA', tribal indian 'TI', research 'R', unknown 'XX' 
       # Removal type: Commercial (Non-EFP) 'C', Commercial(Direct Sales) 'D', Exempted fishing permit(EFP) 'E', Other 'O', Personal use 'P', Research 'R', Unknown 'U'
    
     cat('\nFLEET_CODE by REMOVAL_TYPE_CODE\n')
     print(Table(CompFT$FLEET_CODE, CompFT$REMOVAL_TYPE_CODE)); catf("\n\n")
  }   
   
   # Fleet breakdown including research and tribal catch
      # - Tribal catch is included but not separable in a 'sc' type table.
      # - I would not assume this is all the research catch and would ask the Region what they have.
   
   # ------------------------------------------- INPFC sc table ----------------------------------------------------------------
   
   # Take out research catch for a summary catch (sc) like table
   # change(CompFT[!(CompFT$REMOVAL_TYPE_CODE %in% "R") & CompFT$INPFC_PSMFC_AREA_GROUP %in% 'INPFC',])  <<== !!! WRONG !!! see PACFIN_CATCH_AREA_CODE = INPFC_AREA_TYPE_CODE below
     
   CompFT.INPFC <- CompFT[!(CompFT$REMOVAL_TYPE_CODE %in% "R"), ]
   # Can not use grep() below since ORIG_PACFIN_CATCH_AREA_CODE is matched also
   # ***** This change in names is for the comparison below - the summary catch (sc) PacFIN has this strangeness ****
   names(CompFT.INPFC)[(1:length(names(CompFT.INPFC)))[names(CompFT.INPFC) == "INPFC_AREA_TYPE_CODE"]] <- 'PACFIN_CATCH_AREA_CODE'
   CompFT.INPFC$PACFIN_PORT_CODE <- CompFT.INPFC$W_O_C_Port_Groups
   PacFIN.INPFC.Summary.Catch <- aggregate(list(ROUND_WEIGHT_MTONS = CompFT.INPFC$ROUND_WEIGHT_MTONS), CompFT.INPFC[, c('COUNCIL_CODE', 'DAHL_GROUNDFISH_CODE', 'LANDING_YEAR', 'LANDING_MONTH', 
                                                         'PACFIN_SPECIES_CODE', 'PACFIN_CATCH_AREA_CODE', 'PACFIN_GEAR_CODE', 'PACFIN_GROUP_GEAR_CODE', 'PACFIN_PORT_CODE')], sum, na.rm = TRUE)
                                             
   PacFIN.INPFC.Summary.Catch <- sort.f(PacFIN.INPFC.Summary.Catch, c('LANDING_YEAR', 'LANDING_MONTH', 'PACFIN_CATCH_AREA_CODE', 'PACFIN_GEAR_CODE', 'PACFIN_PORT_CODE'))
   
   SC.INPFC.agg <- agg.table(aggregate(list(Catch.mt = PacFIN.INPFC.Summary.Catch$ROUND_WEIGHT_MTONS), list(LANDING_YEAR = PacFIN.INPFC.Summary.Catch$LANDING_YEAR, 
                                                         PACFIN_PORT_CODE = PacFIN.INPFC.Summary.Catch$PACFIN_PORT_CODE), sum, na.rm = TRUE), Print = FALSE)
   SC.INPFC.agg[is.na(SC.INPFC.agg)] <- 0
  
   # ------------------------------------------- PSMFC Summary Catch Table ---------------------------------------------------------------- 
      
   CompFT.PSMFC <- CompFT[!(CompFT$REMOVAL_TYPE_CODE %in% "R") & CompFT$PACFIN_CATCH_AREA_CODE %in% c("UP","1A", "1B", "1D", "1E", "1C", "2A", "2B", "2C", "2E", "2F", "2D", "3A", "3B", "3S"), ]
   CompFT.PSMFC$PACFIN_PORT_CODE <- CompFT.PSMFC$W_O_C_Port_Groups
   PacFIN.PSMFC.Summary.Catch <- aggregate(list(ROUND_WEIGHT_MTONS = CompFT.PSMFC$ROUND_WEIGHT_MTONS), CompFT.PSMFC[, c('COUNCIL_CODE', 'DAHL_GROUNDFISH_CODE', 'LANDING_YEAR', 'LANDING_MONTH',
                                                         'PACFIN_SPECIES_CODE', 'PACFIN_CATCH_AREA_CODE', 'PACFIN_GEAR_CODE', 'PACFIN_GROUP_GEAR_CODE', 'PACFIN_PORT_CODE')], sum, na.rm = TRUE)
                    
   PacFIN.PSMFC.Summary.Catch <- sort.f(PacFIN.PSMFC.Summary.Catch, c('LANDING_YEAR', 'LANDING_MONTH', 'PACFIN_CATCH_AREA_CODE', 'PACFIN_GEAR_CODE', 'PACFIN_PORT_CODE'))
   
   SC.PSMFC.agg <- agg.table(aggregate(list(Catch.mt = PacFIN.PSMFC.Summary.Catch$ROUND_WEIGHT_MTONS), list(LANDING_YEAR = PacFIN.PSMFC.Summary.Catch$LANDING_YEAR, 
                                                                PACFIN_PORT_CODE = PacFIN.PSMFC.Summary.Catch$PACFIN_PORT_CODE), sum, na.rm = TRUE), Print = FALSE)
   SC.PSMFC.agg[is.na(SC.PSMFC.agg)] <- 0
   
   
   if(verbose) {
   
      print(r(SC.INPFC.agg, 3)); cat("\n\n")
      print(Table(PacFIN.INPFC.Summary.Catch$PACFIN_CATCH_AREA_CODE, PacFIN.INPFC.Summary.Catch$PACFIN_PORT_CODE)); cat("\n\n")
      
      print(r(SC.PSMFC.agg, 3)); cat("\n\n")
      print(Table(PacFIN.PSMFC.Summary.Catch$PACFIN_CATCH_AREA_CODE, PacFIN.PSMFC.Summary.Catch$PACFIN_PORT_CODE)); catf("\n\n")
   } 
      
   # ----------------- Comparison of PSMFC sc table to INPFC sc table ----------------------------- 
   
   names(SC.INPFC.agg) <- paste0(names(SC.INPFC.agg), ".INPFC")
   SC.INPFC.agg <- SC.INPFC.agg[,order(names(SC.INPFC.agg)), drop = FALSE]  
   
   names(SC.PSMFC.agg) <- paste0(names(SC.PSMFC.agg), ".PSMFC")
   SC.PSMFC.agg <- SC.PSMFC.agg[,order(names(SC.PSMFC.agg)), drop = FALSE]
   
   if(verbose) {
      cat('\nMake sure the ordering is correct\n')
      print(SC.INPFC.agg); cat("\n\n")
      print(SC.PSMFC.agg); catf("\n\n")
   }
   
   
   # Early in the year, the last year of data may currently have only one of the area types and hence the number of rows is different
   if(nrow(SC.PSMFC.agg) !=  nrow(SC.INPFC.agg)) {
      commonYears <- sort(intersect(SC.PSMFC.agg$LANDING_YEAR, SC.INPFC.agg$LANDING_YEAR))
      SC.PSMFC.agg <- SC.PSMFC.agg[SC.PSMFC.agg$LANDING_YEAR %in% commonYears, ]
      SC.INPFC.agg <- SC.INPFC.agg[SC.INPFC.agg$LANDING_YEAR %in% commonYears, ]   
   }
 
   N <- nrow(SC.INPFC.agg)
   Diff.and.Ratio <- cbind(SC.INPFC.agg, " " = rep("    ", N), SC.PSMFC.agg, " " = rep("    ", N), 
                           SC.INPFC.agg - SC.PSMFC.agg, " " = rep("    ", N), SC.INPFC.agg/SC.PSMFC.agg)
   
   names(Diff.and.Ratio) <- c(names(SC.INPFC.agg), " ", names(SC.PSMFC.agg), "  ", paste0(substr(names(SC.PSMFC.agg), 2, 3), '.diff'), " ", paste0(substr(names(SC.PSMFC.agg), 2, 3), '.ratio'))
   # Tmp.Diff <- Diff.and.Ratio[, 1:11]
   # Tmp.Diff[is.na(Tmp.Diff )] <- 0
   # Diff.and.Ratio <- cbind(Tmp.Diff, Diff.and.Ratio[, 12:15]) # unsupported matrix index in replacement, so need temp file
   
   cat("\nDifference and ratio of INPFC and PSMFC areas\n\n")
   print(r(Diff.and.Ratio, 2)); catf("\n\n")
   
   if(addColsWithLegacyNames) {
   
    '  # %ino% preserves the order when using matching operators unlike %in%. See my entry on Stack Overflow: '
    '  #  https://stackoverflow.com/questions/10586652/r-preserve-order-when-using-matching-operators-in  '
    '  # RWT_LBS was historically converted to CATCH.LBS in the SQL code, so here ROUND_WEIGHT_LBS is converted to CATCH.LBS  ' 
    
      for(i in (1:nrow(nameConvertVdrfdToCompFT))[nameConvertVdrfdToCompFT$Comp_FT %ino% names(CompFT)]) {
         CompFT[nameConvertVdrfdToCompFT[i, 'vdrfd']] <- CompFT[nameConvertVdrfdToCompFT[i, 'Comp_FT']]
      }  
      CompFT$CATCH.LBS <- CompFT$ROUND_WEIGHT_MTONS * 2204.62262
      CompFT$LWT_LBS <- CompFT$LANDED_WEIGHT_MTONS * 2204.62262
      
      
      for(i in (1:nrow(nameConvertVdrfdToCompFT))[nameConvertVdrfdToCompFT$Comp_FT %ino% names(PacFIN.INPFC.Summary.Catch)]) {
         PacFIN.INPFC.Summary.Catch[nameConvertVdrfdToCompFT[i, 'vdrfd']] <- PacFIN.INPFC.Summary.Catch[nameConvertVdrfdToCompFT[i, 'Comp_FT']]
      }  
      PacFIN.INPFC.Summary.Catch$CATCH.KG <- PacFIN.INPFC.Summary.Catch$ROUND_WEIGHT_MTONS * 1000
      
      
      for(i in (1:nrow(nameConvertVdrfdToCompFT))[nameConvertVdrfdToCompFT$Comp_FT %ino% names(PacFIN.PSMFC.Summary.Catch)]) {
         PacFIN.PSMFC.Summary.Catch[nameConvertVdrfdToCompFT[i, 'vdrfd']] <- PacFIN.PSMFC.Summary.Catch[nameConvertVdrfdToCompFT[i, 'Comp_FT']]
      }   
      PacFIN.PSMFC.Summary.Catch$CATCH.KG <- PacFIN.PSMFC.Summary.Catch$ROUND_WEIGHT_MTONS * 1000
      
   }
   
  invisible(list(CompFT = CompFT, PacFIN.INPFC.Summary.Catch = PacFIN.INPFC.Summary.Catch, PacFIN.PSMFC.Summary.Catch = PacFIN.PSMFC.Summary.Catch, 
                   Catch.mt.by.Agency.Year.Fleet = Catch.mt.by.Agency.Year.Fleet, Tribal.Catch.mt.by.Year.Gear = Tribal.Catch.mt.by.Year.Gear))
}



