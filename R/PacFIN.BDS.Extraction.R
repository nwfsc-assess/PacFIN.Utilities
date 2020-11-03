#' PacFIN Biological Data Sample (BDS) Extraction
#'
#' Extract commercial fisheries BDS data recorded by port samplers deployed at fish processing plants. This data is archived in the Comprehensive_BDS_Comm  
#' table contained within the PacFIN (https://pacfin.psmfc.org/) database.
#'
#' @param PACFIN_SPECIES_CODE A character vector for the main PacFIN species code of a species e.g.: PACFIN_SPECIES_CODE = "'SABL'".  
#' (Nominal species codes are not used in BDS.)
#' @param UID A character vector of for the PacFIN login. The default is 'PacFIN.Login', which can be set prior to running this function.
#' @param PWD A character vector of for the PacFIN password. The default is 'PacFIN.PW', which can be set prior to running this function.
#' @param minYr The minimum year for which data is to be extracted.
#' @param maxYr The maximum year for which data is to be extracted.
#' @param verbose When TRUE (the default), verbose output will be printed.
#' @param PacFIN.Catch.Dahl An R data frame with FTID, catch, and Dahl sector information for the species given in the PACFIN_SPECIES_CODE argument.
#' This is normally supplied via the CompFT data frame in the output R list from the PacFIN.Catch.Extraction() function run using the same species.
#' The Dahl sector information will be matched onto the BDS data using FTID. 
#' @param addColsWithLegacyNames When TRUE, historically used columns will be copied and given legacy names from tables used before the creation of the 
#' Comprehensive_BDS_Comm table. The default is FALSE. 
#'
#' @author John R. Wallace
#' @export
#' @return An R data frame containing BDS information for the species given in the PACFIN_SPECIES_CODE argument.
#' @examples
#' PacFIN.Login <- "jonesj" 
#' PacFIN.PW <- "????????"   
#' CNRY.Catch <- PacFIN.Catch.Extraction("('CNRY','CNR1')", minYr = 2015, maxYr = 2017)
#' CNRY.BDS <- PacFIN.BDS.Extraction("'CNRY'", minYr = 2015, maxYr = 2017, PacFIN.Catch.Dahl = CNRY.Catch$CompFT, verbose = FALSE)
#"
PacFIN.BDS.Extraction <- function(PACFIN_SPECIES_CODE = "'CNRY'", UID = PacFIN.Login, PWD = PacFIN.PW, minYr = 1900, maxYr = 2100, 
             verbose = TRUE, PacFIN.Catch.Dahl = NULL, addColsWithLegacyNames = FALSE) {

    # -------- Import utility Functions --------
    sourceFunctionURL <- function(URL) {
       ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
       require(RCurl)
       File.ASCII <- tempfile()
       on.exit(file.remove(File.ASCII))
       writeLines(paste(readLines(textConnection(RCurl::getURL(URL))), collapse = "\n"), File.ASCII)
       source(File.ASCII, local = parent.env(environment()))
    }
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/import.sql.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/printf.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/catf.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ino.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/PacFIN-Data-Extraction/master/R/nameConvertVdrfdToCompBDS.R")
   
    # Get data from the bds_age table
    
      # Names with missing info
        # WEIGHT_OF_LANDING_LBS  # For just that species' portion of the landed catch
        # SPECIES_WEIGHT_LBS     # In a cluster
       
    if(verbose)
       catf("\nGet bds_fish:", date(), "\n\n")
   
    bds_fish <- import.sql(
       "select BDS_ID, SAMPLE_ID, SAMPLE_NUMBER, SAMPLE_YEAR, SAMPLE_MONTH, SAMPLE_DAY, SAMPLE_TYPE, SAMPLE_METHOD_CODE, SAMPLE_AGENCY, AGENCY_CODE, DATA_TYPE, 
               AGENCY_CONDITION_CODE, PACFIN_CONDITION_CODE, PACFIN_PORT_CODE, PACFIN_PORT_NAME, FTID, PACFIN_GEAR_CODE, PACFIN_GEAR_NAME, VESSEL_ID, 
               PSMFC_CATCH_AREA_CODE, DEPTH_AVERAGE_FATHOMS, DEPTH_MAXIMUM_FATHOMS, DEPTH_MINIMUM_FATHOMS, MARKET_CATEGORY, CLUSTER_SEQUENCE_NUMBER, 
               CLUSTER_WEIGHT_LBS, ADJUSTED_CLUSTER_WEIGHT_LBS, FRAME_CLUSTER_WEIGHT_LBS, PACFIN_SPECIES_CODE, FISH_SEQUENCE_NUMBER, OBSERVED_FREQUENCY, 
               FISH_LENGTH_TYPE_CODE, FISH_LENGTH, FORK_LENGTH, FORK_LENGTH_IS_ESTIMATED, FISH_WEIGHT, SEX_CODE, AGENCY_FISH_MATURITY_CODE, 
               FISH_MATURITY_CODE, WEIGHT_OF_MALES_LBS, WEIGHT_OF_FEMALES_LBS, NUMBER_OF_MALES, NUMBER_OF_FEMALES, WEIGHT_OF_LANDING_LBS, 
               EXPANDED_SAMPLE_WEIGHT, SPECIES_WEIGHT_LBS, FINAL_FISH_AGE_CODE, FINAL_FISH_AGE_IN_YEARS, AGE_SEQUENCE_NUMBER, AGE_METHOD_CODE, AGE_READABILITY, 
               PERSON_WHO_AGED, DATE_AGE_RECORDED, AGE_IN_YEARS, AGE_STRUCTURE_CODE, AGENCY_AGE_STRUCTURE_CODE, PACFIN_LOAD_DATE, AGENCY_GRADE_CODE, 
               PACFIN_GRADE_CODE, FISH_LENGTH_UNITS, FISH_WEIGHT_UNITS, FISH_LENGTH_IS_ESTIMATED, FISH_WEIGHT_UNITS_NAME 
         from  pacfin_marts.Comprehensive_BDS_Comm
         where PACFIN_SPECIES_CODE = any &sp
          and  sample_year between &beginyr and &endyr
      order by sample_year, agency_code, sample_number, cluster_sequence_number, fish_sequence_number, age_sequence_number",
     c("&sp", "&beginyr", "&endyr"), c(PACFIN_SPECIES_CODE, minYr, maxYr), uid = UID, pwd = PWD, dsn = "PacFIN", View.Parsed.Only = FALSE)
 
    if(verbose) {
       printf(bds_fish[1:3, ])
       catf("\nGot bds_fish at", date(), "\n\n")
    }   
 
    # BDS_CLUSTER for all sp
      #  ** The code below selects all clusters in a sample (regardless of species) and then sums the cluster weight. **
      #  ** This is necessary only when there is a chance of clusters that did not contain the target species.        **
      #  ** The problem only seems to occur in CA where the total weight of all clusters is not reported.             **
      
    if(verbose)     
       catf("\nGet bds_cluster for all species:", date(), "\n\n")
     
    bds_clust_all <- import.sql(
       "select sample_no, cluster_no, cluster_wgt, sample_year, source_agid
          from pacfin.bds_cluster
         where sample_year between &beginyr and &endyr", 
     c("&beginyr", "&endyr"), c(minYr, maxYr), uid = UID, pwd = PWD, dsn = "PacFIN")
   
    # Convert to CompBDS names using nameConvertVdrfdToCompBDS
    names(bds_clust_all) <- recode.simple(names(bds_clust_all), nameConvertVdrfdToCompBDS[,2:1])
   
    if(verbose) {
       printf(bds_clust_all[1:3, ])
       catf("\nGot bds_cluster for all species and converted the column names to those in the Comprehensive_BDS_Comm table.:\n\n")
    }
    
    # Take out dups
    bds_clust_all <- bds_clust_all[!duplicated(paste(bds_clust_all$SAMPLE_NUMBER, bds_clust_all$CLUSTER_SEQUENCE_NUMBER)),]

    # Aggregate cluster weight
    bds_clust_all.agg <- aggregate(list(all_cluster_sum_kg = bds_clust_all$CLUSTER_WEIGHT_LBS/2.20462262), bds_clust_all[, c("SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", 
         "SAMPLE_YEAR", "AGENCY_CODE")], sum, na.rm = TRUE)
        
    # Combine bds_clust_all.agg with bds_fish 
    bds_fish <- match.f(bds_fish, bds_clust_all.agg, c("SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "SAMPLE_YEAR", "AGENCY_CODE"), c("SAMPLE_NUMBER", 
         "CLUSTER_SEQUENCE_NUMBER", "SAMPLE_YEAR", "AGENCY_CODE"), "all_cluster_sum_kg")

    # Duplicate all the records with frequency > 1 from Oregon
    bds_fish <- bds_fish[rep(1:nrow(bds_fish), bds_fish$OBSERVED_FREQUENCY),]
 	 
    # Add Dahl sector info
    if(is.null(PacFIN.Catch.Dahl)) {
	    
	  bds_fish$DAHL_GROUNDFISH_CODE <- NA
    
    } else {
	
        if(verbose) {
	       catf("\nAdding Dahl sector information from the catch data provided\n\n")
           printf(Table(PacFIN.Catch.Dahl$DAHL_GROUNDFISH_CODE , PacFIN.Catch.Dahl$ARID)); catf("\n\n")
        }
        
        bds_fish <- renum(match.f(bds_fish, PacFIN.Catch.Dahl, "FTID", "FTID", "DAHL_GROUNDFISH_CODE"))
        
        if(verbose)
           printf(bds_fish[1:4,])
                
        # Percent matching
        notMissing <- Table(!(is.na(bds_fish$DAHL_GROUNDFISH_CODE ) | bds_fish$DAHL_GROUNDFISH_CODE  %in% 'XX'))
		
        if(verbose) {
           catf("\nCount of the number of FTID's with Dahl sector info:\n")
           printf(notMissing)
           catf("\nPercent of FTID with Dahl Sector info:", 100 * notMissing[2]/sum(notMissing), "\n\n")
           catf("\nCount of Dahl groundfish codes by year and agency:\n\n")
           printf(Table(bds_fish$DAHL_GROUNDFISH_CODE, bds_fish$SAMPLE_YEAR, bds_fish$AGENCY_CODE))
        }
    }
    
    # Add columns with legacy names
    if(addColsWithLegacyNames) {
    
        if(verbose)
             catf("\nAdding extra columns with legacy names\n\n")
    
        '  # %ino% preserves the order when using matching operators unlike %in%. See my entry on Stack Overflow: '
        '  #  https://stackoverflow.com/questions/10586652/r-preserve-order-when-using-matching-operators-in  '
        '  # RWT_LBS was historically converted to CATCH.LBS in the SQL code, so here ROUND_WEIGHT_LBS is converted to CATCH.LBS  ' 
    
        for(i in (1:nrow(nameConvertVdrfdToCompBDS))[nameConvertVdrfdToCompBDS$Comp_BDS %ino% names(bds_fish)])
 
           bds_fish[nameConvertVdrfdToCompBDS[i, 2]] <- bds_fish[nameConvertVdrfdToCompBDS[i, 1]]
    }
    
    # Cleanup
    if(default.stringsAsFactors()) 
          warning("Default strings as factors is set to TRUE.")
    
    bds_fish <- data.frame(lapply(bds_fish, function(x) if(is.character(x)) {x[is.na(x)] <- ""; x}  else x))
    
    bds_fish
}





