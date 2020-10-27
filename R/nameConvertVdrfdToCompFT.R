# Same column name (so no need to convert): FTID 
# 'RWT_LBS' historically converted to 'CATCH.LBS' in the SQL code, so here 'ROUND_WEIGHT_LBS' is converted to 'CATCH.LBS'
# It appears that 'PRODUCT_FROM' in vdrfd was meant to be 'PRODUCT_FORM'
# vdrfd PRMTLST (One or more NWR/LE permits under which the vessel fished) doesn't appear to have a corresponding column in the Comprehensive_FT table
# 'COUNCIL' in the vdrfd SQL code is from the 'ar' table
# 'GRGROUP' was matched from the gr (gear) table and now is renamed from 'PACFIN_GROUP_GEAR_CODE'

# -------- Import utility Functions --------

sourceFunctionURL <- function(URL) {
       ' # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() '
       require(RCurl)
       File.ASCII <- tempfile()
       on.exit(file.remove(File.ASCII))
       writeLines(paste(readLines(textConnection(RCurl::getURL(URL))), collapse = "\n"), File.ASCII)
       source(File.ASCII, local = parent.env(environment()))
}

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/scanIn.R")

nameConvertVdrfdToCompFT <- scanIn("

                      Comp_FT                                vdrfd
                   COUNCIL_CODE                             COUNCIL
                   AGENCY_CODE                              AGID
                   LANDING_YEAR                             YEAR
                   LANDING_MONTH                            PERIOD
                   LANDING_DATE                             TDATE
                   PACFIN_SPECIES_CODE                      SPID
                   PARTICIPATION_GROUP_CODE                 PARGRP
                   PACFIN_PORT_CODE                         PCID
                   PACFIN_CATCH_AREA_CODE                   ARID
                   INPFC_AREA_TYPE_CODE                     INPFC_ARID
                   PORT_CODE                                PORT
                   FLEET_CODE                               FLEET
                   VESSEL_NUM                               DRVID
                   PACFIN_GEAR_CODE                         GRID
                   PACFIN_GROUP_GEAR_CODE                   GRGROUP
                   IS_IFQ_LANDING                           IFQ_LANDING
                   REMOVAL_TYPE_CODE                        REMOVAL_TYPE
                   CONDITION_CODE                           COND
                   DISPOSITION_CODE                         DISP
                   EXVESSEL_REVENUE                         REV
                   GRADE_CODE                               GRADE                 
                   ROUND_WEIGHT_LBS                         CATCH.LBS
                   LANDED_WEIGHT_LBS                        LWT_LBS
                   ADJUSTED_GEAR_CODE                       ADJ_GRID
                   DAHL_GROUNDFISH_CODE                     DAHL_SECTOR
                   IS_REMOVAL_LEGAL                         LEGAL_REMOVAL
                   IS_OVERAGE                               OVERAGE
                   PRODUCT_FORM_CODE                        PRODUCT_FROM
                   PRODUCT_USE_CODE                         PRODUCT_USE
                   ORIG_PACFIN_CATCH_AREA_CODE              FTL_ARID
                   ORIG_PACFIN_SPECIES_CODE                 FTL_SPID
                   DEALER_NUM                               PROC
                  
")
