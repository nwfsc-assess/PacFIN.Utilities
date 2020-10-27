# Only column names that need to changed are listed. Some names, such as FTID, remain the same in Comprehensive_BDS_Comm.

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

nameConvertVdrfdToCompBDS <- scanIn("

              Comp_BDS                    vdrfd
          SAMPLE_NUMBER                 SAMPLE_NO                  
          SAMPLE_METHOD_CODE            SAMPLE_METHOD              
          AGENCY_CODE                   SOURCE_AGID                
          AGENCY_CONDITION_CODE         COND_AGCODE                
          PACFIN_CONDITION_CODE         COND                       
          PACFIN_PORT_CODE              PCID                       
          PACFIN_PORT_NAME              PORT                       
          PACFIN_GEAR_CODE              GRID                       
          PACFIN_GEAR_NAME              GEAR                       
          VESSEL_ID                     DRVID                      
          PSMFC_CATCH_AREA_CODE         PSMFC_ARID                 
          DEPTH_AVERAGE_FATHOMS         DEPTH_AVG                  
          DEPTH_MAXIMUM_FATHOMS         DEPTH_MIN                  
          DEPTH_MINIMUM_FATHOMS         DEPTH_MAX                  
          CLUSTER_SEQUENCE_NUMBER       CLUSTER_NO                 
          CLUSTER_WEIGHT_LBS            CLUSTER_WGT                
          ADJUSTED_CLUSTER_WEIGHT_LBS   ADJ_CLWT                   
          FRAME_CLUSTER_WEIGHT_LBS      FRAME_CLWT                 
          PACFIN_SPECIES_CODE           SPID                       
          FISH_SEQUENCE_NUMBER          FISH_NO                    
          OBSERVED_FREQUENCY            FREQ                       
          FISH_LENGTH_TYPE_CODE         FISH_LENGTH_TYPE           
          FISH_LENGTH                   FISH_LENGTH                
          FORK_LENGTH                   FORK_LENGTH                
          FORK_LENGTH_IS_ESTIMATED      FORK_LENGTH_ESTIMATED      
          SEX_CODE                      SEX                        
          AGENCY_FISH_MATURITY_CODE     MATURITY_AGCODE            
          FISH_MATURITY_CODE            MATURITY                   
          WEIGHT_OF_MALES_LBS           MALES_WGT                  
          WEIGHT_OF_FEMALES_LBS         FEMALES_WGT                
          NUMBER_OF_MALES               MALES_NUM                  
          NUMBER_OF_FEMALES             FEMALES_NUM                
          WEIGHT_OF_LANDING_LBS         TOTAL_WGT                  
          EXPANDED_SAMPLE_WEIGHT        EXP_WT                     
          SPECIES_WEIGHT_LBS            SPECIES_WGT                
          FINAL_FISH_AGE_CODE           FISH_AGE_CODE_FINAL        
          FINAL_FISH_AGE_IN_YEARS       FISH_AGE_YEARS_FINAL       
          AGE_SEQUENCE_NUMBER           AGENUM                     
          AGE_METHOD_CODE               AGE_METHOD                 
          PERSON_WHO_AGED               AGED_BY                    
          DATE_AGE_RECORDED             DATE_AGED                  
          AGE_IN_YEARS                  AGE_YEARS                  
          AGENCY_AGE_STRUCTURE_CODE     AGE_STRUCT_AGCODE          
          AGENCY_GRADE_CODE             GRADE_AGCODE               
          PACFIN_GRADE_CODE             GRADE   
                   
")



