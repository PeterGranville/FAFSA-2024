
#### Set up #### 

library(scales)
library(tidyverse)

#### End #### 

#### Write function to load FRPL data ####

loadFRPL <- function(filename, year0, selectedState){
  
  resultsDF <- data.frame(
    `NCESSCH` = numeric(), 
    `SCH_NAME` = character(),
    `STATENAME` = character(), 
    `Year` = numeric(),
    `FRPL source` = character(), 
    `FRPL students` = numeric(), 
    check.names = FALSE
  )
  
  tempDF <- read.csv(filename, header=TRUE) %>% select(
    `NCESSCH`, 
    `SCH_NAME`, 
    `STATENAME`,
    `DATA_GROUP`, 
    `LUNCH_PROGRAM`, 
    `STUDENT_COUNT`
  )
  
  if(selectedState != "None"){
    tempDF <- tempDF %>% filter(
      `STATENAME` == selectedState
    )
  }
  
  for(i in (1:length(unique(tempDF$`NCESSCH`)))){
    
    if(i %% 1000 == 0){
      print(paste("Running school number ", comma(i), " out of ", comma(length(unique(tempDF$`NCESSCH`))), " for the year ", year0, ".", sep=""))
    }
    
    selectedSchool <- tempDF %>% filter(
      `NCESSCH` == unique(tempDF$`NCESSCH`)[i]
    )
    
    selectedSchool.FRPL <- tempDF %>% filter(
      `NCESSCH` == unique(tempDF$`NCESSCH`)[i], 
      `DATA_GROUP` == "Free and Reduced-price Lunch Table", 
      `LUNCH_PROGRAM` %in% c("Free lunch qualified", "Reduced-price lunch qualified"),
      is.na(`STUDENT_COUNT`)==FALSE
    ) 
    selectedSchool.DC <- tempDF %>% filter(
      `NCESSCH` == unique(tempDF$`NCESSCH`)[i], 
      `DATA_GROUP` == "Direct Certification", 
      is.na(`STUDENT_COUNT`)==FALSE
    ) 
    
    # If FRPL is available, use FRPL. 
    # If FRPL is not available, use Direct Certification. 
    if(nrow(selectedSchool.FRPL) > 0){
      selectedSchool.FRPL <- selectedSchool.FRPL %>% filter(
        `LUNCH_PROGRAM` %in% c("Free lunch qualified", "Reduced-price lunch qualified")
      )
      frpl.students <- sum(selectedSchool.FRPL$`STUDENT_COUNT`)
      frpl.source <- "FRPL count"
    }else{
      if(nrow(selectedSchool.DC) > 0){
        frpl.students <- sum(selectedSchool.DC$`STUDENT_COUNT`)
        frpl.source <- "Direct Certification count"
      }else{
        frpl.students <- NA
        frpl.source <- "None found"
      }
    }
    
    resultsDF <- resultsDF %>% add_row(
      `NCESSCH` = selectedSchool$`NCESSCH`[1], 
      `SCH_NAME` = selectedSchool$`SCH_NAME`[1],
      `STATENAME` = selectedSchool$`STATENAME`[1], 
      `Year` = year0,
      `FRPL source` = frpl.source, 
      `FRPL students` = frpl.students
    )
    
    rm(selectedSchool, selectedSchool.FRPL, selectedSchool.DC, frpl.students, frpl.source)
    
  }
  rm(i)
  
  return(resultsDF)
  
}

#### End #### 

#### Load FRPL data ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

frpl17 <- loadFRPL("ccd_sch_033_1617.csv", 2017, "None") 
write.csv(frpl17, "frpl17.csv", row.names=FALSE)

frpl18 <- loadFRPL("ccd_sch_033_1718.csv", 2018, "None")
write.csv(frpl18, "frpl18.csv", row.names=FALSE)

frpl19 <- loadFRPL("ccd_sch_033_1819.csv", 2019, "None")
write.csv(frpl19, "frpl19.csv", row.names=FALSE)

frpl20 <- loadFRPL("ccd_sch_033_1920.csv", 2020, "None") 
write.csv(frpl20, "frpl20.csv", row.names=FALSE)

frpl21 <- loadFRPL("ccd_sch_033_2021.csv", 2021, "None") 
write.csv(frpl21, "frpl21.csv", row.names=FALSE)

frpl22 <- loadFRPL("ccd_sch_033_2122.csv", 2022, "None") 
write.csv(frpl22, "frpl22.csv", row.names=FALSE)

frpl23 <- loadFRPL("ccd_sch_033_2223.csv", 2023, "None")
write.csv(frpl23, "frpl23.csv", row.names=FALSE)

frpl24 <- loadFRPL("ccd_sch_033_2324.csv", 2024, "None")
write.csv(frpl24, "frpl24.csv", row.names=FALSE)

#### End #### 


