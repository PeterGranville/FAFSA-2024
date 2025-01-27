
#### Setup ####

library(tidyverse)

#### End #### 

#### Create state lookup ####

stateLookup <- data.frame(
  `ST` = numeric(), 
  `State` = character()
)

stateLookup <- stateLookup %>% add_row(`ST`=1, `State` = "AL")
stateLookup <- stateLookup %>% add_row(`ST`=2, `State` = "AK")
stateLookup <- stateLookup %>% add_row(`ST`=4, `State` = "AZ")
stateLookup <- stateLookup %>% add_row(`ST`=5, `State` = "AR")
stateLookup <- stateLookup %>% add_row(`ST`=6, `State` = "CA")
stateLookup <- stateLookup %>% add_row(`ST`=8, `State` = "CO")
stateLookup <- stateLookup %>% add_row(`ST`=9, `State` = "CT")
stateLookup <- stateLookup %>% add_row(`ST`=10, `State` = "DE")
stateLookup <- stateLookup %>% add_row(`ST`=11, `State` = "DC")
stateLookup <- stateLookup %>% add_row(`ST`=12, `State` = "FL")
stateLookup <- stateLookup %>% add_row(`ST`=13, `State` = "GA")
stateLookup <- stateLookup %>% add_row(`ST`=15, `State` = "HI")
stateLookup <- stateLookup %>% add_row(`ST`=16, `State` = "ID")
stateLookup <- stateLookup %>% add_row(`ST`=17, `State` = "IL")
stateLookup <- stateLookup %>% add_row(`ST`=18, `State` = "IN")
stateLookup <- stateLookup %>% add_row(`ST`=19, `State` = "IA")
stateLookup <- stateLookup %>% add_row(`ST`=20, `State` = "KS")
stateLookup <- stateLookup %>% add_row(`ST`=21, `State` = "KY")
stateLookup <- stateLookup %>% add_row(`ST`=22, `State` = "LA")
stateLookup <- stateLookup %>% add_row(`ST`=23, `State` = "ME")
stateLookup <- stateLookup %>% add_row(`ST`=24, `State` = "MD")
stateLookup <- stateLookup %>% add_row(`ST`=25, `State` = "MA")
stateLookup <- stateLookup %>% add_row(`ST`=26, `State` = "MI")
stateLookup <- stateLookup %>% add_row(`ST`=27, `State` = "MN")
stateLookup <- stateLookup %>% add_row(`ST`=28, `State` = "MS")
stateLookup <- stateLookup %>% add_row(`ST`=29, `State` = "MO")
stateLookup <- stateLookup %>% add_row(`ST`=30, `State` = "MT")
stateLookup <- stateLookup %>% add_row(`ST`=31, `State` = "NE")
stateLookup <- stateLookup %>% add_row(`ST`=32, `State` = "NV")
stateLookup <- stateLookup %>% add_row(`ST`=33, `State` = "NH")
stateLookup <- stateLookup %>% add_row(`ST`=34, `State` = "NJ")
stateLookup <- stateLookup %>% add_row(`ST`=35, `State` = "NM")
stateLookup <- stateLookup %>% add_row(`ST`=36, `State` = "NY")
stateLookup <- stateLookup %>% add_row(`ST`=37, `State` = "NC")
stateLookup <- stateLookup %>% add_row(`ST`=38, `State` = "ND")
stateLookup <- stateLookup %>% add_row(`ST`=39, `State` = "OH")
stateLookup <- stateLookup %>% add_row(`ST`=40, `State` = "OK")
stateLookup <- stateLookup %>% add_row(`ST`=41, `State` = "OR")
stateLookup <- stateLookup %>% add_row(`ST`=42, `State` = "PA")
stateLookup <- stateLookup %>% add_row(`ST`=44, `State` = "RI")
stateLookup <- stateLookup %>% add_row(`ST`=45, `State` = "SC")
stateLookup <- stateLookup %>% add_row(`ST`=46, `State` = "SD")
stateLookup <- stateLookup %>% add_row(`ST`=47, `State` = "TN")
stateLookup <- stateLookup %>% add_row(`ST`=48, `State` = "TX")
stateLookup <- stateLookup %>% add_row(`ST`=49, `State` = "UT")
stateLookup <- stateLookup %>% add_row(`ST`=50, `State` = "VT")
stateLookup <- stateLookup %>% add_row(`ST`=51, `State` = "VA")
stateLookup <- stateLookup %>% add_row(`ST`=53, `State` = "WA")
stateLookup <- stateLookup %>% add_row(`ST`=54, `State` = "WV")
stateLookup <- stateLookup %>% add_row(`ST`=55, `State` = "WI")
stateLookup <- stateLookup %>% add_row(`ST`=56, `State` = "WY")
stateLookup <- stateLookup %>% add_row(`ST`=72, `State` = "PR")

#### End #### 

#### Load ACS data ####

setwd("/Users/peter_granville/Various-TCF-Projects")

loadPUMS <- function(filename){
  
  tempDF <- read.csv(filename, header=TRUE) 
  
  tempDF <- tempDF %>% rename(
    `ST` = `STATE`
  ) %>% select(
    `PWGTP`, 
    `SERIALNO`,
    `ST`, 
    `CIT`, 
    `MAR`,
    `AGEP`, 
    `SCHG`, 
    `SFN`,
    `SFR`
  ) %>% filter(
    grepl("GQ", `SERIALNO`)==FALSE
  ) %>% mutate(
    `SFN` = ifelse(is.na(`SFN`), 0, `SFN`)
  ) %>% mutate(
    `SERIALNO` = as.character(`SERIALNO`), 
    `SFN` = as.character(`SFN`)
  ) %>% mutate(
    `SNO-SFN` = paste(`SERIALNO`, `SFN`, sep="-")
  ) %>% mutate(
    `Noncitizen adult` = ifelse(
      (`AGEP` >= 32) & (`CIT`==5), 
      "Noncitizen adult", 
      "Not a noncitizen adult"
    ), 
    `Citizen aged 17 to 23` = ifelse(
      (`AGEP` %in% c(17:23)) & (`CIT` < 5) & (`MAR`==5) & (`SCHG` != 16), 
      "Citizen aged 17 to 23", 
      "Not a citizen aged 17 to 23"
    ), 
    `Citizen grade 12 student` = ifelse(
      (`SCHG`==14) & (`CIT` < 5) & (`MAR`==5), 
      "Citizen grade 12 student", 
      "Not a citizen grade 12 student"
    )
  )
  
  return(tempDF)
  
}

pums <- rbind(
  loadPUMS("psam2023_pusa.csv"), 
  loadPUMS("psam2023_pusb.csv")
)

pums <- left_join(x=pums, y=stateLookup, by="ST")

potentialFAFSA <- pums %>% filter(
  (`SCHG`==14) | (`AGEP` %in% (17:23))
)
eligibleSubfamilies <- unique(potentialFAFSA$`SNO-SFN`)

#### End #### 

#### Identify subfamilies that have a noncitizen adult and a FAFSA-aged child ####

mixedStatusFamily.17to23 <- character()
mixedStatusFamily.G12 <- character() 

for(i in (1:length(eligibleSubfamilies))){

  if(i %% 100==0){
    print(paste("Running subfamily ", comma(i), " of ", comma(length(eligibleSubfamilies)), ".", sep=""))
  }
  
  tempDF <- pums %>% filter(
    `SNO-SFN` == eligibleSubfamilies[i]
  ) 
  
  if(("Noncitizen adult" %in% tempDF$`Noncitizen adult`) & ("Citizen aged 17 to 23" %in% tempDF$`Citizen aged 17 to 23`)){
    mixedStatusFamily.17to23 <- c(mixedStatusFamily.17to23, tempDF$`SNO-SFN`[1])
  }
  if(("Noncitizen adult" %in% tempDF$`Noncitizen adult`) & ("Citizen grade 12 student" %in% tempDF$`Citizen grade 12 student`)){
    mixedStatusFamily.G12 <- c(mixedStatusFamily.G12, tempDF$`SNO-SFN`[1])
  }
  
  rm(tempDF)
}
rm(i)

#### End #### 

#### Aggregate totals ####

agg1723 <- pums %>% filter(
  `SNO-SFN` %in% mixedStatusFamily.17to23, 
  `Citizen aged 17 to 23` == "Citizen aged 17 to 23"
) 
agg1723 <- aggregate(data=agg1723, `PWGTP` ~ `State`, FUN=sum)

aggG12 <- pums %>% filter(
  `SNO-SFN` %in% mixedStatusFamily.G12, 
  `Citizen grade 12 student` == "Citizen grade 12 student"
)
aggG12 <- aggregate(data=aggG12, `PWGTP` ~ `State`, FUN=sum)

#### End #### 

#### Write files ####

setwd("/Users/peter_granville/FAFSA-2024")

write.csv(agg1723, "ACS-MS agg1723.csv", row.names=FALSE)
write.csv(aggG12, "ACS-MS aggG12.csv", row.names=FALSE)

#### End #### 



