
#### Setup ####

library(dplyr)
library(scales)
library(readxl)
library(plotly)
library(stringr)
library(viridis)
library(zipcodeR)
library(geosphere)
library(tidyverse)
library(data.table)
library(stringdist)

quintileDF <- data.frame(`InterestVar` = (1:5), `Tile` = c(
  "1st", 
  "2nd", 
  "3rd", 
  "4th",
  "5th"
))

decileDF <- data.frame(`InterestVar` = (1:10), `Tile` = c(
  "1st", 
  "2nd", 
  "3rd", 
  "4th",
  "5th", 
  "6th", 
  "7th", 
  "8th", 
  "9th", 
  "10th"
))

#### End #### 

#### Set up ZIPs data set ####

setwd("/Users/peter_granville/FAFSA-2024/ZIP data")
zips <- read.csv("uszips.csv") %>% select(`zip`, `city`, `state_id`)
zips <- zips %>% mutate(`zip` = as.character(`zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==1, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==2, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==3, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==4, paste("0", `zip`, sep=""), `zip`))

#### End #### 

#### Set up private school data set ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

privateSchools20 <- read.csv("ELSI Private Schools 19-20.csv", header=TRUE, skip=6, nrow=21100, check.names=FALSE)
privateSchools18 <- read.csv("ELSI Private Schools 17-18.csv", header=TRUE, skip=6, nrow=22434, check.names=FALSE)
privateSchools16 <- read.csv("ELSI Private Schools 15-16.csv", header=TRUE, skip=6, nrow=21895, check.names=FALSE)
privateSchools14 <- read.csv("ELSI Private Schools 13-14.csv", header=TRUE, skip=6, nrow=24085, check.names=FALSE)
privateSchools12 <- read.csv("ELSI Private Schools 11-12.csv", header=TRUE, skip=6, nrow=26519, check.names=FALSE)
privateSchools10 <- read.csv("ELSI Private Schools 09-10.csv", header=TRUE, skip=6, nrow=27760, check.names=FALSE)
privateSchools08 <- read.csv("ELSI Private Schools 07-08.csv", header=TRUE, skip=6, nrow=28425, check.names=FALSE)

privateSchools20 <- privateSchools20 %>% rename(
  `Name 2019-20` = `Private School Name`, 
  `State 2019-20` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2019-20` = `City [Private School] 2019-20`, 
  `ZIP 2019-20` = `ZIP [Private School] 2019-20`, 
)
privateSchools18 <- privateSchools18 %>% rename(
  `Name 2017-18` = `Private School Name`, 
  `State 2017-18` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2017-18` = `City [Private School] 2017-18`, 
  `ZIP 2017-18` = `ZIP [Private School] 2017-18`, 
)
privateSchools16 <- privateSchools16 %>% rename(
  `Name 2015-16` = `Private School Name`, 
  `State 2015-16` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2015-16` = `City [Private School] 2015-16`, 
  `ZIP 2015-16` = `ZIP [Private School] 2015-16`, 
)
privateSchools14 <- privateSchools14 %>% rename(
  `Name 2013-14` = `Private School Name`, 
  `State 2013-14` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2013-14` = `City [Private School] 2013-14`, 
  `ZIP 2013-14` = `ZIP [Private School] 2013-14`, 
)
privateSchools12 <- privateSchools12 %>% rename(
  `Name 2011-12` = `Private School Name`, 
  `State 2011-12` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2011-12` = `City [Private School] 2011-12`, 
  `ZIP 2011-12` = `ZIP [Private School] 2011-12`, 
)
privateSchools10 <- privateSchools10 %>% rename(
  `Name 2009-10` = `Private School Name`, 
  `State 2009-10` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2009-10` = `City [Private School] 2009-10`, 
  `ZIP 2009-10` = `ZIP [Private School] 2009-10`, 
)
privateSchools08 <- privateSchools08 %>% rename(
  `Name 2007-08` = `Private School Name`, 
  `State 2007-08` = `State Name [Private School] Latest available year`, 
  `School ID` = `School ID - NCES Assigned [Private School] Latest available year`, 
  `City 2007-08` = `City [Private School] 2007-08`, 
  `ZIP 2007-08` = `ZIP [Private School] 2007-08`, 
)

privateSchools <- full_join(x=privateSchools20, y=privateSchools18, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools16, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools14, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools12, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools10, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools08, by="School ID", relationship = "many-to-many")

rm(privateSchools20, privateSchools18, privateSchools16, privateSchools14, privateSchools12, privateSchools10, privateSchools08)

#### End #### 

#### Process FAFSA data #### 

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")

fafsa24 <- read_excel("HS_ARCHIVE07312024.xls", skip=3)
fafsa23 <- read_excel("HS_ARCHIVE07312023.xls", skip=3)
fafsa22 <- read_excel("HS_ARCHIVE07312022.xls", skip=3)
fafsa21 <- read_excel("HS_ARCHIVE07312021.xls", skip=3)
fafsa20 <- read_excel("HS_ARCHIVE07312020.xls", skip=3)
fafsa19 <- read_excel("HS_ARCHIVE07312019.xls", skip=3)
fafsa18 <- read_excel("HS_ARCHIVE07312018.xls", skip=3)
fafsa17 <- read_excel("HS_ARCHIVE07312017.xls", skip=3)

processFafsa <- function(fafsa){
  
  if(names(fafsa)[1]=="School Code"){
    fafsa <- fafsa[, (1:6)]
    names(fafsa)[5] <- "Submissions"
    names(fafsa)[6] <- "Completions"
  }else{
    fafsa <- fafsa[, (1:5)]
    names(fafsa)[4] <- "Submissions"
    names(fafsa)[5] <- "Completions"
  }
  
  fafsa$`Submissions`[fafsa$`Submissions`=="<5"] <- "0"
  fafsa$`Submissions`[fafsa$`Completions`=="<5"] <- "0"
  
  suppressWarnings({
    fafsa <- fafsa %>% mutate(
      `Submissions` = as.numeric(`Submissions`), 
      `Completions` = as.numeric(`Completions`))
  })

  fafsa <- fafsa %>% mutate(
    `Name` = toupper(`Name`), 
    `State` = toupper(`State`) 
  )
  
  if(names(fafsa)[1]=="School Code"){
    fafsa <- fafsa %>% mutate(`Count` = rep(1))
    codeCount <- aggregate(data=fafsa, `Count` ~ `School Code`, FUN=sum) %>% filter(`Count` > 1)
    fafsa <- fafsa %>% select(-(`Count`))
    problemCodes <- codeCount$`School Code`
    rm(codeCount)
    for(i in (1:length(problemCodes))){
      tempData <- fafsa %>% filter(`School Code` == problemCodes[i])
      fafsa <- fafsa %>% filter(`School Code` != problemCodes[i])
      fafsa <- fafsa %>% add_row(
        `School Code` = tempData$`School Code`[1], 
        `Name` = tempData$`Name`[1], 
        `City` = tempData$`City`[1], 
        `State` = tempData$`State`[1], 
        `Submissions` = sum(tempData$`Submissions`, na.rm=TRUE), 
        `Completions` = sum(tempData$`Completions`, na.rm=TRUE)
      )
      rm(tempData)
    }
    rm(problemCodes, i)
  }
  
  fafsa <- fafsa %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)
  
  fafsa <- fafsa %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)
  
  fafsa <- fafsa %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)
  
  return(fafsa)
  
}

fafsa24 <- processFafsa(fafsa24)
fafsa23 <- processFafsa(fafsa23)
fafsa22 <- processFafsa(fafsa22)
fafsa21 <- processFafsa(fafsa21)
fafsa20 <- processFafsa(fafsa20)
fafsa19 <- processFafsa(fafsa19)
fafsa18 <- processFafsa(fafsa18)
fafsa17 <- processFafsa(fafsa17)

#### End #### 

#### Process ELSI data ####

set.seed(111)

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

elsi24 <- read.csv("ccd_sch_029_2324.csv")
elsi23 <- read.csv("ccd_sch_029_2223.csv")
elsi22 <- read.csv("ccd_sch_029_2122.csv")
elsi21 <- read.csv("ccd_sch_029_2021.csv")
elsi20 <- read.csv("ccd_sch_029_1920.csv")
elsi19 <- read.csv("ccd_sch_029_1819.csv")
elsi18 <- read.csv("ccd_sch_029_1718.csv")
elsi17 <- read.csv("ccd_sch_029_1617.csv")

processElsi <- function(elsi){
  
  elsi <- elsi %>% select(`NCESSCH`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`)
  
  elsi <- elsi %>% filter(`G_12_OFFERED` != "No")
  elsi <- elsi %>% mutate(`NCESSCH` = as.character(`NCESSCH`))
  
  elsi <- elsi %>% mutate(`MZIP` = as.character(`MZIP`))
  elsi <- elsi %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==1, paste("0", `MZIP`, sep=""), `MZIP`))
  elsi <- elsi %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==2, paste("0", `MZIP`, sep=""), `MZIP`))
  elsi <- elsi %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==3, paste("0", `MZIP`, sep=""), `MZIP`))
  elsi <- elsi %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==4, paste("0", `MZIP`, sep=""), `MZIP`))
  elsi <- elsi %>% mutate(`LZIP` = as.character(`LZIP`))
  elsi <- elsi %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==1, paste("0", `LZIP`, sep=""), `LZIP`))
  elsi <- elsi %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==2, paste("0", `LZIP`, sep=""), `LZIP`))
  elsi <- elsi %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==3, paste("0", `LZIP`, sep=""), `LZIP`))
  elsi <- elsi %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==4, paste("0", `LZIP`, sep=""), `LZIP`))
  
  elsi <- elsi %>% mutate(`ComboName` = paste(`SCH_NAME`, `MCITY`, `ST`, `MZIP`, sep="-")) %>% mutate(`Count` = rep(1))
  comboCount <- aggregate(data=elsi, `Count` ~ `ComboName`, FUN=sum) %>% filter(`Count` > 1)

  for(i in (1:length(unique(comboCount$`ComboName`)))){
    
    selectedSchool <- elsi %>% filter(`ComboName` == unique(comboCount$`ComboName`)[i])
    elsi <- elsi %>% filter(`ComboName` != unique(comboCount$`ComboName`)[i])
    selectedSchool <- selectedSchool %>% mutate(`FAFSA Match` = ifelse(`NCESSCH` %in% fafsa24$`School Code`, 1, 0))
    if(sum(selectedSchool$`FAFSA Match`) > 0){
      selectedSchool <- selectedSchool %>% filter(`FAFSA Match`==1) %>% select(-(`FAFSA Match`))
      elsi <- rbind(elsi, selectedSchool)
    }else{
      selectedSchool <- selectedSchool %>% select(-(`FAFSA Match`))
      selectedSchool <- sample_n(selectedSchool, 1)
      elsi <- rbind(elsi, selectedSchool)
    }
    rm(selectedSchool)
  }
  rm(i, comboCount)
  elsi <- elsi %>% select(-(`Count`)) %>% select(-(`ComboName`))
  
  elsi <- elsi %>% mutate(
    `SCH_NAME` = str_replace_all(`SCH_NAME`, "[^a-zA-Z0-9 [:punct:]]", ""), 
    `MCITY` = str_replace_all(`MCITY`, "[^a-zA-Z0-9 [:punct:]]", ""), 
    `LCITY` = str_replace_all(`LCITY`, "[^a-zA-Z0-9 [:punct:]]", "")
  )
  
  elsi <- elsi %>% mutate(
    `SCH_NAME` = toupper(`SCH_NAME`), 
    `MCITY` = toupper(`MCITY`), 
    `LCITY` = toupper(`LCITY`)
  )
  
  return(elsi)
  
}

elsi24 <- processElsi(elsi24)
elsi23 <- processElsi(elsi23)
elsi22 <- processElsi(elsi22)
elsi21 <- processElsi(elsi21)
elsi20 <- processElsi(elsi20)
elsi19 <- processElsi(elsi19)
elsi18 <- processElsi(elsi18)
elsi17 <- processElsi(elsi17)

#### End #### 

#### Import school codes where the match is perfect #### 

fafsa24.copy <- fafsa24 %>% select(`School Code`, `Name`, `City`, `State`)

fafsa22 <- left_join(x=fafsa22, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)
fafsa21 <- left_join(x=fafsa21, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)
fafsa20 <- left_join(x=fafsa20, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)
fafsa19 <- left_join(x=fafsa19, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)
fafsa18 <- left_join(x=fafsa18, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)
fafsa17 <- left_join(x=fafsa17, y=fafsa24.copy, by=c("Name", "City", "State")) %>% select(`School Code`, `Name`, `City`, `State`, `Submissions`, `Completions`)

rm(fafsa24.copy)

#### End #### 

#### Remove schools known to be private schools #### 

privateSchoolRemove <- function(fafsa){
  fafsa <- fafsa %>% filter((`School Code` %in% privateSchools$`School ID`)==FALSE)
  fafsa <- fafsa %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)
}

fafsa24 <- privateSchoolRemove(fafsa24)
fafsa23 <- privateSchoolRemove(fafsa23)
fafsa22 <- privateSchoolRemove(fafsa22)
fafsa21 <- privateSchoolRemove(fafsa21)
fafsa20 <- privateSchoolRemove(fafsa20)
fafsa19 <- privateSchoolRemove(fafsa19)
fafsa18 <- privateSchoolRemove(fafsa18)
fafsa17 <- privateSchoolRemove(fafsa17)

rm(privateSchools)

#### End #### 

#### Remove rows with no FAFSAs ####

fafsa24$`Submissions`[is.na(fafsa24$`Submissions`)] <- 0
fafsa24$`Completions`[is.na(fafsa24$`Completions`)] <- 0

fafsa23$`Submissions`[is.na(fafsa23$`Submissions`)] <- 0
fafsa23$`Completions`[is.na(fafsa23$`Completions`)] <- 0

fafsa22$`Submissions`[is.na(fafsa22$`Submissions`)] <- 0
fafsa22$`Completions`[is.na(fafsa22$`Completions`)] <- 0

fafsa21$`Submissions`[is.na(fafsa21$`Submissions`)] <- 0
fafsa21$`Completions`[is.na(fafsa21$`Completions`)] <- 0

fafsa20$`Submissions`[is.na(fafsa20$`Submissions`)] <- 0
fafsa20$`Completions`[is.na(fafsa20$`Completions`)] <- 0

fafsa19$`Submissions`[is.na(fafsa19$`Submissions`)] <- 0
fafsa19$`Completions`[is.na(fafsa19$`Completions`)] <- 0

fafsa18$`Submissions`[is.na(fafsa18$`Submissions`)] <- 0
fafsa18$`Completions`[is.na(fafsa18$`Completions`)] <- 0

fafsa17$`Submissions`[is.na(fafsa17$`Submissions`)] <- 0
fafsa17$`Completions`[is.na(fafsa17$`Completions`)] <- 0

fafsa24 <- fafsa24 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa23 <- fafsa23 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa22 <- fafsa22 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa21 <- fafsa21 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa20 <- fafsa20 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa19 <- fafsa19 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa18 <- fafsa18 %>% filter((`Submissions` > 0) | (`Completions` > 0))
fafsa17 <- fafsa17 %>% filter((`Submissions` > 0) | (`Completions` > 0))

#### End #### 

#### Write merge function ####

mergeFunction <- function(com){
  
  fafsaData <- com[[1]]
  elsiData <- com[[2]]
  
  # Round 1 
  fafsaData <- fafsaData %>% rename(`NCESSCH` = `School Code`) %>% mutate(`Index` = (1:nrow(fafsaData))) 
  goodMerge <- inner_join(x=fafsaData, y=elsiData, by="NCESSCH") %>% mutate(`Merge Round` = rep("Round 1"))
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH` %in% goodMerge$NCESSCH)==FALSE) %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
  goodMerge <- goodMerge %>% mutate(`NCESSCH-ELSI` = `NCESSCH`)
  elsiData <- elsiData %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
  
  # Round 2 
  newFafsa <- remainingFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
  newElsi <- remainingElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))
  newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 2"))
  goodMerge <- rbind(goodMerge, newMerge)
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
  rm(newFafsa, newElsi, newMerge)
  
  # Round 3 
  newFafsa <- remainingFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
  newElsi <- remainingElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `LCITY`, `ST`, sep="-"))
  newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 3"))
  goodMerge <- rbind(goodMerge, newMerge)
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
  rm(newFafsa, newElsi, newMerge)
  
  # Round 4 
  newFafsa <- remainingFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-")) %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))
  newElsi <- remainingElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-")) %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))
  newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 4"))
  goodMerge <- rbind(goodMerge, newMerge)
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
  rm(newFafsa, newElsi, newMerge)
  
  # Round 5 
  newFafsa <- remainingFafsa 
  newElsi <- remainingElsi 
  middleFafsa <- data.frame(
    `NCESSCH` = character(),
    `Name` = character(), 
    `City` = character(),
    `State` = character(),
    `Applications submitted through May 24, 2024` = numeric(), 
    `Applications completed through May 24, 2024` = numeric(), 
    `Applications submitted through May 24, 2023` = numeric(), 
    `Applications completed through May 24, 2023` = numeric(), 
    `Index` = numeric(),
    `MZIP` = character(), 
    check.names=FALSE
  )
  for(i in (1:length(unique(newFafsa$`Index`)))){
    tempFafsa <- newFafsa %>% filter(`Index` == unique(newFafsa$`Index`)[i])
    tempZips <- zips %>% rename(`MZIP` = `zip`, `City` = `city`, `State` = `state_id`) %>% mutate(`City` = toupper(`City`))
    tempFafsa <- left_join(x=tempFafsa, y=tempZips, by=c("City", "State"), relationship="many-to-many")
    middleFafsa <- rbind(middleFafsa, tempFafsa)
    rm(tempFafsa, tempZips)
  }
  rm(i)
  newMerge <- inner_join(x=middleFafsa, y=newElsi, by="MZIP", relationship="many-to-many")
  newMerge <- newMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`))
  newMerge <- newMerge %>% filter(`School Name Similarity Index` > 0.75)
  newMerge <- newMerge %>% select(`NCESSCH`, `Name`, `City`, `State`, `Submissions`, `Completions`, `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`)
  newMerge <- newMerge %>% mutate(`Merge Round` = rep("Round 5"))
  goodMerge <- rbind(goodMerge, newMerge)
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
  rm(newFafsa, newElsi, newMerge, middleFafsa)
  
  # Round 6
  newFafsa <- remainingFafsa 
  newElsi <- remainingElsi 
  middleFafsa <- newFafsa %>% select(`Name`, `State`, `City`, `Index`)
  middleElsi <- newElsi %>% filter(`G_12_OFFERED`=="Yes") %>% select(`SCH_NAME`, `ST`, `LCITY`, `NCESSCH-ELSI`) %>% rename(`State` = `ST`)
  middleMerge <- left_join(x=middleFafsa, y=middleElsi, by="State", relationship="many-to-many")
  rm(middleFafsa, middleElsi)
  middleMerge <- middleMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`)) %>% filter(`School Name Similarity Index` > 0.9) 
  middleMerge <- middleMerge %>% arrange(desc(`School Name Similarity Index`)) %>% filter(duplicated(`Index`)==FALSE)
  middleMerge <- middleMerge %>% select(`Index`, `NCESSCH-ELSI`)
  middleMerge <- left_join(x=middleMerge, y=newFafsa, by="Index")
  middleMerge <- left_join(x=middleMerge, y=newElsi, by="NCESSCH-ELSI")
  middleMerge <- middleMerge %>% select(`NCESSCH`, `Name`, `City`, `State`, `Submissions`, `Completions`, `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`) %>% mutate(`Merge Round` = rep("Round 6"))
  goodMerge <- rbind(goodMerge, middleMerge)
  remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
  remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
  rm(newFafsa, newElsi, middleMerge)
  
  output <- list(goodMerge, remainingFafsa, remainingElsi)
  return(output)
  rm(goodMerge, remainingFafsa, remainingElsi)
  
}

#### End #### 

#### Merge 2024 data ####

merge24 <- mergeFunction(list(fafsa24, elsi24))[[1]]
merge23 <- mergeFunction(list(fafsa23, elsi23))[[1]]
merge22 <- mergeFunction(list(fafsa22, elsi22))[[1]]
merge21 <- mergeFunction(list(fafsa21, elsi21))[[1]]
merge20 <- mergeFunction(list(fafsa20, elsi20))[[1]]
merge19 <- mergeFunction(list(fafsa19, elsi19))[[1]]
merge18 <- mergeFunction(list(fafsa18, elsi18))[[1]]
merge17 <- mergeFunction(list(fafsa17, elsi17))[[1]]

rm(
  fafsa24, elsi24, 
  fafsa23, elsi23, 
  fafsa22, elsi22, 
  fafsa21, elsi21, 
  fafsa20, elsi20, 
  fafsa19, elsi19, 
  fafsa18, elsi18, 
  fafsa17, elsi17 
)

#### End #### 

#### Merge grade 12 enrollment ####

processEnrollment1 <- function(seniors){
  seniors <- seniors %>% select(
    `NCESSCH`, 
    `GRADE`, 
    `STUDENT_COUNT`, 
    `TOTAL_INDICATOR`
  ) %>% filter(
    `GRADE` == "Grade 12", 
    `TOTAL_INDICATOR` == "Subtotal 4 - By Grade"
  ) %>% select(-(`GRADE`)) %>% select(-(`TOTAL_INDICATOR`)) %>% rename(
    `NCESSCH-ELSI` = `NCESSCH`, 
    `Grade 12 students` = `STUDENT_COUNT`
  ) %>% mutate(
    `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
  ) %>% mutate(
    `Grade 12 students` = `Grade 12 students` * (4165201 / 3917352) # Go back and figure out how best to align with NCAN's numbers 
  )
  return(seniors)
}

processEnrollment2 <- function(seniors){
  names(seniors) <- c(
    "Name", 
    "State Name", 
    "NCESSCH-ELSI", 
    "Total students all grades", 
    "Grade 12 students"
  )
  suppressWarnings({
    seniors <- seniors %>% select(
      `NCESSCH-ELSI`, `Grade 12 students`
    ) %>% mutate(
      `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`), 
      `Grade 12 students` = as.numeric(`Grade 12 students`)
    ) %>% mutate(
      `Grade 12 students` = `Grade 12 students` * (4165201 / 3917352) # Go back and figure out how best to align with NCAN's numbers 
    )
  })
  return(seniors)
}

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

merge24 <- left_join(
  x=merge24, 
  y=processEnrollment1(read.csv("ccd_sch_052_2223.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge23 <- left_join(
  x=merge23, 
  y=processEnrollment1(read.csv("ccd_sch_052_2223.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge22 <- left_join(
  x=merge22, 
  y=processEnrollment2(read.csv("ELSI Enrollment 2021-22.csv", skip=5, header=TRUE, check.names=FALSE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge21 <- left_join(
  x=merge21, 
  y=processEnrollment1(read.csv("ccd_sch_052_2021.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge20 <- left_join(
  x=merge20, 
  y=processEnrollment1(read.csv("ccd_sch_052_1920.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge19 <- left_join(
  x=merge19, 
  y=processEnrollment1(read.csv("ccd_sch_052_1819.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge18 <- left_join(
  x=merge18, 
  y=processEnrollment2(read.csv("ELSI Enrollment 2017-18.csv", skip=5, header=TRUE, check.names=FALSE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge17 <- left_join(
  x=merge17, 
  y=processEnrollment2(read.csv("ELSI Enrollment 2016-17.csv", skip=5, header=TRUE, check.names=FALSE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)

#### End #### 

#### Census data: Native vs. foreign-born ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSDP5Y2022.DP02")
census1 <- read.csv("ACSDP5Y2022.DP02-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census1 <- census1 %>% select(
  `Geographic Area Name`,
  `Estimate!!PLACE OF BIRTH!!Total population`, 
  `Estimate!!PLACE OF BIRTH!!Total population!!Native`,
  `Estimate!!PLACE OF BIRTH!!Total population!!Foreign born`, 
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Europe`,
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Asia`, 
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Africa`,
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Oceania`, 
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Latin America`, 
  `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Northern America`
) 
census1 <- census1 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Total population` = `Estimate!!PLACE OF BIRTH!!Total population`, 
  `Native-born population` = `Estimate!!PLACE OF BIRTH!!Total population!!Native`,
  `Foreign-born population` = `Estimate!!PLACE OF BIRTH!!Total population!!Foreign born`, 
  `Foreign-born: Europe` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Europe`,
  `Foreign-born: Asia` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Asia`, 
  `Foreign-born: Africa` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Africa`,
  `Foreign-born: Oceania` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Oceania`, 
  `Foreign-born: Latin America` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Latin America`, 
  `Foreign-born: Northern America` = `Estimate!!WORLD REGION OF BIRTH OF FOREIGN BORN!!Foreign-born population, excluding population born at sea!!Northern America`
) 
census1 <- census1 %>% mutate(
  `Total population` = as.numeric(`Total population`), 
  `Native-born population` = as.numeric(`Native-born population`), 
  `Foreign-born population` = as.numeric(`Foreign-born population`), 
  `Foreign-born: Europe` = as.numeric(`Foreign-born: Europe`), 
  `Foreign-born: Asia` = as.numeric(`Foreign-born: Asia`), 
  `Foreign-born: Africa` = as.numeric(`Foreign-born: Africa`), 
  `Foreign-born: Oceania` = as.numeric(`Foreign-born: Oceania`), 
  `Foreign-born: Latin America` = as.numeric(`Foreign-born: Latin America`), 
  `Foreign-born: Northern America` = as.numeric(`Foreign-born: Northern America`)
) 
census1 <- census1 %>% filter(
  is.na(`Total population`)==FALSE, 
  is.na(`Native-born population`)==FALSE, 
  is.na(`Foreign-born population`)==FALSE, 
) 
census1 <- census1 %>% mutate(
  `Native-born share` = `Native-born population` / `Total population`, 
  `Foreign-born share` = `Foreign-born population` / `Total population`, 
  `Foreign-born share: Europe` = `Foreign-born: Europe` / `Total population`, 
  `Foreign-born share: Asia` = `Foreign-born: Asia` / `Total population`, 
  `Foreign-born share: Africa` = `Foreign-born: Africa` / `Total population`, 
  `Foreign-born share: Oceania` = `Foreign-born: Oceania` / `Total population`, 
  `Foreign-born share: Latin America` = `Foreign-born: Latin America` / `Total population`, 
  `Foreign-born share: Northern America` = `Foreign-born: Northern America` / `Total population`
) %>% rename(`Total population (C1)` = `Total population`)
census1 <- census1 %>% mutate(
  `Foreign-born share: AAOA` = `Foreign-born share: Asia` + `Foreign-born share: Africa` + `Foreign-born share: Oceania` + `Foreign-born share: Latin America` + `Foreign-born share: Northern America`
)

#### End #### 

#### Census data: Racial demographics ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSDT5Y2022.B03002")
census2 <- read.csv("ACSDT5Y2022.B03002-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census2 <- census2 %>% select(
  `Geographic Area Name`, 
  `Estimate!!Total:`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!White alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!Some other race alone`,
  `Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:`,
  `Estimate!!Total:!!Hispanic or Latino:`,
  `Estimate!!Total:!!Hispanic or Latino:!!Some other race alone`,
  `Estimate!!Total:!!Hispanic or Latino:!!Two or more races:`
)
census2 <- census2 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Total population` = `Estimate!!Total:`,
  `White` = `Estimate!!Total:!!Not Hispanic or Latino:!!White alone`,
  `Black` = `Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone`,
  `Native American` = `Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone`,
  `Asian` = `Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone`,
  `Pacific Islander` = `Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone`,
  `Other race` = `Estimate!!Total:!!Not Hispanic or Latino:!!Some other race alone`,
  `Two or more races` = `Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:`,
  `Hispanic or Latino` = `Estimate!!Total:!!Hispanic or Latino:`,
  `Hispanic or Latino (some other race)` = `Estimate!!Total:!!Hispanic or Latino:!!Some other race alone`,
  `Hispanic or Latino (two or more)` = `Estimate!!Total:!!Hispanic or Latino:!!Two or more races:`
) 
census2 <- census2 %>% mutate(
  `Total population` = as.numeric(`Total population`),
  `White` = as.numeric(`White`),
  `Black` = as.numeric(`Black`),
  `Native American` = as.numeric(`Native American`),
  `Asian` =  as.numeric(`Asian`),
  `Pacific Islander` = as.numeric(`Pacific Islander`),
  `Other race` = as.numeric(`Other race`),
  `Two or more races` = as.numeric(`Two or more races`),
  `Hispanic or Latino` = as.numeric(`Hispanic or Latino`),
  `Hispanic or Latino (some other race)` = as.numeric(`Hispanic or Latino (some other race)`),
  `Hispanic or Latino (two or more)` = as.numeric(`Hispanic or Latino (two or more)`)
) 
census2 <- census2 %>% mutate(
  `Hispanic or Latino` = `Hispanic or Latino` - `Hispanic or Latino (some other race)`, 
  `Other race` = `Other race` + `Hispanic or Latino (some other race)`
) %>% select(-(`Hispanic or Latino (some other race)`))
census2 <- census2 %>% mutate(
  `Hispanic or Latino` = `Hispanic or Latino` - `Hispanic or Latino (two or more)`, 
  `Two or more races` = `Two or more races` + `Hispanic or Latino (two or more)`
) %>% select(-(`Hispanic or Latino (two or more)`))

# Checking correct calculations: 
# test <- census2 %>% mutate(
#   `Alt total` = `White` + `Black` + `Native American` + `Asian` + `Pacific Islander` + `Other race` + `Two or more races` + `Hispanic or Latino`
# )
# table(test$`Total population`==test$`Alt total`)

census2 <- census2 %>% filter(
  is.na(`Total population`)==FALSE, 
  is.na(`White`)==FALSE, 
  is.na(`Black`)==FALSE, 
  is.na(`Native American`)==FALSE, 
  is.na(`Asian`)==FALSE, 
  is.na(`Pacific Islander`)==FALSE, 
  is.na(`Other race`)==FALSE, 
  is.na(`Two or more races`)==FALSE, 
  is.na(`Hispanic or Latino`)==FALSE
) 
census2 <- census2 %>% mutate(
  `White share` = `White` / `Total population`, 
  `Black share` = `Black` / `Total population`, 
  `Native American share` = `Native American` / `Total population`, 
  `Asian share` = `Asian` / `Total population`, 
  `Pacific Islander share` = `Pacific Islander` / `Total population`, 
  `Other race share` = `Other race` / `Total population`, 
  `Two or more races share` = `Two or more races` / `Total population`, 
  `Hispanic or Latino share` = `Hispanic or Latino` / `Total population`
) %>% rename(`Total population (C2)` = `Total population`)

#### End ####

#### Census data: Educational attainment ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSST5Y2022.S1501")
census3 <- read.csv("ACSST5Y2022.S1501-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census3 <- census3 %>% select(
  `Geographic Area Name`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Associate's degree`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree`,
  `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Graduate or professional degree`
)
census3 <- census3 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Total population 25+` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over`,
  `Less than 9th grade` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade`,
  `High school, no diploma` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma`,
  `High school diploma` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)`,
  `Some college, no degree` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree`,
  `Associate's degree` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Associate's degree`,
  `Bachelor's degree` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree`,
  `Graduate degree` = `Estimate!!Total!!AGE BY EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Graduate or professional degree`
)
census3 <- census3 %>% mutate(
  `Total population 25+` = as.numeric(`Total population 25+`),
  `Less than 9th grade` = as.numeric(`Less than 9th grade`),
  `High school, no diploma` = as.numeric(`High school, no diploma`),
  `High school diploma` = as.numeric(`High school diploma`),
  `Some college, no degree` = as.numeric(`Some college, no degree`),
  `Associate's degree` = as.numeric(`Associate's degree`),
  `Bachelor's degree` = as.numeric(`Bachelor's degree`),
  `Graduate degree` = as.numeric(`Graduate degree`)
)
census3 <- census3 %>% filter(
  is.na(`Total population 25+`)==FALSE, 
  is.na(`Less than 9th grade`)==FALSE, 
  is.na(`High school, no diploma`)==FALSE, 
  is.na(`High school diploma`)==FALSE, 
  is.na(`Some college, no degree`)==FALSE, 
  is.na(`Associate's degree`)==FALSE, 
  is.na(`Bachelor's degree`)==FALSE,
  is.na(`Graduate degree`)==FALSE
)
census3 <- census3 %>% mutate(
  `Less than 9th grade share` = `Less than 9th grade` / `Total population 25+`, 
  `High school, no diploma share` = `High school, no diploma` / `Total population 25+`, 
  `High school diploma share` = `High school diploma` / `Total population 25+`, 
  `Some college, no degree share` = `Some college, no degree` / `Total population 25+`, 
  `Associate's degree share` = `Associate's degree` / `Total population 25+`, 
  `Bachelor's degree share` = `Bachelor's degree` / `Total population 25+`,
  `Graduate degree share` = `Graduate degree` / `Total population 25+`
) %>% rename(`Total population 25+ (C3)` = `Total population 25+`) 

#### End #### 

#### Census data: SNAP recipiency ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSST5Y2022.S2201")
census4 <- read.csv("ACSST5Y2022.S2201-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census4 <- census4 %>% select(
  `Geographic Area Name`,
  `Estimate!!Total!!Households`,
  `Estimate!!Households receiving food stamps/SNAP!!Households`
)
census4 <- census4 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Total households` = `Estimate!!Total!!Households`,
  `Households receiving SNAP` = `Estimate!!Households receiving food stamps/SNAP!!Households`
)
census4 <- census4 %>% mutate(
  `Total households` = as.numeric(`Total households`), 
  `Households receiving SNAP` = as.numeric(`Households receiving SNAP`)
)
census4 <- census4 %>% filter(
  is.na(`Total households`)==FALSE, 
  is.na(`Households receiving SNAP`)==FALSE
)
census4 <- census4 %>% mutate(
  `Households receiving SNAP share` = `Households receiving SNAP` / `Total households`
) %>% rename(`Total households (C4)` = `Total households`)

#### End #### 

#### Census data: Limited English speaking households ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSST5Y2022.S1602")
census5 <- read.csv("ACSST5Y2022.S1602-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census5 <- census5 %>% select(
  `Geographic Area Name`,
  `Estimate!!Total!!All households`, 
  `Estimate!!Limited English-speaking households!!All households`
)
census5 <- census5 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Total households` = `Estimate!!Total!!All households`, 
  `Limited English households` = `Estimate!!Limited English-speaking households!!All households`
)
census5 <- census5 %>% mutate(
  `Total households` = as.numeric(`Total households`), 
  `Limited English households` = as.numeric(`Limited English households`)
)
census5 <- census5 %>% filter(
  is.na(`Total households`)==FALSE, 
  is.na(`Limited English households`)==FALSE
)
census5 <- census5 %>% mutate(
  `Limited English share` = `Limited English households` / `Total households`
) %>% rename(`Total households (C5)` = `Total households`)

#### End #### 

#### Census data: Poverty status ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSST5Y2022.S1701")
census6 <- read.csv("ACSST5Y2022.S1701-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census6 <- census6 %>% select(
  `Geographic Area Name`,
  `Estimate!!Total!!Population for whom poverty status is determined`, 
  `Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years`,
  `Estimate!!Below poverty level!!Population for whom poverty status is determined`, 
  `Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years`
)
census6 <- census6 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Population assessed for poverty` = `Estimate!!Total!!Population for whom poverty status is determined`, 
  `Children assessed for poverty` = `Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years`,
  `Population under poverty level` = `Estimate!!Below poverty level!!Population for whom poverty status is determined`, 
  `Children under poverty level` = `Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years`
)
census6 <- census6 %>% mutate(
  `Population assessed for poverty` = as.numeric(`Population assessed for poverty`), 
  `Children assessed for poverty` = as.numeric(`Children assessed for poverty`),
  `Population under poverty level` = as.numeric(`Population under poverty level`), 
  `Children under poverty level` = as.numeric(`Children under poverty level`)
)
census6 <- census6 %>% filter(
  is.na(`Population assessed for poverty`)==FALSE, 
  is.na(`Children assessed for poverty`)==FALSE, 
  is.na(`Population under poverty level`)==FALSE, 
  is.na(`Children under poverty level`)==FALSE
)
census6 <- census6 %>% mutate(
  `Share of population in poverty` = `Population under poverty level` / `Population assessed for poverty`, 
  `Share of children in poverty` = `Children under poverty level` / `Children assessed for poverty`
)

#### End #### 

#### Census data: Average income ####

setwd("/Users/peter_granville/FAFSA-2024/Census data/ACSST5Y2022.S1902")
census7 <- read.csv("ACSST5Y2022.S1902-Data.csv", header=TRUE, skip=1, check.names=FALSE)
census7 <- census7 %>% select(
  `Geographic Area Name`,
  `Estimate!!Number!!HOUSEHOLD INCOME!!All households`,
  `Estimate!!Number!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income`,
  `Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households`,
  `Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income`
)
census7 <- census7 %>% rename(
  `ZCTA5` = `Geographic Area Name`,
  `Number of households` = `Estimate!!Number!!HOUSEHOLD INCOME!!All households`,
  `Number of households with wages` = `Estimate!!Number!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income`,
  `Average household income` = `Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households`,
  `Average household income (with wages)` = `Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income`
)
census7 <- census7 %>% mutate(
  `Number of households` = as.numeric(`Number of households`), 
  `Number of households with wages` = as.numeric(`Number of households with wages`), 
  `Average household income` = as.numeric(`Average household income`), 
  `Average household income (with wages)` = as.numeric(`Average household income (with wages)`)
)
census7 <- census7 %>% filter(
  is.na(`Number of households`)==FALSE,
  is.na(`Number of households with wages`)==FALSE,
  is.na(`Average household income`)==FALSE,
  is.na(`Average household income (with wages)`)==FALSE
)

#### End #### 

#### Merge Census data ####

census <- full_join(x=census1, y=census2, by="ZCTA5")
census <- full_join(x=census, y=census3, by="ZCTA5")
census <- full_join(x=census, y=census4, by="ZCTA5")
census <- full_join(x=census, y=census5, by="ZCTA5")
census <- full_join(x=census, y=census6, by="ZCTA5")
census <- full_join(x=census, y=census7, by="ZCTA5")
rm(census1, census2, census3, census4, census5, census6, census7)

census <- census %>% filter(
  `Total population (C1)` != 0,
  `Total population (C2)` != 0, 
  `Total population 25+ (C3)` != 0,
  `Total households (C4)` != 0, 
  `Total households (C5)` != 0, 
  is.na(`Population under poverty level`)==FALSE,
  is.na(`Average household income`)==FALSE
)

#### End #### 

#### Write function to link in Census data ####

mergeCensus <- function(merge, census){
  merge <- merge %>% mutate(`ZCTA5` = paste("ZCTA5 ", substr(`LZIP`, 1, 5), sep=""))
  analysis <- aggregate(data=merge, cbind(
    `Grade 12 students`, 
    `Submissions`, 
    `Completions`
  ) ~ `ZCTA5`, FUN=sum)
  analysis <- left_join(x=analysis, y=census, by="ZCTA5")
  analysis <- analysis %>% mutate(
    `Black or Latino share` = `Black share` + `Hispanic or Latino share`,
    `Black, Latino, or Native American share` = `Black share` + `Hispanic or Latino share` + `Native American share`,
    `Black, Latino, Native American, or Pacific Islander share` = `Black share` + `Hispanic or Latino share` + `Native American share` + `Pacific Islander share`,
    `No college share` = `Less than 9th grade share` + `High school, no diploma share` + `High school diploma share`, 
    `Associate's or higher share` = `Associate's degree share` + `Bachelor's degree share` + `Graduate degree share`
  )
  return(analysis)
  rm(analysis)
}

analysis24 <- mergeCensus(merge24, census)
analysis23 <- mergeCensus(merge23, census)
analysis22 <- mergeCensus(merge22, census)
analysis21 <- mergeCensus(merge21, census)
analysis20 <- mergeCensus(merge20, census)
analysis19 <- mergeCensus(merge19, census)
analysis18 <- mergeCensus(merge18, census)
analysis17 <- mergeCensus(merge17, census)

#### End #### 

#### Derive state from ZIP ####

badZIPs <- c(
  "ZCTA5 03209",
  "ZCTA5 19326",
  "ZCTA5 22464",
  "ZCTA5 23135",
  "ZCTA5 26839",
  "ZCTA5 27899",
  "ZCTA5 36363",
  "ZCTA5 43624",
  "ZCTA5 70488",
  "ZCTA5 71875",
  "ZCTA5 95057",
  "ZCTA5 96361", 
  "ZCTA5 97003"
)

addState <- function(analysis){
  
  analysis <- analysis %>% mutate(`State` = rep(NA))
  
  for(i in (1:nrow(analysis))){
    if((analysis$`ZCTA5`[i] %in% badZIPs)==FALSE){
      analysis$`State`[i] <- reverse_zipcode(substr(analysis$`ZCTA5`[i], 7, 11))$state
    }
  }
  
  return(analysis)
}

analysis24 <- addState(analysis24)
analysis23 <- addState(analysis23)
analysis22 <- addState(analysis22)
analysis21 <- addState(analysis21)
analysis20 <- addState(analysis20)
analysis19 <- addState(analysis19)
analysis18 <- addState(analysis18)
analysis17 <- addState(analysis17)

rm(badZIPs)

#### End #### 

#### Save CSVs for use in state data ####

# analysis17 <- analysis17 %>% mutate(`Year` = rep("Class of 2017"))
# analysis18 <- analysis18 %>% mutate(`Year` = rep("Class of 2018"))
# analysis19 <- analysis19 %>% mutate(`Year` = rep("Class of 2019"))
# analysis20 <- analysis20 %>% mutate(`Year` = rep("Class of 2020"))
# analysis21 <- analysis21 %>% mutate(`Year` = rep("Class of 2021"))
# analysis22 <- analysis22 %>% mutate(`Year` = rep("Class of 2022"))
# analysis23 <- analysis23 %>% mutate(`Year` = rep("Class of 2023"))
# analysis24 <- analysis24 %>% mutate(`Year` = rep("Class of 2024"))
# 
# analysis0 <- rbind(
#   analysis17,
#   analysis18,
#   analysis19,
#   analysis20,
#   analysis21,
#   analysis22,
#   analysis23,
#   analysis24
# )
# 
# setwd("/Users/peter_granville/FAFSA-2024")
# 
# write.csv(analysis0, "FAFSA by ZIP 2017 to 2024.csv", row.names=FALSE)
# rm(analysis0)

#### End #### 

#### Write function to calculate percentiles ####

calcPercentiles <- function(analysis, nGroups, year, stateLever, stateSelection, inclusiveExclusive){
  
  rankInputs <- analysis %>% select(
    `ZCTA5`, 
    `State`,
    `Native-born share`,
    `Foreign-born share: AAOA`, 
    `White share`, 
    `Black share`, 
    `Native American share`,
    `Asian share`, 
    `Pacific Islander share`, 
    `Other race share`, 
    `Two or more races share`,
    `Hispanic or Latino share`,
    `Black or Latino share`,
    `Black, Latino, or Native American share`,
    `Black, Latino, Native American, or Pacific Islander share`,
    `Less than 9th grade share`, 
    `High school, no diploma share`, 
    `High school diploma share`, 
    `Some college, no degree share`,
    `Associate's degree share`, 
    `Bachelor's degree share`, 
    `Graduate degree share`, 
    `No college share`, 
    `Associate's or higher share`,
    `Households receiving SNAP share`, 
    `Limited English share`,
    `Share of population in poverty`,
    `Share of children in poverty`, 
    `Average household income`, 
    `Average household income (with wages)`
  )
  rankOutputs <- analysis %>% select(
    `ZCTA5`, 
    `State`,
    `Grade 12 students`,
    `Submissions`, 
    `Completions`
  )
  
  if(stateLever==TRUE){
    if(inclusiveExclusive=="Inclusive"){
      rankInputs <- rankInputs %>% filter(`State` %in% stateSelection)
      rankOutputs <- rankOutputs %>% filter(`State` %in% stateSelection)
    }else{
      rankInputs <- rankInputs %>% filter((`State` %in% stateSelection)==FALSE)
      rankOutputs <- rankOutputs %>% filter((`State` %in% stateSelection)==FALSE)
    }
  }
  
  for(i in (2:ncol(rankInputs))){
    
    tempTiles <- rankInputs %>% select(`ZCTA5`, names(rankInputs)[i])
    names(tempTiles)[2] <- "InterestVar"
    tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
    names(tempTiles)[2] <- paste("Groups: ", names(rankInputs)[i], sep="")
    if(i == 2){
      nTiles <- tempTiles
    }else{
      nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
    }
    rm(tempTiles)
  }
  rm(i, nGroups)
  nTiles <- full_join(x=nTiles, y=rankOutputs, by="ZCTA5") %>% mutate(`Class` = rep(year))
  
  if(stateLever==TRUE){
    if(inclusiveExclusive=="Inclusive"){
      if(length(stateSelection) == 1){
        nTiles <- nTiles %>% mutate(`State` = rep(paste("Selected state: ", stateSelection[1], sep="")))
      }else{
        nTiles <- nTiles %>% mutate(`State` = rep("Selected states"))
      }
    }else{
      nTiles <- nTiles %>% mutate(`State` = rep("Rest of U.S."))
    }
  }
  
  return(nTiles)
  rm(rankInputs, rankOutputs, nTiles)
}

#### End #### 

########################################
#### Quintiles                      ####
########################################

#### Create quintile datasets #### 

quintiles.national <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis23, 5, "Class of 2023", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis22, 5, "Class of 2022", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis21, 5, "Class of 2021", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis20, 5, "Class of 2020", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis19, 5, "Class of 2019", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis18, 5, "Class of 2018", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis17, 5, "Class of 2017", FALSE, c("None"), "Neither") 
)

quintiles.louisiana <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("LA"), "Exclusive") 
)

quintiles.illinois <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("IL"), "Exclusive") 
)

quintiles.texas <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("TX"), "Exclusive") 
)

quintiles.alabama <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("AL"), "Exclusive") 
)

quintiles.california <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("CA"), "Exclusive") 
)

quintiles.indiana <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("IN"), "Exclusive") 
)

quintiles.newhampshire <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("NH"), "Exclusive") 
)

quintiles.mandateStates <- rbind(
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis24, 5, "Class of 2024", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis23, 5, "Class of 2023", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis22, 5, "Class of 2022", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis21, 5, "Class of 2021", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis20, 5, "Class of 2020", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis19, 5, "Class of 2019", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis18, 5, "Class of 2018", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis17, 5, "Class of 2017", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive") 
)

#### End #### 

#### Write function to chart quintiles #### 

quintilePlot <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
  
  newTiles <- tilesDF
  newVar <- tilesVar
  
  if(stateBreakout==TRUE){
    newTiles <- newTiles %>% select(
      `Class`,
      `State`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "Quintile", newVar)
    analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
    analysis1$State <- factor(analysis1$State)
    analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`)) + geom_bar(stat="identity", position = "dodge2") + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 

  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "Quintile", newVar)
    analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`)) + geom_bar(stat="identity", position = "dodge2") + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.7)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
  }
  
  return(figA)
  rm(analysis1, newTiles, figA, newVar)
}

onefivePlot <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
  
  newTiles <- tilesDF
  newVar <- tilesVar
  
  if(stateBreakout==TRUE){
    newTiles <- newTiles %>% select(
      `Class`,
      `State`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "Quintile", newVar)
    analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
    analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "5th"))
    analysis1$State <- factor(analysis1$State)
    analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
    figB <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, color=`Tile`, group=`Tile`)) + geom_point() + geom_line() + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", color=newVar) + scale_color_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(2)) + theme(legend.position='bottom') 
    
  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "Quintile", newVar)
    analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
    analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "5th"))
    figB <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, color=`Tile`, group=`Tile`)) + geom_point() + geom_line() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", color=newVar) + scale_color_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(2)) + theme(legend.position='bottom') 
  }
  
  return(figB)
  rm(analysis1, newTiles, figB, newVar)
}

#### End #### 

#### Run completion rate by year function ####

# Alabama
quintilesA1a <- quintilePlot(quintiles.alabama, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesA1b <- quintilePlot(quintiles.alabama, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesA1c <- quintilePlot(quintiles.alabama, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesA2a <- quintilePlot(quintiles.alabama, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesA2b <- quintilePlot(quintiles.alabama, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesA2c <- quintilePlot(quintiles.alabama, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesA3a <- onefivePlot(quintiles.alabama, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesA3b <- onefivePlot(quintiles.alabama, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesA3c <- onefivePlot(quintiles.alabama, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# California
quintilesB1a <- quintilePlot(quintiles.california, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesB1b <- quintilePlot(quintiles.california, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesB1c <- quintilePlot(quintiles.california, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesB2a <- quintilePlot(quintiles.california, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
quintilesB2b <- quintilePlot(quintiles.california, "Groups: No college share", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
quintilesB2c <- quintilePlot(quintiles.california, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
quintilesB3a <- onefivePlot(quintiles.california, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesB3b <- onefivePlot(quintiles.california, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesB3c <- onefivePlot(quintiles.california, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Illinois
quintilesC1a <- quintilePlot(quintiles.illinois, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesC1b <- quintilePlot(quintiles.illinois, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesC1c <- quintilePlot(quintiles.illinois, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesC2a <- quintilePlot(quintiles.illinois, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
quintilesC2b <- quintilePlot(quintiles.illinois, "Groups: No college share", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
quintilesC2c <- quintilePlot(quintiles.illinois, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
quintilesC3a <- onefivePlot(quintiles.illinois, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesC3b <- onefivePlot(quintiles.illinois, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesC3c <- onefivePlot(quintiles.illinois, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Indiana
quintilesD1a <- quintilePlot(quintiles.indiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesD1b <- quintilePlot(quintiles.indiana, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesD1c <- quintilePlot(quintiles.indiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesD2a <- quintilePlot(quintiles.indiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesD2b <- quintilePlot(quintiles.indiana, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesD2c <- quintilePlot(quintiles.indiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesD3a <- onefivePlot(quintiles.indiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesD3b <- onefivePlot(quintiles.indiana, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesD3c <- onefivePlot(quintiles.indiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Louisiana
quintilesE1a <- quintilePlot(quintiles.louisiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesE1b <- quintilePlot(quintiles.louisiana, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesE1c <- quintilePlot(quintiles.louisiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesE2a <- quintilePlot(quintiles.louisiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
quintilesE2b <- quintilePlot(quintiles.louisiana, "Groups: No college share", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
quintilesE2c <- quintilePlot(quintiles.louisiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
quintilesE3a <- onefivePlot(quintiles.louisiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesE3b <- onefivePlot(quintiles.louisiana, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesE3c <- onefivePlot(quintiles.louisiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# New Hampshire
quintilesF1a <- quintilePlot(quintiles.newhampshire, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesF1b <- quintilePlot(quintiles.newhampshire, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesF1c <- quintilePlot(quintiles.newhampshire, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesF2a <- quintilePlot(quintiles.newhampshire, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesF2b <- quintilePlot(quintiles.newhampshire, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesF2c <- quintilePlot(quintiles.newhampshire, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
quintilesF3a <- onefivePlot(quintiles.newhampshire, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesF3b <- onefivePlot(quintiles.newhampshire, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesF3c <- onefivePlot(quintiles.newhampshire, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Texas
quintilesG1a <- quintilePlot(quintiles.texas, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesG1b <- quintilePlot(quintiles.texas, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesG1c <- quintilePlot(quintiles.texas, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesG2a <- quintilePlot(quintiles.texas, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesG2b <- quintilePlot(quintiles.texas, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesG2c <- quintilePlot(quintiles.texas, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
quintilesG3a <- onefivePlot(quintiles.texas, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesG3b <- onefivePlot(quintiles.texas, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesG3c <- onefivePlot(quintiles.texas, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# All mandate states
quintilesX1a <- quintilePlot(quintiles.mandateStates, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesX1b <- quintilePlot(quintiles.mandateStates, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesX1c <- quintilePlot(quintiles.mandateStates, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
quintilesX3a <- onefivePlot(quintiles.mandateStates, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
quintilesX3b <- onefivePlot(quintiles.mandateStates, "Groups: No college share", TRUE, FALSE, c("None"))
quintilesX3c <- onefivePlot(quintiles.mandateStates, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# National
quintilesY1a <- quintilePlot(quintiles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
quintilesY1b <- quintilePlot(quintiles.national, "Groups: No college share", FALSE, FALSE, c("None"))
quintilesY1c <- quintilePlot(quintiles.national, "Groups: Share of population in poverty", FALSE, FALSE, c("None"))
quintilesY3a <- onefivePlot(quintiles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
quintilesY3b <- onefivePlot(quintiles.national, "Groups: No college share", FALSE, FALSE, c("None"))
quintilesY3c <- onefivePlot(quintiles.national, "Groups: Share of population in poverty", FALSE, FALSE, c("None"))

#### End #### 

########################################
#### Deciles                        ####
########################################

#### Create decile datasets #### 

deciles.national <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis23, 10, "Class of 2023", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis22, 10, "Class of 2022", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis21, 10, "Class of 2021", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis20, 10, "Class of 2020", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis19, 10, "Class of 2019", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis18, 10, "Class of 2018", FALSE, c("None"), "Neither"), 
  calcPercentiles(analysis17, 10, "Class of 2017", FALSE, c("None"), "Neither") 
)

deciles.louisiana <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("LA"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("LA"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("LA"), "Exclusive") 
)

deciles.illinois <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("IL"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("IL"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("IL"), "Exclusive") 
)

deciles.texas <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("TX"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("TX"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("TX"), "Exclusive") 
)

deciles.alabama <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("AL"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("AL"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("AL"), "Exclusive") 
)

deciles.california <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("CA"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("CA"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("CA"), "Exclusive") 
)

deciles.indiana <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("IN"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("IN"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("IN"), "Exclusive") 
)

deciles.newhampshire <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("NH"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("NH"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("NH"), "Exclusive") 
)

deciles.mandateStates <- rbind(
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Inclusive"), 
  calcPercentiles(analysis24, 10, "Class of 2024", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis23, 10, "Class of 2023", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis22, 10, "Class of 2022", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis21, 10, "Class of 2021", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis20, 10, "Class of 2020", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis19, 10, "Class of 2019", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis18, 10, "Class of 2018", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive"), 
  calcPercentiles(analysis17, 10, "Class of 2017", TRUE, c("AL", "CA", "IL", "IN", "LA", "NH", "TX"), "Exclusive") 
)

#### End #### 

#### Write function to chart deciles #### 

decilePlot <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
  
  newTiles <- tilesDF
  newVar <- tilesVar
  
  if(stateBreakout==TRUE){
    newTiles <- newTiles %>% select(
      `Class`,
      `State`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "decile", newVar)
    analysis1 <- left_join(x=analysis1, y=decileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"))
    analysis1$State <- factor(analysis1$State)
    analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`)) + geom_bar(stat="identity", position = "dodge2") + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(10)) + theme(legend.position='bottom') 
    
  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "decile", newVar)
    analysis1 <- left_join(x=analysis1, y=decileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"))
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`)) + geom_bar(stat="identity", position = "dodge2") + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.7)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(10)) + theme(legend.position='bottom') 
  }
  
  return(figA)
  rm(analysis1, newTiles, figA, newVar)
}

onetenPlot <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
  
  newTiles <- tilesDF
  newVar <- tilesVar
  
  if(stateBreakout==TRUE){
    newTiles <- newTiles %>% select(
      `Class`,
      `State`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "decile", newVar)
    analysis1 <- left_join(x=analysis1, y=decileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"))
    analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "10th"))
    analysis1$State <- factor(analysis1$State)
    analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
    figB <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, color=`Tile`, group=`Tile`)) + geom_point() + geom_line() + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", color=newVar) + scale_color_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(2)) + theme(legend.position='bottom') 
    
  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    analysis1 <- aggregate(data=newTiles, cbind(
      `Completions`, 
      `Grade 12 students`
    ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
      `FAFSA completion rate` = `Completions` / `Grade 12 students`
    ) 
    if(yearLever==TRUE){
      analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
    }
    newVar <- gsub("Groups", "decile", newVar)
    analysis1 <- left_join(x=analysis1, y=decileDF, by="InterestVar")
    analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"))
    analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "10th"))
    figB <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, color=`Tile`, group=`Tile`)) + geom_point() + geom_line() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", color=newVar) + scale_color_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(2)) + theme(legend.position='bottom') 
  }
  
  return(figB)
  rm(analysis1, newTiles, figB, newVar)
}

#### End #### 

#### Run completion rate by year function ####

# Alabama
decilesA1a <- decilePlot(deciles.alabama, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesA1b <- decilePlot(deciles.alabama, "Groups: No college share", TRUE, FALSE, c("None"))
decilesA1c <- decilePlot(deciles.alabama, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesA2a <- decilePlot(deciles.alabama, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesA2b <- decilePlot(deciles.alabama, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesA2c <- decilePlot(deciles.alabama, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesA3a <- onetenPlot(deciles.alabama, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesA3b <- onetenPlot(deciles.alabama, "Groups: No college share", TRUE, FALSE, c("None"))
decilesA3c <- onetenPlot(deciles.alabama, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# California
decilesB1a <- decilePlot(deciles.california, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesB1b <- decilePlot(deciles.california, "Groups: No college share", TRUE, FALSE, c("None"))
decilesB1c <- decilePlot(deciles.california, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesB2a <- decilePlot(deciles.california, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
decilesB2b <- decilePlot(deciles.california, "Groups: No college share", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
decilesB2c <- decilePlot(deciles.california, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2022", "Class of 2023"))
decilesB3a <- onetenPlot(deciles.california, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesB3b <- onetenPlot(deciles.california, "Groups: No college share", TRUE, FALSE, c("None"))
decilesB3c <- onetenPlot(deciles.california, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Illinois
decilesC1a <- decilePlot(deciles.illinois, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesC1b <- decilePlot(deciles.illinois, "Groups: No college share", TRUE, FALSE, c("None"))
decilesC1c <- decilePlot(deciles.illinois, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesC2a <- decilePlot(deciles.illinois, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
decilesC2b <- decilePlot(deciles.illinois, "Groups: No college share", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
decilesC2c <- decilePlot(deciles.illinois, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
decilesC3a <- onetenPlot(deciles.illinois, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesC3b <- onetenPlot(deciles.illinois, "Groups: No college share", TRUE, FALSE, c("None"))
decilesC3c <- onetenPlot(deciles.illinois, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Indiana
decilesD1a <- decilePlot(deciles.indiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesD1b <- decilePlot(deciles.indiana, "Groups: No college share", TRUE, FALSE, c("None"))
decilesD1c <- decilePlot(deciles.indiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesD2a <- decilePlot(deciles.indiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesD2b <- decilePlot(deciles.indiana, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesD2c <- decilePlot(deciles.indiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesD3a <- onetenPlot(deciles.indiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesD3b <- onetenPlot(deciles.indiana, "Groups: No college share", TRUE, FALSE, c("None"))
decilesD3c <- onetenPlot(deciles.indiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Louisiana
decilesE1a <- decilePlot(deciles.louisiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesE1b <- decilePlot(deciles.louisiana, "Groups: No college share", TRUE, FALSE, c("None"))
decilesE1c <- decilePlot(deciles.louisiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesE2a <- decilePlot(deciles.louisiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
decilesE2b <- decilePlot(deciles.louisiana, "Groups: No college share", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
decilesE2c <- decilePlot(deciles.louisiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2017", "Class of 2018"))
decilesE3a <- onetenPlot(deciles.louisiana, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesE3b <- onetenPlot(deciles.louisiana, "Groups: No college share", TRUE, FALSE, c("None"))
decilesE3c <- onetenPlot(deciles.louisiana, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# New Hampshire
decilesF1a <- decilePlot(deciles.newhampshire, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesF1b <- decilePlot(deciles.newhampshire, "Groups: No college share", TRUE, FALSE, c("None"))
decilesF1c <- decilePlot(deciles.newhampshire, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesF2a <- decilePlot(deciles.newhampshire, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesF2b <- decilePlot(deciles.newhampshire, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesF2c <- decilePlot(deciles.newhampshire, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
decilesF3a <- onetenPlot(deciles.newhampshire, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesF3b <- onetenPlot(deciles.newhampshire, "Groups: No college share", TRUE, FALSE, c("None"))
decilesF3c <- onetenPlot(deciles.newhampshire, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# Texas
decilesG1a <- decilePlot(deciles.texas, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesG1b <- decilePlot(deciles.texas, "Groups: No college share", TRUE, FALSE, c("None"))
decilesG1c <- decilePlot(deciles.texas, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesG2a <- decilePlot(deciles.texas, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesG2b <- decilePlot(deciles.texas, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesG2c <- decilePlot(deciles.texas, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
decilesG3a <- onetenPlot(deciles.texas, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesG3b <- onetenPlot(deciles.texas, "Groups: No college share", TRUE, FALSE, c("None"))
decilesG3c <- onetenPlot(deciles.texas, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# All mandate states
decilesX1a <- decilePlot(deciles.mandateStates, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesX1b <- decilePlot(deciles.mandateStates, "Groups: No college share", TRUE, FALSE, c("None"))
decilesX1c <- decilePlot(deciles.mandateStates, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))
decilesX3a <- onetenPlot(deciles.mandateStates, "Groups: Black or Latino share", TRUE, FALSE, c("None"))
decilesX3b <- onetenPlot(deciles.mandateStates, "Groups: No college share", TRUE, FALSE, c("None"))
decilesX3c <- onetenPlot(deciles.mandateStates, "Groups: Share of population in poverty", TRUE, FALSE, c("None"))

# National
decilesY1a <- decilePlot(deciles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
decilesY1b <- decilePlot(deciles.national, "Groups: No college share", FALSE, FALSE, c("None"))
decilesY1c <- decilePlot(deciles.national, "Groups: Share of population in poverty", FALSE, FALSE, c("None"))
decilesY3a <- onetenPlot(deciles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
decilesY3b <- onetenPlot(deciles.national, "Groups: No college share", FALSE, FALSE, c("None"))
decilesY3c <- onetenPlot(deciles.national, "Groups: Share of population in poverty", FALSE, FALSE, c("None"))

#### End #### 

#### For October commentary ####

# Run the whole gamut of variables, including Hispanic 

quintilePlot(quintiles.national, "Groups: Native-born share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Foreign-born share: AAOA" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: White share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Black share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Native American share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Asian share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Pacific Islander share" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Other race share" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Two or more races share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Hispanic or Latino share" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Black, Latino, or Native American share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Less than 9th grade share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: High school, no diploma share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: High school diploma share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Some college, no degree share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Associate's degree share" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Bachelor's degree share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Graduate degree share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: No college share" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Associate's or higher share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Households receiving SNAP share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Limited English share", FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Share of population in poverty" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Share of children in poverty" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Average household income" , FALSE, FALSE, c("None"))
quintilePlot(quintiles.national, "Groups: Average household income (with wages)", FALSE, FALSE, c("None"))

decilePlot(deciles.national, "Groups: Native-born share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Foreign-born share: AAOA" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: White share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Black share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Native American share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Asian share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Pacific Islander share" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Other race share" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Two or more races share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Hispanic or Latino share" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Black or Latino share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Black, Latino, or Native American share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Less than 9th grade share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: High school, no diploma share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: High school diploma share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Some college, no degree share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Associate's degree share" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Bachelor's degree share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Graduate degree share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: No college share" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Associate's or higher share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Households receiving SNAP share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Limited English share", FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Share of population in poverty" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Share of children in poverty" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Average household income" , FALSE, FALSE, c("None"))
decilePlot(deciles.national, "Groups: Average household income (with wages)", FALSE, FALSE, c("None"))


#### End #### 

########################################
#### Which state has the best FAFSA ####
#### completion rate in communities ####
#### with at least 25% of the pop.  #### 
#### living in poverty?             ####
########################################

#### Aggregate data ####

analysis17 <- analysis17 %>% mutate(`Year` = rep("Class of 2017"))
analysis18 <- analysis18 %>% mutate(`Year` = rep("Class of 2018"))
analysis19 <- analysis19 %>% mutate(`Year` = rep("Class of 2019"))
analysis20 <- analysis20 %>% mutate(`Year` = rep("Class of 2020"))
analysis21 <- analysis21 %>% mutate(`Year` = rep("Class of 2021"))
analysis22 <- analysis22 %>% mutate(`Year` = rep("Class of 2022"))
analysis23 <- analysis23 %>% mutate(`Year` = rep("Class of 2023"))
analysis24 <- analysis24 %>% mutate(`Year` = rep("Class of 2024"))

Q1 <- rbind(
  analysis17, 
  analysis18, 
  analysis19, 
  analysis20, 
  analysis21, 
  analysis22, 
  analysis23, 
  analysis24
) %>% select(
  `ZCTA5`, 
  `Grade 12 students`, 
  `Completions`, 
  `Average household income`, 
  `Share of population in poverty`,
  `State`, 
  `Year`
) 

#### End #### 

#### Select variables ####  

Q2 <- Q1 %>% mutate(
  `Income under $65,000` = ifelse(`Average household income` < 65000, "Under $65,000", "Over $65,000"),
  `Poverty rate over 25%` = ifelse(`Share of population in poverty` > 0.25, "Over 25% poverty rate", "Under 25% poverty rate"),
  `Count` = rep(1)
)

Q2A <- aggregate(
  data=Q2, `Count` ~ `Income under $65,000` + `Year`, FUN=sum
) %>% pivot_wider(
  id_cols=c(`Year`), names_from=`Income under $65,000`, values_from=`Count`
)
Q2B <- aggregate(
  data=Q2, `Count` ~ `Poverty rate over 25%` + `Year`, FUN=sum
) %>% pivot_wider(
  id_cols=c(`Year`), names_from=`Poverty rate over 25%`, values_from=`Count`
)

Q2C <- aggregate(
  data=Q2, `Count` ~ `Income under $65,000` + `Year` + `State`, FUN=sum
) %>% filter(
  `Year` == "Class of 2023"
) %>% select(-(`Year`)) %>% pivot_wider(
  id_cols=c(`State`), names_from=`Income under $65,000`, values_from=`Count`
)
Q2D <- aggregate(
  data=Q2, `Count` ~ `Poverty rate over 25%` + `Year` + `State`, FUN=sum
) %>% filter(
  `Year` == "Class of 2023"
) %>% select(-(`Year`)) %>% pivot_wider(
  id_cols=c(`State`), names_from=`Poverty rate over 25%`, values_from=`Count`
)

#### End #### 

#### Chart each state by FAFSA completion rate for ZIPs with under $65,000 average house income #### 

Q3 <- Q2 %>% filter(
  `Income under $65,000` == "Under $65,000"
)
Q3$`State`[(Q3$`State` %in% c("AL", "CA", "IL", "IN", "LA", "NH", "TX"))==FALSE] <- "Rest of U.S."
Q3 <- aggregate(
  data=Q3, cbind(
    `Grade 12 students`, `Completions`
  ) ~ `State` + `Year`, FUN=sum
) %>% mutate(
  `FAFSA completion rate` = `Completions` / `Grade 12 students`
) 
figQ3 <- ggplot(data=Q3, mapping=aes(x=`Year`, y=`FAFSA completion rate`, color=`State`, group=`State`)) + geom_point() + geom_line() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.8))
ggplotly(figQ3)

Q4 <- Q2 %>% filter(
  `Poverty rate over 25%` == "Over 25% poverty rate"
)
Q4$`State`[(Q4$`State` %in% c("AL", "CA", "IL", "IN", "LA", "NH", "TX"))==FALSE] <- "Rest of U.S."
Q4 <- aggregate(
  data=Q4, cbind(
    `Grade 12 students`, `Completions`
  ) ~ `State` + `Year`, FUN=sum
) %>% mutate(
  `FAFSA completion rate` = `Completions` / `Grade 12 students`
)
figQ4 <- ggplot(data=Q4, mapping=aes(x=`Year`, y=`FAFSA completion rate`, color=`State`, group=`State`)) + geom_point() + geom_line() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.8))
ggplotly(figQ4)

#### End #### 

########################################
#### For commentary with Bill       ####
########################################

#### Derive county from ZIP ####

badZIPs <- c(
  "ZCTA5 03209",
  "ZCTA5 19326",
  "ZCTA5 22464",
  "ZCTA5 23135",
  "ZCTA5 26839",
  "ZCTA5 27899",
  "ZCTA5 36363",
  "ZCTA5 43624",
  "ZCTA5 70488",
  "ZCTA5 71875",
  "ZCTA5 95057",
  "ZCTA5 96361", 
  "ZCTA5 97003"
)

addCounty <- function(analysis0){
  
  analysis0 <- analysis0 %>% mutate(`County` = rep(NA))
  for(i in (1:nrow(analysis0))){
    if((analysis0$`ZCTA5`[i] %in% badZIPs)==FALSE){
      analysis0$`County`[i] <- reverse_zipcode(substr(analysis0$`ZCTA5`[i], 7, 11))$county
    }
  }
  
  return(analysis0)
}

analysis17 <- addCounty(analysis17)
analysis18 <- addCounty(analysis18)
analysis19 <- addCounty(analysis19)
analysis20 <- addCounty(analysis20)
analysis21 <- addCounty(analysis21)
analysis22 <- addCounty(analysis22)
analysis23 <- addCounty(analysis23)
analysis24 <- addCounty(analysis24)

rm(badZIPs)

#### End #### 

#### Analyze FAFSAs by county ####

householdInc <- function(analysis0, incomeVal, yearVal){
  
  labelUnder <- paste("Under ", dollar(incomeVal), sep="")
  labelOver <- paste("At least ", dollar(incomeVal), sep="")
  
  analysis <- analysis0 
  
  analysis <- analysis %>% filter(
    is.na(`County`)==FALSE, 
    is.na(`State`)==FALSE
  ) %>% mutate(
    `County-State` = paste(`County`, `State`, sep=", ")
  )
  
  agg1A <- aggregate(data=analysis, cbind(
    `Grade 12 students`, `Submissions`, `Completions`
  ) ~ `County-State`, FUN=sum)
  
  agg1B <- analysis %>% group_by(`County-State`) %>% summarize(
    `Average household income` = weighted.mean(x=`Average household income`, w=`Total households (C5)`))
  
  agg1 <- left_join(x=agg1A, y=agg1B, by="County-State")
  rm(agg1A, agg1B)
  
  agg1 <- agg1 %>% filter(
    is.na(`Average household income`)==FALSE, 
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  )
  
  agg1 <- agg1 %>% mutate(
    `Average household income under threshold` = ifelse(
      `Average household income` < incomeVal, labelUnder, labelOver
    )
  )
  
  agg1 <- agg1 %>% mutate(
    `FAFSA Completion Rate` = `Completions` / `Grade 12 students`
  )
  
  medianVal <- median(agg1$`FAFSA Completion Rate`, na.rm=TRUE)
  # medianVal <- sum(agg1$`Completions`, na.rm=TRUE) / sum(agg1$`Grade 12 students`, na.rm=TRUE)
  
  agg1 <- agg1 %>% mutate(
    `Over median completion rate` = ifelse(`FAFSA Completion Rate` >= medianVal, "Over median", "Under median")
  )
  
  agg1 <- agg1 %>% mutate(
    # `Count` = rep(1)
    `Count` = `Grade 12 students`
  )
  
  agg2 <- aggregate(
    data=agg1, `Count` ~ `Over median completion rate` + `Average household income under threshold`, FUN=sum
  ) %>% mutate(
    `Year` = rep(yearVal)
  )
  
  return(agg2)
  rm(agg2, agg1, medianVal, labelUnder, labelOver, analysis)
  
}

compare55 <- rbind(
  householdInc(analysis17, 55000, 2017), 
  householdInc(analysis18, 55000, 2018), 
  householdInc(analysis19, 55000, 2019), 
  householdInc(analysis20, 55000, 2020),
  householdInc(analysis21, 55000, 2021), 
  householdInc(analysis22, 55000, 2022), 
  householdInc(analysis23, 55000, 2023), 
  householdInc(analysis24, 55000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $55,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)

compare65 <- rbind(
  householdInc(analysis17, 65000, 2017), 
  householdInc(analysis18, 65000, 2018), 
  householdInc(analysis19, 65000, 2019), 
  householdInc(analysis20, 65000, 2020),
  householdInc(analysis21, 65000, 2021), 
  householdInc(analysis22, 65000, 2022), 
  householdInc(analysis23, 65000, 2023), 
  householdInc(analysis24, 65000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $65,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)

compare75 <- rbind(
  householdInc(analysis17, 75000, 2017), 
  householdInc(analysis18, 75000, 2018), 
  householdInc(analysis19, 75000, 2019), 
  householdInc(analysis20, 75000, 2020),
  householdInc(analysis21, 75000, 2021), 
  householdInc(analysis22, 75000, 2022), 
  householdInc(analysis23, 75000, 2023), 
  householdInc(analysis24, 75000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $75,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)


#### End #### 

#### Analyze FAFSAs by ZIP ####

zipInc <- function(analysis0, incomeVal, yearVal){
  
  labelUnder <- paste("Under ", dollar(incomeVal), sep="")
  labelOver <- paste("At least ", dollar(incomeVal), sep="")
  
  analysis <- analysis0 
  
  analysis <- analysis %>% filter(
    is.na(`Average household income`)==FALSE, 
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) %>% mutate(
    `Average household income under threshold` = ifelse(
      `Average household income` < incomeVal, labelUnder, labelOver
    )
  )
  
  analysis <- analysis %>% mutate(
    `FAFSA Completion Rate` = `Completions` / `Grade 12 students`
  )
  
  # medianVal <- median(analysis$`FAFSA Completion Rate`, na.rm=TRUE)
  medianVal <- sum(analysis$`Completions`, na.rm=TRUE) / sum(analysis$`Grade 12 students`, na.rm=TRUE)
  
  analysis <- analysis %>% mutate(
    `Over median completion rate` = ifelse(`FAFSA Completion Rate` >= medianVal, "Over median", "Under median"), 
    # `Count` = rep(1)
    `Count` = `Grade 12 students`
  )
  
  agg1 <- aggregate(
    data=analysis, `Count` ~ `Over median completion rate` + `Average household income under threshold`, FUN=sum
  ) %>% mutate(
    `Year` = rep(yearVal)
  )
  
  return(agg1)
  rm(agg1, medianVal, labelUnder, labelOver, analysis)
  
}

zip85 <- rbind(
  zipInc(analysis17, 85000, 2017), 
  zipInc(analysis18, 85000, 2018), 
  zipInc(analysis19, 85000, 2019), 
  zipInc(analysis20, 85000, 2020),
  zipInc(analysis21, 85000, 2021), 
  zipInc(analysis22, 85000, 2022), 
  zipInc(analysis23, 85000, 2023), 
  zipInc(analysis24, 85000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $85,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)

zip65 <- rbind(
  zipInc(analysis17, 65000, 2017), 
  zipInc(analysis18, 65000, 2018), 
  zipInc(analysis19, 65000, 2019), 
  zipInc(analysis20, 65000, 2020),
  zipInc(analysis21, 65000, 2021), 
  zipInc(analysis22, 65000, 2022), 
  zipInc(analysis23, 65000, 2023), 
  zipInc(analysis24, 65000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $65,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)

zip45 <- rbind(
  zipInc(analysis17, 45000, 2017), 
  zipInc(analysis18, 45000, 2018), 
  zipInc(analysis19, 45000, 2019), 
  zipInc(analysis20, 45000, 2020),
  zipInc(analysis21, 45000, 2021), 
  zipInc(analysis22, 45000, 2022), 
  zipInc(analysis23, 45000, 2023), 
  zipInc(analysis24, 45000, 2024)
) %>% pivot_wider(
  id_cols=c(`Year`, `Average household income under threshold`), 
  names_from=`Over median completion rate`, 
  values_from=`Count`
) %>% filter(
  `Average household income under threshold`=="Under $45,000"
) %>% mutate(
  `Share over median` = `Over median` / (`Over median` + `Under median`)
)

#### End #### 

#### High schools in high-poverty ZIPs ####

highPovZips <- census %>% select(
  `ZCTA5`,
  `Share of population in poverty`, 
  `Share of children in poverty`
) 

highPovFunction <- function(merge0, povVal, yearVal){
  
  merge1 <- merge0 %>% mutate(
    `ZCTA5` = paste("ZCTA5 ", `LZIP`, sep="")
  )
    
  merge1 <- left_join(x=merge1, y=highPovZips, by="ZCTA5")
  
  merge1 <- merge1 %>% filter(
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) %>% mutate(
    `FAFSA completion rate` = `Completions` / `Grade 12 students`
  )
  
  # Pick one of these: 
  medianVal <- sum(merge1$`Completions`) / sum(merge1$`Grade 12 students`)
  # medianVal <- median(merge1$`FAFSA completion rate`, na.rm=TRUE)
  
  merge1 <- merge1 %>% mutate(
    `Completion rate above median` = ifelse(`FAFSA completion rate` >= medianVal, "Over FAFSA median", "Under FAFSA median"), 
    `Poverty rate over threshold` = ifelse(`Share of population in poverty` >= povVal, "Over poverty rate threshold", "Under poverty rate threshold"), 
    # `Count` = rep(1)
    `Count` = `Grade 12 students`
  )
  
  agg1 <- aggregate(
    data=merge1, `Count` ~ `Completion rate above median` + `Poverty rate over threshold`, FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Poverty rate over threshold`), 
    names_from=`Completion rate above median`, 
    values_from=`Count`
  ) %>% mutate(
    `Share over FAFSA median` = `Over FAFSA median` / (`Over FAFSA median` + `Under FAFSA median`), 
    `Year` = rep(yearVal)
  )
  
  return(agg1)
  rm(agg1, merge1, medianVal)
  
}

highSchoolPov <- rbind(
  highPovFunction(merge17, 0.25, 2017), 
  highPovFunction(merge18, 0.25, 2018), 
  highPovFunction(merge19, 0.25, 2019), 
  highPovFunction(merge20, 0.25, 2020), 
  highPovFunction(merge21, 0.25, 2021), 
  highPovFunction(merge22, 0.25, 2022), 
  highPovFunction(merge23, 0.25, 2023), 
  highPovFunction(merge24, 0.25, 2024) 
) %>% filter(
  `Poverty rate over threshold`=="Over poverty rate threshold"
)

#### End #### 

#### Share of students at high schools in the bottom 20% by poverty that are in the bottom 20% by FAFSA completion #### 

highPovZips <- census %>% select(
  `ZCTA5`,
  `Share of population in poverty`, 
  `Share of children in poverty`
) 

twentyPov <- function(merge0, yearVal){
  
  merge1 <- merge0 %>% mutate(
    `ZCTA5` = paste("ZCTA5 ", `LZIP`, sep="")
  )
  
  merge1 <- left_join(x=merge1, y=highPovZips, by="ZCTA5")
  
  merge1 <- merge1 %>% filter(
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) %>% mutate(
    `FAFSA completion rate` = `Completions` / `Grade 12 students`
  )
  
  merge1 <- merge1 %>% mutate(
    `Quintile: FAFSA completion rate` = ntile(`FAFSA completion rate`, 5), 
    `Quintile: Share of population in poverty` = ntile(`Share of population in poverty`, 5),
    # `Count` = rep(1)
    `Count` = `Grade 12 students`
  )
  
  agg1 <- aggregate(
    data=merge1, `Count` ~ `Quintile: Share of population in poverty` + `Quintile: FAFSA completion rate`, FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Quintile: Share of population in poverty`), 
    names_from=`Quintile: FAFSA completion rate`, 
    values_from=`Count`
  ) %>% filter(
    `Quintile: Share of population in poverty`==5
  ) %>% mutate(
    `Total` = `1` + `2` + `3` + `4` + `5`
  ) %>% mutate(
    `Share in bottom 20% by FAFSA` = `1` / `Total`, 
    `Share in top 20% by FAFSA` = `5` / `Total`
  ) %>% select(
    `Quintile: Share of population in poverty`, 
    `Share in bottom 20% by FAFSA`, 
    `Share in top 20% by FAFSA`
  ) %>% mutate(
    `Year` = rep(yearVal)
  )
  
  return(agg1)
  rm(agg1, merge1)
  
}

twentyTest <- rbind(
  twentyPov(merge17, 2017), 
  twentyPov(merge18, 2018), 
  twentyPov(merge19, 2019), 
  twentyPov(merge20, 2020), 
  twentyPov(merge21, 2021), 
  twentyPov(merge22, 2022), 
  twentyPov(merge23, 2023), 
  twentyPov(merge24, 2024)
)

#### End #### 

#### Bottom 20% of high schools' share of all FAFSAs ####

highPovZips <- census %>% select(
  `ZCTA5`,
  `Share of population in poverty`, 
  `Share of children in poverty`
) 

twentyShare <- function(merge0, yearVal){
  
  merge1 <- merge0 %>% mutate(
    `ZCTA5` = paste("ZCTA5 ", `LZIP`, sep="")
  )
  
  merge1 <- left_join(x=merge1, y=highPovZips, by="ZCTA5")
  
  merge1 <- merge1 %>% filter(
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) 
  
  merge1 <- merge1 %>% mutate(
    `Quintile: Share of population in poverty` = ntile(`Share of population in poverty`, 5)
  )
  
  agg1 <- aggregate(
    data=merge1, cbind(`Completions`, `Grade 12 students`) ~ `Quintile: Share of population in poverty`, FUN=sum
  ) 
  totalFAFSA <- sum(agg1$Completions)
  totalSeniors <- sum(agg1$`Grade 12 students`)
  agg1 <- agg1 %>% mutate(
    `Share of FAFSAs` = `Completions` / `totalFAFSA`, 
    `Share of seniors` = `Grade 12 students` / `totalSeniors`
  # ) %>% filter(
  #   `Quintile: Share of population in poverty` == 5
  ) %>% select(
    `Quintile: Share of population in poverty`,
    `Share of FAFSAs`, 
    `Share of seniors`
  ) %>% mutate(
    `Year` = rep(yearVal)
  )
  
  return(agg1)
  rm(totalFAFSA, totalSeniors, agg1, merge1)
  
}

twentyShareTest <- rbind(
  twentyShare(merge17, 2017), 
  twentyShare(merge18, 2018), 
  twentyShare(merge19, 2019), 
  twentyShare(merge20, 2020), 
  twentyShare(merge21, 2021), 
  twentyShare(merge22, 2022), 
  twentyShare(merge23, 2023), 
  twentyShare(merge24, 2024)
) %>% arrange(
  `Quintile: Share of population in poverty`, `Year`
) %>% mutate(
  `Index` = `Share of FAFSAs` / `Share of seniors`
)

#### End #### 

#### Expected vs. actual FAFSA completions ####

highPovZips <- census %>% select(
  `ZCTA5`,
  `Share of population in poverty`, 
  `Share of children in poverty`
) 

expectedActual <- function(merge0, yearVal){
  
  merge1 <- merge0 %>% mutate(
    `ZCTA5` = paste("ZCTA5 ", `LZIP`, sep="")
  )
  
  merge1 <- left_join(x=merge1, y=highPovZips, by="ZCTA5")
  
  merge1 <- merge1 %>% filter(
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) 
  
  overallRate <- sum(merge1$`Completions`, na.rm=TRUE) / sum(merge1$`Grade 12 students`, na.rm=TRUE)
  
  merge1 <- merge1 %>% mutate(
    `Quintile: Share of population in poverty` = ntile(`Share of population in poverty`, 5)
  )
  
  agg1 <- aggregate(
    data=merge1, cbind(`Completions`, `Grade 12 students`) ~ `Quintile: Share of population in poverty`, FUN=sum
  ) %>% mutate(
    `Expected FAFSAs` = `Grade 12 students` * overallRate,
    `Actual FAFSAs` = `Completions`
  ) %>% mutate(
    `Delta` = `Actual FAFSAs` - `Expected FAFSAs`
  ) %>% mutate(
    `Delta per 10,000 students` = (`Delta` / `Grade 12 students`) * 10000, 
    `Year` = rep(yearVal)
  ) 
  
  return(agg1)
  rm(agg1, overallRate, merge1)
}

expectedActualTest <- rbind(
  expectedActual(merge17, 2017), 
  expectedActual(merge18, 2018), 
  expectedActual(merge19, 2019), 
  expectedActual(merge20, 2020), 
  expectedActual(merge21, 2021), 
  expectedActual(merge22, 2022), 
  expectedActual(merge23, 2023), 
  expectedActual(merge24, 2024)
) %>% arrange(
  `Quintile: Share of population in poverty`, `Year`
)

plot1 <- expectedActualTest %>% filter(
  `Quintile: Share of population in poverty` == 5
) %>% select(
  `Year`,
  `Expected FAFSAs`, 
  `Actual FAFSAs`
) %>% pivot_longer(
  cols=c(`Expected FAFSAs`, `Actual FAFSAs`),
  names_to="Measure", 
  values_to="Number of FAFSAs"
)
plot1$`Measure`[plot1$`Measure`=="Expected FAFSAs"] <- "Expected FAFSAs Based on National Completion Rate"
plot1$`Measure`[plot1$`Measure`=="Actual FAFSAs"] <- "Actual FAFSAs, High-Poverty Communities"
ggplot(data=plot1, mapping=aes(x=Year, y=`Number of FAFSAs`, group=`Measure`, color=`Measure`)) + geom_point(size=2.5) + geom_line(linewidth=1.5) + scale_y_continuous(limits=c(200000, 350000), labels=comma) + theme(legend.position = "bottom") + labs(y="Number of FAFSA completions", color="") + scale_color_manual(values=c("#8DAFCA", "blue4")) + guides(color = guide_legend(nrow = 2))

#### End #### 

#### Charts on income, attainment, and race ####

quintilePlot(quintiles.national, "Groups: Average household income", FALSE, TRUE, c("Class of 2017", "Class of 2023"))
quintilePlot(quintiles.national, "Groups: Share of population in poverty", FALSE, TRUE, c("Class of 2017", "Class of 2023"))
quintilePlot(quintiles.national, "Groups: No college share", FALSE, TRUE, c("Class of 2017", "Class of 2023"))
quintilePlot(quintiles.national, "Groups: Black or Latino share", FALSE, TRUE, c("Class of 2017", "Class of 2023"))

#### End #### 

#### [Alternative methods] Share of high schools in the bottom 20% by poverty that are in the bottom 20% by FAFSA completion #### 

highPovZips <- census %>% select(
  `ZCTA5`,
  `Share of population in poverty`, 
  `Share of children in poverty`
) 

twentyPov2 <- function(merge0, yearVal, seniorThreshold){
  
  merge1 <- merge0 %>% mutate(
    `ZCTA5` = paste("ZCTA5 ", `LZIP`, sep="")
  )
  
  merge1 <- left_join(x=merge1, y=highPovZips, by="ZCTA5")
  
  merge1 <- merge1 %>% filter(
    is.na(`Completions`)==FALSE, 
    is.na(`Grade 12 students`)==FALSE
  ) %>% mutate(
    `FAFSA completion rate` = `Completions` / `Grade 12 students`
  ) %>% filter(
    `Grade 12 students` >= seniorThreshold
  )
  
  merge1 <- merge1 %>% mutate(
    `Quintile: FAFSA completion rate` = ntile(`FAFSA completion rate`, 5), 
    `Quintile: Share of population in poverty` = ntile(`Share of population in poverty`, 5),
    `Count` = rep(1)
    # `Count` = `Grade 12 students`
  )
  
  agg1 <- aggregate(
    data=merge1, `Count` ~ `Quintile: Share of population in poverty` + `Quintile: FAFSA completion rate`, FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Quintile: Share of population in poverty`), 
    names_from=`Quintile: FAFSA completion rate`, 
    values_from=`Count`
  ) %>% filter(
    `Quintile: Share of population in poverty`==5
  ) %>% mutate(
    `Total` = `1` + `2` + `3` + `4` + `5`
  ) %>% mutate(
    `Share in bottom 20% by FAFSA` = `1` / `Total`, 
    `Share in top 20% by FAFSA` = `5` / `Total`
  ) %>% select(
    `Quintile: Share of population in poverty`, 
    `Share in bottom 20% by FAFSA`, 
    `Share in top 20% by FAFSA`, 
    `Total`
  ) %>% mutate(
    `Year` = rep(yearVal)
  )
  
  return(agg1)
  rm(agg1, merge1)
  
}

twentyTest2 <- rbind(
  twentyPov2(merge17, 2017, 30), 
  twentyPov2(merge18, 2018, 30), 
  twentyPov2(merge19, 2019, 30), 
  twentyPov2(merge20, 2020, 30), 
  twentyPov2(merge21, 2021, 30), 
  twentyPov2(merge22, 2022, 30), 
  twentyPov2(merge23, 2023, 30), 
  twentyPov2(merge24, 2024, 30)
)

#### End #### 

