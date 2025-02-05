
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
library(kableExtra)

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

########################################
#### Set up demographic dataset     ####
########################################

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
  fafsa$`Completions`[fafsa$`Completions`=="<5"] <- "0"
  
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
    print(paste("The number of school codes with duplicate schools is ", length(problemCodes), ".", sep=""))
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
  print(paste("The number of problem schools is ", nrow(comboCount), ".", sep=""))
  
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

########################################
#### Table 1: Change in completions ####
#### (Public schools only)          ####
########################################

#### Write function ####

runTable1 <- function(fafsa0, state0, year1, year2){
  
  fafsa1 <- fafsa0 %>% filter(
    `Year` %in% c(year1, year2) 
  ) %>% mutate(
    `State Category` = ifelse(
      `State` == state0, "Selected State", "Rest of U.S."
    )
  ) 
  fafsa1 <- aggregate(
    data=fafsa1, `Completions` ~ `State Category` + `Year`, FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`State Category`), 
    names_from=`Year`, 
    values_from=`Completions`
  ) 
  names(fafsa1)[2] <- "Before policy"
  names(fafsa1)[3] <- "After policy"
  
  fafsa1 <- fafsa1 %>% mutate(
    `Change` = (`After policy` - `Before policy`) / `Before policy`
  )
  
  baselineVal <- fafsa1$Change[1] 
  fafsa1 <- fafsa1 %>% mutate(
    `Over baseline` = `Change` - baselineVal
  )
  
  return(fafsa1)
  rm(fafsa1, baselineVal)
  
}

#### End #### 

#### Merge data #### 

t1publics17 <- fafsa17 %>% mutate(`Year` = "Class of 2017")
t1publics18 <- fafsa18 %>% mutate(`Year` = "Class of 2018")
t1publics19 <- fafsa19 %>% mutate(`Year` = "Class of 2019")
t1publics20 <- fafsa20 %>% mutate(`Year` = "Class of 2020")
t1publics21 <- fafsa21 %>% mutate(`Year` = "Class of 2021")
t1publics22 <- fafsa22 %>% mutate(`Year` = "Class of 2022")
t1publics23 <- fafsa23 %>% mutate(`Year` = "Class of 2023")
t1publics24 <- fafsa24 %>% mutate(`Year` = "Class of 2024")

t1publics <- rbind(
  t1publics17,
  t1publics18,
  t1publics19,
  t1publics20,
  t1publics21,
  t1publics22,
  t1publics23,
  t1publics24
)

rm(
  t1publics17,
  t1publics18,
  t1publics19,
  t1publics20,
  t1publics21,
  t1publics22,
  t1publics23,
  t1publics24
)

#### End #### 

#### Find results #### 

t1.LA <- runTable1(t1publics, "LA", "Class of 2017", "Class of 2018")
t1.IL <- runTable1(t1publics, "IL", "Class of 2020", "Class of 2021")
t1.AL <- runTable1(t1publics, "AL", "Class of 2021", "Class of 2022")
t1.TX <- runTable1(t1publics, "TX", "Class of 2021", "Class of 2022")
t1.IN <- runTable1(t1publics, "IN", "Class of 2023", "Class of 2024")
t1.NH <- runTable1(t1publics, "NH", "Class of 2023", "Class of 2024")
t1.CA <- runTable1(t1publics, "CA", "Class of 2022", "Class of 2023")
t1.CO <- runTable1(t1publics, "CO", "Class of 2021", "Class of 2022")
t1.MD <- runTable1(t1publics, "MD", "Class of 2022", "Class of 2023")

rm(t1publics)

#### End #### 

########################################
#### Finish high school merge       ####
########################################

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
    `Submissions` = numeric(), 
    `Completions` = numeric(), 
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

#### Merge data ####

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

processEnrollment <- function(seniors){
  seniors <- seniors %>% select(
    `NCESSCH`, 
    `LEAID`,
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
    `Grade 12 students` = `Grade 12 students` * (4165201 / 3917352) # A correction to match completion rates with NCAN's tracker 
  )
  return(seniors)
}

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

merge24 <- left_join(
  x=merge24, 
  y=processEnrollment(read.csv("ccd_sch_052_2324.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge23 <- left_join(
  x=merge23, 
  y=processEnrollment(read.csv("ccd_sch_052_2223.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge22 <- left_join(
  x=merge22, 
  y=processEnrollment(read.csv("ccd_sch_052_2122.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge21 <- left_join(
  x=merge21, 
  y=processEnrollment(read.csv("ccd_sch_052_2021.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge20 <- left_join(
  x=merge20, 
  y=processEnrollment(read.csv("ccd_sch_052_1920.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge19 <- left_join(
  x=merge19, 
  y=processEnrollment(read.csv("ccd_sch_052_1819.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge18 <- left_join(
  x=merge18, 
  y=processEnrollment(read.csv("ccd_sch_052_1718.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)
merge17 <- left_join(
  x=merge17, 
  y=processEnrollment(read.csv("ccd_sch_052_1617.csv", header=TRUE)), 
  by="NCESSCH-ELSI"
) %>% filter(is.na(`Grade 12 students`)==FALSE)

#### End #### 

#### Save high school-level dataset ####

addYear <- function(tempDF, year0){
  
  tempDF <- tempDF %>% mutate(
    `Year` = rep(year0)
  )
  return(tempDF)
  
}

mergeAll <- rbind(
  addYear(merge24, "Class of 2024"), 
  addYear(merge23, "Class of 2023"), 
  addYear(merge22, "Class of 2022"), 
  addYear(merge21, "Class of 2021"), 
  addYear(merge20, "Class of 2020"), 
  addYear(merge19, "Class of 2019"), 
  addYear(merge18, "Class of 2018"), 
  addYear(merge17, "Class of 2017")
)

rm(addYear)

# altMethods requires LEAID: 
altMethods <- mergeAll

# The rest do not: 
mergeAll <- mergeAll %>% select(-(`LEAID`))
calSoap <- mergeAll

#### End #### 

########################################
#### Demographic analysis           ####
########################################

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
# rm(test)

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
  
  for(i in (3:ncol(rankInputs))){
    
    tempTiles <- rankInputs %>% select(`ZCTA5`, names(rankInputs)[i])
    names(tempTiles)[2] <- "InterestVar"
    tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
    names(tempTiles)[2] <- paste("Groups: ", names(rankInputs)[i], sep="")
    if(i == 3){
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
    if(tilesVar=="Groups: No college share"){
      newVar <- "Groups: Share of adults without a college degree"
    }
    if(tilesVar=="Groups: Black or Latino share"){
      newVar <- "Groups: Share of pop. that is Black or Latino"
    }
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
    analysis1$`For Tooltip` <- paste(
      "State: ", gsub("Selected state: ", "", analysis1$State), '\n',
      "Quintile of demographic variable: ", analysis1$`Tile`, '\n',
      "Class: ", analysis1$`Class`, '\n',
      "FAFSA Completion Rate: ", percent(analysis1$`FAFSA completion rate`, accuracy=0.1),
      sep=""
    )
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`, text=`For Tooltip`)) + geom_bar(stat="identity", position = "dodge2") + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
    
  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    if(tilesVar=="Groups: No college share"){
      newVar <- "Groups: Share of adults without a college degree"
    }
    if(tilesVar=="Groups: Black or Latino share"){
      newVar <- "Groups: Share of pop. that is Black or Latino"
    }
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
    analysis1$`For Tooltip` <- paste(
      "Quintile of demographic variable: ", analysis1$`Tile`, '\n',
      "Class: ", analysis1$`Class`, '\n',
      "FAFSA Completion Rate: ", percent(analysis1$`FAFSA completion rate`, accuracy=0.1),
      sep=""
    )
    figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`, text=`For Tooltip`)) + geom_bar(stat="identity", position = "dodge2") + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.7)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
  }
  
  return(figA)
  rm(analysis1, newTiles, figA, newVar)
}

#### End #### 

#### Write function to make table of Q1 and Q5 #### 

onefiveTable <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
  
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
    if(tilesVar=="Groups: No college share"){
      newVar <- "Groups: Share of adults without a college degree"
    }
    if(tilesVar=="Groups: Black or Latino share"){
      newVar <- "Groups: Share of pop. that is Black or Latino"
    }
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
    analysis1 <- analysis1 %>% pivot_wider(
      id_cols=c(`Class`, `State`), 
      names_from=`Tile`, 
      values_from=`FAFSA completion rate`
    ) 
    analysis1 <- analysis1 %>% mutate(
      `Percentage point difference` = `1st` - `5th`
    )
    analysis1 <- analysis1 %>% arrange(
      `State`, `Class`
    ) %>% mutate(
      `Demographic` = rep(newVar),
      `1st` = percent(`1st`, accuracy=0.1), 
      `5th` = percent(`5th`, accuracy=0.1), 
      `Percentage point difference` = ifelse(
        `Percentage point difference` >= 0, 
        paste("+", round(`Percentage point difference` * 100, 2), " pp", sep=""), 
        paste("-", round(`Percentage point difference` * 100, 2), " pp", sep="")
      )
    ) %>% rename(
      `FAFSA completion rate, 1st quintile of demographic` = `1st`, 
      `FAFSA completion rate, 5th quintile of demographic` = `5th`
    ) %>% select(
      `Demographic`, 
      `State`, 
      `Class`, 
      `FAFSA completion rate, 1st quintile of demographic`, 
      `FAFSA completion rate, 5th quintile of demographic`,
      `Percentage point difference`
    )
  }else{
    newTiles <- newTiles %>% select(
      `Class`,
      `Completions`, 
      `Grade 12 students`,
      all_of(tilesVar)
    ) %>% rename(
      `InterestVar` = tilesVar
    ) 
    if(tilesVar=="Groups: No college share"){
      newVar <- "Groups: Share of adults without a college degree"
    }
    if(tilesVar=="Groups: Black or Latino share"){
      newVar <- "Groups: Share of pop. that is Black or Latino"
    }
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
    analysis1 <- analysis1 %>% pivot_wider(
      id_cols=c(`Class`), 
      names_from=`Tile`, 
      values_from=`FAFSA completion rate`
    ) 
    analysis1 <- analysis1 %>% mutate(
      `Percentage point difference` = `1st` - `5th`
    )
    analysis1 <- analysis1 %>% arrange(
      `Class`
    ) %>% mutate(
      `Demographic` = rep(newVar),
      `1st` = percent(`1st`, accuracy=0.1), 
      `5th` = percent(`5th`, accuracy=0.1), 
      `Percentage point difference` = ifelse(
        `Percentage point difference` >= 0, 
        paste("+", round(`Percentage point difference` * 100, 2), " pp", sep=""), 
        paste("-", round(`Percentage point difference` * 100, 2), " pp", sep="")
      )
    ) %>% rename(
      `FAFSA completion rate, 1st quintile of demographic` = `1st`, 
      `FAFSA completion rate, 5th quintile of demographic` = `5th`
    ) %>% select(
      `Demographic`, 
      `Class`, 
      `FAFSA completion rate, 1st quintile of demographic`, 
      `FAFSA completion rate, 5th quintile of demographic`,
      `Percentage point difference`
    )
  }
  
  return(analysis1)
  rm(analysis1, newTiles, newVar)
}

#### End #### 

#### Quintiles: Poverty ####

qLA1 <- quintilePlot(quintiles.louisiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

qIL1 <- quintilePlot(quintiles.illinois, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2020", "Class of 2021"))

qAL1 <- quintilePlot(quintiles.alabama, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))

qTX1 <- quintilePlot(quintiles.texas, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

qCA1 <- quintilePlot(quintiles.california, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

qIN1 <- quintilePlot(quintiles.indiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

qNH1 <- quintilePlot(quintiles.newhampshire, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

#### End #### 

#### Quintiles: Degree attainment ####

qLA2 <- quintilePlot(quintiles.louisiana, "Groups: No college share", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

qIL2 <- quintilePlot(quintiles.illinois, "Groups: No college share", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 

qAL2 <- quintilePlot(quintiles.alabama, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

qTX2 <- quintilePlot(quintiles.texas, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

qCA2 <- quintilePlot(quintiles.california, "Groups: No college share", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

qIN2 <- quintilePlot(quintiles.indiana, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

qNH2 <- quintilePlot(quintiles.newhampshire, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

#### End ####

#### Quintiles: Race and ethincity ####

qLA3 <- quintilePlot(quintiles.louisiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

qIL3 <- quintilePlot(quintiles.illinois, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 

qAL3 <- quintilePlot(quintiles.alabama, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

qTX3 <- quintilePlot(quintiles.texas, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

qCA3 <- quintilePlot(quintiles.california, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

qIN3 <- quintilePlot(quintiles.indiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

qNH3 <- quintilePlot(quintiles.newhampshire, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

#### End #### 

#### One-five table: Poverty ####

xLA1 <- onefiveTable(quintiles.louisiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

xIL1 <- onefiveTable(quintiles.illinois, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2020", "Class of 2021"))

xAL1 <- onefiveTable(quintiles.alabama, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022"))

xTX1 <- onefiveTable(quintiles.texas, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

xCA1 <- onefiveTable(quintiles.california, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

xIN1 <- onefiveTable(quintiles.indiana, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

xNH1 <- onefiveTable(quintiles.newhampshire, "Groups: Share of population in poverty", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

#### End #### 

#### One-five table: Degree attainment ####

xLA2 <- onefiveTable(quintiles.louisiana, "Groups: No college share", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

xIL2 <- onefiveTable(quintiles.illinois, "Groups: No college share", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 

xAL2 <- onefiveTable(quintiles.alabama, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

xTX2 <- onefiveTable(quintiles.texas, "Groups: No college share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

xCA2 <- onefiveTable(quintiles.california, "Groups: No college share", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

xIN2 <- onefiveTable(quintiles.indiana, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

xNH2 <- onefiveTable(quintiles.newhampshire, "Groups: No college share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

#### End ####

#### One-five table: Race and ethincity ####

xLA3 <- onefiveTable(quintiles.louisiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 

xIL3 <- onefiveTable(quintiles.illinois, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 

xAL3 <- onefiveTable(quintiles.alabama, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

xTX3 <- onefiveTable(quintiles.texas, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 

xCA3 <- onefiveTable(quintiles.california, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 

xIN3 <- onefiveTable(quintiles.indiana, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 

xNH3 <- onefiveTable(quintiles.newhampshire, "Groups: Black or Latino share", TRUE, TRUE, c("Class of 2023", "Class of 2024"))

#### End #### 

########################################
#### For commentary with Bill       ####
########################################

# #### Charts on income, attainment, and race ####
# 
# quintilePlot(quintiles.national, "Groups: Average household income", FALSE, TRUE, c("Class of 2017", "Class of 2023")) + geom_text(aes(label=percent(`FAFSA completion rate`, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.55, size=3.5)
# quintilePlot(quintiles.national, "Groups: Share of population in poverty", FALSE, TRUE, c("Class of 2017", "Class of 2023")) + geom_text(aes(label=percent(`FAFSA completion rate`, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.55, size=3.5)
# quintilePlot(quintiles.national, "Groups: No college share", FALSE, TRUE, c("Class of 2017", "Class of 2023")) + geom_text(aes(label=percent(`FAFSA completion rate`, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.55, size=3.5)
# quintilePlot(quintiles.national, "Groups: Black or Latino share", FALSE, TRUE, c("Class of 2017", "Class of 2023")) + geom_text(aes(label=percent(`FAFSA completion rate`, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.55, size=3.5)
# 
# #### End #### 

########################################
#### By pre-policy completion rate  ####
########################################

#### Write function ####

processT2 <- function(data0, state0, year1, year2, measurement){
  
  data1 <- data0 %>% filter(
    `State` == state0,
    `Year` %in% c(year1, year2)
  ) %>% mutate(
    `Year` = ifelse(
      `Year`==year1, "Before policy", ifelse(
        `Year`==year2, "After policy", NA)
    )
  ) 
  
  data1 <- aggregate(data=data1, cbind(`Completions`, `Grade 12 students`) ~ `NCESSCH-ELSI` + `Year`, FUN=sum)
  
  data1.students <- data1 %>% pivot_wider(
    id_cols=c(`NCESSCH-ELSI`), 
    names_from=`Year`, 
    values_from=`Grade 12 students`
  ) %>% rename(
    `Grade 12 students before policy` = `Before policy`,
    `Grade 12 students after policy` = `After policy`
  )
  data1.completions <- data1 %>% pivot_wider(
    id_cols=c(`NCESSCH-ELSI`), 
    names_from=`Year`, 
    values_from=`Completions`
  ) %>% rename(
    `Completions before policy` = `Before policy`,
    `Completions after policy` = `After policy`
  )
  data1 <- full_join(x=data1.students, y=data1.completions, by="NCESSCH-ELSI")
  rm(data1.students, data1.completions)
  
  print(paste("Number of rows before removing those with NAs: ", nrow(data1)))
  data1 <- data1 %>% filter(
    is.na(`Grade 12 students before policy`)==FALSE,
    is.na(`Grade 12 students after policy`)==FALSE,
    is.na(`Completions after policy`)==FALSE, 
    is.na(`Completions before policy`)==FALSE
  )
  print(paste("Number of rows after removing those with NAs: ", nrow(data1)))
  
  data1 <- data1 %>% mutate(
    `Completion rate before policy` = `Completions before policy` / `Grade 12 students before policy`
  ) %>% mutate(
    `Completion rate bracket` = ifelse(
      between(`Completion rate before policy`, 0, 0.45), "0% to 45%", ifelse(
        between(`Completion rate before policy`, 0.45, 0.55), "45% to 55%", ifelse(
          between(`Completion rate before policy`, 0.55, 0.65), "55% to 65%", ifelse(
            between(`Completion rate before policy`, 0.65, 1), "65% to 100%", NA
          ) 
        )
      )
    )
  )
  
  if(measurement=="absolute"){
    
    data1 <- aggregate(
      data=data1, cbind(`Completions before policy`, `Completions after policy`, `Grade 12 students before policy`, `Grade 12 students after policy`) ~ `Completion rate bracket`, FUN=sum
    ) %>% mutate(
      `Completion rate before policy` = `Completions before policy` / `Grade 12 students before policy`, 
      `Completion rate after policy` = `Completions after policy` / `Grade 12 students after policy`
    ) %>% mutate(
      `Change in completion rate` = (`Completion rate after policy` - `Completion rate before policy`)
    )
    
    return(data1)
    rm(data1)
    
  }else{
    
    data1 <- aggregate(
      data=data1, cbind(`Completions before policy`, `Completions after policy`) ~ `Completion rate bracket`, FUN=sum
    ) %>% mutate(
      `Change in completions` = (`Completions after policy` - `Completions before policy`) / `Completions before policy`
    )
    
    return(data1)
    rm(data1)
  }
}

#### End #### 

#### Run function for absolute changes ####

t2abs.LA <- processT2(
  data0=mergeAll, 
  state0="LA", 
  year1="Class of 2017", 
  year2="Class of 2018", 
  measurement="absolute"
)
t2abs.IL <- processT2(
  data0=mergeAll, 
  state0="IL", 
  year1="Class of 2020", 
  year2="Class of 2021", 
  measurement="absolute"
)
t2abs.AL <- processT2(
  data0=mergeAll, 
  state0="AL", 
  year1="Class of 2021", 
  year2="Class of 2022", 
  measurement="absolute"
)
t2abs.TX <- processT2(
  data0=mergeAll, 
  state0="TX", 
  year1="Class of 2021", 
  year2="Class of 2022", 
  measurement="absolute"
)
t2abs.CA <- processT2(
  data0=mergeAll, 
  state0="CA", 
  year1="Class of 2022", 
  year2="Class of 2023", 
  measurement="absolute"
)
t2abs.IN <- processT2(
  data0=mergeAll, 
  state0="IN", 
  year1="Class of 2023", 
  year2="Class of 2024", 
  measurement="absolute"
)
t2abs.NH <- processT2(
  data0=mergeAll, 
  state0="NH", 
  year1="Class of 2023", 
  year2="Class of 2024", 
  measurement="absolute"
)

#### End #### 

#### Run function for relative changes ####

t2rel.LA <- processT2(
  data0=mergeAll, 
  state0="LA", 
  year1="Class of 2017", 
  year2="Class of 2018", 
  measurement="relative"
)
t2rel.IL <- processT2(
  data0=mergeAll, 
  state0="IL", 
  year1="Class of 2020", 
  year2="Class of 2021", 
  measurement="relative"
)
t2rel.AL <- processT2(
  data0=mergeAll, 
  state0="AL", 
  year1="Class of 2021", 
  year2="Class of 2022", 
  measurement="relative"
)
t2rel.TX <- processT2(
  data0=mergeAll, 
  state0="TX", 
  year1="Class of 2021", 
  year2="Class of 2022", 
  measurement="relative"
)
t2rel.CA <- processT2(
  data0=mergeAll, 
  state0="CA", 
  year1="Class of 2022", 
  year2="Class of 2023", 
  measurement="relative"
)
t2rel.IN <- processT2(
  data0=mergeAll, 
  state0="IN", 
  year1="Class of 2023", 
  year2="Class of 2024", 
  measurement="relative"
)
t2rel.NH <- processT2(
  data0=mergeAll, 
  state0="NH", 
  year1="Class of 2023", 
  year2="Class of 2024", 
  measurement="relative"
)

#### End #### 

########################################
#### Pell change data               ####
########################################

#### Load Pell data ####

processPell <- function(filename, year0){
  
  setwd("/Users/peter_granville/FAFSA-2024/Pell data")
  
  tempDF <- read_excel(filename, sheet="Award Year Summary", col_names=FALSE, skip=7) %>% select(`...1`, `...2`, `...3`, `...5`, `...6`, `...7`)
  
  names(tempDF) <- c("OPEID", "Name", "State", "Control", "Recipients", "Disbursements")
  
  tempDF$`Year` <- rep(year0, nrow(tempDF))
  
  return(tempDF)
  rm(tempDF)
  
}

suppressWarnings({
  pell <- rbind(
    processPell("grants-ay17-18-q4.xls", "2017-18"),
    processPell("grants-ay18-19-q4.xls", "2018-19"),
    processPell("grants-ay19-20-q4.xls", "2019-20"),
    processPell("grants-ay20-21-q4.xls", "2020-21"),
    processPell("grants-ay21-22-q4.xls", "2021-22"),
    processPell("grants-ay22-23-q4.xls", "2022-23"), 
    processPell("grants-ay23-24-q4.xls", "2023-24")
  ) %>% mutate(
    `Disbursements` = as.numeric(`Disbursements`)
  ) %>% mutate(
    `Recipients` = as.numeric(`Recipients`)
  ) %>% filter(
    (`State` %in% c("AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE
  )
})

pell.disbursements <- pell %>% pivot_wider(
  id_cols=c(`OPEID`, `Name`, `State`, `Control`), 
  names_from=`Year`, 
  values_from=`Disbursements`
)
pell.recipients <- pell %>% pivot_wider(
  id_cols=c(`OPEID`, `Name`, `State`, `Control`), 
  names_from=`Year`, 
  values_from=`Recipients`
)

#### End #### 

#### Load Scorecard data ####

setwd("/Users/peter_granville/FAFSA-2024/Scorecard data")

scorecard <- read.csv(
  "Most-Recent-Cohorts-Institution-Sep2024.csv", header=TRUE
) %>% select(
  `OPEID`,
  `MAIN`,
  `CONTROL`,
  `PREDDEG`,
  `HBCU`,
  `PBI`,
  `ANNHI`,
  `TRIBAL`,
  `AANAPII`,
  `HSI`,
  `NANTI`
) %>% arrange(
  desc(`MAIN`)
) %>% filter(
  duplicated(`OPEID`)==FALSE
) %>% select(
  -(`MAIN`)
) %>% mutate(
  `MSI` = ifelse(
    (`HBCU`==1) | (`PBI`==1) | (`ANNHI`==1) | (`TRIBAL`==1) | (`AANAPII`==1) | (`HSI`==1) | (`NANTI`==1), 
    1, 0
  )
) %>% filter(
  `OPEID` > 0
) %>% mutate(
  `OPEID` = as.character(`OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==1, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==2, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==3, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==4, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==5, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==6, paste("0", `OPEID`, sep=""), `OPEID`)
) %>% mutate(
  `OPEID` = ifelse(nchar(`OPEID`)==7, paste("0", `OPEID`, sep=""), `OPEID`)
)

#### End #### 

#### Write function ####

processPell <- function(data0, state0, year1, year2, interestVar, printTotals){
  
  setwd("/Users/peter_granville/FAFSA-2024/IPEDS data")
  
  data1 <- data0 
  
  data1[is.na(data1)] <- 0
  data1 <- aggregate(data=data1, cbind(
    `2017-18`, `2018-19`, `2019-20`, `2020-21`, `2021-22`, `2022-23`, `2023-24`
  ) ~ `OPEID` + `Name` + `State`, FUN=sum)
  
  data1 <- left_join(x=data1, y=scorecard, by="OPEID")
  
  data1 <- data1 %>% mutate(
    `State Category` = ifelse(
      `State`==state0, "Selected State", "Rest of U.S."
    )
  ) %>% select(
    `OPEID`, 
    `CONTROL`,
    `PREDDEG`,
    `HBCU`,
    `PBI`,
    `ANNHI`,
    `TRIBAL`,
    `AANAPII`,
    `HSI`,
    `NANTI`,
    `MSI`,
    `Name`, 
    `State`, 
    `State Category`,
    all_of(year1), 
    all_of(year2)
  )
  names(data1)[15] <- "Before policy"
  names(data1)[16] <- "After policy"
  
  if(interestVar == "CONTROL"){data1 <- data1 %>% mutate(`Variable` = `CONTROL`)}
  if(interestVar == "PREDDEG"){data1 <- data1 %>% mutate(`Variable` = `PREDDEG`)}
  if(interestVar == "HBCU"){data1 <- data1 %>% mutate(`Variable` = `HBCU`)}
  if(interestVar == "PBI"){data1 <- data1 %>% mutate(`Variable` = `PBI`)}
  if(interestVar == "ANNHI"){data1 <- data1 %>% mutate(`Variable` = `ANNHI`)}
  if(interestVar == "TRIBAL"){data1 <- data1 %>% mutate(`Variable` = `TRIBAL`)}
  if(interestVar == "AANAPII"){data1 <- data1 %>% mutate(`Variable` = `AANAPII`)}
  if(interestVar == "HSI"){data1 <- data1 %>% mutate(`Variable` = `HSI`)}
  if(interestVar == "NANTI"){data1 <- data1 %>% mutate(`Variable` = `NANTI`)}
  if(interestVar == "MSI"){data1 <- data1 %>% mutate(`Variable` = `MSI`)}
  if(interestVar == "None"){data1 <- data1 %>% mutate(`Variable` = rep("All"))}
  
  data1 <- aggregate(data=data1, cbind(
    `Before policy`, `After policy`
  ) ~ `State Category` + `Variable`, FUN=sum) %>% mutate(
    `Percentage change` = (`After policy` - `Before policy`) / `Before policy`
  ) 
  
  if(printTotals==TRUE){
    data1 <- data1 %>% mutate(
      `Selected state` = rep(state0)
    )
    return(data1)
  }else{
    data1 <- data1 %>% pivot_wider(
      id_cols=c(`Variable`), 
      names_from=`State Category`, 
      values_from=`Percentage change`
    ) %>% mutate(
      `Performance over rest of U.S.` = `Selected State` - `Rest of U.S.`, 
      `Variable name` = rep(interestVar), 
      `Selected state` = rep(state0), 
      `Comparison window` = rep(paste(year1, " to ", year2, sep=""))
    )
    
    return(data1)
  }
  rm(data1)
  
}

#### End #### 

#### Run data ####

recipientsPull <- rbind(
  
  # Overall
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "None", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "None", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "None", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "None", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "None", printTotals=FALSE),
  
  # CONTROL	1	Public
  # CONTROL	2	Private not-for-profit
  # CONTROL	3	Private for-profit
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "CONTROL", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "CONTROL", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "CONTROL", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "CONTROL", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "CONTROL", printTotals=FALSE),
  
  #   PREDDEG 0	Not classified
  #   PREDDEG 1	Predominantly certificate-degree granting
  #   PREDDEG 2	Predominantly associate's-degree granting
  #   PREDDEG 3	Predominantly bachelor's-degree granting
  #   PREDDEG 4	Entirely graduate-degree granting
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "PREDDEG", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "PREDDEG", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "PREDDEG", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "PREDDEG", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "PREDDEG", printTotals=FALSE), 
  
  # HBCU 0	No
  # HBCU 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "HBCU", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "HBCU", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "HBCU", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "HBCU", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "HBCU", printTotals=FALSE), 
  
  # PBI 0	No
  # PBI 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "PBI", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "PBI", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "PBI", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "PBI", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "PBI", printTotals=FALSE), 
  
  # ANNHI 0	No
  # ANNHI 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "ANNHI", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "ANNHI", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "ANNHI", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "ANNHI", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "ANNHI", printTotals=FALSE), 
  
  # TRIBAL 0	No
  # TRIBAL 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "TRIBAL", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "TRIBAL", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "TRIBAL", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "TRIBAL", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "TRIBAL", printTotals=FALSE), 
  
  # AANAPII 0	No
  # AANAPII 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "AANAPII", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "AANAPII", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "AANAPII", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "AANAPII", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "AANAPII", printTotals=FALSE), 
  
  # HSI 0	No
  # HSI 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "HSI", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "HSI", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "HSI", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "HSI", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "HSI", printTotals=FALSE), 
  
  # NANTI 0	No
  # NANTI 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "NANTI", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "NANTI", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "NANTI", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "NANTI", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "NANTI", printTotals=FALSE), 
  
  # MSI 0	No
  # MSI 1	Yes
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "MSI", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "MSI", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "MSI", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "MSI", printTotals=FALSE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "MSI", printTotals=FALSE) 
  
)

disbursementsPull <- rbind(
  
  # Overall
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "None", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "None", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "None", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "None", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "None", printTotals=FALSE),
  
  # CONTROL	1	Public
  # CONTROL	2	Private not-for-profit
  # CONTROL	3	Private for-profit
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "CONTROL", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "CONTROL", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "CONTROL", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "CONTROL", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "CONTROL", printTotals=FALSE),
  
  #   PREDDEG 0	Not classified
  #   PREDDEG 1	Predominantly certificate-degree granting
  #   PREDDEG 2	Predominantly associate's-degree granting
  #   PREDDEG 3	Predominantly bachelor's-degree granting
  #   PREDDEG 4	Entirely graduate-degree granting
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "PREDDEG", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "PREDDEG", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "PREDDEG", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "PREDDEG", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "PREDDEG", printTotals=FALSE),
  
  # HBCU 0	No
  # HBCU 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "HBCU", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "HBCU", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "HBCU", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "HBCU", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "HBCU", printTotals=FALSE), 
  
  # PBI 0	No
  # PBI 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "PBI", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "PBI", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "PBI", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "PBI", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "PBI", printTotals=FALSE), 
  
  # ANNHI 0	No
  # ANNHI 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "ANNHI", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "ANNHI", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "ANNHI", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "ANNHI", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "ANNHI", printTotals=FALSE), 
  
  # TRIBAL 0	No
  # TRIBAL 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "TRIBAL", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "TRIBAL", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "TRIBAL", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "TRIBAL", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "TRIBAL", printTotals=FALSE), 
  
  # AANAPII 0	No
  # AANAPII 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "AANAPII", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "AANAPII", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "AANAPII", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "AANAPII", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "AANAPII", printTotals=FALSE), 
  
  # HSI 0	No
  # HSI 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "HSI", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "HSI", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "HSI", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "HSI", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "HSI", printTotals=FALSE), 
  
  # NANTI 0	No
  # NANTI 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "NANTI", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "NANTI", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "NANTI", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "NANTI", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "NANTI", printTotals=FALSE), 
  
  # MSI 0	No
  # MSI 1	Yes
  processPell(pell.disbursements, "LA", "2017-18", "2018-19", "MSI", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2021-22", "MSI", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2022-23", "MSI", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2022-23", "MSI", printTotals=FALSE),
  processPell(pell.disbursements, "CA", "2022-23", "2023-24", "MSI", printTotals=FALSE)
  
)

#### End #### 

#### Capture totals ####

totalsPull <- rbind(
  processPell(pell.recipients, "LA", "2017-18", "2018-19", "None", printTotals=TRUE),
  processPell(pell.recipients, "IL", "2020-21", "2021-22", "None", printTotals=TRUE),
  processPell(pell.recipients, "AL", "2021-22", "2022-23", "None", printTotals=TRUE),
  processPell(pell.recipients, "TX", "2021-22", "2022-23", "None", printTotals=TRUE),
  processPell(pell.recipients, "CA", "2022-23", "2023-24", "None", printTotals=TRUE)
)

#### End #### 

#### Take largest possible window ####

bigWindowR <- rbind(
  processPell(pell.recipients, "LA", "2017-18", "2023-24", "None", printTotals=FALSE),
  processPell(pell.recipients, "IL", "2020-21", "2023-24", "None", printTotals=FALSE),
  processPell(pell.recipients, "AL", "2021-22", "2023-24", "None", printTotals=FALSE),
  processPell(pell.recipients, "TX", "2021-22", "2023-24", "None", printTotals=FALSE)
)

bigWindowD <- rbind(
  processPell(pell.disbursements, "LA", "2017-18", "2023-24", "None", printTotals=FALSE),
  processPell(pell.disbursements, "IL", "2020-21", "2023-24", "None", printTotals=FALSE),
  processPell(pell.disbursements, "AL", "2021-22", "2023-24", "None", printTotals=FALSE),
  processPell(pell.disbursements, "TX", "2021-22", "2023-24", "None", printTotals=FALSE)
)

#### End #### 

########################################
#### Enrollment change data         ####
########################################

#### Load enrollment data ####

processEnrollment <- function(filename, year0){
  
  setwd("/Users/peter_granville/FAFSA-2024/IPEDS data")
  
  tempDF <- read.csv(filename, header=TRUE)
  
  tempDF <- tempDF %>% select(
    `UNITID`, 
    `EFALEVEL`, 
    `EFTOTLT`, 
    `EFTOTLM`, 
    `EFTOTLW`, 
    `EFAIANT`, 
    `EFASIAT`, 
    `EFBKAAT`, 
    `EFHISPT`, 
    `EFNHPIT`, 
    `EFWHITT`, 
    `EF2MORT`
  ) %>% filter(
    `EFALEVEL` %in% c(
      4, #	All students, Undergraduate, Degree/certificate-seeking, First-time
      24, #	Full-time students, Undergraduate, Degree/certificate-seeking, First-time
      44 #	Part-time students, Undergraduate, Degree/certificate-seeking, First-time
    )
  ) %>% mutate(
    `Year` = rep(year0)
  )
  
  return(tempDF)
  rm(tempDF)
  
}

enrollment <- rbind(
  processEnrollment("ef2017a_rv.csv", 2017), 
  processEnrollment("ef2018a_rv.csv", 2018), 
  processEnrollment("ef2019a_rv.csv", 2019), 
  processEnrollment("ef2020a_rv.csv", 2020), 
  processEnrollment("ef2021a_rv.csv", 2021), 
  processEnrollment("ef2022a_rv.csv", 2022), 
  processEnrollment("ef2023a.csv", 2023)
)

#### End ####

#### Load Scorecard data #### 

setwd("/Users/peter_granville/FAFSA-2024/Scorecard data")

importScorecard <- read.csv(
  "Most-Recent-Cohorts-Institution-Sep2024.csv", header=TRUE
) %>% select(
  `UNITID`,
  `STABBR`,
  `CONTROL`,
  `PREDDEG`,
  `PCTPELL`,
  `HBCU`,
  `PBI`,
  `ANNHI`,
  `TRIBAL`,
  `AANAPII`,
  `HSI`,
  `NANTI`
) %>% mutate(
  `MSI` = ifelse(
    (`HBCU`==1) | (`PBI`==1) | (`ANNHI`==1) | (`TRIBAL`==1) | (`AANAPII`==1) | (`HSI`==1) | (`NANTI`==1), 
    1, 0
  )
) %>% mutate(
  `PCTPELL` = ifelse(
    between(`PCTPELL`, 0, 0.3), "Low Pell", ifelse(
      between(`PCTPELL`, 0.3, 0.6), "Medium Pell", ifelse(
        between(`PCTPELL`, 0.6, 1), "High Pell", NA
      )
    )
  )
)
enrollment <- left_join(x=enrollment, y=importScorecard, by="UNITID")
rm("importScorecard")

#### End #### 

#### Write enrollment change function ####

processEnrollment <- function(data0, state0, year1, year2, students, demographic, interestVar, printTotals){
  
  data1 <- data0 
  
  data1 <- data1 %>% filter(
    `Year` %in% c(year1, year2)
  ) %>% mutate(
    `Year` = ifelse(
      `Year`==year1, "Before policy", "After policy"
    )
  ) %>% mutate(
    `State Category` = ifelse(
      `STABBR`==state0, "Selected State", "Rest of U.S."
    )
  ) 
  
  if(students == "First-time undergraduates"){data1 <- data1 %>% filter(`EFALEVEL` == 4)}
  if(students == "Full-time first-time undergraduates"){data1 <- data1 %>% filter(`EFALEVEL` == 24)}
  if(students == "Part-time first-time undergraduates"){data1 <- data1 %>% filter(`EFALEVEL` == 44)}
  if(students %in% c(
    "First-time undergraduates", 
    "Full-time first-time undergraduates",
    "Part-time first-time undergraduates"
  )==FALSE){data1 <- data1 %>% filter(`EFALEVEL` == 9999999)}
  
  if(demographic == "TOTL"){data1 <- data1 %>% mutate(`Total students` = `EFTOTLT`)}
  if(demographic == "AIAN"){data1 <- data1 %>% mutate(`Total students` = `EFAIANT`)}
  if(demographic == "ASIA"){data1 <- data1 %>% mutate(`Total students` = `EFASIAT`)}
  if(demographic == "BKAA"){data1 <- data1 %>% mutate(`Total students` = `EFBKAAT`)}
  if(demographic == "HISP"){data1 <- data1 %>% mutate(`Total students` = `EFHISPT`)}
  if(demographic == "NHPI"){data1 <- data1 %>% mutate(`Total students` = `EFNHPIT`)}
  if(demographic == "WHIT"){data1 <- data1 %>% mutate(`Total students` = `EFWHITT`)}
  if(demographic == "2MOR"){data1 <- data1 %>% mutate(`Total students` = `EF2MORT`)}
  
  if(interestVar == "CONTROL"){data1 <- data1 %>% mutate(`Variable` = `CONTROL`)}
  if(interestVar == "PREDDEG"){data1 <- data1 %>% mutate(`Variable` = `PREDDEG`)}
  if(interestVar == "PCTPELL"){data1 <- data1 %>% mutate(`Variable` = `PCTPELL`)}
  if(interestVar == "HBCU"){data1 <- data1 %>% mutate(`Variable` = `HBCU`)}
  if(interestVar == "PBI"){data1 <- data1 %>% mutate(`Variable` = `PBI`)}
  if(interestVar == "ANNHI"){data1 <- data1 %>% mutate(`Variable` = `ANNHI`)}
  if(interestVar == "TRIBAL"){data1 <- data1 %>% mutate(`Variable` = `TRIBAL`)}
  if(interestVar == "AANAPII"){data1 <- data1 %>% mutate(`Variable` = `AANAPII`)}
  if(interestVar == "HSI"){data1 <- data1 %>% mutate(`Variable` = `HSI`)}
  if(interestVar == "NANTI"){data1 <- data1 %>% mutate(`Variable` = `NANTI`)}
  if(interestVar == "MSI"){data1 <- data1 %>% mutate(`Variable` = `MSI`)}
  if(interestVar == "None"){data1 <- data1 %>% mutate(`Variable` = rep("All"))}
  
  data1 <- aggregate(
    data=data1, `Total students` ~ `State Category` + `Year` + `Variable`, FUN=sum
  ) 
  
  data1$`Year` <- as.factor(data1$`Year`)
  data1$`Year` <- factor(data1$`Year`, levels=c(
    "Before policy", "After policy"
  ))
  
  data1 <- data1 %>% pivot_wider(
    id_cols=c(`State Category`, `Variable`),
    names_from=`Year`, 
    values_from=`Total students`, 
    names_sort=TRUE
  ) %>% mutate(
    `Change` = (`After policy` - `Before policy`) / `Before policy`
  ) 
  
  if(printTotals==TRUE){
    data1 <- data1 %>% mutate(
      `Selected state` = rep(state0), 
      `Comparison years (Fall semester)` = rep(paste(year1, " to ", year2, sep="")), 
      `Student category` = rep(students), 
      `Demographic group` = rep(demographic), 
      `Variable of interest` = rep(interestVar)
    ) 
    return(data1)
  }else{
    data1$`State Category` <- factor(data1$`State Category`, levels=c(
      "Selected State", "Rest of U.S."
    ))
    data1 <- data1 %>% pivot_wider(
      id_cols=c(`Variable`), 
      names_from=`State Category`,
      values_from=`Change`, 
      names_sort=TRUE
    ) %>% mutate(
      `Percentage point difference` = `Selected State` - `Rest of U.S.`
    ) %>% mutate(
      `Selected state` = rep(state0), 
      `Comparison years` = rep(paste(year1, " to ", year2, sep="")), 
      `Student category` = rep(students), 
      `Demographic group` = rep(demographic), 
      `Variable of interest` = rep(interestVar),
      `Enrollment change, selected state` = percent(`Selected State`, accuracy=0.1), 
      `Enrollment change, rest of U.S.` = percent(`Rest of U.S.`, accuracy=0.1), 
      `Percentage point difference` = ifelse(
        `Percentage point difference` >= 0, 
        paste("+", round(`Percentage point difference` * 100, 1), " pp", sep=""), 
        paste("-", round(`Percentage point difference` * 100, 1), " pp", sep="")
      ), 
      `Variable` = as.character(`Variable`)
    )
    return(data1)
  }
  
  rm(data1)
}

#### End #### 

#### Run function: All undergraduates by race/ethnicity ####

# All First-time undergraduates by race/ethnicity: TOTL
enroll.TOTL.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.TOTL.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.TOTL.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.TOTL.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.TOTL.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: ASIA
enroll.ASIA.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=FALSE)
enroll.ASIA.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=FALSE)
enroll.ASIA.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=FALSE)
enroll.ASIA.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=FALSE)
enroll.ASIA.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: BKAA
enroll.BKAA.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=FALSE)
enroll.BKAA.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=FALSE)
enroll.BKAA.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=FALSE)
enroll.BKAA.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=FALSE)
enroll.BKAA.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: HISP
enroll.HISP.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=FALSE)
enroll.HISP.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=FALSE)
enroll.HISP.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=FALSE)
enroll.HISP.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=FALSE)
enroll.HISP.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: AIAN
enroll.AIAN.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=FALSE)
enroll.AIAN.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=FALSE)
enroll.AIAN.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=FALSE)
enroll.AIAN.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=FALSE)
enroll.AIAN.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: NHPI
enroll.NHPI.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=FALSE)
enroll.NHPI.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=FALSE)
enroll.NHPI.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=FALSE)
enroll.NHPI.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=FALSE)
enroll.NHPI.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: WHIT
enroll.WHIT.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=FALSE)
enroll.WHIT.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=FALSE)
enroll.WHIT.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=FALSE)
enroll.WHIT.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=FALSE)
enroll.WHIT.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=FALSE)

# All First-time undergraduates by race/ethnicity: 2MOR
enroll.2MOR.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=FALSE)
enroll.2MOR.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=FALSE)
enroll.2MOR.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=FALSE)
enroll.2MOR.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=FALSE)
enroll.2MOR.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=FALSE)

#### End #### 

#### Run function: Full-time status ####

# Full-time, first-time undergraduate students: TOTL
enroll.FT1TUG.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.FT1TUG.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.FT1TUG.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.FT1TUG.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.FT1TUG.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)

# Part-time, first-time undergraduate students: TOTL
enroll.PT1GUG.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.PT1GUG.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.PT1GUG.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.PT1GUG.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)
enroll.PT1GUG.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=FALSE)

#### End #### 

#### Run function: Pell share of undergraduates ####

# All First-time undergraduates by Pell share: TOTL
enroll.PCTPELL.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=FALSE)
enroll.PCTPELL.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=FALSE)
enroll.PCTPELL.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=FALSE)
enroll.PCTPELL.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=FALSE)
enroll.PCTPELL.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=FALSE)

#### End #### 

#### Run function: Institutional control ####

# All First-time undergraduates by institutional control: TOTL
enroll.CONTROL.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=FALSE)
enroll.CONTROL.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=FALSE)
enroll.CONTROL.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=FALSE)
enroll.CONTROL.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=FALSE)
enroll.CONTROL.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=FALSE)

#### End #### 

#### Run function: Predominant degree awarded ####

# All First-time undergraduates by predominant degree awarded: TOTL
enroll.PREDDEG.LA <- processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=FALSE) %>% filter(`Variable` %in% c(1, 2, 3))
enroll.PREDDEG.IL <- processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=FALSE) %>% filter(`Variable` %in% c(1, 2, 3))
enroll.PREDDEG.AL <- processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=FALSE) %>% filter(`Variable` %in% c(1, 2, 3))
enroll.PREDDEG.TX <- processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=FALSE) %>% filter(`Variable` %in% c(1, 2, 3))
enroll.PREDDEG.CA <- processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=FALSE) %>% filter(`Variable` %in% c(1, 2, 3))

#### End #### 

#### Run function: Total ####

enroll.totals <- rbind(
  
  # All First-time undergraduates by race/ethnicity: ASIA
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="ASIA", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: BKAA
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="BKAA", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: HISP
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="HISP", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: AIAN
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="AIAN", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: NHPI
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="NHPI", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: WHIT
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="WHIT", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by race/ethnicity: 2MOR
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="2MOR", interestVar="None", printTotals=TRUE),
  
  # First-time undergraduate students: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  
  # Full-time, first-time undergraduate students: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="Full-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  
  # Part-time, first-time undergraduate students: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="Part-time first-time undergraduates", demographic="TOTL", interestVar="None", printTotals=TRUE),
  
  # All First-time undergraduates by Pell share: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="PCTPELL", printTotals=TRUE),
  
  # All First-time undergraduates by institutional control: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=TRUE),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="CONTROL", printTotals=TRUE),
  
  # All First-time undergraduates by predominant degree awarded: TOTL
  processEnrollment(data0=enrollment, state0="LA", year1=2017, year2=2018, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=TRUE) %>% filter(`Variable` %in% c(1, 2, 3)),
  processEnrollment(data0=enrollment, state0="IL", year1=2020, year2=2021, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=TRUE) %>% filter(`Variable` %in% c(1, 2, 3)),
  processEnrollment(data0=enrollment, state0="AL", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=TRUE) %>% filter(`Variable` %in% c(1, 2, 3)),
  processEnrollment(data0=enrollment, state0="TX", year1=2021, year2=2022, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=TRUE) %>% filter(`Variable` %in% c(1, 2, 3)),
  processEnrollment(data0=enrollment, state0="CA", year1=2022, year2=2023, students="First-time undergraduates", demographic="TOTL", interestVar="PREDDEG", printTotals=TRUE) %>% filter(`Variable` %in% c(1, 2, 3))
  
)

#### End #### 

########################################
#### Number of students in a state  ####
#### with an active policy          ####
########################################

#### Calculate numbers ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

active <- read.csv(
  "ELSI_csv_export_6387193472552700715167.csv", skip=6, header=TRUE, nrow=51, check.names=FALSE
) %>% mutate(
  `Mandatory FAFSA 2025` = ifelse(
    `State Name` %in% c(
      "ALABAMA", "CALIFORNIA", "COLORADO", "ILLINOIS", "INDIANA", "MARYLAND", "NEBRASKA", "NEW JERSEY", "NEW YORK", "OKLAHOMA", "TEXAS"
    ), "Mandate", "No Mandate"
  )
)
total.active <- sum(active$`Grade 12 Students [State] 2023-24`)

activeStudents <- aggregate(data=active, `Grade 12 Students [State] 2023-24` ~ `Mandatory FAFSA 2025`, FUN=sum) %>% mutate(`Share of total` = `Grade 12 Students [State] 2023-24` / total.active)

rm(active, total.active)

#### End #### 

########################################
#### CalSOAP analysis               ####
########################################

#### Load school list ####

soapSchools <- data.frame(`Consortium` = character(), `Name` = character(), `City` = character())
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Cabrillo High", `City` = "Lompoc")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Lompoc High", `City` = "Lompoc")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Arroyo Grande High", `City` = "Arroyo Grande")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Nipomo High", `City` = "Nipomo")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Lopez Continuation High", `City` = "Arroyo Grande")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Paso Robles High", `City` = "Paso Robles")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Liberty High (Continuation)", `City` = "Paso Robles")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Santa Maria High", `City` = "Santa Maria")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Pioneer Valley High", `City` = "Santa Maria")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Ernest Righetti High", `City` = "Santa Maria")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Delta High", `City` = "Santa Maria")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Coast Consortium", `Name` = "Santa Ynez Valley High", `City` = "Santa Ynez")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Valley Consortium", `Name` = "Firebaugh High", `City` = "Firebaugh")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Valley Consortium", `Name` = "Mendota High", `City` = "Mendota")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Valley Consortium", `Name` = "Kerman High", `City` = "Kerman")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Central Valley Consortium", `Name` = "Tranquillity High", `City` = "Tranquillity")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Centennial High School", `City` = "Compton")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Compton High", `City` = "Compton")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Compton Early College", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Dominguez High", `City` = "Compton")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Cabrillo High", `City` = "Long Beach")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Jordan High", `City` = "Long Beach")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Lakewood High", `City` = "Lakewood")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Millikan High", `City` = "Long Beach")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Poly and PAAL High School", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Firebaugh High School", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Lynwood High", `City` = "Lynwood")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "John H. Glenn High", `City` = "Norwalk")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "California High", `City` = "Whittier")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "La Serna High", `City` = "Whittier")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Pioneer High", `City` = "Whittier")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Santa Fe High", `City` = "Santa Fe Springs")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Long Beach Consortium", `Name` = "Whittier High", `City` = "Whittier")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "El Rancho High", `City` = "Pico Rivera")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Blair High", `City` = "Pasadena")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "John Muir High", `City` = "Pasadena")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "El Monte High", `City` = "El Monte")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Arroyo High", `City` = "El Monte")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Huntington Park Senior High", `City` = "Huntington Park")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Jordan High School", `City` = "Los Angeles")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Sotomayor Arts and Sciences Magnet", `City` = "Los Angeles")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "South East High", `City` = "South Gate")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "South Gate Senior High", `City` = "South Gate")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Maywood Center for Enriched Studies", `City` = "Maywood")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Santee Education Complex", `City` = "Los Angeles")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Sun Valley Magnet", `City` = "Sun Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Valley Oaks Center for Enriched Studies", `City` = "Sun Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Linda Esperanza Marquez High B LIBRA Academy", `City` = "Huntington Park")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Los Angeles Cal-SOAP", `Name` = "Montebello High", `City` = "Montebello")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Atwater High", `City` = "Atwater")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Buhach Colony High", `City` = "Atwater")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Golden Valley High", `City` = "Merced")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Livingston High", `City` = "Livingston")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Merced High", `City` = "Merced")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Come Back Butte Charter", `City` = "Oroville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Valley Merced Community", `City` = "Merced") # Merced Campus
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Valley Atwater Community School", `City` = "Atwater") # Atwater Campus
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Valley Los Banos Community", `City` = "Los Banos") # Los Banos Campus
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Chowchilla Union High", `City` = "Chowchilla")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Dos Palos High", `City` = "Dos Palos")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "El Capitan High", `City` = "Merced")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Los Banos High", `City` = "Los Banos")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Pacheco High", `City` = "Los Banos")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Le Grand High", `City` = "Le Grand")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Merced Consortium", `Name` = "Hilmar High", `City` = "Hilmar")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Del Norte High", `City` = "Crescent City")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Alder Grove Charter 2", `City` = "Eureka")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Arcata High", `City` = "Arcata")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "McKinleyville High", `City` = "McKinleyville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Six Rivers Charter High", `City` = "Arcata")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Eureka Senior High", `City` = "Eureka")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Fortuna Union High", `City` = "Fortuna")
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "Northern United - Humboldt Charter", `City` = "Eureka") # Name changed: NORTHERN UNITED HUMBOLDT CHARTER SCHOOL
soapSchools <- soapSchools %>% add_row(`Consortium` = "North Coast Consortium", `Name` = "South Fork High", `City` = "Miranda") # Name changed: SOUTH FORK JUNIOR - SENIOR HIGH
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Academy of Innovation", `City` = "Hemet")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Alessandro High", `City` = "Hemet")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Hamilton High", `City` = "Anza")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Hemet High", `City` = "Hemet")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Tahquitz High", `City` = "Hemet")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "West Valley High", `City` = "Hemet")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Canyon Springs High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Moreno Valley High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Valley View High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Vista del Lago High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Bayside Community Day", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "March Mountain High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Murrieta Valley High", `City` = "Murrieta")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Vista Murrieta High", `City` = "Murrieta")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Murrieta Canyon Academy", `City` = "Murrieta")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "California Military Institute", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Heritage High", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Liberty High", `City` = "Winchester")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Paloma Valley High", `City` = "Menifee")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Perris High", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Perris Lake High (Continuation)", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Chaparral High", `City` = "Temecula")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Great Oak High", `City` = "Temecula")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Temecula Valley High", `City` = "Temecula")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Susan H. Nelson", `City` = "Temecula")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Rancho Vista High", `City` = "Temecula")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "San Jacinto High", `City` = "San Jacinto")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Mountain View High", `City` = "San Jacinto")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Mountain Heights Academy", `City` = "San Jacinto")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Citrus Hill High", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Orange Vista High", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Rancho Verde High", `City` = "Moreno Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Riverside County Consortium ", `Name` = "Val Verde High", `City` = "Perris")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Rosemont High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "John F. Kennedy High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Hiram W. Johnson High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Luther Burbank High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Grant Union High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Rio Linda High", `City` = "Rio Linda")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Foothill High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Highlands High", `City` = "North Highlands")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Natomas High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Inderkum High", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Natomas Charter", `City` = "Sacramento")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Westlake High", `City` = "Westlake Village")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Cordova High", `City` = "Rancho Cordova")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Sacramento Consortium", `Name` = "Live Oak High", `City` = "Live Oak")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Apple Valley High", `City` = "Apple Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Granite Hills High", `City` = "Apple Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "High Desert Premier Academy", `City` = "Apple Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Barstow High", `City` = "Barstow")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "BUSD School of Opportunity", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Central High (Continuation)", `City` = "Barstow")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Canyon Ridge High", `City` = "Hesperia")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Hesperia Community Day", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Hesperia High", `City` = "Hesperia")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Mojave High", `City` = "Hesperia")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Oak Hills High", `City` = "Oak Hills")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Shadow Ridge", `City` = "Hesperia")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Sultana High", `City` = "Hesperia")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Black Rock Alternative/Continuation", `City` = "Yucca Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Twentynine Palms High", `City` = "Twentynine Palms")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Yucca Valley High", `City` = "Yucca Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Dr. John H. Milor High Continuation", `City` = "Rialto") # Name Change: MILOR CONTINUATION HIGH
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Eisenhower High", `City` = "Rialto") # Name Change: EISENHOWER SENIOR HIGH
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Rialto High", `City` = "Rialto")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Wilmer Amina Carter High", `City` = "Rialto")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Zupanic Virtual Academy", `City` = "")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Alternative Learning Center", `City` = "")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Anderson", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Arroyo Valley High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Cajon High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Indian Springs High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Middle College High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Pacific High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "San Andreas High", `City` = "Highland")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "San Bernardino City Community Day", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "San Bernardino High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "San Gorgonio High", `City` = "San Bernardino")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Sierra High", `City` = "San Bernardino")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Virtual Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Chaparral High", `City` = "Phelan")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Eagle Summit Community Day", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Serrano High", `City` = "Phelan")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Snowline Academy", `City` = "Phelan")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Hillside High", `City` = "Upland")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Upland High", `City` = "Upland")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Adelanto High", `City` = "Victorville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Cobalt Institute of Math and Science Academy", `City` = "Victorville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Goodwill High", `City` = "Victorville")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Goodwill Independent Study", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Lakeview Leadership Academy", `City` = "Victorville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Silverado High", `City` = "Victorville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "University Preparatory", `City` = "Victorville")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Victor Valley High", `City` = "Victorville")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Victor Valley Virtual Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Green Valley High", `City` = "Yucaipa")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Oak View High School & Education Center", `City` = "Yucaipa")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Bernardino County Consortium", `Name` = "Yucaipa High", `City` = "Yucaipa")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Chaparral High", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "El Cajon Valley High", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "El Capitan High", `City` = "Lakeside")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Elite Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Granite Hills High", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Grossmont High", `City` = "La Mesa")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Grossmont Middle College High", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "IDEA Center", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "MERIT Academy", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Monte Vista High", `City` = "Spring Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Mount Miguel High", `City` = "Spring Valley")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "REACH Academy", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Santana High", `City` = "Santee")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Valhalla High", `City` = "El Cajon")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "West Hills High", `City` = "Santee")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Imperial County Juvenile Hall/Community", `City` = "")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Imperial County Special Education", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Imperial Pathways Charter", `City` = "El Centro")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Valley Academy", `City` = "El Centro")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "El Camino High", `City` = "Oceanside")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Oceanside High", `City` = "Oceanside")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Oceanside Unified School District Adult Transition Program", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Surfside Academy", `City` = "Oceanside")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Surfside High (Continuation)", `City` = "Oceanside")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Monarch", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "North Coastal Consortium Schools", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego County Community", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego County Court", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego County Special Education", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Pasqual Academy", `City` = "Escondido")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "ALBA", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Canyon Hills High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Clairemont High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Crawford High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "East Village Middle College High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Garfield High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Henry High", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Home & Hosp/Transition Support", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Hoover High", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "John Muir Language Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Kearny College Connections", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Kearny Digital Media & Design", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Kearny Eng Innov & Design", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Kearny School of Biomedical Science and Technology", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "La Jolla High", `City` = "La Jolla")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Lincoln High", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Logan Memorial Educational Campus", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Madison High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Mira Mesa High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Mission Bay High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Morse High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Mt. Everest Academy", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Point Loma High", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Riley/New Dawn", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego Metro Career and Tech", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Diego SCPA", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Scripps Ranch High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "TRACE", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Twain High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "University City High", `City` = "San Diego")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Whittier K-12", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Alta Vista Academy", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Bonita Vista Senior High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Castle Park Senior High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Chula Vista Senior High", `City` = "Chula Vista")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "East Hills Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Eastlake High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Hilltop Senior High", `City` = "Chula Vista")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Launch Virtual Academy", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Mar Vista Senior High", `City` = "Imperial Beach")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Montgomery Senior High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Olympian High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Options Secondary", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Otay Ranch Senior High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Palomar High", `City` = "Chula Vista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "San Ysidro High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Southwest Senior High", `City` = "San Diego")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Diego & Imperial Counties Consortium", `Name` = "Sweetwater High", `City` = "National City")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Francisco Consortium", `Name` = "Burton (Phillip and Sala) Aacademic High", `City` = "San Francisco")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Francisco Consortium", `Name` = "Galileo High", `City` = "San Francisco")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Francisco Consortium", `Name` = "Mission High", `City` = "San Francisco")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Francisco Consortium", `Name` = "Marshall (Thurgood) High", `City` = "San Francisco")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Francisco Consortium", `Name` = "Jefferson High", `City` = "Daly City")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Jose Consortium", `Name` = "Andrew P. Hill High", `City` = "San Jose")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Jose Consortium", `Name` = "James Lick High", `City` = "San Jose")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Jose Consortium", `Name` = "William C. Overfelt High", `City` = "San Jose")
soapSchools <- soapSchools %>% add_row(`Consortium` = "San Jose Consortium", `Name` = "Yerba Buena High", `City` = "San Jose")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "Alta Vista Alternative High School", `City` = "Santa Barbara")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "La Cuesta Continuation High", `City` = "Santa Barbara")
# soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "Bishop Garcia Diego High School", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "Dos Pueblos Senior High", `City` = "Goleta")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "Santa Barbara Senior High", `City` = "Santa Barbara")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "Carpinteria Senior High", `City` = "Carpinteria")
soapSchools <- soapSchools %>% add_row(`Consortium` = "Santa Barbara Consortium", `Name` = "San Marcos High", `City` = "San Marcos")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Anzar High", `City` = "San Juan Bautista")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Mt. Madonna High", `City` = "Gilroy")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Christopher High", `City` = "Gilroy")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Gilroy High", `City` = "Gilroy")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Ann Sobrato High", `City` = "Morgan Hill")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Central High School", `City` = "")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Live Oak High School", `City` = "Morgan Hill")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South County Consortium", `Name` = "Hollister High", `City` = "Hollister")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Arvin High", `City` = "Arvin")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Bakersfield High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Centennial High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Del Oro High", `City` = "Loomis")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "East Bakersfield High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Foothill High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Frontier High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Golden Valley High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Highland High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Independence High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Kern Valley High", `City` = "Lake Isabella")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Liberty High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Mira Monte High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "North High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Ridgeview High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Shafter High", `City` = "Shafter")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "South High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Stockdale High", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "West High School", `City` = "Bakersfield")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Frazier Mountain High", `City` = "Lebec")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Maricopa High School", `City` = "Maricopa")
soapSchools <- soapSchools %>% add_row(`Consortium` = "South San Joaquin Consoritum", `Name` = "Taft Union High", `City` = "Taft")

#### End #### 

#### Merge data ####

calSoap <- calSoap %>% filter(
  `State`=="CA"
)
soapSchools <- soapSchools %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)
calSoap <- left_join(x=calSoap, y=soapSchools, by=c("Name", "City"))

calSoap <- calSoap %>% mutate(
  `CalSOAP` = ifelse(is.na(`Consortium`)==FALSE, "CalSOAP participant", "Not a participant")
)

#### End #### 

#### Compare 2022 to 2023 ####

comp1 <- calSoap %>% filter(
  `Year` %in% c("Class of 2022", "Class of 2023"), 
  is.na(`NCESSCH`)==FALSE
) %>% filter(
  (`Consortium` %in% c("San Bernardino County Consortium", "San Diego & Imperial Counties Consortium"))==FALSE
) %>% select(
  `CalSOAP`, 
  `Completions`,
  `Grade 12 students`,
  `Year`, 
  `LZIP`
) 

comp1 <- aggregate(data=comp1, cbind(
  `Completions`, 
  `Grade 12 students`
) ~ `CalSOAP` + `Year`, FUN=sum) %>% mutate(
  `FAFSA completion rate` = `Completions` / `Grade 12 students`
)

#### End #### 

########################################
#### Mixed-status families          ####
########################################

#### Load and process data ####

# See "ACS PUMS Mixed Status FAFSA Families.R" 

setwd("/Users/peter_granville/FAFSA-2024")

agg1723 <- read.csv(
  "ACS-MS agg1723.csv", header=TRUE
) %>% rename(
  `Young adults aged 17 to 23` = `PWGTP`
)
aggG12 <- read.csv(
  "ACS-MS aggG12.csv", header=TRUE
) %>% rename(
  `Grade 12 students` = `PWGTP`
)

aggMS <- full_join(
  x=agg1723, y=aggG12, by="State"
) %>% mutate(
  `Grade 12 students` = ifelse(
    is.na(`Grade 12 students`), 0, `Grade 12 students`
  )
)
rm(agg1723, aggG12)

#### End #### 

########################################
#### Alternative quintiles          #### 
########################################

# #### Format altMethods NCESSCH and Year ####
# 
# altMethods <- altMethods %>% mutate(
#   `NCESSCH-ELSI` = as.numeric(`NCESSCH-ELSI`), 
#   `Year` = as.numeric(substr(`Year`, 10, 13))
# )
# 
# #### End #### 
# 
# #### Write function to load overall school enrollment data ####
# 
# loadTotalEnroll <- function(filename){
#   
#   tempDF <- read.csv(filename, header=TRUE) %>% select(
#     `NCESSCH`,
#     `LEAID`,
#     `STUDENT_COUNT`,
#     `TOTAL_INDICATOR`
#   ) %>% filter(
#     `TOTAL_INDICATOR`=="Derived - Education Unit Total minus Adult Education Count"
#   ) %>% select(
#     -(`TOTAL_INDICATOR`)
#   ) %>% mutate(
#     `STUDENT_COUNT` = as.numeric(`STUDENT_COUNT`)
#   ) %>% rename(
#     `FRPL_DENOM` = `STUDENT_COUNT`, 
#     `NCESSCH-ELSI` = `NCESSCH`
#   )
# 
#   return(tempDF)
#   
# }
# 
# #### End ####
# 
# #### Write function to load FRPL data and merge with enrollment data ####
# 
# prepFRPL <- function(filename.FRPL, filename.Enroll){
#   
#   setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
#   
#   importFRPL <- read.csv(
#     filename.FRPL, header=TRUE, check.names=FALSE
#   ) %>% rename(
#     `NCESSCH-ELSI` = `NCESSCH`
#   )
#   
#   importEnrollment <- loadTotalEnroll(filename.Enroll)
#   
#   importFRPL <- full_join(
#     x=importFRPL, y=importEnrollment, by="NCESSCH-ELSI"
#   ) %>% filter(
#     is.na(`FRPL students`)==FALSE, 
#     is.na(`FRPL_DENOM`)==FALSE, 
#     `FRPL_DENOM` > 0
#   ) 
#   
#   importFRPL <- aggregate(
#     data=importFRPL, cbind(`FRPL students`, `FRPL_DENOM`) ~ `LEAID`, FUN=sum
#   ) %>% mutate(
#     `FRPL share of enrollment` = `FRPL students` / `FRPL_DENOM`
#   ) %>% mutate(
#     `FRPL share of enrollment` = ifelse(is.na(`FRPL share of enrollment`), 0, `FRPL share of enrollment`)
#   )
#   
#   return(importFRPL)
#   rm(importFRPL, importEnrollment)
# }
# 
# #### End ####
# 
# #### Run function #### 
# 
# frpl <- rbind(
#   prepFRPL(filename.FRPL="frpl17.csv", filename.Enroll="ccd_sch_052_1617.csv"), 
#   prepFRPL(filename.FRPL="frpl18.csv", filename.Enroll="ccd_sch_052_1718.csv"), 
#   prepFRPL(filename.FRPL="frpl19.csv", filename.Enroll="ccd_sch_052_1819.csv"), 
#   prepFRPL(filename.FRPL="frpl20.csv", filename.Enroll="ccd_sch_052_1920.csv"), 
#   prepFRPL(filename.FRPL="frpl21.csv", filename.Enroll="ccd_sch_052_2021.csv"), 
#   prepFRPL(filename.FRPL="frpl22.csv", filename.Enroll="ccd_sch_052_2122.csv"), 
#   prepFRPL(filename.FRPL="frpl23.csv", filename.Enroll="ccd_sch_052_2223.csv"), 
#   prepFRPL(filename.FRPL="frpl24.csv", filename.Enroll="ccd_sch_052_2324.csv")
# )
# 
# frplAvg <- aggregate(data=frpl, `FRPL share of enrollment` ~ `LEAID`, FUN=mean)
# 
# frplAvg <- frplAvg %>% mutate(
#   `FRPL share of enrollment` = ifelse(
#     `FRPL share of enrollment` > 1, 1, `FRPL share of enrollment`
#   )
# )
# 
# #### End ####
# 
# #### Write function to load demographics of enrollment ####
# 
# loadDemoEnroll <- function(filename, year0){
#   
#   setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
#   
#   tempDF <- read.csv(filename, header=TRUE) %>% select(
#     `NCESSCH`,
#     `LEAID`,
#     `STATENAME`,
#     `STUDENT_COUNT`,
#     `RACE_ETHNICITY`,
#     `TOTAL_INDICATOR`
#   ) %>% filter(
#     `TOTAL_INDICATOR`=="Derived - Subtotal by Race/Ethnicity and Sex minus Adult Education Count"
#   ) %>% select(
#     -(`TOTAL_INDICATOR`)
#   ) %>% mutate(
#     `STUDENT_COUNT` = as.numeric(`STUDENT_COUNT`)
#   ) 
#   
#   tempDF1 <- aggregate(
#     data=tempDF, `STUDENT_COUNT` ~ `LEAID` + `RACE_ETHNICITY`, FUN=sum
#   ) %>% pivot_wider(
#     id_cols=c(`LEAID`), 
#     names_from=`RACE_ETHNICITY`, 
#     values_from=`STUDENT_COUNT`
#   )
#   tempDF2 <- aggregate(
#     data=tempDF, `STUDENT_COUNT` ~ `LEAID`, FUN=sum
#   ) %>% rename(
#     `RACE_ETH_DENOM` = `STUDENT_COUNT`
#   )
#   
#   tempDF <- full_join(x=tempDF1, y=tempDF2, by="LEAID") %>% filter(
#     is.na(`RACE_ETH_DENOM`)==FALSE,
#     `RACE_ETH_DENOM` > 0
#   ) %>% mutate(
#     `American Indian or Alaska Native` = ifelse(is.na(`American Indian or Alaska Native`), 0, `American Indian or Alaska Native`),
#     `Asian` = ifelse(is.na(`Asian`), 0, `Asian`),
#     `Black or African American` = ifelse(is.na(`Black or African American`), 0, `Black or African American`),
#     `Hispanic/Latino` = ifelse(is.na(`Hispanic/Latino`), 0, `Hispanic/Latino`),
#     `Native Hawaiian or Other Pacific Islander` = ifelse(is.na(`Native Hawaiian or Other Pacific Islander`), 0, `Native Hawaiian or Other Pacific Islander`),
#     `Two or more races` = ifelse(is.na(`Two or more races`), 0, `Two or more races`),
#     `White` = ifelse(is.na(`White`), 0, `White`)
#   ) %>% mutate(
#     `American Indian or Alaska Native share of enrollment` = `American Indian or Alaska Native` / `RACE_ETH_DENOM`, # American Indian or Alaska Native
#     `Asian share of enrollment` = `Asian` / `RACE_ETH_DENOM`, # Asian
#     `Black or African American share of enrollment` = `Black or African American` / `RACE_ETH_DENOM`, # Black or African American
#     `Hispanic/Latino share of enrollment` = `Hispanic/Latino` / `RACE_ETH_DENOM`, # Hispanic/Latino
#     `Native Hawaiian or Other Pacific Islander share of enrollment` = `Native Hawaiian or Other Pacific Islander` / `RACE_ETH_DENOM`, # Native Hawaiian or Other Pacific Islander
#     `Two or more races share of enrollment` = `Two or more races` / `RACE_ETH_DENOM`, # Two or more races
#     `White share of enrollment` = `White` / `RACE_ETH_DENOM`, # White
#     `Year` = rep(year0)
#   ) %>% select(
#     `LEAID`,
#     `American Indian or Alaska Native share of enrollment`, 
#     `Asian share of enrollment`,
#     `Black or African American share of enrollment`,
#     `Hispanic/Latino share of enrollment`,
#     `Native Hawaiian or Other Pacific Islander share of enrollment`,
#     `Two or more races share of enrollment`,
#     `White share of enrollment`,
#     `Year`
#   )
#   
#   return(tempDF)
#   rm(tempDF, tempDF1, tempDF2)
#   
# }
# 
# #### End #### 
# 
# #### Load demographic school enrollment data ####
# 
# demoEnroll <- rbind(
#   loadDemoEnroll("ccd_sch_052_1617.csv", 2017),
#   loadDemoEnroll("ccd_sch_052_1718.csv", 2018),
#   loadDemoEnroll("ccd_sch_052_1819.csv", 2019),
#   loadDemoEnroll("ccd_sch_052_1920.csv", 2020),
#   loadDemoEnroll("ccd_sch_052_2021.csv", 2021),
#   loadDemoEnroll("ccd_sch_052_2122.csv", 2022),
#   loadDemoEnroll("ccd_sch_052_2223.csv", 2023), 
#   loadDemoEnroll("ccd_sch_052_2324.csv", 2024)
# ) 
# 
# demoAvg <- aggregate(data=demoEnroll, cbind(
#   `American Indian or Alaska Native share of enrollment`, 
#   `Asian share of enrollment`,
#   `Black or African American share of enrollment`,
#   `Hispanic/Latino share of enrollment`,
#   `Native Hawaiian or Other Pacific Islander share of enrollment`,
#   `Two or more races share of enrollment`,
#   `White share of enrollment`
# ) ~ `LEAID`, FUN=mean)
# 
# demoAvg <- demoAvg %>% mutate(
#   `American Indian or Alaska Native share of enrollment` = ifelse(`American Indian or Alaska Native share of enrollment` > 1, 1, `American Indian or Alaska Native share of enrollment`),
#   `Asian share of enrollment` = ifelse(`Asian share of enrollment` > 1, 1, `Asian share of enrollment`),
#   `Black or African American share of enrollment` = ifelse(`Black or African American share of enrollment` > 1, 1, `Black or African American share of enrollment`),
#   `Hispanic/Latino share of enrollment` = ifelse(`Hispanic/Latino share of enrollment` > 1, 1, `Hispanic/Latino share of enrollment`),
#   `Native Hawaiian or Other Pacific Islander share of enrollment` = ifelse(`Native Hawaiian or Other Pacific Islander share of enrollment` > 1, 1, `Native Hawaiian or Other Pacific Islander share of enrollment`),
#   `Two or more races share of enrollment` = ifelse(`Two or more races share of enrollment` > 1, 1, `Two or more races share of enrollment`),
#   `White share of enrollment` = ifelse(`White share of enrollment` > 1, 1, `White share of enrollment`)
# )
# 
# #### End #### 
# 
# #### Aggregate altMethods by LEAID and import demographic data ####
# 
# districtData <- aggregate(data=altMethods, cbind(
#   `Completions`, 
#   `Grade 12 students`
# ) ~ `LEAID` + `State` + `Year`, FUN=sum)
# 
# districtData <- left_join(x=districtData, y=frplAvg, by="LEAID")
# districtData <- left_join(x=districtData, y=demoAvg, by="LEAID")
# 
# districtData <- districtData %>% mutate(
#   `Black or Latino share of enrollment` = `Black or African American share of enrollment` + `Hispanic/Latino share of enrollment`
# )
# 
# districtData17 <- districtData %>% filter(`Year`==2017)
# districtData18 <- districtData %>% filter(`Year`==2018)
# districtData19 <- districtData %>% filter(`Year`==2019)
# districtData20 <- districtData %>% filter(`Year`==2020)
# districtData21 <- districtData %>% filter(`Year`==2021)
# districtData22 <- districtData %>% filter(`Year`==2022)
# districtData23 <- districtData %>% filter(`Year`==2023)
# districtData24 <- districtData %>% filter(`Year`==2024)
# 
# #### End #### 
# 
# #### Write function to calculate percentiles ####
# 
# calcPercentiles <- function(analysis, nGroups, year, stateLever, stateSelection, inclusiveExclusive){
#   
#   rankInputs <- analysis %>% select(
#     `LEAID`, 
#     `State`,
#     `FRPL share of enrollment`,
#     `American Indian or Alaska Native share of enrollment`, 
#     `Asian share of enrollment`,
#     `Black or African American share of enrollment`,
#     `Hispanic/Latino share of enrollment`,
#     `Native Hawaiian or Other Pacific Islander share of enrollment`,
#     `Two or more races share of enrollment`,
#     `White share of enrollment`, 
#     `Black or Latino share of enrollment`
#   )
#   rankOutputs <- analysis %>% select(
#     `LEAID`, 
#     `State`,
#     `Grade 12 students`,
#     `Completions`
#   )
#   
#   if(stateLever==TRUE){
#     if(inclusiveExclusive=="Inclusive"){
#       rankInputs <- rankInputs %>% filter(`State` %in% stateSelection)
#       rankOutputs <- rankOutputs %>% filter(`State` %in% stateSelection)
#     }else{
#       rankInputs <- rankInputs %>% filter((`State` %in% stateSelection)==FALSE)
#       rankOutputs <- rankOutputs %>% filter((`State` %in% stateSelection)==FALSE)
#     }
#   }
#   
#   for(i in (3:ncol(rankInputs))){
#     
#     tempTiles <- rankInputs %>% select(`LEAID`, names(rankInputs)[i])
#     names(tempTiles)[2] <- "InterestVar"
#     tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
#     names(tempTiles)[2] <- paste("Groups: ", names(rankInputs)[i], sep="")
#     if(i == 3){
#       nTiles <- tempTiles
#     }else{
#       nTiles <- full_join(x=nTiles, y=tempTiles, by="LEAID")
#     }
#     rm(tempTiles)
#   }
#   rm(i, nGroups)
#   nTiles <- full_join(x=nTiles, y=rankOutputs, by="LEAID") %>% mutate(`Class` = rep(year))
#   
#   if(stateLever==TRUE){
#     if(inclusiveExclusive=="Inclusive"){
#       if(length(stateSelection) == 1){
#         nTiles <- nTiles %>% mutate(`State` = rep(paste("Selected state: ", stateSelection[1], sep="")))
#       }else{
#         nTiles <- nTiles %>% mutate(`State` = rep("Selected states"))
#       }
#     }else{
#       nTiles <- nTiles %>% mutate(`State` = rep("Rest of U.S."))
#     }
#   }
#   
#   return(nTiles)
#   rm(rankInputs, rankOutputs, nTiles)
# }
# 
# #### End #### 
# 
# #### Create quintile datasets #### 
# 
# quintiles.national <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", FALSE, c("None"), "Neither"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", FALSE, c("None"), "Neither") 
# )
# 
# quintiles.louisiana <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("LA"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("LA"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("LA"), "Exclusive") 
# )
# 
# quintiles.illinois <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("IL"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("IL"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("IL"), "Exclusive") 
# )
# 
# quintiles.texas <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("TX"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("TX"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("TX"), "Exclusive") 
# )
# 
# quintiles.alabama <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("AL"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("AL"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("AL"), "Exclusive") 
# )
# 
# quintiles.california <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("CA"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("CA"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("CA"), "Exclusive") 
# )
# 
# quintiles.indiana <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("IN"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("IN"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("IN"), "Exclusive") 
# )
# 
# quintiles.newhampshire <- rbind(
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("NH"), "Inclusive"), 
#   calcPercentiles(districtData24, 5, "Class of 2024", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData23, 5, "Class of 2023", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData22, 5, "Class of 2022", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData21, 5, "Class of 2021", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData20, 5, "Class of 2020", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData19, 5, "Class of 2019", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData18, 5, "Class of 2018", TRUE, c("NH"), "Exclusive"), 
#   calcPercentiles(districtData17, 5, "Class of 2017", TRUE, c("NH"), "Exclusive") 
# )
# 
# #### End #### 
# 
# #### Write function to chart quintiles #### 
# 
# quintilePlot <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
#   
#   newTiles <- tilesDF
#   newVar <- tilesVar
#   
#   if(stateBreakout==TRUE){
#     newTiles <- newTiles %>% select(
#       `Class`,
#       `State`,
#       `Completions`, 
#       `Grade 12 students`,
#       all_of(tilesVar)
#     ) %>% rename(
#       `InterestVar` = tilesVar
#     ) 
#     analysis1 <- aggregate(data=newTiles, cbind(
#       `Completions`, 
#       `Grade 12 students`
#     ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
#       `FAFSA completion rate` = `Completions` / `Grade 12 students`
#     ) 
#     if(yearLever==TRUE){
#       analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
#     }
#     newVar <- gsub("Groups", "Quintile", newVar)
#     analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
#     analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
#     analysis1$State <- factor(analysis1$State)
#     analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
#     analysis1$`For Tooltip` <- paste(
#       "State: ", gsub("Selected state: ", "", analysis1$State), '\n',
#       "Quintile of demographic variable: ", analysis1$`Tile`, '\n',
#       "Class: ", analysis1$`Class`, '\n',
#       "FAFSA Completion Rate: ", percent(analysis1$`FAFSA completion rate`, accuracy=0.1),
#       sep=""
#     )
#     figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`, text=`For Tooltip`)) + geom_bar(stat="identity", position = "dodge2") + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
#     
#   }else{
#     newTiles <- newTiles %>% select(
#       `Class`,
#       `Completions`, 
#       `Grade 12 students`,
#       all_of(tilesVar)
#     ) %>% rename(
#       `InterestVar` = tilesVar
#     ) 
#     analysis1 <- aggregate(data=newTiles, cbind(
#       `Completions`, 
#       `Grade 12 students`
#     ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
#       `FAFSA completion rate` = `Completions` / `Grade 12 students`
#     ) 
#     if(yearLever==TRUE){
#       analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
#     }
#     newVar <- gsub("Groups", "Quintile", newVar)
#     analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
#     analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
#     analysis1$`For Tooltip` <- paste(
#       "Quintile of demographic variable: ", analysis1$`Tile`, '\n',
#       "Class: ", analysis1$`Class`, '\n',
#       "FAFSA Completion Rate: ", percent(analysis1$`FAFSA completion rate`, accuracy=0.1),
#       sep=""
#     )
#     figA <- ggplot(data=analysis1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Tile`, text=`For Tooltip`)) + geom_bar(stat="identity", position = "dodge2") + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.7)) + labs(x="Year", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
#   }
#   
#   return(figA)
#   rm(analysis1, newTiles, figA, newVar)
# }
# 
# #### End #### 
# 
# #### Write function to make table of Q1 and Q5 #### 
# 
# onefiveTable <- function(tilesDF, tilesVar, stateBreakout, yearLever, selectedYears){
#   
#   newTiles <- tilesDF
#   newVar <- tilesVar
#   
#   if(stateBreakout==TRUE){
#     newTiles <- newTiles %>% select(
#       `Class`,
#       `State`,
#       `Completions`, 
#       `Grade 12 students`,
#       all_of(tilesVar)
#     ) %>% rename(
#       `InterestVar` = tilesVar
#     ) 
#     analysis1 <- aggregate(data=newTiles, cbind(
#       `Completions`, 
#       `Grade 12 students`
#     ) ~ `InterestVar` + `State` + `Class`, FUN=sum) %>% mutate(
#       `FAFSA completion rate` = `Completions` / `Grade 12 students`
#     ) 
#     if(yearLever==TRUE){
#       analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
#     }
#     newVar <- gsub("Groups", "Quintile", newVar)
#     analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
#     analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
#     analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "5th"))
#     analysis1$State <- factor(analysis1$State)
#     analysis1$State <- factor(analysis1$State, levels=rev(levels(analysis1$State)))
#     analysis1 <- analysis1 %>% pivot_wider(
#       id_cols=c(`Class`, `State`), 
#       names_from=`Tile`, 
#       values_from=`FAFSA completion rate`
#     ) 
#     analysis1 <- analysis1 %>% mutate(
#       `Percentage point difference` = `1st` - `5th`
#     )
#     analysis1 <- analysis1 %>% arrange(
#       `State`, `Class`
#     ) %>% mutate(
#       `Demographic` = rep(newVar),
#       `1st` = percent(`1st`, accuracy=0.1), 
#       `5th` = percent(`5th`, accuracy=0.1), 
#       `Percentage point difference` = ifelse(
#         `Percentage point difference` >= 0, 
#         paste("+", round(`Percentage point difference` * 100, 2), " pp", sep=""), 
#         paste("-", round(`Percentage point difference` * 100, 2), " pp", sep="")
#       )
#     ) %>% rename(
#       `FAFSA completion rate, 1st quintile of demographic` = `1st`, 
#       `FAFSA completion rate, 5th quintile of demographic` = `5th`
#     ) %>% select(
#       `Demographic`, 
#       `State`, 
#       `Class`, 
#       `FAFSA completion rate, 1st quintile of demographic`, 
#       `FAFSA completion rate, 5th quintile of demographic`,
#       `Percentage point difference`
#     )
#   }else{
#     newTiles <- newTiles %>% select(
#       `Class`,
#       `Completions`, 
#       `Grade 12 students`,
#       all_of(tilesVar)
#     ) %>% rename(
#       `InterestVar` = tilesVar
#     ) 
#     analysis1 <- aggregate(data=newTiles, cbind(
#       `Completions`, 
#       `Grade 12 students`
#     ) ~ `InterestVar` + `Class`, FUN=sum) %>% mutate(
#       `FAFSA completion rate` = `Completions` / `Grade 12 students`
#     ) 
#     if(yearLever==TRUE){
#       analysis1 <- analysis1 %>% filter(`Class` %in% selectedYears)
#     }
#     newVar <- gsub("Groups", "Quintile", newVar)
#     analysis1 <- left_join(x=analysis1, y=quintileDF, by="InterestVar")
#     analysis1$`Tile` <- factor(analysis1$`Tile`, levels=c("1st", "2nd", "3rd", "4th", "5th"))
#     analysis1 <- analysis1 %>% filter(`Tile` %in% c("1st", "5th"))
#     analysis1 <- analysis1 %>% pivot_wider(
#       id_cols=c(`Class`), 
#       names_from=`Tile`, 
#       values_from=`FAFSA completion rate`
#     ) 
#     analysis1 <- analysis1 %>% mutate(
#       `Percentage point difference` = `1st` - `5th`
#     )
#     analysis1 <- analysis1 %>% arrange(
#       `Class`
#     ) %>% mutate(
#       `Demographic` = rep(newVar),
#       `1st` = percent(`1st`, accuracy=0.1), 
#       `5th` = percent(`5th`, accuracy=0.1), 
#       `Percentage point difference` = ifelse(
#         `Percentage point difference` >= 0, 
#         paste("+", round(`Percentage point difference` * 100, 2), " pp", sep=""), 
#         paste("-", round(`Percentage point difference` * 100, 2), " pp", sep="")
#       )
#     ) %>% rename(
#       `FAFSA completion rate, 1st quintile of demographic` = `1st`, 
#       `FAFSA completion rate, 5th quintile of demographic` = `5th`
#     ) %>% select(
#       `Demographic`, 
#       `Class`, 
#       `FAFSA completion rate, 1st quintile of demographic`, 
#       `FAFSA completion rate, 5th quintile of demographic`,
#       `Percentage point difference`
#     )
#   }
#   
#   return(analysis1)
#   rm(analysis1, newTiles, newVar)
# }
# 
# #### End #### 
# 
# #### Quintiles: FRPL share of enrollment ####
# 
# pLA1 <- quintilePlot(quintiles.louisiana, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 
# 
# pIL1 <- quintilePlot(quintiles.illinois, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
# 
# pAL1 <- quintilePlot(quintiles.alabama, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
# 
# pTX1 <- quintilePlot(quintiles.texas, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# pCA1 <- quintilePlot(quintiles.california, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 
# 
# pIN1 <- quintilePlot(quintiles.indiana, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
# 
# pNH1 <- quintilePlot(quintiles.newhampshire, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
# 
# #### End #### 
# 
# #### Quintiles: Black or Latino share of enrollment ####
# 
# pLA2 <- quintilePlot(quintiles.louisiana, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 
# 
# pIL2 <- quintilePlot(quintiles.illinois, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 
# 
# pAL2 <- quintilePlot(quintiles.alabama, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# pTX2 <- quintilePlot(quintiles.texas, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# pCA2 <- quintilePlot(quintiles.california, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 
# 
# pIN2 <- quintilePlot(quintiles.indiana, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 
# 
# pNH2 <- quintilePlot(quintiles.newhampshire, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 
# 
# #### End ####
# 
# #### One-five table: FRPL share of enrollment ####
# 
# yLA1 <- onefiveTable(quintiles.louisiana, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 
# 
# yIL1 <- onefiveTable(quintiles.illinois, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2020", "Class of 2021"))
# 
# yAL1 <- onefiveTable(quintiles.alabama, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022"))
# 
# yTX1 <- onefiveTable(quintiles.texas, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# yCA1 <- onefiveTable(quintiles.california, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 
# 
# yIN1 <- onefiveTable(quintiles.indiana, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
# 
# yNH1 <- onefiveTable(quintiles.newhampshire, "Groups: FRPL share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024"))
# 
# #### End #### 
# 
# #### One-five table: Black or Latino share of enrollment ####
# 
# yLA2 <- onefiveTable(quintiles.louisiana, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2017", "Class of 2018")) 
# 
# yIL2 <- onefiveTable(quintiles.illinois, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2020", "Class of 2021")) 
# 
# yAL2 <- onefiveTable(quintiles.alabama, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# yTX2 <- onefiveTable(quintiles.texas, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2021", "Class of 2022")) 
# 
# yCA2 <- onefiveTable(quintiles.california, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2022", "Class of 2023")) 
# 
# yIN2 <- onefiveTable(quintiles.indiana, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 
# 
# yNH2 <- onefiveTable(quintiles.newhampshire, "Groups: Black or Latino share of enrollment", TRUE, TRUE, c("Class of 2023", "Class of 2024")) 
# 
# #### End ####

########################################
#### Alternative quintiles 2        #### 
########################################

#### Format altMethods NCESSCH and Year ####

altMethods <- altMethods %>% mutate(
  `NCESSCH-ELSI` = as.numeric(`NCESSCH-ELSI`), 
  `Year` = as.numeric(substr(`Year`, 10, 13))
)

#### End #### 

#### Write function to load overall school enrollment data ####

loadTotalEnroll <- function(filename){
  
  tempDF <- read.csv(filename, header=TRUE) %>% select(
    `NCESSCH`,
    `LEAID`,
    `STUDENT_COUNT`,
    `TOTAL_INDICATOR`
  ) %>% filter(
    `TOTAL_INDICATOR`=="Derived - Education Unit Total minus Adult Education Count"
  ) %>% select(
    -(`TOTAL_INDICATOR`)
  ) %>% mutate(
    `STUDENT_COUNT` = as.numeric(`STUDENT_COUNT`)
  ) %>% rename(
    `FRPL_DENOM` = `STUDENT_COUNT`, 
    `NCESSCH-ELSI` = `NCESSCH`
  )
  
  return(tempDF)
  
}

#### End ####

#### Write function to load FRPL data and merge with enrollment data ####

prepFRPL <- function(filename.FRPL, filename.Enroll, year0){
  
  setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
  
  importFRPL <- read.csv(
    filename.FRPL, header=TRUE, check.names=FALSE
  ) %>% rename(
    `NCESSCH-ELSI` = `NCESSCH`
  )
  
  importEnrollment <- loadTotalEnroll(filename.Enroll)
  
  importFRPL <- full_join(
    x=importFRPL, y=importEnrollment, by="NCESSCH-ELSI"
  ) %>% filter(
    is.na(`FRPL students`)==FALSE, 
    is.na(`FRPL_DENOM`)==FALSE, 
    `FRPL_DENOM` > 0
  ) 
  
  importFRPL <- aggregate(
    data=importFRPL, cbind(`FRPL students`, `FRPL_DENOM`) ~ `LEAID`, FUN=sum
  ) %>% mutate(
    `FRPL share of enrollment` = `FRPL students` / `FRPL_DENOM`
  ) %>% mutate(
    `FRPL share of enrollment` = ifelse(is.na(`FRPL share of enrollment`), 0, `FRPL share of enrollment`), 
    `Year` = rep(year0)
  )
  
  return(importFRPL)
  rm(importFRPL, importEnrollment)
}

#### End ####

#### Run function #### 

frpl <- rbind(
  prepFRPL(filename.FRPL="frpl17.csv", filename.Enroll="ccd_sch_052_1617.csv", 2017), 
  prepFRPL(filename.FRPL="frpl18.csv", filename.Enroll="ccd_sch_052_1718.csv", 2018), 
  prepFRPL(filename.FRPL="frpl19.csv", filename.Enroll="ccd_sch_052_1819.csv", 2019), 
  prepFRPL(filename.FRPL="frpl20.csv", filename.Enroll="ccd_sch_052_1920.csv", 2020), 
  prepFRPL(filename.FRPL="frpl21.csv", filename.Enroll="ccd_sch_052_2021.csv", 2021), 
  prepFRPL(filename.FRPL="frpl22.csv", filename.Enroll="ccd_sch_052_2122.csv", 2022), 
  prepFRPL(filename.FRPL="frpl23.csv", filename.Enroll="ccd_sch_052_2223.csv", 2023), 
  prepFRPL(filename.FRPL="frpl24.csv", filename.Enroll="ccd_sch_052_2324.csv", 2024)
)

#### End #### 

#### Write function to load demographics of enrollment ####

loadDemoEnroll <- function(filename, year0){
  
  setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
  
  tempDF <- read.csv(filename, header=TRUE) %>% select(
    `NCESSCH`,
    `LEAID`,
    `STATENAME`,
    `STUDENT_COUNT`,
    `RACE_ETHNICITY`,
    `TOTAL_INDICATOR`
  ) %>% filter(
    `TOTAL_INDICATOR`=="Derived - Subtotal by Race/Ethnicity and Sex minus Adult Education Count"
  ) %>% select(
    -(`TOTAL_INDICATOR`)
  ) %>% mutate(
    `STUDENT_COUNT` = as.numeric(`STUDENT_COUNT`)
  ) 
  
  tempDF1 <- aggregate(
    data=tempDF, `STUDENT_COUNT` ~ `LEAID` + `RACE_ETHNICITY`, FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`LEAID`), 
    names_from=`RACE_ETHNICITY`, 
    values_from=`STUDENT_COUNT`
  )
  tempDF2 <- aggregate(
    data=tempDF, `STUDENT_COUNT` ~ `LEAID`, FUN=sum
  ) %>% rename(
    `RACE_ETH_DENOM` = `STUDENT_COUNT`
  )
  
  tempDF <- full_join(x=tempDF1, y=tempDF2, by="LEAID") %>% filter(
    is.na(`RACE_ETH_DENOM`)==FALSE,
    `RACE_ETH_DENOM` > 0
  ) %>% mutate(
    `American Indian or Alaska Native` = ifelse(is.na(`American Indian or Alaska Native`), 0, `American Indian or Alaska Native`),
    `Asian` = ifelse(is.na(`Asian`), 0, `Asian`),
    `Black or African American` = ifelse(is.na(`Black or African American`), 0, `Black or African American`),
    `Hispanic/Latino` = ifelse(is.na(`Hispanic/Latino`), 0, `Hispanic/Latino`),
    `Native Hawaiian or Other Pacific Islander` = ifelse(is.na(`Native Hawaiian or Other Pacific Islander`), 0, `Native Hawaiian or Other Pacific Islander`),
    `Two or more races` = ifelse(is.na(`Two or more races`), 0, `Two or more races`),
    `White` = ifelse(is.na(`White`), 0, `White`)
  ) %>% mutate(
    `American Indian or Alaska Native share of enrollment` = `American Indian or Alaska Native` / `RACE_ETH_DENOM`, # American Indian or Alaska Native
    `Asian share of enrollment` = `Asian` / `RACE_ETH_DENOM`, # Asian
    `Black or African American share of enrollment` = `Black or African American` / `RACE_ETH_DENOM`, # Black or African American
    `Hispanic/Latino share of enrollment` = `Hispanic/Latino` / `RACE_ETH_DENOM`, # Hispanic/Latino
    `Native Hawaiian or Other Pacific Islander share of enrollment` = `Native Hawaiian or Other Pacific Islander` / `RACE_ETH_DENOM`, # Native Hawaiian or Other Pacific Islander
    `Two or more races share of enrollment` = `Two or more races` / `RACE_ETH_DENOM`, # Two or more races
    `White share of enrollment` = `White` / `RACE_ETH_DENOM`, # White
    `Year` = rep(year0)
  ) %>% select(
    `LEAID`,
    `American Indian or Alaska Native share of enrollment`, 
    `Asian share of enrollment`,
    `Black or African American share of enrollment`,
    `Hispanic/Latino share of enrollment`,
    `Native Hawaiian or Other Pacific Islander share of enrollment`,
    `Two or more races share of enrollment`,
    `White share of enrollment`,
    `Year`
  )
  
  return(tempDF)
  rm(tempDF, tempDF1, tempDF2)
  
}

#### End #### 

#### Load demographic school enrollment data ####

demoEnroll <- rbind(
  loadDemoEnroll("ccd_sch_052_1617.csv", 2017),
  loadDemoEnroll("ccd_sch_052_1718.csv", 2018),
  loadDemoEnroll("ccd_sch_052_1819.csv", 2019),
  loadDemoEnroll("ccd_sch_052_1920.csv", 2020),
  loadDemoEnroll("ccd_sch_052_2021.csv", 2021),
  loadDemoEnroll("ccd_sch_052_2122.csv", 2022),
  loadDemoEnroll("ccd_sch_052_2223.csv", 2023), 
  loadDemoEnroll("ccd_sch_052_2324.csv", 2024)
) 

#### End #### 

#### New function 1 ####

newFunction1 <- function(fafsaData, frplData, demoData, year1, year2, selectedState, selectedVar, calcOption, outputType){

  fafsa1 <- fafsaData %>% select(
    `LEAID`, 
    `Year`, 
    `State`, 
    `Completions`,
    `Grade 12 students`
  ) 
  
  frpl1 <- frplData %>% select(
    `LEAID`,
    `Year`, 
    `FRPL share of enrollment`
  ) 
  
  demo1 <- demoData %>% mutate(
    `Black or Latino share of enrollment` = `Black or African American share of enrollment` + `Hispanic/Latino share of enrollment`
  )
  
  merge1 <- left_join(x=fafsa1, y=frpl1, by=c("LEAID", "Year"))
  merge1 <- left_join(x=merge1, y=demo1, by=c("LEAID", "Year"))
  
  merge1 <- merge1 %>% mutate(
    `State` = ifelse(
      `State`==selectedState, "Selected State", "Rest of U.S."
    )
  )
  
  if(selectedVar=="FRPL share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `FRPL share of enrollment`)}
  if(selectedVar=="American Indian or Alaska Native share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `American Indian or Alaska Native share of enrollment`)}
  if(selectedVar=="Asian share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Asian share of enrollment`)}
  if(selectedVar=="Black or African American share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Black or African American share of enrollment`)}
  if(selectedVar=="Hispanic/Latino share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Hispanic/Latino share of enrollment`)}
  if(selectedVar=="Native Hawaiian or Other Pacific Islander share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Native Hawaiian or Other Pacific Islander share of enrollment`)}
  if(selectedVar=="Two or more races share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Two or more races share of enrollment`)}
  if(selectedVar=="White share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `White share of enrollment`)}
  if(selectedVar=="Black or Latino share of enrollment"){merge1 <- merge1 %>% mutate(`SelectedVar` = `Black or Latino share of enrollment`)}
  
  merge1 <- merge1 %>% select(
    `LEAID`, 
    `Year`, 
    `State`, 
    `Completions`, 
    `Grade 12 students`, 
    `SelectedVar`
  )
  
  if(calcOption=="Option 1"){
    # One quintile per district calculated using every year from 2017 to 2024 
    merge1.inputs <- aggregate(data=merge1, `SelectedVar` ~ `LEAID` + `State`, FUN=mean)
    merge1.inputs.select <- merge1.inputs %>% filter(`State`=="Selected State") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.inputs.others <- merge1.inputs %>% filter(`State`=="Rest of U.S.") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.inputs <- rbind(merge1.inputs.select, merge1.inputs.others) %>% select(`LEAID`, `Quintile`)
    rm(merge1.inputs.select, merge1.inputs.others)
    merge1.outputs <- merge1 %>% filter(`Year` %in% c(year1, year2))
    merge1.outputs <- aggregate(data=merge1.outputs, cbind(`Completions`, `Grade 12 students`) ~ `LEAID` + `State` + `Year`, FUN=sum)
    merge1 <- left_join(x=merge1.outputs, y=merge1.inputs, by="LEAID")
    rm(merge1.inputs, merge1.outputs)
  }
  if(calcOption=="Option 2"){
    # One quintile per district calculated using year1 and year2
    merge1 <- merge1 %>% filter(`Year` %in% c(year1, year2))
    merge1.inputs <- aggregate(data=merge1, `SelectedVar` ~ `LEAID` + `State`, FUN=mean)
    merge1.inputs.select <- merge1.inputs %>% filter(`State`=="Selected State") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.inputs.others <- merge1.inputs %>% filter(`State`=="Rest of U.S.") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.inputs <- rbind(merge1.inputs.select, merge1.inputs.others) %>% select(`LEAID`, `Quintile`)
    rm(merge1.inputs.select, merge1.inputs.others)
    merge1.outputs <- aggregate(data=merge1, cbind(`Completions`, `Grade 12 students`) ~ `LEAID` + `State` + `Year`, FUN=sum)
    merge1 <- left_join(x=merge1.outputs, y=merge1.inputs, by="LEAID")
    rm(merge1.inputs, merge1.outputs)
  }
  if(calcOption=="Option 3"){
    # A separate quintile per year for year1 and year2
    merge1.y1.select <- merge1 %>% filter(`Year`==year1, `State`=="Selected State") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.y1.others <- merge1 %>% filter(`Year`==year1, `State`=="Rest of U.S.") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.y2.select <- merge1 %>% filter(`Year`==year2, `State`=="Selected State") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1.y2.others <- merge1 %>% filter(`Year`==year2, `State`=="Rest of U.S.") %>% mutate(`Quintile` = ntile(`SelectedVar`, 5))
    merge1 <- rbind(merge1.y1.select, merge1.y1.others, merge1.y2.select, merge1.y2.others)
    rm(merge1.y1.select, merge1.y1.others, merge1.y2.select, merge1.y2.others)
  }
  
  agg1 <- aggregate(
    data=merge1, cbind(
      `Completions`, `Grade 12 students`
    ) ~ `Quintile` + `Year` + `State`, FUN=sum
  ) %>% mutate(
    `FAFSA completion rate` = `Completions` / `Grade 12 students`
  ) %>% arrange(
    `State`, `Quintile`, `Year`
  ) %>% mutate(
    `State` = ifelse(
      `State`=="Selected State", paste("Selected state: ", selectedState, sep=""), "Rest of U.S."
    ), 
    `Class` = paste("Class of ", `Year`, sep="")
  )

  newVar <- paste("Quintile: ", selectedVar, sep="")
  
  if(outputType=="Quintile plot"){
    
    agg1 <- agg1 %>% mutate(
      `Quintile` = as.character(`Quintile`)
    )
    agg1$State <- factor(agg1$State)
    agg1$State <- factor(agg1$State, levels=rev(levels(agg1$State)))
    agg1$`For Tooltip` <- paste(
      "State: ", gsub("Selected state: ", "", agg1$State), '\n',
      "Quintile of demographic variable: ", agg1$`Quintile`, '\n',
      "Class: ", agg1$`Class`, '\n',
      "FAFSA Completion Rate: ", percent(agg1$`FAFSA completion rate`, accuracy=0.1),
      sep=""
    )
    figA <- ggplot(data=agg1, mapping=aes(x=`Class`, y=`FAFSA completion rate`, fill=`Quintile`, text=`For Tooltip`)) + geom_bar(stat="identity", position = "dodge2") + facet_grid(`State` ~ .) + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.75)) + labs(x="Class", fill=newVar) + scale_fill_manual(values=colorRampPalette(colors = c("#8DAFCA", "blue4"))(5)) + theme(legend.position='bottom') 
    return(figA)
    rm(figA)
    
  }
  if(outputType=="One-five table"){
    
    agg1 <- agg1 %>% filter(`Quintile` %in% c(1, 5)) %>% mutate(
      `Quintile` = as.character(`Quintile`)
    )
    agg1$State <- factor(agg1$State)
    agg1$State <- factor(agg1$State, levels=rev(levels(agg1$State)))
    agg1 <- agg1 %>% pivot_wider(
      id_cols=c(`Class`, `State`), 
      names_from=`Quintile`, 
      values_from=`FAFSA completion rate`
    ) 
    agg1 <- agg1 %>% mutate(
      `Percentage point difference` = `1` - `5`
    )
    agg1 <- agg1 %>% arrange(
      `State`, `Class`
    ) %>% mutate(
      `Demographic` = rep(newVar),
      `1` = percent(`1`, accuracy=0.1), 
      `5` = percent(`5`, accuracy=0.1), 
      `Percentage point difference` = ifelse(
        `Percentage point difference` >= 0, 
        paste("+", round(`Percentage point difference` * 100, 1), " pp", sep=""), 
        paste("-", round(`Percentage point difference` * 100, 1), " pp", sep="")
      )
    ) %>% rename(
      `FAFSA completion rate, 1st quintile of demographic` = `1`, 
      `FAFSA completion rate, 5th quintile of demographic` = `5`
    ) %>% select(
      `Demographic`, 
      `State`, 
      `Class`, 
      `FAFSA completion rate, 1st quintile of demographic`, 
      `FAFSA completion rate, 5th quintile of demographic`,
      `Percentage point difference`
    )
    return(agg1)
    
  }
  
  rm(fafsa1, frpl1, demo1, merge1, agg1)
}

#### End #### 

#### Run function #### 

# Quintiles: FRPL share of enrollment
selectedVarSetting <- "FRPL share of enrollment"
calcOptionSetting <- "Option 2"
outputTypeSetting <- "Quintile plot"
pLA1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2017, year2 = 2018, selectedState = "LA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pIL1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2020, year2 = 2021, selectedState = "IL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pAL1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "AL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pTX1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "TX", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pCA1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2022, year2 = 2023, selectedState = "CA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pIN1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "IN", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pNH1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "NH", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)

# Quintiles: Black or Latino share of enrollment
selectedVarSetting <- "Black or Latino share of enrollment"
calcOptionSetting <- "Option 2"
outputTypeSetting <- "Quintile plot"
pLA2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2017, year2 = 2018, selectedState = "LA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pIL2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2020, year2 = 2021, selectedState = "IL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pAL2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "AL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pTX2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "TX", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pCA2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2022, year2 = 2023, selectedState = "CA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pIN2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "IN", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
pNH2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "NH", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)

# One-five table: FRPL share of enrollment
selectedVarSetting <- "FRPL share of enrollment"
calcOptionSetting <- "Option 2"
outputTypeSetting <- "One-five table"
yLA1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2017, year2 = 2018, selectedState = "LA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yIL1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2020, year2 = 2021, selectedState = "IL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yAL1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "AL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yTX1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "TX", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yCA1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2022, year2 = 2023, selectedState = "CA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yIN1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "IN", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yNH1 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "NH", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)

# One-five table: Black or Latino share of enrollment
selectedVarSetting <- "Black or Latino share of enrollment"
calcOptionSetting <- "Option 2"
outputTypeSetting <- "One-five table"
yLA2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2017, year2 = 2018, selectedState = "LA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yIL2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2020, year2 = 2021, selectedState = "IL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yAL2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "AL", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yTX2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2021, year2 = 2022, selectedState = "TX", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yCA2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2022, year2 = 2023, selectedState = "CA", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yIN2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "IN", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)
yNH2 <- newFunction1(
  fafsaData = altMethods, frplData = frpl, demoData = demoEnroll, 
  year1 = 2023, year2 = 2024, selectedState = "NH", 
  selectedVar = selectedVarSetting, calcOption = calcOptionSetting, outputType = outputTypeSetting
)

#### End #### 
