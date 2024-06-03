
#### Setup #### 

library(dplyr)
library(scales)
library(readxl)
library(plotly)
library(zipcodeR)
library(geosphere)
library(tidyverse)
library(data.table)
library(stringdist)

#### End #### 

####################################
#### April                      ####
####################################

#### Load in April 30 FAFSA datasets #### 

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")
fafsaData16 <- read_excel("HS_ARCHIVE04302016.xls", skip=3)
fafsaData17 <- read_excel("HS_ARCHIVE04302017.xls", skip=3)
fafsaData18 <- read_excel("HS_ARCHIVE04302018.xls", skip=3)
fafsaData19 <- read_excel("HS_ARCHIVE04302019.xls", skip=3)
fafsaData20 <- read_excel("HS_ARCHIVE04302020.xls", skip=3)
fafsaData21 <- read_excel("HS_ARCHIVE04302021.xls", skip=3)
fafsaData22 <- read_excel("HS_ARCHIVE04302022.xls", skip=3)
fafsaData23 <- read_excel("HS_ARCHIVE04302023.xls", skip=3)

#### End #### 

#### Select variables #### 

schoolcodes <- fafsaData23 %>% select("School Code", "Name", "City", "State") %>% filter(`School Code` != "000") %>% filter(`School Code` != "000000000000")

fafsaData16 <- fafsaData16 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2016") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData17 <- fafsaData17 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData18 <- fafsaData18 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2018") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData19 <- fafsaData19 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2019") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData20 <- fafsaData20 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2020") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData21 <- fafsaData21 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2021") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData22 <- fafsaData22 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2022") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData23 <- fafsaData23 %>% select("Name", "City", "State", "Applications\nComplete\nApr30  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)

names(fafsaData16) <- c("Name", "City", "State", "FAFSA Completions, Class of 2016")
names(fafsaData17) <- c("Name", "City", "State", "FAFSA Completions, Class of 2017")
names(fafsaData18) <- c("Name", "City", "State", "FAFSA Completions, Class of 2018")
names(fafsaData19) <- c("Name", "City", "State", "FAFSA Completions, Class of 2019")
names(fafsaData20) <- c("Name", "City", "State", "FAFSA Completions, Class of 2020")
names(fafsaData21) <- c("Name", "City", "State", "FAFSA Completions, Class of 2021")
names(fafsaData22) <- c("Name", "City", "State", "FAFSA Completions, Class of 2022")
names(fafsaData23) <- c("Name", "City", "State", "FAFSA Completions, Class of 2023")

fafsaData <- full_join(x=schoolcodes, y=fafsaData16, by=c("Name", "City", "State")) 
fafsaData <- full_join(x=fafsaData, y=fafsaData17, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData18, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData19, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData20, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData21, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData22, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData23, by=c("Name", "City", "State"))

rm("fafsaData16", "fafsaData17", "fafsaData18", "fafsaData19", "fafsaData20", "fafsaData21", "fafsaData22", "fafsaData23")

#### End #### 

#### Remove no-FAFSA schools #### 

fafsaData <- fafsaData %>% filter((
  (`FAFSA Completions, Class of 2016` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2017` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2018` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2019` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2020` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2021` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2022` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2023` %in% c("<5", NA))
)==FALSE) 

#### End #### 

#### Turn "<5" to 0s ####

fafsaData$`FAFSA Completions, Class of 2016` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2016`)
fafsaData$`FAFSA Completions, Class of 2017` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2017`)
fafsaData$`FAFSA Completions, Class of 2018` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2018`)
fafsaData$`FAFSA Completions, Class of 2019` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2019`)
fafsaData$`FAFSA Completions, Class of 2020` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2020`)
fafsaData$`FAFSA Completions, Class of 2021` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2021`)
fafsaData$`FAFSA Completions, Class of 2022` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2022`)
fafsaData$`FAFSA Completions, Class of 2023` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2023`)

#### End #### 

#### Conversions and formatting ####

fafsaData <- fafsaData %>% mutate(
  `FAFSA Completions, Class of 2016` = as.numeric(`FAFSA Completions, Class of 2016`), 
  `FAFSA Completions, Class of 2017` = as.numeric(`FAFSA Completions, Class of 2017`), 
  `FAFSA Completions, Class of 2018` = as.numeric(`FAFSA Completions, Class of 2018`), 
  `FAFSA Completions, Class of 2019` = as.numeric(`FAFSA Completions, Class of 2019`), 
  `FAFSA Completions, Class of 2020` = as.numeric(`FAFSA Completions, Class of 2020`), 
  `FAFSA Completions, Class of 2021` = as.numeric(`FAFSA Completions, Class of 2021`), 
  `FAFSA Completions, Class of 2022` = as.numeric(`FAFSA Completions, Class of 2022`), 
  `FAFSA Completions, Class of 2023` = as.numeric(`FAFSA Completions, Class of 2023`)
)

fafsaData <- fafsaData %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

#### End #### 

#### Turn NAs to 0s ####

fafsaData$`FAFSA Completions, Class of 2016`[is.na(fafsaData$`FAFSA Completions, Class of 2016`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2017`[is.na(fafsaData$`FAFSA Completions, Class of 2017`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2018`[is.na(fafsaData$`FAFSA Completions, Class of 2018`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2019`[is.na(fafsaData$`FAFSA Completions, Class of 2019`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2020`[is.na(fafsaData$`FAFSA Completions, Class of 2020`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2021`[is.na(fafsaData$`FAFSA Completions, Class of 2021`)] <- 0
fafsaData$`FAFSA Completions, Class of 2022`[is.na(fafsaData$`FAFSA Completions, Class of 2022`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2023`[is.na(fafsaData$`FAFSA Completions, Class of 2023`)] <- 0

#### End #### 

#### Combining rows with the same school code ####

fafsaData <- fafsaData %>% mutate(`Count` = rep(1))
codeCount <- aggregate(data=fafsaData, `Count` ~ `School Code`, FUN=sum) %>% filter(`Count` > 1)
fafsaData <- fafsaData %>% select(-(`Count`))
problemCodes <- codeCount$`School Code`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- fafsaData %>% filter(`School Code` == problemCodes[i])
  fafsaData <- fafsaData %>% filter(`School Code` != problemCodes[i])
  fafsaData <- fafsaData %>% add_row(
    `School Code` = tempData$`School Code`[1], 
    `Name` = tempData$`Name`[1], 
    `City` = tempData$`City`[1], 
    `State` = tempData$`State`[1], 
    `FAFSA Completions, Class of 2016` = sum(tempData$`FAFSA Completions, Class of 2016`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2017` = sum(tempData$`FAFSA Completions, Class of 2017`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2018` = sum(tempData$`FAFSA Completions, Class of 2018`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2019` = sum(tempData$`FAFSA Completions, Class of 2019`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2020` = sum(tempData$`FAFSA Completions, Class of 2020`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2021` = sum(tempData$`FAFSA Completions, Class of 2021`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2022` = sum(tempData$`FAFSA Completions, Class of 2022`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2023` = sum(tempData$`FAFSA Completions, Class of 2023`, na.rm=TRUE)
  )
  rm(tempData)
  
}
rm(problemCodes, i)

#### End #### 

#### Remove private schools ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
privateSchools20 <- read.csv("ELSI Private Schools 19-20.csv", header=TRUE, skip=6, nrow=21100, check.names=FALSE)
privateSchools18 <- read.csv("ELSI Private Schools 17-18.csv", header=TRUE, skip=6, nrow=22434, check.names=FALSE)
privateSchools16 <- read.csv("ELSI Private Schools 15-16.csv", header=TRUE, skip=6, nrow=21895, check.names=FALSE)
privateSchools14 <- read.csv("ELSI Private Schools 13-14.csv", header=TRUE, skip=6, nrow=24085, check.names=FALSE)
privateSchools12 <- read.csv("ELSI Private Schools 11-12.csv", header=TRUE, skip=6, nrow=26519, check.names=FALSE)
privateSchools10 <- read.csv("ELSI Private Schools 09-10.csv", header=TRUE, skip=6, nrow=27760, check.names=FALSE)
privateSchools08 <- read.csv("ELSI Private Schools 07-08.csv", header=TRUE, skip=6, nrow=28425, check.names=FALSE)

names(privateSchools20) <- c("Name 2019-20", "State 2019-20", "School ID", "City 2019-20", "ZIP 2019-20")
names(privateSchools18) <- c("Name 2017-18", "State 2017-18", "School ID", "City 2017-18", "ZIP 2017-18")
names(privateSchools16) <- c("Name 2015-16", "State 2015-16", "School ID", "City 2015-16", "ZIP 2015-16")
names(privateSchools14) <- c("Name 2013-14", "State 2013-14", "School ID", "City 2013-14", "ZIP 2013-14")
names(privateSchools12) <- c("Name 2011-12", "State 2011-12", "School ID", "City 2011-12", "ZIP 2011-12")
names(privateSchools10) <- c("Name 2009-10", "State 2009-10", "School ID", "City 2009-10", "ZIP 2009-10")
names(privateSchools08) <- c("Name 2007-08", "State 2007-08", "School ID", "City 2007-08", "ZIP 2007-08")
privateSchools <- full_join(x=privateSchools20, y=privateSchools18, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools16, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools14, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools12, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools10, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools08, by="School ID", relationship = "many-to-many")
rm(privateSchools20, privateSchools18, privateSchools16, privateSchools14, privateSchools12, privateSchools10, privateSchools08)
fafsaData <- fafsaData %>% filter((`School Code` %in% privateSchools$`School ID`)==FALSE)

#### End #### 

#### Additional FAFSA cleaning ####

# Remove any other schools with alphabet characters in name (indicating private): 
fafsaData <- fafsaData %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)

# Remove other schools with religious terms in names:  
fafsaData <- fafsaData %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)

# Remove schools we won't be able to link to Census data: 
fafsaData <- fafsaData %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

# Remove virtual schools 
fafsaData <- fafsaData %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)

# Index the FAFSA data: 
fafsaData <- fafsaData %>% mutate(`Index` = (1:nrow(fafsaData))) %>% rename(`NCESSCH` = `School Code`)

#### End #### 

#### Load ELSI data ####

set.seed(111)
setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
elsiData <- read.csv("ccd_sch_029_2223.csv") %>% select(`NCESSCH`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`)
elsiData <- elsiData %>% filter(`G_12_OFFERED` != "No")
elsiData <- elsiData %>% mutate(`NCESSCH` = as.character(`NCESSCH`))
elsiData <- elsiData %>% mutate(`MZIP` = as.character(`MZIP`))
elsiData <- elsiData %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==1, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData <- elsiData %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==2, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData <- elsiData %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==3, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData <- elsiData %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==4, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData <- elsiData %>% mutate(`LZIP` = as.character(`LZIP`))
elsiData <- elsiData %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==1, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData <- elsiData %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==2, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData <- elsiData %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==3, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData <- elsiData %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==4, paste("0", `LZIP`, sep=""), `LZIP`))

#### End #### 

#### Accounting for rows of identical ELSI data ####

elsiData <- elsiData %>% mutate(`ComboName` = paste(`SCH_NAME`, `MCITY`, `ST`, `MZIP`, sep="-")) %>% mutate(`Count` = rep(1))
comboCount <- aggregate(data=elsiData, `Count` ~ `ComboName`, FUN=sum) %>% filter(`Count` > 1)
comboCode <- elsiData %>% select(`NCESSCH`, `ComboName`) %>% filter(`ComboName` %in% comboCount$`ComboName`)
comboCode <- comboCode %>% mutate(`FAFSA Match` = ifelse(`NCESSCH` %in% fafsaData$`NCESSCH`, 1, 0))

for(i in (1:length(unique(comboCount$`ComboName`)))){
  
  selectedSchool <- elsiData %>% filter(`ComboName` == unique(comboCount$`ComboName`)[i])
  elsiData <- elsiData %>% filter(`ComboName` != unique(comboCount$`ComboName`)[i])
  selectedSchool <- selectedSchool %>% mutate(`FAFSA Match` = ifelse(`NCESSCH` %in% fafsaData$`NCESSCH`, 1, 0))
  if(sum(selectedSchool$`FAFSA Match`) > 0){
    selectedSchool <- selectedSchool %>% filter(`FAFSA Match`==1) %>% select(-(`FAFSA Match`))
    elsiData <- rbind(elsiData, selectedSchool)
  }else{
    selectedSchool <- selectedSchool %>% select(-(`FAFSA Match`))
    selectedSchool <- sample_n(selectedSchool, 1)
    elsiData <- rbind(elsiData, selectedSchool)
  }
  rm(selectedSchool)
}
rm(i, comboCode, comboCount)
elsiData <- elsiData %>% select(-(`Count`)) %>% select(-(`ComboName`))

elsiData <- elsiData %>% mutate(
  `SCH_NAME` = toupper(`SCH_NAME`), 
  `MCITY` = toupper(`MCITY`), 
  `LCITY` = toupper(`LCITY`)
)

#### End #### 

#### Merge: Round 1 ####

goodMerge <- inner_join(x=fafsaData, y=elsiData, by="NCESSCH") %>% mutate(`Merge Round` = rep("Round 1"))
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH` %in% goodMerge$NCESSCH)==FALSE) %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
goodMerge <- goodMerge %>% mutate(`NCESSCH-ELSI` = `NCESSCH`)
elsiData <- elsiData %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 2 #### 

# Start: 
newFafsa <- remainingFafsa 
newElsi <- remainingElsi

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 2"))
goodMerge <- rbind(goodMerge, newMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 3 ####

# Start: 
newFafsa <- remainingFafsa 
newElsi <- remainingElsi 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `LCITY`, `ST`, sep="-"))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 3"))
goodMerge <- rbind(goodMerge, newMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 4 ####

# Start: 
newFafsa <- remainingFafsa 
newElsi <- remainingElsi 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newFafsa <- newFafsa %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 4"))
goodMerge <- rbind(goodMerge, newMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 5 ####

setwd("/Users/peter_granville/FAFSA-2024/ZIP Data")
zips <- read.csv("uszips.csv") %>% select(`zip`, `city`, `state_id`)
zips <- zips %>% mutate(`zip` = as.character(`zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==1, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==2, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==3, paste("0", `zip`, sep=""), `zip`))
zips <- zips %>% mutate(`zip` = ifelse(nchar(`zip`)==4, paste("0", `zip`, sep=""), `zip`))

# Start: 
newFafsa <- remainingFafsa 
newElsi <- remainingElsi 

# Middle: 
middleFafsa <- data.frame(
  `NCESSCH` = character(),
  `Name` = character(), 
  `City` = character(),
  `State` = character(),
  `FAFSA Completions, Class of 2016` = numeric(), 
  `FAFSA Completions, Class of 2017` = numeric(), 
  `FAFSA Completions, Class of 2018` = numeric(), 
  `FAFSA Completions, Class of 2019` = numeric(), 
  `FAFSA Completions, Class of 2020` = numeric(), 
  `FAFSA Completions, Class of 2021` = numeric(), 
  `FAFSA Completions, Class of 2022` = numeric(), 
  `FAFSA Completions, Class of 2023` = numeric(), 
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
newMerge <- newMerge %>% select(
  `NCESSCH`, `Name`, `City`, `State`, 
  `FAFSA Completions, Class of 2016`, 
  `FAFSA Completions, Class of 2017`, 
  `FAFSA Completions, Class of 2018`, 
  `FAFSA Completions, Class of 2019`, 
  `FAFSA Completions, Class of 2020`, 
  `FAFSA Completions, Class of 2021`, 
  `FAFSA Completions, Class of 2022`, 
  `FAFSA Completions, Class of 2023`, 
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`)

# End: 
newMerge <- newMerge %>% mutate(`Merge Round` = rep("Round 5"))
goodMerge <- rbind(goodMerge, newMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge, middleFafsa)
nrow(remainingFafsa) + nrow(goodMerge)
rm(zips)

# Special clean-up: "COLORADO EARLY COLLEGES FORT COLLINS" 
goodMerge <- goodMerge %>% filter(duplicated(`Index`)==FALSE)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 6 ####

newFafsa <- remainingFafsa 
newElsi <- remainingElsi 

# Middle: 
middleFafsa <- newFafsa %>% select(`Name`, `State`, `City`, `Index`)
middleElsi <- newElsi %>% filter(`G_12_OFFERED`=="Yes") %>% select(`SCH_NAME`, `ST`, `LCITY`, `NCESSCH-ELSI`) %>% rename(`State` = `ST`)
middleMerge <- left_join(x=middleFafsa, y=middleElsi, by="State", relationship="many-to-many")
rm(middleFafsa, middleElsi)
middleMerge <- middleMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`)) %>% filter(`School Name Similarity Index` > 0.92) # This value was chosen after scanning the data and seeing a wrong match at the 0.91 level. 
middleMerge <- middleMerge %>% arrange(desc(`School Name Similarity Index`)) %>% filter(duplicated(`Index`)==FALSE)
middleMerge <- middleMerge %>% select(`Index`, `NCESSCH-ELSI`)
middleMerge <- left_join(x=middleMerge, y=newFafsa, by="Index")
middleMerge <- left_join(x=middleMerge, y=newElsi, by="NCESSCH-ELSI")

# End: 
middleMerge <- middleMerge %>% select(
  `NCESSCH`, `Name`, `City`, `State`, 
  `FAFSA Completions, Class of 2016`, 
  `FAFSA Completions, Class of 2017`, 
  `FAFSA Completions, Class of 2018`, 
  `FAFSA Completions, Class of 2019`, 
  `FAFSA Completions, Class of 2020`, 
  `FAFSA Completions, Class of 2021`, 
  `FAFSA Completions, Class of 2022`, 
  `FAFSA Completions, Class of 2023`,  
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`
) %>% mutate(`Merge Round` = rep("Round 6"))
goodMerge <- rbind(goodMerge, middleMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, middleMerge)
nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Checking the shares of FAFSA coverage we have: ####

sum(goodMerge$`FAFSA Completions, Class of 2023`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2023`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2022`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2022`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2021`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2021`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2020`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2020`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2019`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2019`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2018`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2018`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2017`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2017`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2016`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2016`, na.rm=TRUE)

#### End #### 

####################################
#### May                        ####
####################################

#### Load in May 31 FAFSA datasets ####

rm(fafsaData, elsiData, privateSchools, remainingElsi, remainingFafsa)

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")
fafsaData16 <- read_excel("HS_ARCHIVE05312016.xls", skip=3)
fafsaData17 <- read_excel("HS_ARCHIVE05312017.xls", skip=3)
fafsaData18 <- read_excel("HS_ARCHIVE05312018.xls", skip=3)
fafsaData19 <- read_excel("HS_ARCHIVE05312019.xls", skip=3)
fafsaData20 <- read_excel("HS_ARCHIVE05312020.xls", skip=3)
fafsaData21 <- read_excel("HS_ARCHIVE05312021.xls", skip=3)
fafsaData22 <- read_excel("HS_ARCHIVE05312022.xls", skip=3)
fafsaData23 <- read_excel("HS_ARCHIVE05312023.xls", skip=3)

#### End #### 

#### Select variables ####

schoolcodes <- fafsaData23 %>% select("School Code", "Name", "City", "State") %>% filter(`School Code` != "000") %>% filter(`School Code` != "000000000000")

fafsaData16 <- fafsaData16 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2016") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData17 <- fafsaData17 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData18 <- fafsaData18 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2018") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData19 <- fafsaData19 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2019") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData20 <- fafsaData20 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2020") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData21 <- fafsaData21 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2021") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData22 <- fafsaData22 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2022") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
fafsaData23 <- fafsaData23 %>% select("Name", "City", "State", "Applications\nComplete\nMay31  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)

names(fafsaData16) <- c("Name", "City", "State", "FAFSA Completions, Class of 2016")
names(fafsaData17) <- c("Name", "City", "State", "FAFSA Completions, Class of 2017")
names(fafsaData18) <- c("Name", "City", "State", "FAFSA Completions, Class of 2018")
names(fafsaData19) <- c("Name", "City", "State", "FAFSA Completions, Class of 2019")
names(fafsaData20) <- c("Name", "City", "State", "FAFSA Completions, Class of 2020")
names(fafsaData21) <- c("Name", "City", "State", "FAFSA Completions, Class of 2021")
names(fafsaData22) <- c("Name", "City", "State", "FAFSA Completions, Class of 2022")
names(fafsaData23) <- c("Name", "City", "State", "FAFSA Completions, Class of 2023")

fafsaData <- full_join(x=schoolcodes, y=fafsaData16, by=c("Name", "City", "State")) 
fafsaData <- full_join(x=fafsaData, y=fafsaData17, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData18, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData19, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData20, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData21, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData22, by=c("Name", "City", "State"))
fafsaData <- full_join(x=fafsaData, y=fafsaData23, by=c("Name", "City", "State"))

rm("fafsaData16", "fafsaData17", "fafsaData18", "fafsaData19", "fafsaData20", "fafsaData21", "fafsaData22", "fafsaData23")

#### End #### 

#### Remove no-FAFSA schools #### 

fafsaData <- fafsaData %>% filter((
  (`FAFSA Completions, Class of 2016` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2017` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2018` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2019` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2020` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2021` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2022` %in% c("<5", NA)) 
  & (`FAFSA Completions, Class of 2023` %in% c("<5", NA))
)==FALSE) 

#### End #### 

#### Turn "<5" to 0s ####

fafsaData$`FAFSA Completions, Class of 2016` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2016`)
fafsaData$`FAFSA Completions, Class of 2017` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2017`)
fafsaData$`FAFSA Completions, Class of 2018` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2018`)
fafsaData$`FAFSA Completions, Class of 2019` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2019`)
fafsaData$`FAFSA Completions, Class of 2020` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2020`)
fafsaData$`FAFSA Completions, Class of 2021` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2021`)
fafsaData$`FAFSA Completions, Class of 2022` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2022`)
fafsaData$`FAFSA Completions, Class of 2023` <- gsub("<5", "0", fafsaData$`FAFSA Completions, Class of 2023`)

#### End #### 

#### Conversions and formatting ####

fafsaData <- fafsaData %>% mutate(
  `FAFSA Completions, Class of 2016` = as.numeric(`FAFSA Completions, Class of 2016`), 
  `FAFSA Completions, Class of 2017` = as.numeric(`FAFSA Completions, Class of 2017`), 
  `FAFSA Completions, Class of 2018` = as.numeric(`FAFSA Completions, Class of 2018`), 
  `FAFSA Completions, Class of 2019` = as.numeric(`FAFSA Completions, Class of 2019`), 
  `FAFSA Completions, Class of 2020` = as.numeric(`FAFSA Completions, Class of 2020`), 
  `FAFSA Completions, Class of 2021` = as.numeric(`FAFSA Completions, Class of 2021`), 
  `FAFSA Completions, Class of 2022` = as.numeric(`FAFSA Completions, Class of 2022`), 
  `FAFSA Completions, Class of 2023` = as.numeric(`FAFSA Completions, Class of 2023`)
)

fafsaData <- fafsaData %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

#### End #### 

#### Turn NAs to 0s ####

fafsaData$`FAFSA Completions, Class of 2016`[is.na(fafsaData$`FAFSA Completions, Class of 2016`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2017`[is.na(fafsaData$`FAFSA Completions, Class of 2017`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2018`[is.na(fafsaData$`FAFSA Completions, Class of 2018`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2019`[is.na(fafsaData$`FAFSA Completions, Class of 2019`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2020`[is.na(fafsaData$`FAFSA Completions, Class of 2020`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2021`[is.na(fafsaData$`FAFSA Completions, Class of 2021`)] <- 0
fafsaData$`FAFSA Completions, Class of 2022`[is.na(fafsaData$`FAFSA Completions, Class of 2022`)] <- 0 
fafsaData$`FAFSA Completions, Class of 2023`[is.na(fafsaData$`FAFSA Completions, Class of 2023`)] <- 0

#### End #### 

#### Combining rows with the same school code ####

fafsaData <- fafsaData %>% mutate(`Count` = rep(1))
codeCount <- aggregate(data=fafsaData, `Count` ~ `School Code`, FUN=sum) %>% filter(`Count` > 1)
fafsaData <- fafsaData %>% select(-(`Count`))
problemCodes <- codeCount$`School Code`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- fafsaData %>% filter(`School Code` == problemCodes[i])
  fafsaData <- fafsaData %>% filter(`School Code` != problemCodes[i])
  fafsaData <- fafsaData %>% add_row(
    `School Code` = tempData$`School Code`[1], 
    `Name` = tempData$`Name`[1], 
    `City` = tempData$`City`[1], 
    `State` = tempData$`State`[1], 
    `FAFSA Completions, Class of 2016` = sum(tempData$`FAFSA Completions, Class of 2016`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2017` = sum(tempData$`FAFSA Completions, Class of 2017`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2018` = sum(tempData$`FAFSA Completions, Class of 2018`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2019` = sum(tempData$`FAFSA Completions, Class of 2019`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2020` = sum(tempData$`FAFSA Completions, Class of 2020`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2021` = sum(tempData$`FAFSA Completions, Class of 2021`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2022` = sum(tempData$`FAFSA Completions, Class of 2022`, na.rm=TRUE), 
    `FAFSA Completions, Class of 2023` = sum(tempData$`FAFSA Completions, Class of 2023`, na.rm=TRUE)
  )
  rm(tempData)
  
}
rm(problemCodes, i)

#### End #### 

#### Remove private schools ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
privateSchools20 <- read.csv("ELSI Private Schools 19-20.csv", header=TRUE, skip=6, nrow=21100, check.names=FALSE)
privateSchools18 <- read.csv("ELSI Private Schools 17-18.csv", header=TRUE, skip=6, nrow=22434, check.names=FALSE)
privateSchools16 <- read.csv("ELSI Private Schools 15-16.csv", header=TRUE, skip=6, nrow=21895, check.names=FALSE)
privateSchools14 <- read.csv("ELSI Private Schools 13-14.csv", header=TRUE, skip=6, nrow=24085, check.names=FALSE)
privateSchools12 <- read.csv("ELSI Private Schools 11-12.csv", header=TRUE, skip=6, nrow=26519, check.names=FALSE)
privateSchools10 <- read.csv("ELSI Private Schools 09-10.csv", header=TRUE, skip=6, nrow=27760, check.names=FALSE)
privateSchools08 <- read.csv("ELSI Private Schools 07-08.csv", header=TRUE, skip=6, nrow=28425, check.names=FALSE)

names(privateSchools20) <- c("Name 2019-20", "State 2019-20", "School ID", "City 2019-20", "ZIP 2019-20")
names(privateSchools18) <- c("Name 2017-18", "State 2017-18", "School ID", "City 2017-18", "ZIP 2017-18")
names(privateSchools16) <- c("Name 2015-16", "State 2015-16", "School ID", "City 2015-16", "ZIP 2015-16")
names(privateSchools14) <- c("Name 2013-14", "State 2013-14", "School ID", "City 2013-14", "ZIP 2013-14")
names(privateSchools12) <- c("Name 2011-12", "State 2011-12", "School ID", "City 2011-12", "ZIP 2011-12")
names(privateSchools10) <- c("Name 2009-10", "State 2009-10", "School ID", "City 2009-10", "ZIP 2009-10")
names(privateSchools08) <- c("Name 2007-08", "State 2007-08", "School ID", "City 2007-08", "ZIP 2007-08")
privateSchools <- full_join(x=privateSchools20, y=privateSchools18, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools16, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools14, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools12, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools10, by="School ID", relationship = "many-to-many")
privateSchools <- full_join(x=privateSchools, y=privateSchools08, by="School ID", relationship = "many-to-many")
rm(privateSchools20, privateSchools18, privateSchools16, privateSchools14, privateSchools12, privateSchools10, privateSchools08)
fafsaData <- fafsaData %>% filter((`School Code` %in% privateSchools$`School ID`)==FALSE)

#### End #### 

#### Additional FAFSA cleaning ####

# Remove any other schools with alphabet characters in name (indicating private): 
fafsaData <- fafsaData %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)

# Remove other schools with religious terms in names:  
fafsaData <- fafsaData %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)

# Remove schools we won't be able to link to Census data: 
fafsaData <- fafsaData %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

# Remove virtual schools 
fafsaData <- fafsaData %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)

#### End #### 

####################################
#### Combining April and May    ####
####################################

#### Rename variables ####

goodMerge <- goodMerge %>% rename(
  `FAFSA Completions, Class of 2016 (April 30)` = `FAFSA Completions, Class of 2016`, 
  `FAFSA Completions, Class of 2017 (April 30)` = `FAFSA Completions, Class of 2017`, 
  `FAFSA Completions, Class of 2018 (April 30)` = `FAFSA Completions, Class of 2018`, 
  `FAFSA Completions, Class of 2019 (April 30)` = `FAFSA Completions, Class of 2019`, 
  `FAFSA Completions, Class of 2020 (April 30)` = `FAFSA Completions, Class of 2020`, 
  `FAFSA Completions, Class of 2021 (April 30)` = `FAFSA Completions, Class of 2021`, 
  `FAFSA Completions, Class of 2022 (April 30)` = `FAFSA Completions, Class of 2022`, 
  `FAFSA Completions, Class of 2023 (April 30)` = `FAFSA Completions, Class of 2023`
)

fafsaData <- fafsaData %>% rename(
  `FAFSA Completions, Class of 2016 (May 31)` = `FAFSA Completions, Class of 2016`, 
  `FAFSA Completions, Class of 2017 (May 31)` = `FAFSA Completions, Class of 2017`, 
  `FAFSA Completions, Class of 2018 (May 31)` = `FAFSA Completions, Class of 2018`, 
  `FAFSA Completions, Class of 2019 (May 31)` = `FAFSA Completions, Class of 2019`, 
  `FAFSA Completions, Class of 2020 (May 31)` = `FAFSA Completions, Class of 2020`, 
  `FAFSA Completions, Class of 2021 (May 31)` = `FAFSA Completions, Class of 2021`, 
  `FAFSA Completions, Class of 2022 (May 31)` = `FAFSA Completions, Class of 2022`, 
  `FAFSA Completions, Class of 2023 (May 31)` = `FAFSA Completions, Class of 2023`
)

#### End #### 

#### Merge April and May #### 

goodMerge <- left_join(x=goodMerge, y=fafsaData, by=c("Name", "City", "State"))
goodMerge <- goodMerge %>% filter(
  is.na(`FAFSA Completions, Class of 2023 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2022 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2021 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2020 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2019 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2018 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2017 (May 31)`)==FALSE, 
  is.na(`FAFSA Completions, Class of 2016 (May 31)`)==FALSE
)

sum(goodMerge$`FAFSA Completions, Class of 2023 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2023 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2022 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2022 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2021 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2021 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2020 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2020 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2019 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2019 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2018 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2018 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2017 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2017 (May 31)`, na.rm=TRUE)
sum(goodMerge$`FAFSA Completions, Class of 2016 (May 31)`, na.rm=TRUE) / sum(fafsaData$`FAFSA Completions, Class of 2016 (May 31)`, na.rm=TRUE)

rm(fafsaData)

#### End #### 

#### Calculate May 24 estimates ####

goodMerge <- goodMerge %>% mutate(
  `FAFSA Completions, Class of 2016` = round(`FAFSA Completions, Class of 2016 (April 30)` + ((`FAFSA Completions, Class of 2016 (May 31)` - `FAFSA Completions, Class of 2016 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2017` = round(`FAFSA Completions, Class of 2017 (April 30)` + ((`FAFSA Completions, Class of 2017 (May 31)` - `FAFSA Completions, Class of 2017 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2018` = round(`FAFSA Completions, Class of 2018 (April 30)` + ((`FAFSA Completions, Class of 2018 (May 31)` - `FAFSA Completions, Class of 2018 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2019` = round(`FAFSA Completions, Class of 2019 (April 30)` + ((`FAFSA Completions, Class of 2019 (May 31)` - `FAFSA Completions, Class of 2019 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2020` = round(`FAFSA Completions, Class of 2020 (April 30)` + ((`FAFSA Completions, Class of 2020 (May 31)` - `FAFSA Completions, Class of 2020 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2021` = round(`FAFSA Completions, Class of 2021 (April 30)` + ((`FAFSA Completions, Class of 2021 (May 31)` - `FAFSA Completions, Class of 2021 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2022` = round(`FAFSA Completions, Class of 2022 (April 30)` + ((`FAFSA Completions, Class of 2022 (May 31)` - `FAFSA Completions, Class of 2022 (April 30)`) * (24/31))), 
  `FAFSA Completions, Class of 2023` = round(`FAFSA Completions, Class of 2023 (April 30)` + ((`FAFSA Completions, Class of 2023 (May 31)` - `FAFSA Completions, Class of 2023 (April 30)`) * (24/31)))
)

#### End #### 

#### Remove April and May variables ####

goodMerge <- goodMerge %>% select(
  -(`FAFSA Completions, Class of 2016 (April 30)`), 
  -(`FAFSA Completions, Class of 2017 (April 30)`), 
  -(`FAFSA Completions, Class of 2018 (April 30)`), 
  -(`FAFSA Completions, Class of 2019 (April 30)`), 
  -(`FAFSA Completions, Class of 2020 (April 30)`), 
  -(`FAFSA Completions, Class of 2021 (April 30)`), 
  -(`FAFSA Completions, Class of 2022 (April 30)`), 
  -(`FAFSA Completions, Class of 2023 (April 30)`),
  
  -(`FAFSA Completions, Class of 2016 (May 31)`), 
  -(`FAFSA Completions, Class of 2017 (May 31)`), 
  -(`FAFSA Completions, Class of 2018 (May 31)`), 
  -(`FAFSA Completions, Class of 2019 (May 31)`), 
  -(`FAFSA Completions, Class of 2020 (May 31)`), 
  -(`FAFSA Completions, Class of 2021 (May 31)`), 
  -(`FAFSA Completions, Class of 2022 (May 31)`), 
  -(`FAFSA Completions, Class of 2023 (May 31)`)
)

#### End #### 

####################################
#### Write file                 ####
####################################

#### Write data file #### 

setwd("/Users/peter_granville/FAFSA-2024")
write.csv(goodMerge, "Alternative FAFSA Year Dataset.csv", row.names=FALSE)

#### End #### 

