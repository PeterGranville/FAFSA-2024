
#### Setup ####

library(dplyr)
library(scales)
library(readxl)
library(plotly)
library(viridis)
library(zipcodeR)
library(geosphere)
library(tidyverse)
library(data.table)
library(stringdist)

#### End #### 

####################################
#### Baseline analysis          ####
####################################

#### Loading FAFSA data ####

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")
fafsaData <- read_xls("HS_ARCHIVE04302024.xls", skip=4, col_names=FALSE) %>% select(`...1`, `...2`, `...3`, `...4`, `...5`, `...6`, `...7`, `...8`)

names(fafsaData) <- c("School Code", "Name", "City", "State", 
                      "Applications submitted through April 30, 2024", 
                      "Applications completed through April 30, 2024", 
                      "Applications submitted through April 30, 2023", 
                      "Applications completed through April 30, 2023")
fafsaData$`Applications submitted through April 30, 2024` <- gsub("<5", "0", fafsaData$`Applications submitted through April 30, 2024`)
fafsaData$`Applications completed through April 30, 2024` <- gsub("<5", "0", fafsaData$`Applications completed through April 30, 2024`)
fafsaData$`Applications submitted through April 30, 2023` <- gsub("<5", "0", fafsaData$`Applications submitted through April 30, 2023`)
fafsaData$`Applications completed through April 30, 2023` <- gsub("<5", "0", fafsaData$`Applications completed through April 30, 2023`)

# Sensitivity test: 
# fafsaData$`Applications submitted through April 30, 2024` <- gsub("<5", "4", fafsaData$`Applications submitted through April 30, 2024`)
# fafsaData$`Applications completed through April 30, 2024` <- gsub("<5", "4", fafsaData$`Applications completed through April 30, 2024`)
# fafsaData$`Applications submitted through April 30, 2023` <- gsub("<5", "4", fafsaData$`Applications submitted through April 30, 2023`)
# fafsaData$`Applications completed through April 30, 2023` <- gsub("<5", "4", fafsaData$`Applications completed through April 30, 2023`)


fafsaData <- fafsaData %>% mutate(
  `Applications submitted through April 30, 2024` = as.numeric(`Applications submitted through April 30, 2024`), 
  `Applications completed through April 30, 2024` = as.numeric(`Applications completed through April 30, 2024`), 
  `Applications submitted through April 30, 2023` = as.numeric(`Applications submitted through April 30, 2023`), 
  `Applications completed through April 30, 2023` = as.numeric(`Applications completed through April 30, 2023`)
)

fafsaData <- fafsaData %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

# Combining rows with the same code: 
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
    `Applications submitted through April 30, 2024` = sum(tempData$`Applications submitted through April 30, 2024`, na.rm=TRUE), 
    `Applications completed through April 30, 2024` = sum(tempData$`Applications completed through April 30, 2024`, na.rm=TRUE), 
    `Applications submitted through April 30, 2023` = sum(tempData$`Applications submitted through April 30, 2023`, na.rm=TRUE), 
    `Applications completed through April 30, 2023` = sum(tempData$`Applications completed through April 30, 2023`, na.rm=TRUE)
  )
  rm(tempData)
  
}
rm(problemCodes, i)

# Remove private schools: 
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

# Remove any other schools with alphabet characters in name (indicating private): 
fafsaData <- fafsaData %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)

# Remove other schools with religious terms in names:  
fafsaData <- fafsaData %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)

# Remove schools we won't be able to link to Census data: 
fafsaData <- fafsaData %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

# Remove FAFSA records where all values are 0: 
fafsaData <- fafsaData %>% filter(`Applications submitted through April 30, 2024`
                                  + `Applications completed through April 30, 2024` 
                                  + `Applications submitted through April 30, 2023` 
                                  + `Applications completed through April 30, 2023` > 0)

# Remove virtual schools 
fafsaData <- fafsaData %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)

# Index the FAFSA data: 
fafsaData <- fafsaData %>% mutate(`Index` = (1:nrow(fafsaData))) %>% rename(`NCESSCH` = `School Code`)

#### End #### 

#### Loading ELSI data ####

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

# Accounting for multiple rows of identical data: 
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
# nrow(remainingFafsa) + nrow(goodMerge)

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
# nrow(remainingFafsa) + nrow(goodMerge)

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
# nrow(remainingFafsa) + nrow(goodMerge)

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
# nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Merge: Round 5 ####

setwd("/Users/peter_granville/FAFSA-2024/ZIP data")
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
  `Applications submitted through April 30, 2024` = numeric(), 
  `Applications completed through April 30, 2024` = numeric(), 
  `Applications submitted through April 30, 2023` = numeric(), 
  `Applications completed through April 30, 2023` = numeric(), 
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
newMerge <- newMerge %>% select(`NCESSCH`, `Name`, `City`, `State`, `Applications submitted through April 30, 2024`, `Applications completed through April 30, 2024`, `Applications submitted through April 30, 2023`, `Applications completed through April 30, 2023`, `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`)

# End: 
newMerge <- newMerge %>% mutate(`Merge Round` = rep("Round 5"))
goodMerge <- rbind(goodMerge, newMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge, middleFafsa)
# nrow(remainingFafsa) + nrow(goodMerge)
rm(zips)

#### End #### 

#### Merge: Round 6 ####

newFafsa <- remainingFafsa 
newElsi <- remainingElsi 

# Middle: 
middleFafsa <- newFafsa %>% select(`Name`, `State`, `City`, `Index`)
middleElsi <- newElsi %>% filter(`G_12_OFFERED`=="Yes") %>% select(`SCH_NAME`, `ST`, `LCITY`, `NCESSCH-ELSI`) %>% rename(`State` = `ST`)
middleMerge <- left_join(x=middleFafsa, y=middleElsi, by="State", relationship="many-to-many")
rm(middleFafsa, middleElsi)
middleMerge <- middleMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`)) %>% filter(`School Name Similarity Index` > 0.9) 
middleMerge <- middleMerge %>% arrange(desc(`School Name Similarity Index`)) %>% filter(duplicated(`Index`)==FALSE)
middleMerge <- middleMerge %>% select(`Index`, `NCESSCH-ELSI`)
middleMerge <- left_join(x=middleMerge, y=newFafsa, by="Index")
middleMerge <- left_join(x=middleMerge, y=newElsi, by="NCESSCH-ELSI")

# End: 
middleMerge <- middleMerge %>% select(`NCESSCH`, `Name`, `City`, `State`, `Applications submitted through April 30, 2024`, `Applications completed through April 30, 2024`, `Applications submitted through April 30, 2023`, `Applications completed through April 30, 2023`, `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`) %>% mutate(`Merge Round` = rep("Round 6"))
goodMerge <- rbind(goodMerge, middleMerge)
remainingFafsa <- fafsaData %>% filter((`Index` %in% goodMerge$Index)==FALSE)
remainingElsi <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, middleMerge)
# nrow(remainingFafsa) + nrow(goodMerge)

#### End #### 

#### Load in and merge high school 12th grade enrollment ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
seniors <- fread("ccd_sch_052_2223.csv", header=TRUE, select=c(
  "NCESSCH", 
  "GRADE", 
  "STUDENT_COUNT", 
  "TOTAL_INDICATOR"
)) %>% filter(
  `GRADE` == "Grade 12", 
  `TOTAL_INDICATOR` == "Subtotal 4 - By Grade"
) %>% select(-(`GRADE`)) %>% select(-(`TOTAL_INDICATOR`)) %>% rename(
  `NCESSCH-ELSI` = `NCESSCH`, 
  `Grade 12 students, 2022-23` = `STUDENT_COUNT`
) %>% mutate(
  `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
)
publicGrade12.2024 <- sum(seniors$`Grade 12 students, 2022-23`, na.rm=TRUE) 

goodMerge <- left_join(x=goodMerge, y=seniors, by="NCESSCH-ELSI")
rm(seniors)

#### End #### 

#### Load in and merge additional FAFSA numbers ####

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")

fafsaJanuary <- read_xls("HSARCHIVE01312024.xls", skip=4, col_names=FALSE) %>% select(`...2`, `...3`, `...4`, `...5`, `...7`)
names(fafsaJanuary) <- c("Name", "City", "State", 
                          "Applications submitted through January 31, 2024", 
                          "Applications submitted through January 31, 2023")
fafsaFebruary <- read_xls("HSARCHIVE02292024.xls", skip=4, col_names=FALSE) %>% select(`...2`, `...3`, `...4`, `...5`, `...7`)
names(fafsaFebruary) <- c("Name", "City", "State", 
                      "Applications submitted through February 29, 2024", 
                      "Applications submitted through February 29, 2023")
fafsaData2 <- full_join(x=fafsaJanuary, y=fafsaFebruary, by=c("Name", "City", "State"))
fafsaMarch <- read_xls("HS_ARCHIVE03292024.xls", skip=4, col_names=FALSE) %>% select(`...2`, `...3`, `...4`, `...5`, `...6`, `...7`, `...8`)
names(fafsaMarch) <- c("Name", "City", "State", 
                       "Applications submitted through March 29, 2024",
                       "Applications completed through March 29, 2024",
                       "Applications submitted through March 29, 2023", 
                       "Applications completed through March 29, 2023")
fafsaData2 <- full_join(x=fafsaData2, y=fafsaMarch, by=c("Name", "City", "State"))

fafsaData2$`Applications submitted through March 29, 2024` <- gsub("<5", "0", fafsaData2$`Applications submitted through March 29, 2024`)
fafsaData2$`Applications completed through March 29, 2024` <- gsub("<5", "0", fafsaData2$`Applications completed through March 29, 2024`)
fafsaData2$`Applications submitted through March 29, 2023` <- gsub("<5", "0", fafsaData2$`Applications submitted through March 29, 2023`)
fafsaData2$`Applications completed through March 29, 2023` <- gsub("<5", "0", fafsaData2$`Applications completed through March 29, 2023`)
fafsaData2$`Applications submitted through February 29, 2024` <- gsub("<5", "0", fafsaData2$`Applications submitted through February 29, 2024`)
fafsaData2$`Applications submitted through February 29, 2023` <- gsub("<5", "0", fafsaData2$`Applications submitted through February 29, 2023`)
fafsaData2$`Applications submitted through January 31, 2024` <- gsub("<5", "0", fafsaData2$`Applications submitted through January 31, 2024`)
fafsaData2$`Applications submitted through January 31, 2023` <- gsub("<5", "0", fafsaData2$`Applications submitted through January 31, 2023`)

fafsaData2 <- fafsaData2 %>% mutate(
  `Applications submitted through March 29, 2024` = as.numeric(`Applications submitted through March 29, 2024`), 
  `Applications completed through March 29, 2024` = as.numeric(`Applications completed through March 29, 2024`), 
  `Applications submitted through March 29, 2023` = as.numeric(`Applications submitted through March 29, 2023`),
  `Applications completed through March 29, 2023` = as.numeric(`Applications completed through March 29, 2023`),
  `Applications submitted through February 29, 2024` = as.numeric(`Applications submitted through February 29, 2024`), 
  `Applications submitted through February 29, 2023` = as.numeric(`Applications submitted through February 29, 2023`), 
  `Applications submitted through January 31, 2024` = as.numeric(`Applications submitted through January 31, 2024`), 
  `Applications submitted through January 31, 2023` = as.numeric(`Applications submitted through January 31, 2023`)
)

fafsaData2 <- fafsaData2 %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

goodMerge <- left_join(x=goodMerge, y=fafsaData2, by=c("Name", "City", "State"))
rm(fafsaData2)

goodMerge$`Applications submitted through March 29, 2024`[is.na(goodMerge$`Applications submitted through March 29, 2024`)] <- 0
goodMerge$`Applications completed through March 29, 2024`[is.na(goodMerge$`Applications completed through March 29, 2024`)] <- 0
goodMerge$`Applications submitted through March 29, 2023`[is.na(goodMerge$`Applications submitted through March 29, 2023`)] <- 0
goodMerge$`Applications completed through March 29, 2023`[is.na(goodMerge$`Applications completed through March 29, 2023`)] <- 0
goodMerge$`Applications submitted through February 29, 2024`[is.na(goodMerge$`Applications submitted through February 29, 2024`)] <- 0
goodMerge$`Applications submitted through February 29, 2023`[is.na(goodMerge$`Applications submitted through February 29, 2023`)] <- 0
goodMerge$`Applications submitted through January 31, 2023`[is.na(goodMerge$`Applications submitted through January 31, 2023`)] <- 0
goodMerge$`Applications submitted through January 31, 2024`[is.na(goodMerge$`Applications submitted through January 31, 2024`)] <- 0

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

#### Census data checks ####

# C1: 
# sum(census$`Native-born share`) + sum(census$`Foreign-born share`) # 5819
# sum(census$`Foreign-born share: Europe`) + sum(census$`Foreign-born share: Asia`) + sum(census$`Foreign-born share: Africa`) + sum(census$`Foreign-born share: Oceania`) + sum(census$`Foreign-born share: Latin America`) + sum(census$`Foreign-born share: Northern America`) + sum(census$`Native-born share`) # ~5819

# C2: 
# sum(census$`White share`) + sum(census$`Black share`) + sum(census$`Native American share`) + sum(census$`Asian share`) + sum(census$`Pacific Islander share`) + sum(census$`Other race share`) + sum(census$`Two or more races share`) + sum(census$`Hispanic or Latino share`) # 5819

# C3 
# sum(census$`Less than 9th grade share`) + sum(census$`High school, no diploma share`) + sum(census$`High school diploma share`) + sum(census$`Some college, no degree share`) + sum(census$`Associate's degree share`) + sum(census$`Bachelor's degree share`) + sum(census$`Graduate degree share`) # 5819

# # C4 
# summary(census$`Households receiving SNAP share`)

# # C5 
# summary(census$`Limited English share`)
 
# # C6
# summary(census$`Share of population in poverty`)
 
# # C7
# summary(census$`Average household income`)

#### End #### 

#### Merge Census data with FAFSA data ####

goodMerge <- goodMerge %>% mutate(
  `ZCTA5` = paste("ZCTA5 ", substr(`LZIP`, 1, 5), sep="")
)
analysis <- aggregate(data=goodMerge, cbind(
  `Grade 12 students, 2022-23`,
  `Applications submitted through January 31, 2024`, 
  `Applications submitted through January 31, 2023`, 
  `Applications submitted through February 29, 2024`, 
  `Applications submitted through February 29, 2023`, 
  `Applications submitted through March 29, 2024`, 
  `Applications completed through March 29, 2024`, 
  `Applications submitted through March 29, 2023`, 
  `Applications completed through March 29, 2023`, 
  `Applications submitted through April 30, 2024`, 
  `Applications completed through April 30, 2024`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through April 30, 2023`
) ~ `ZCTA5`, FUN=sum)

analysis <- left_join(x=analysis, y=census, by="ZCTA5")

#### End #### 

#### Group ZCTA5 ####

analysis <- analysis %>% mutate(
  `Black or Latino share` = `Black share` + `Hispanic or Latino share`,
  `Black, Latino, or Native American share` = `Black share` + `Hispanic or Latino share` + `Native American share`,
  `Black, Latino, Native American, or Pacific Islander share` = `Black share` + `Hispanic or Latino share` + `Native American share` + `Pacific Islander share`,
  `No college share` = `Less than 9th grade share` + `High school, no diploma share` + `High school diploma share`, 
  `Associate's or higher share` = `Associate's degree share` + `Bachelor's degree share` + `Graduate degree share`
)

rankInputs <- analysis %>% select(
  `ZCTA5`, 
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
  `Grade 12 students, 2022-23`,
  `Applications submitted through January 31, 2024`, 
  `Applications submitted through January 31, 2023`, 
  `Applications submitted through February 29, 2024`, 
  `Applications submitted through February 29, 2023`,
  `Applications submitted through March 29, 2024`, 
  `Applications completed through March 29, 2024`, 
  `Applications submitted through March 29, 2023`, 
  `Applications completed through March 29, 2023`, 
  `Applications submitted through April 30, 2024`, 
  `Applications completed through April 30, 2024`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through April 30, 2023`
)

nGroups <- 20
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
nTiles20 <- full_join(x=nTiles, y=rankOutputs, by="ZCTA5")
rm(nTiles)

nGroups <- 5
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
nTiles5 <- full_join(x=nTiles, y=rankOutputs, by="ZCTA5")
rm(nTiles)

nGroups <- 100
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
nTiles100 <- full_join(x=nTiles, y=rankOutputs, by="ZCTA5")
rm(nTiles)

#### End #### 

#### Overlap analysis: Race, education, income ####

# overlap <- nTiles5 %>% mutate(`Count` = rep(1))
# 
# overlap1 <- aggregate(data=overlap, `Count` ~ `Groups: Share of population in poverty`, FUN=sum)
# 
# overlap2 <- overlap %>% filter(`Groups: Share of population in poverty` == 5)
# overlap2A <- aggregate(data=overlap2, `Count` ~ `Groups: No college share`, FUN=sum)
# overlap2B <- aggregate(data=overlap2, `Count` ~ `Groups: Black or Latino share`, FUN=sum)

#### End #### 

#### Charts of YOY decrease in completions (20-tiles) (function1) ####

function1 <- function(tilesDF, tilesVar, printData){
  
  newTiles <- tilesDF
  
  newTiles <- newTiles %>% select(
    `Applications completed through April 30, 2024`, 
    `Applications completed through April 30, 2023`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  ) 
  analysis1 <- aggregate(data=newTiles, cbind(
    `Applications completed through April 30, 2024`, 
    `Applications completed through April 30, 2023` 
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `Decline in YoY completions` = (`Applications completed through April 30, 2024` - `Applications completed through April 30, 2023`) / `Applications completed through April 30, 2023`
  ) 
  figA <- ggplot(data=analysis1, mapping=aes(x=`InterestVar`, y=`Decline in YoY completions`)) + geom_bar(stat="identity", width=0.1) + geom_point() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(-0.5, 0)) + labs(x=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  return(figA)
  rm(analysis1, newTiles, figA)
}

#### End #### 

#### Run function1 ####

figA1 <- function1(nTiles20, "Groups: Native-born share", FALSE)
figA2 <- function1(nTiles20, "Groups: Foreign-born share: AAOA", FALSE)
figA3 <- function1(nTiles20, "Groups: Limited English share", FALSE)
figA4 <- function1(nTiles20, "Groups: White share", FALSE)
figA5 <- function1(nTiles20, "Groups: Black share", FALSE)
figA6 <- function1(nTiles20, "Groups: Hispanic or Latino share", FALSE)
figA7 <- function1(nTiles20, "Groups: Native American share", FALSE)
figA8 <- function1(nTiles20, "Groups: Asian share", FALSE)
figA9 <- function1(nTiles20, "Groups: Pacific Islander share", FALSE)
figA10 <- function1(nTiles20, "Groups: Other race share", FALSE)
figA11 <- function1(nTiles20, "Groups: Two or more races share", FALSE)
figA12 <- function1(nTiles20, "Groups: Black or Latino share", FALSE)
figA13 <- function1(nTiles20, "Groups: Black, Latino, or Native American share", FALSE)
figA14 <- function1(nTiles20, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figA15 <- function1(nTiles20, "Groups: No college share", FALSE)
figA16 <- function1(nTiles20, "Groups: Associate's or higher share", FALSE)
figA17 <- function1(nTiles20, "Groups: Households receiving SNAP share", FALSE)
figA18 <- function1(nTiles20, "Groups: Share of population in poverty", FALSE)
figA19 <- function1(nTiles20, "Groups: Average household income", FALSE)

#### End #### 

#### Charts of FAFSA completion rate (20-tiles) (function2) ####

function2 <- function(tilesDF, tilesVar, printData){
  
  newTiles <- tilesDF
  
  newTiles <- newTiles %>% select(
    `Applications completed through April 30, 2024`, 
    `Grade 12 students, 2022-23`,
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  ) 
  analysis1 <- aggregate(data=newTiles, cbind(
    `Applications completed through April 30, 2024`, 
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `FAFSA completion rate` = `Applications completed through April 30, 2024` / `Grade 12 students, 2022-23`
  ) 
  figB <- ggplot(data=analysis1, mapping=aes(x=`InterestVar`, y=`FAFSA completion rate`)) + geom_bar(stat="identity", width=0.1) + geom_point() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.5)) + labs(x=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  return(figB)
  rm(analysis1, newTiles, figB)
}

#### End #### 

#### Run function2 ####

figB1 <- function2(nTiles20, "Groups: Native-born share", FALSE)
figB2 <- function2(nTiles20, "Groups: Foreign-born share: AAOA", FALSE)
figB3 <- function2(nTiles20, "Groups: Limited English share", FALSE)
figB4 <- function2(nTiles20, "Groups: White share", FALSE)
figB5 <- function2(nTiles20, "Groups: Black share", FALSE)
figB6 <- function2(nTiles20, "Groups: Hispanic or Latino share", FALSE)
figB7 <- function2(nTiles20, "Groups: Native American share", FALSE)
figB8 <- function2(nTiles20, "Groups: Asian share", FALSE)
figB9 <- function2(nTiles20, "Groups: Pacific Islander share", FALSE)
figB10 <- function2(nTiles20, "Groups: Other race share", FALSE)
figB11 <- function2(nTiles20, "Groups: Two or more races share", FALSE)
figB12 <- function2(nTiles20, "Groups: Black or Latino share", FALSE)
figB13 <- function2(nTiles20, "Groups: Black, Latino, or Native American share", FALSE)
figB14 <- function2(nTiles20, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figB15 <- function2(nTiles20, "Groups: No college share", FALSE)
figB16 <- function2(nTiles20, "Groups: Associate's or higher share", FALSE)
figB17 <- function2(nTiles20, "Groups: Households receiving SNAP share", FALSE)
figB18 <- function2(nTiles20, "Groups: Share of population in poverty", FALSE)
figB19 <- function2(nTiles20, "Groups: Average household income", FALSE)

#### End #### 

#### Charts of FAFSA submission rate (5-tiles) (function3) ####

function3 <- function(tilesDF, tilesVar, printData){
  
  newTiles <- tilesDF
  
  newTiles <- newTiles %>% select(
    `Applications submitted through April 30, 2024`, 
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`,
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  ) 
  analysis1 <- aggregate(data=newTiles, cbind(
    `Applications submitted through April 30, 2024`, 
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `End of January` = `Applications submitted through January 31, 2024` / `Grade 12 students, 2022-23`, 
    `End of February` = `Applications submitted through February 29, 2024` / `Grade 12 students, 2022-23`, 
    `End of March` = `Applications submitted through March 29, 2024` / `Grade 12 students, 2022-23`,
    `End of April` = `Applications submitted through April 30, 2024` / `Grade 12 students, 2022-23`
  ) %>% select(
    `InterestVar`, 
    `End of January`, 
    `End of February`, 
    `End of March`, 
    `End of April`
  ) %>% pivot_longer(cols=c(
    `End of January`, 
    `End of February`, 
    `End of March`, 
    `End of April`
  ), names_to="Date", values_to="Submission rate") %>% mutate(
    `Date` = factor(`Date`, levels=c(
      "End of January", 
      "End of February", 
      "End of March", 
      "End of April"
    ))
  )
  figC <- ggplot(data=analysis1, mapping=aes(x=`Date`, y=`Submission rate`, fill=`InterestVar`)) + geom_bar(stat="identity", position = "dodge2") + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.5)) + theme(legend.position="bottom") + labs(fill=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  return(figC)
  rm(analysis1, newTiles, figC)
}

#### End #### 

#### Run function3 ####

figC1 <- function3(nTiles5, "Groups: Native-born share", FALSE)
figC2 <- function3(nTiles5, "Groups: Foreign-born share: AAOA", FALSE)
figC3 <- function3(nTiles5, "Groups: Limited English share", FALSE)
figC4 <- function3(nTiles5, "Groups: White share", FALSE)
figC5 <- function3(nTiles5, "Groups: Black share", FALSE)
figC6 <- function3(nTiles5, "Groups: Hispanic or Latino share", FALSE)
figC7 <- function3(nTiles5, "Groups: Native American share", FALSE)
figC8 <- function3(nTiles5, "Groups: Asian share", FALSE)
figC9 <- function3(nTiles5, "Groups: Pacific Islander share", FALSE)
figC10 <- function3(nTiles5, "Groups: Other race share", FALSE)
figC11 <- function3(nTiles5, "Groups: Two or more races share", FALSE)
figC12 <- function3(nTiles5, "Groups: Black or Latino share", FALSE)
figC13 <- function3(nTiles5, "Groups: Black, Latino, or Native American share", FALSE)
figC14 <- function3(nTiles5, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figC15 <- function3(nTiles5, "Groups: No college share", FALSE)
figC16 <- function3(nTiles5, "Groups: Associate's or higher share", FALSE)
figC17 <- function3(nTiles5, "Groups: Households receiving SNAP share", FALSE)
figC18 <- function3(nTiles5, "Groups: Share of population in poverty", FALSE)
figC19 <- function3(nTiles5, "Groups: Average household income", FALSE)

#### End #### 

#### Charts of the share of submitted FAFSAs that are incomplete (20-tiles) (function4) ####

function4 <- function(tilesDF, tilesVar, printData){
  
  newTiles <- tilesDF
  
  newTiles <- newTiles %>% select(
    `Applications submitted through April 30, 2024`, 
    `Applications completed through April 30, 2024`,
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  ) 
  analysis1 <- aggregate(data=newTiles, cbind(
    `Applications submitted through April 30, 2024`, 
    `Applications completed through April 30, 2024`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `Share of submitted FAFSAs that are incomplete` = 1 - (`Applications completed through April 30, 2024` / `Applications submitted through April 30, 2024`)
  ) 
  figD <-  ggplot(data=analysis1, mapping=aes(x=`InterestVar`, y=`Share of submitted FAFSAs that are incomplete`)) + geom_bar(stat="identity", width=0.1) + geom_point() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0, 0.2)) + labs(x=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  return(figD)
  rm(analysis1, newTiles, figD)
}

#### End #### 

#### Run function4 ####

figD1 <- function4(nTiles20, "Groups: Native-born share", FALSE)
figD2 <- function4(nTiles20, "Groups: Foreign-born share: AAOA", FALSE)
figD3 <- function4(nTiles20, "Groups: Limited English share", FALSE)
figD4 <- function4(nTiles20, "Groups: White share", FALSE)
figD5 <- function4(nTiles20, "Groups: Black share", FALSE)
figD6 <- function4(nTiles20, "Groups: Hispanic or Latino share", FALSE)
figD7 <- function4(nTiles20, "Groups: Native American share", FALSE)
figD8 <- function4(nTiles20, "Groups: Asian share", FALSE)
figD9 <- function4(nTiles20, "Groups: Pacific Islander share", FALSE)
figD10 <- function4(nTiles20, "Groups: Other race share", FALSE)
figD11 <- function4(nTiles20, "Groups: Two or more races share", FALSE)
figD12 <- function4(nTiles20, "Groups: Black or Latino share", FALSE)
figD13 <- function4(nTiles20, "Groups: Black, Latino, or Native American share", FALSE)
figD14 <- function4(nTiles20, "Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figD15 <- function4(nTiles20, "Groups: No college share", FALSE)
figD16 <- function4(nTiles20, "Groups: Associate's or higher share", FALSE)
figD17 <- function4(nTiles20, "Groups: Households receiving SNAP share", FALSE)
figD18 <- function4(nTiles20, "Groups: Share of population in poverty", FALSE)
figD19 <- function4(nTiles20, "Groups: Average household income", FALSE)

#### End #### 

#### Changes by community [ZIP3] ####

setwd("/Users/peter_granville/FAFSA-2024")
zip3 <- goodMerge %>% mutate(`LZIP3` = substr(`LZIP`, 1, 3))
zip3 <- aggregate(data=zip3, cbind(
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`, 
  `Applications completed through March 29, 2024`, 
  `Grade 12 students, 2022-23`
) ~ `LZIP3`, FUN=sum) %>% mutate(
  `Dropoff, April 2023 to April 2024` = (`Applications completed through April 30, 2024` - `Applications completed through April 30, 2023`) / `Applications completed through April 30, 2023`, 
  `Incease, March 2024 to April 2024` = (`Applications completed through April 30, 2024` - `Applications completed through March 29, 2024`) / `Applications completed through March 29, 2024`
) %>% filter(
  `Grade 12 students, 2022-23` > 250
)
zip3A <- zip3 %>% select(
  `LZIP3`, 
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`, 
  `Dropoff, April 2023 to April 2024`
) %>% arrange(`Dropoff, April 2023 to April 2024`)
zip3B <- zip3 %>% select(
  `LZIP3`, 
  `Applications completed through April 30, 2024`, 
  `Applications completed through March 29, 2024`,
  `Incease, March 2024 to April 2024`
) %>% arrange(desc(`Incease, March 2024 to April 2024`))
write.csv(zip3A, "Dropoffs by ZIP3.csv", row.names=FALSE)
write.csv(zip3B, "Improvements by ZIP3.csv", row.names=FALSE)

#### End #### 

#### Changes by community [ZIP2] ####

setwd("/Users/peter_granville/FAFSA-2024")
zip2 <- goodMerge %>% mutate(`LZIP2` = substr(`LZIP`, 1, 2))
zip2 <- aggregate(data=zip2, cbind(
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`, 
  `Applications completed through March 29, 2024`, 
  `Grade 12 students, 2022-23`
) ~ `LZIP2`, FUN=sum) %>% mutate(
  `Dropoff, April 2023 to April 2024` = (`Applications completed through April 30, 2024` - `Applications completed through April 30, 2023`) / `Applications completed through April 30, 2023`, 
  `Incease, March 2024 to April 2024` = (`Applications completed through April 30, 2024` - `Applications completed through March 29, 2024`) / `Applications completed through March 29, 2024`
) %>% filter(
  `Grade 12 students, 2022-23` > 250
)
zip2A <- zip2 %>% select(
  `LZIP2`, 
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`, 
  `Dropoff, April 2023 to April 2024`
) %>% arrange(`Dropoff, April 2023 to April 2024`)
zip2B <- zip2 %>% select(
  `LZIP2`, 
  `Applications completed through April 30, 2024`, 
  `Applications completed through March 29, 2024`,
  `Incease, March 2024 to April 2024`
) %>% arrange(desc(`Incease, March 2024 to April 2024`))
write.csv(zip2A, "Dropoffs by ZIP2.csv", row.names=FALSE)
write.csv(zip2B, "Improvements by ZIP2.csv", row.names=FALSE)

#### End #### 

####################################
#### School counselors          ####
####################################

#### Linking in data on school counselors by LEA ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
directory <- read.csv("ccd_sch_029_2223.csv") %>% select(`NCESSCH`, `LEAID`, `LEA_NAME`) %>% rename(
  `NCESSCH-ELSI` = `NCESSCH`
) %>% mutate(
  `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
)
districts <- left_join(x=goodMerge, y=directory, by="NCESSCH-ELSI")

districts <- aggregate(data=districts, cbind(
  `Grade 12 students, 2022-23`,
  `Applications submitted through January 31, 2024`,
  `Applications submitted through January 31, 2023`,
  `Applications submitted through February 29, 2024`,
  `Applications submitted through February 29, 2023`,
  `Applications submitted through March 29, 2024`,
  `Applications completed through March 29, 2024`,
  `Applications submitted through March 29, 2023`,
  `Applications completed through March 29, 2023`,
  `Applications submitted through April 30, 2024`,
  `Applications completed through April 30, 2024`,
  `Applications submitted through April 30, 2023`,
  `Applications completed through April 30, 2023`
) ~ `LEAID`, FUN=sum)

enrollment <- fread("ccd_lea_052_2223.csv", header=TRUE, select=c(
  "LEAID",
  "STUDENT_COUNT",
  "TOTAL_INDICATOR"
)) %>% filter(
  `TOTAL_INDICATOR` == "Derived - Education Unit Total minus Adult Education Count"
) %>% select(-(`TOTAL_INDICATOR`)) %>% rename(
  `Total enrollment, 2022-23` = `STUDENT_COUNT`
)
districts <- left_join(x=districts, y=enrollment, by="LEAID")

counselors <- read.csv("ccd_lea_059_2223.csv") %>% select(
  `LEAID`,
  `STAFF`,
  `STAFF_COUNT`
) %>% filter(`STAFF` %in% c("Secondary School Counselors", "Guidance Counselors")) %>% pivot_wider(
  id_cols=c(`LEAID`),
  names_from=`STAFF`,
  values_from=`STAFF_COUNT`
)
districts <- left_join(x=districts, y=counselors, by="LEAID")

districts1 <- districts %>% filter(is.na(`Guidance Counselors`)==FALSE) %>% select(-(`Secondary School Counselors`)) %>% mutate(
  `FTE guidance counselors per student` = `Guidance Counselors` / `Total enrollment, 2022-23`
)
districts2 <- districts %>% filter(is.na(`Secondary School Counselors`)==FALSE) %>% select(-(`Guidance Counselors`)) %>% mutate(
  `FTE secondary school counselors per senior` = `Secondary School Counselors` / `Grade 12 students, 2022-23`
)
districts3 <- districts1
districts4 <- districts2

nGroups <- 20
districts1 <- districts1 %>% mutate(
  `Groups: FTE guidance counselors per student` = ntile(`FTE guidance counselors per student`, nGroups)
)
districts2 <- districts2 %>% mutate(
  `Groups: FTE secondary school counselors per senior` = ntile(`FTE secondary school counselors per senior`, nGroups)
)

nGroups <- 5
districts3 <- districts3 %>% mutate(
  `Groups: FTE guidance counselors per student` = ntile(`FTE guidance counselors per student`, nGroups)
)
districts4 <- districts4 %>% mutate(
  `Groups: FTE secondary school counselors per senior` = ntile(`FTE secondary school counselors per senior`, nGroups)
)

#### End ####

#### Run charts ####

figA20 <- function1(districts1, "Groups: FTE guidance counselors per student", FALSE)
figB20 <- function2(districts1, "Groups: FTE guidance counselors per student", FALSE)
figC20 <- function3(districts3, "Groups: FTE guidance counselors per student", FALSE)
figD20 <- function4(districts1, "Groups: FTE guidance counselors per student", FALSE)

figA21 <- function1(districts2, "Groups: FTE secondary school counselors per senior", FALSE)
figB21 <- function2(districts2, "Groups: FTE secondary school counselors per senior", FALSE)
figC21 <- function3(districts4, "Groups: FTE secondary school counselors per senior", FALSE)
figD21 <- function4(districts2, "Groups: FTE secondary school counselors per senior", FALSE)

#### End #### 

####################################
#### College proximity          ####
####################################

#### Linking in data on college proximity ####

setwd("/Users/peter_granville/FAFSA-2024/IPEDS data")
hd <- fread("hd2022.csv", header=TRUE, select=c(
  "UNITID",
  "ZIP"
)) %>% mutate(`ZIP` = substr(`ZIP`, 1, 5))
effy <- fread("effy2022_dist.csv", header=TRUE, select=c(
 "UNITID",
 "EFFYDLEV",
 "EFYDETOT", # All students enrolled
 "EFYDEEXC", # Students enrolled exclusively in distance education courses
 "EFYDESOM", # Students enrolled in some but not all distance education courses
 "EFYDENON"  # Students not enrolled in any distance education courses
)) %>% filter(`EFFYDLEV`==1) %>% select(-(`EFFYDLEV`)) %>% mutate(
  `Share of students who are enrolled in-person` = (`EFYDENON` + `EFYDESOM`) / `EFYDETOT`
) %>% filter(
  `Share of students who are enrolled in-person` > 0.20
)
hd <- hd %>% filter(`UNITID` %in% effy$`UNITID`)
rm(effy)

zipDistances <- data.frame(`ZCTA5` = unique(goodMerge$`ZCTA5`))
zipDistances <- zipDistances %>% mutate(
  `Has college` = ifelse(substr(`ZCTA5`, 7, 11) %in% hd$`ZIP`, "Yes", "No")
)
zipYes <- zipDistances %>% filter(`Has college`=="Yes") %>% mutate(`Distance to nearest college` = rep(0)) %>% select(-(`Has college`))
zipNo <- zipDistances %>% filter(`Has college`=="No") %>% select(-(`Has college`))
rm(zipDistances)

zipCalcs <- data.frame(`ZCTA5` = character(), `Distance to nearest college` = numeric(), check.names=FALSE)
for(i in (1:nrow(zipNo))){

  tempZips <- data.frame(`ZIP of college` = unique(hd$`ZIP`), check.names=FALSE)
  tempZips <- tempZips %>% mutate(
    `Distance to ZCTA` = zip_distance(`ZIP of college`, substr(zipNo$`ZCTA5`[i], 7, 11))$distance
  ) %>% filter(is.na(`Distance to ZCTA`)==FALSE) %>% filter(is.infinite(`Distance to ZCTA`)==FALSE)
  if(nrow(tempZips) > 0){
    zipCalcs <- zipCalcs %>% add_row(
      `ZCTA5` = zipNo$`ZCTA5`[i],
      `Distance to nearest college` = min(tempZips$`Distance to ZCTA`, na.rm=TRUE)
    )
  }else{
    zipCalcs <- zipCalcs %>% add_row(
      `ZCTA5` = zipNo$`ZCTA5`[i],
      `Distance to nearest college` = NA
    )
  }

}
rm(zipNo)

zipDistances <- rbind(zipYes, zipCalcs)

rankOutputs <- analysis %>% select(
  `ZCTA5`,
  `Grade 12 students, 2022-23`,
  `Applications submitted through January 31, 2024`,
  `Applications submitted through January 31, 2023`,
  `Applications submitted through February 29, 2024`,
  `Applications submitted through February 29, 2023`,
  `Applications submitted through March 29, 2024`,
  `Applications completed through March 29, 2024`,
  `Applications submitted through March 29, 2023`,
  `Applications completed through March 29, 2023`,
  `Applications submitted through April 30, 2024`,
  `Applications completed through April 30, 2024`,
  `Applications submitted through April 30, 2023`,
  `Applications completed through April 30, 2023`
)

zipDistances <- zipDistances %>% mutate(
  `20-tiles: Distance to nearest college` = ntile(`Distance to nearest college`, 20),
  `5-tiles: Distance to nearest college` = ntile(`Distance to nearest college`, 5)
)

zipDistances <- full_join(x=zipDistances, y=rankOutputs, by="ZCTA5")

#### End #### 

#### Run charts #### 

figA22 <- function1(zipDistances, "20-tiles: Distance to nearest college", FALSE)
figB22 <- function2(zipDistances, "20-tiles: Distance to nearest college", FALSE)
figC22 <- function3(zipDistances, "5-tiles: Distance to nearest college", FALSE)
figD22 <- function4(zipDistances, "20-tiles: Distance to nearest college", FALSE)

#### End ####

####################################
#### Alternative years          ####
####################################

#### Load in prior years' data for mandatory FAFSA states ####

setwd("/Users/peter_granville/FAFSA-2024")
priorYears <- fread("Alternative FAFSA Year Dataset.csv", header=TRUE, check.names=FALSE, keepLeadingZeros=TRUE, select=c(
  "NCESSCH", "NCESSCH-ELSI",
  "FAFSA Completions, Class of 2016", 
  "FAFSA Completions, Class of 2017", 
  "FAFSA Completions, Class of 2018", 
  "FAFSA Completions, Class of 2019", 
  "FAFSA Completions, Class of 2020", 
  "FAFSA Completions, Class of 2021", 
  "FAFSA Completions, Class of 2022" 
)) %>% rename(
  `Applications completed through April 30, 2016` = `FAFSA Completions, Class of 2016`, 
  `Applications completed through April 30, 2017` = `FAFSA Completions, Class of 2017`, 
  `Applications completed through April 30, 2018` = `FAFSA Completions, Class of 2018`, 
  `Applications completed through April 30, 2019` = `FAFSA Completions, Class of 2019`, 
  `Applications completed through April 30, 2020` = `FAFSA Completions, Class of 2020`, 
  `Applications completed through April 30, 2021` = `FAFSA Completions, Class of 2021`, 
  `Applications completed through April 30, 2022` = `FAFSA Completions, Class of 2022`
) 
priorYears <- priorYears %>% mutate(`Index` = (1:nrow(priorYears))) %>% mutate(
  `NCESSCH` = as.character(`NCESSCH`), 
  `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
)
priorYears.save <- priorYears

priorYears <- priorYears %>% mutate(
  `NCESSCH match` = ifelse(
    `NCESSCH` %in% goodMerge$`NCESSCH`, 
  1, 0),
  `NCESSCH-ELSI match` = ifelse(
    `NCESSCH-ELSI` %in% goodMerge$`NCESSCH-ELSI`, 
  1, 0)
)

# Round 1 
round1A <- priorYears %>% filter(`NCESSCH match`==1) %>% select(
  `NCESSCH`, 
  `Applications completed through April 30, 2016`, 
  `Applications completed through April 30, 2017`,
  `Applications completed through April 30, 2018`,
  `Applications completed through April 30, 2019`, 
  `Applications completed through April 30, 2020`, 
  `Applications completed through April 30, 2021`, 
  `Applications completed through April 30, 2022`
)
round1B <- goodMerge %>% filter(`NCESSCH` %in% round1A$`NCESSCH`)
round1 <- left_join(x=round1B, y=round1A, by="NCESSCH")
priorYears <- priorYears %>% filter((`NCESSCH` %in% round1$`NCESSCH`)==FALSE)

# Round 2
round2A <- priorYears %>% filter(`NCESSCH-ELSI match`==1) %>% select(
  `NCESSCH-ELSI`, 
  `Applications completed through April 30, 2016`, 
  `Applications completed through April 30, 2017`,
  `Applications completed through April 30, 2018`,
  `Applications completed through April 30, 2019`, 
  `Applications completed through April 30, 2020`, 
  `Applications completed through April 30, 2021`, 
  `Applications completed through April 30, 2022`
)
round2B <- goodMerge %>% filter(`NCESSCH-ELSI` %in% round2A$`NCESSCH-ELSI`)
round2 <- left_join(x=round2B, y=round2A, by="NCESSCH-ELSI")
priorYears <- priorYears %>% filter((`NCESSCH-ELSI` %in% round2$`NCESSCH-ELSI`)==FALSE)

altMerge <- rbind(round1, round2)
rm(round1A, round1B, round2A, round2B)

# 1 - sum(altMerge$`Applications completed through April 30, 2016`) / sum(priorYears.save$`Applications completed through April 30, 2016`)
# 1 - sum(altMerge$`Applications completed through April 30, 2017`) / sum(priorYears.save$`Applications completed through April 30, 2017`)
# 1 - sum(altMerge$`Applications completed through April 30, 2018`) / sum(priorYears.save$`Applications completed through April 30, 2018`)
# 1 - sum(altMerge$`Applications completed through April 30, 2019`) / sum(priorYears.save$`Applications completed through April 30, 2019`)
# 1 - sum(altMerge$`Applications completed through April 30, 2020`) / sum(priorYears.save$`Applications completed through April 30, 2020`)
# 1 - sum(altMerge$`Applications completed through April 30, 2021`) / sum(priorYears.save$`Applications completed through April 30, 2021`)
# 1 - sum(altMerge$`Applications completed through April 30, 2022`) / sum(priorYears.save$`Applications completed through April 30, 2022`)
rm(priorYears.save)

#### End #### 

#### Reset mandatory FAFSA states' comparison year ####

altMerge <- altMerge %>% mutate(`Comparison year` = rep(2023))
altMerge$`Comparison year`[altMerge$State %in% c("CA")] <- 2022
altMerge$`Comparison year`[altMerge$State %in% c("TX", "AL")] <- 2019 # I've decided to make this 2019 to avoid pandemic years. 
altMerge$`Comparison year`[altMerge$State %in% c("IL")] <- 2019 # I've decided to make this 2019 to avoid pandemic years. 
altMerge$`Comparison year`[altMerge$State %in% c("LA")] <- 2017

altMerge <- altMerge %>% mutate(`Comparison completions` = 
  ifelse(`Comparison year`==2023, `Applications completed through April 30, 2023`, 
         ifelse(`Comparison year`==2022, `Applications completed through April 30, 2022`, 
                ifelse(`Comparison year`==2021, `Applications completed through April 30, 2021`, 
                       ifelse(`Comparison year`==2020, `Applications completed through April 30, 2020`, 
                              ifelse(`Comparison year`==2019, `Applications completed through April 30, 2019`, 
                                     ifelse(`Comparison year`==2017, `Applications completed through April 30, 2017`, NA)
                              )
                        )
                )
         )
  )
)

#### End #### 

#### Merge Census data ####

altMerge <- altMerge %>% mutate(
  `ZCTA5` = paste("ZCTA5 ", substr(`LZIP`, 1, 5), sep="")
)
altAnalysis <- aggregate(data=altMerge, cbind(
  `Grade 12 students, 2022-23`,
  `Applications completed through April 30, 2024`, 
  `Comparison completions`
) ~ `ZCTA5`, FUN=sum)

altAnalysis <- left_join(x=altAnalysis, y=census, by="ZCTA5")

#### End #### 

#### Run nTile function #### 

altAnalysis <- altAnalysis %>% mutate(
  `Black or Latino share` = `Black share` + `Hispanic or Latino share`,
  `Black, Latino, or Native American share` = `Black share` + `Hispanic or Latino share` + `Native American share`,
  `Black, Latino, Native American, or Pacific Islander share` = `Black share` + `Hispanic or Latino share` + `Native American share` + `Pacific Islander share`,
  `No college share` = `Less than 9th grade share` + `High school, no diploma share` + `High school diploma share`, 
  `Associate's or higher share` = `Associate's degree share` + `Bachelor's degree share` + `Graduate degree share`
)

altRankInputs <- altAnalysis %>% select(
  `ZCTA5`, 
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
altRankOutputs <- altAnalysis %>% select(
  `ZCTA5`, 
  `Grade 12 students, 2022-23`,
  `Applications completed through April 30, 2024`, 
  `Comparison completions`
)

nGroups <- 20
for(i in (2:ncol(altRankInputs))){
  
  tempTiles <- altRankInputs %>% select(`ZCTA5`, names(altRankInputs)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(altRankInputs)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
altnTiles20 <- full_join(x=nTiles, y=altRankOutputs, by="ZCTA5")
rm(nTiles)

nGroups <- 5
for(i in (2:ncol(altRankInputs))){
  
  tempTiles <- altRankInputs %>% select(`ZCTA5`, names(altRankInputs)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(altRankInputs)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
altnTiles5 <- full_join(x=nTiles, y=altRankOutputs, by="ZCTA5")
rm(nTiles)

nGroups <- 100
for(i in (2:ncol(altRankInputs))){
  
  tempTiles <- altRankInputs %>% select(`ZCTA5`, names(altRankInputs)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(altRankInputs)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
altnTiles100 <- full_join(x=nTiles, y=altRankOutputs, by="ZCTA5")
rm(nTiles)

#### End #### 

#### Charts of YOY decrease in completions with alternative comparison years (20-tiles) (function5) ####

function5 <- function(tilesVar, printData){
  
  newAltTiles <- altnTiles20
  
  newAltTiles <- newAltTiles %>% select(
    `Applications completed through April 30, 2024`, 
    `Comparison completions`, 
    `Grade 12 students, 2022-23`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  ) 
  altAnalysis1 <- aggregate(data=newAltTiles, cbind(
    `Applications completed through April 30, 2024`, 
    `Comparison completions`, 
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `Decline in YoY completions` = (`Applications completed through April 30, 2024` - `Comparison completions`) / `Comparison completions`
  ) 
  figE <- ggplot(data=altAnalysis1, mapping=aes(x=`InterestVar`, y=`Decline in YoY completions`)) + geom_bar(stat="identity", width=0.1) + geom_point() + scale_y_continuous(labels=percent_format(accuracy=1), limits=c(-0.5, 0)) + labs(x=tilesVar)
  if(printData==TRUE){tempDF <- altAnalysis1}
  return(figE)
  rm(altAnalysis1, newAltTiles, figE)
}

#### End #### 

#### Run function5 ####

figE1 <- function5("Groups: Native-born share", FALSE)
figE2 <- function5("Groups: Foreign-born share: AAOA", FALSE)
figE3 <- function5("Groups: Limited English share", FALSE)
figE4 <- function5("Groups: White share", FALSE)
figE5 <- function5("Groups: Black share", FALSE)
figE6 <- function5("Groups: Hispanic or Latino share", FALSE)
figE7 <- function5("Groups: Native American share", FALSE)
figE8 <- function5("Groups: Asian share", FALSE)
figE9 <- function5("Groups: Pacific Islander share", FALSE)
figE10 <- function5("Groups: Other race share", FALSE)
figE11 <- function5("Groups: Two or more races share", FALSE)
figE12 <- function5("Groups: Black or Latino share", FALSE)
figE13 <- function5("Groups: Black, Latino, or Native American share", FALSE)
figE14 <- function5("Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figE15 <- function5("Groups: No college share", FALSE)
figE16 <- function5("Groups: Associate's or higher share", FALSE)
figE17 <- function5("Groups: Households receiving SNAP share", FALSE)
figE18 <- function5("Groups: Share of population in poverty", FALSE)
figE19 <- function5("Groups: Average household income", FALSE)

#### End #### 

#### State gut-check ####

states1 <- aggregate(data=goodMerge, cbind(
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`
) ~ `State`, FUN=sum) %>% mutate(
  `Decline in YoY completions` = (`Applications completed through April 30, 2024` - `Applications completed through April 30, 2023`) / `Applications completed through April 30, 2023`
) 

states2 <- aggregate(data=altMerge, cbind(
  `Applications completed through April 30, 2024`, 
  `Comparison completions`
) ~ `State`, FUN=sum) %>% mutate(
  `Decline in YoY completions` = (`Applications completed through April 30, 2024` - `Comparison completions`) / `Comparison completions`
) 

states1A <- states1 %>% filter((`State` %in% c("CA", "TX", "AL", "IL", "LA"))==FALSE)
# (sum(states1A$`Applications completed through April 30, 2024`) - sum(states1A$`Applications completed through April 30, 2023`)) / sum(states1A$`Applications completed through April 30, 2023`)

states2A <- states2 %>% filter((`State` %in% c("CA", "TX", "AL", "IL", "LA"))==FALSE)
# (sum(states2A$`Applications completed through April 30, 2024`) - sum(states2A$`Comparison completions`)) / sum(states2A$`Comparison completions`)

states1B <- aggregate(data=states1, cbind(
  `Applications completed through April 30, 2024`, 
  `Applications completed through April 30, 2023`
) ~ `State`, FUN=sum) %>% mutate(
  `Change 1` = (`Applications completed through April 30, 2024` - `Applications completed through April 30, 2023`) / `Applications completed through April 30, 2023`
) %>% select(`State`, `Change 1`)
states2B <- aggregate(data=states2, cbind(
  `Applications completed through April 30, 2024`, 
  `Comparison completions`
) ~ `State`, FUN=sum) %>% mutate(
  `Change 2` = (`Applications completed through April 30, 2024` - `Comparison completions`) / `Comparison completions`
) %>% select(`State`, `Change 2`)
states3 <- full_join(x=states1B, y=states2B, by="State") %>% mutate(
  `Difference` = round(`Change 1` - `Change 2`, 5)
)

#### End #### 

####################################
#### Comparison: 2017           ####
####################################

#### Load FAFSA data #### 

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")

jan17 <- read_excel("HS_ARCHIVE01312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJan31  2017", "Applications\nComplete\nJan31  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
feb17 <- read_excel("HS_ARCHIVE02282017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nFeb28  2017", "Applications\nComplete\nFeb28  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
mar17 <- read_excel("HS_ARCHIVE03312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMar31  2017", "Applications\nComplete\nMar31  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
apr17 <- read_excel("HS_ARCHIVE04302017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nApr30  2017", "Applications\nComplete\nApr30  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
may17 <- read_excel("HS_ARCHIVE05312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMay31  2017", "Applications\nComplete\nMay31  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
jun17 <- read_excel("HS_ARCHIVE06302017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJun30  2017", "Applications\nComplete\nJun30  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
jul17 <- read_excel("HS_ARCHIVE07312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJul31  2017", "Applications\nComplete\nJul31  2017") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)

jan17 <- jan17 %>% rename(
  `Applications submitted through January 31, 2017` = `Applications\nSubmitted\nJan31  2017`, 
  `Applications completed through January 31, 2017` = `Applications\nComplete\nJan31  2017`
)
feb17 <- feb17 %>% rename(
  `Applications submitted through February 28, 2017` = `Applications\nSubmitted\nFeb28  2017`, 
  `Applications completed through February 28, 2017` = `Applications\nComplete\nFeb28  2017`
)
mar17 <- mar17 %>% rename(
  `Applications submitted through March 31, 2017` = `Applications\nSubmitted\nMar31  2017`, 
  `Applications completed through March 31, 2017` = `Applications\nComplete\nMar31  2017`
)
apr17 <- apr17 %>% rename(
  `Applications submitted through April 30, 2017` = `Applications\nSubmitted\nApr30  2017`, 
  `Applications completed through April 30, 2017` = `Applications\nComplete\nApr30  2017`
)
may17 <- may17 %>% rename(
  `Applications submitted through May 31, 2017` = `Applications\nSubmitted\nMay31  2017`, 
  `Applications completed through May 31, 2017` = `Applications\nComplete\nMay31  2017`
)
jun17 <- jun17 %>% rename(
  `Applications submitted through June 30, 2017` = `Applications\nSubmitted\nJun30  2017`, 
  `Applications completed through June 30, 2017` = `Applications\nComplete\nJun30  2017`
)
jul17 <- jul17 %>% rename(
  `Applications submitted through July 31, 2017` = `Applications\nSubmitted\nJul31  2017`, 
  `Applications completed through July 31, 2017` = `Applications\nComplete\nJul31  2017`
)

fafsaData2017 <- full_join(x=jan17, y=feb17, by=c("Name", "City", "State")) 
fafsaData2017 <- full_join(x=fafsaData2017, y=mar17, by=c("Name", "City", "State"))
fafsaData2017 <- full_join(x=fafsaData2017, y=apr17, by=c("Name", "City", "State"))
fafsaData2017 <- full_join(x=fafsaData2017, y=may17, by=c("Name", "City", "State"))
fafsaData2017 <- full_join(x=fafsaData2017, y=jun17, by=c("Name", "City", "State"))
fafsaData2017 <- full_join(x=fafsaData2017, y=jul17, by=c("Name", "City", "State"))

rm(jan17, feb17, mar17, apr17, may17, jun17, jul17) 

fafsaData2017 <- fafsaData2017 %>% filter((
  (`Applications completed through January 31, 2017` %in% c("<5", NA)) 
  & (`Applications completed through February 28, 2017` %in% c("<5", NA)) 
  & (`Applications completed through March 31, 2017` %in% c("<5", NA)) 
  & (`Applications completed through April 30, 2017` %in% c("<5", NA)) 
  & (`Applications completed through May 31, 2017` %in% c("<5", NA)) 
  & (`Applications completed through June 30, 2017` %in% c("<5", NA)) 
  & (`Applications completed through July 31, 2017` %in% c("<5", NA))
)==FALSE) 

fafsaData2017$`Applications completed through January 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through January 31, 2017`)
fafsaData2017$`Applications completed through February 28, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through February 28, 2017`)
fafsaData2017$`Applications completed through March 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through March 31, 2017`)
fafsaData2017$`Applications completed through April 30, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through April 30, 2017`)
fafsaData2017$`Applications completed through May 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through May 31, 2017`)
fafsaData2017$`Applications completed through June 30, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through June 30, 2017`)
fafsaData2017$`Applications completed through July 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications completed through July 31, 2017`)

fafsaData2017$`Applications submitted through January 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through January 31, 2017`)
fafsaData2017$`Applications submitted through February 28, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through February 28, 2017`)
fafsaData2017$`Applications submitted through March 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through March 31, 2017`)
fafsaData2017$`Applications submitted through April 30, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through April 30, 2017`)
fafsaData2017$`Applications submitted through May 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through May 31, 2017`)
fafsaData2017$`Applications submitted through June 30, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through June 30, 2017`)
fafsaData2017$`Applications submitted through July 31, 2017` <- gsub("<5", "0", fafsaData2017$`Applications submitted through July 31, 2017`)

fafsaData2017 <- fafsaData2017 %>% mutate(
  `Applications submitted through January 31, 2017` = as.numeric(`Applications submitted through January 31, 2017`), 
  `Applications submitted through February 28, 2017` = as.numeric(`Applications submitted through February 28, 2017`), 
  `Applications submitted through March 31, 2017` = as.numeric(`Applications submitted through March 31, 2017`), 
  `Applications submitted through April 30, 2017` = as.numeric(`Applications submitted through April 30, 2017`), 
  `Applications submitted through May 31, 2017` = as.numeric(`Applications submitted through May 31, 2017`), 
  `Applications submitted through June 30, 2017` = as.numeric(`Applications submitted through June 30, 2017`), 
  `Applications submitted through July 31, 2017` = as.numeric(`Applications submitted through July 31, 2017`)
)
fafsaData2017 <- fafsaData2017 %>% mutate(
  `Applications completed through January 31, 2017` = as.numeric(`Applications completed through January 31, 2017`), 
  `Applications completed through February 28, 2017` = as.numeric(`Applications completed through February 28, 2017`), 
  `Applications completed through March 31, 2017` = as.numeric(`Applications completed through March 31, 2017`), 
  `Applications completed through April 30, 2017` = as.numeric(`Applications completed through April 30, 2017`), 
  `Applications completed through May 31, 2017` = as.numeric(`Applications completed through May 31, 2017`), 
  `Applications completed through June 30, 2017` = as.numeric(`Applications completed through June 30, 2017`), 
  `Applications completed through July 31, 2017` = as.numeric(`Applications completed through July 31, 2017`)
)

fafsaData2017$`Applications submitted through January 31, 2017`[is.na(fafsaData2017$`Applications submitted through January 31, 2017`)] <- 0
fafsaData2017$`Applications submitted through February 28, 2017`[is.na(fafsaData2017$`Applications submitted through February 28, 2017`)] <- 0
fafsaData2017$`Applications submitted through March 31, 2017`[is.na(fafsaData2017$`Applications submitted through March 31, 2017`)] <- 0
fafsaData2017$`Applications submitted through April 30, 2017`[is.na(fafsaData2017$`Applications submitted through April 30, 2017`)] <- 0
fafsaData2017$`Applications submitted through May 31, 2017`[is.na(fafsaData2017$`Applications submitted through May 31, 2017`)] <- 0
fafsaData2017$`Applications submitted through June 30, 2017`[is.na(fafsaData2017$`Applications submitted through June 30, 2017`)] <- 0
fafsaData2017$`Applications submitted through July 31, 2017`[is.na(fafsaData2017$`Applications submitted through July 31, 2017`)] <- 0

fafsaData2017$`Applications completed through January 31, 2017`[is.na(fafsaData2017$`Applications completed through January 31, 2017`)] <- 0
fafsaData2017$`Applications completed through February 28, 2017`[is.na(fafsaData2017$`Applications completed through February 28, 2017`)] <- 0
fafsaData2017$`Applications completed through March 31, 2017`[is.na(fafsaData2017$`Applications completed through March 31, 2017`)] <- 0
fafsaData2017$`Applications completed through April 30, 2017`[is.na(fafsaData2017$`Applications completed through April 30, 2017`)] <- 0
fafsaData2017$`Applications completed through May 31, 2017`[is.na(fafsaData2017$`Applications completed through May 31, 2017`)] <- 0
fafsaData2017$`Applications completed through June 30, 2017`[is.na(fafsaData2017$`Applications completed through June 30, 2017`)] <- 0
fafsaData2017$`Applications completed through July 31, 2017`[is.na(fafsaData2017$`Applications completed through July 31, 2017`)] <- 0

fafsaData2017 <- fafsaData2017 %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

#### End #### 

#### Load in school codes ####

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")
schoolCodes <- read_xls("NL.xls", skip=4, col_names=FALSE) %>% select(`...1`, `...2`, `...3`, `...4`)
names(schoolCodes) <- c("School Code", "Name", "City", "State")
fafsaData2017 <- right_join(x=schoolCodes, y=fafsaData2017, by=c("Name", "City", "State"))
rm(schoolCodes)

#### End #### 

#### Data cleaning ####

# Remove private schools:
fafsaData2017 <- fafsaData2017 %>% filter((`School Code` %in% privateSchools$`School ID`)==FALSE)

# Remove any other schools with alphabet characters in name (indicating private): 
fafsaData2017 <- fafsaData2017 %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)

# Remove other schools with religious terms in names:  
fafsaData2017 <- fafsaData2017 %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)

# Remove schools we won't be able to link to Census data: 
fafsaData2017 <- fafsaData2017 %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

# Remove virtual schools 
fafsaData2017 <- fafsaData2017 %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)

# Index the FAFSA data: 
fafsaData2017 <- fafsaData2017 %>% mutate(`Index` = (1:nrow(fafsaData2017))) %>% rename(`NCESSCH` = `School Code`)

#### End #### 

#### Load ELSI data ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
elsiData2017 <- read.csv("ccd_sch_029_1617.csv") %>% select(`NCESSCH`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`)
elsiData2017 <- elsiData2017 %>% filter(`G_12_OFFERED` != "No")
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = as.character(`NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`MZIP` = as.character(`MZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==1, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==2, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==3, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`MZIP` = ifelse(nchar(`MZIP`)==4, paste("0", `MZIP`, sep=""), `MZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`LZIP` = as.character(`LZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==1, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==2, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==3, paste("0", `LZIP`, sep=""), `LZIP`))
elsiData2017 <- elsiData2017 %>% mutate(`LZIP` = ifelse(nchar(`LZIP`)==4, paste("0", `LZIP`, sep=""), `LZIP`))

elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==1, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==2, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==3, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==4, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==5, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==6, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==7, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==8, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==9, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==10, paste("0", `NCESSCH`, sep=""), `NCESSCH`))
elsiData2017 <- elsiData2017 %>% mutate(`NCESSCH` = ifelse(nchar(`NCESSCH`)==11, paste("0", `NCESSCH`, sep=""), `NCESSCH`))

elsiData2017 <- elsiData2017 %>% mutate(`ComboName` = paste(`SCH_NAME`, `MCITY`, `ST`, `MZIP`, sep="-")) %>% mutate(`Count` = rep(1))
comboCount <- aggregate(data=elsiData2017, `Count` ~ `ComboName`, FUN=sum) %>% filter(`Count` > 1)
comboCode <- elsiData2017 %>% select(`NCESSCH`, `ComboName`) %>% filter(`ComboName` %in% comboCount$`ComboName`)
comboCode <- comboCode %>% mutate(`FAFSA Match` = ifelse(`NCESSCH` %in% fafsaData2017$`NCESSCH`, 1, 0))

for(i in (1:length(unique(comboCount$`ComboName`)))){
  
  selectedSchool <- elsiData2017 %>% filter(`ComboName` == unique(comboCount$`ComboName`)[i])
  elsiData2017 <- elsiData2017 %>% filter(`ComboName` != unique(comboCount$`ComboName`)[i])
  selectedSchool <- selectedSchool %>% mutate(`FAFSA Match` = ifelse(`NCESSCH` %in% fafsaData2017$`NCESSCH`, 1, 0))
  if(sum(selectedSchool$`FAFSA Match`) > 0){
    selectedSchool <- selectedSchool %>% filter(`FAFSA Match`==1) %>% select(-(`FAFSA Match`))
    elsiData2017 <- rbind(elsiData2017, selectedSchool)
  }else{
    selectedSchool <- selectedSchool %>% select(-(`FAFSA Match`))
    selectedSchool <- sample_n(selectedSchool, 1)
    elsiData2017 <- rbind(elsiData2017, selectedSchool)
  }
  rm(selectedSchool)
}
rm(i, comboCode, comboCount)
elsiData2017 <- elsiData2017 %>% select(-(`Count`)) %>% select(-(`ComboName`))

elsiData2017 <- elsiData2017 %>% mutate(
  `Code match` = ifelse(`NCESSCH` %in% fafsaData2017$`NCESSCH`, 1, 0)
) %>% arrange(desc(`Code match`)) %>% filter((duplicated(`NCESSCH`)==FALSE) | (is.na(`NCESSCH`))) %>% select(-(`Code match`))

elsiData2017 <- elsiData2017 %>% mutate(
  `SCH_NAME` = iconv(`SCH_NAME`, from = 'UTF-8', to = 'ASCII//TRANSLIT')
)
elsiData2017 <- elsiData2017 %>% mutate(
  `SCH_NAME` = toupper(`SCH_NAME`), 
  `MCITY` = toupper(`MCITY`), 
  `LCITY` = toupper(`LCITY`)
)

# Clean: 
elsiData2017 <- elsiData2017 %>% filter((`SCH_NAME` != "AMANDLA CHARTER SCHOOL") | (`MZIP`=="60621"))
elsiData2017 <- elsiData2017 %>% filter((`SCH_NAME` != "OPTIONS SCHOOL") | (`MZIP` != "80015"))

#### End #### 

#### Merge: Round 1 ####

goodMerge2017 <- inner_join(x=fafsaData2017, y=elsiData2017, by="NCESSCH") %>% mutate(`Merge Round` = rep("Round 1"))
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH` %in% goodMerge2017$NCESSCH)==FALSE) %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
goodMerge2017 <- goodMerge2017 %>% mutate(`NCESSCH-ELSI` = `NCESSCH`)
elsiData2017 <- elsiData2017 %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

#### End #### 

#### Merge: Round 2 #### 

# Start: 
newFafsa <- remainingFafsa2017 
newElsi <- remainingElsi2017

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 2"))
goodMerge2017 <- rbind(goodMerge2017, newMerge)
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH-ELSI` %in% goodMerge2017$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

#### End #### 

#### Merge: Round 3 ####

# Start: 
newFafsa <- remainingFafsa2017 
newElsi <- remainingElsi2017 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `LCITY`, `ST`, sep="-"))

# End: 
newMerge2017 <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 3"))
goodMerge2017 <- rbind(goodMerge2017, newMerge2017)
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH-ELSI` %in% goodMerge2017$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge2017)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

#### End #### 

#### Merge: Round 4 ####

# Start: 
newFafsa <- remainingFafsa2017 
newElsi <- remainingElsi2017 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newFafsa <- newFafsa %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 4"))
goodMerge2017 <- rbind(goodMerge2017, newMerge)
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH-ELSI` %in% goodMerge2017$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

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
newFafsa <- remainingFafsa2017 
newElsi <- remainingElsi2017 

# Middle: 
middleFafsa <- data.frame(
  `NCESSCH` = character(),
  `Name` = character(), 
  `City` = character(),
  `State` = character(),
  `Applications completed through January 31, 2017` = numeric(), 
  `Applications submitted through January 31, 2017` = numeric(), 
  `Applications completed through February 28, 2017` = numeric(), 
  `Applications submitted through February 28, 2017` = numeric(), 
  `Applications completed through March 31, 2017` = numeric(), 
  `Applications submitted through March 31, 2017` = numeric(), 
  `Applications completed through April 30, 2017` = numeric(), 
  `Applications submitted through April 30, 2017` = numeric(), 
  `Applications completed through May 31, 2017` = numeric(), 
  `Applications submitted through May 31, 2017` = numeric(), 
  `Applications completed through June 30, 2017` = numeric(), 
  `Applications submitted through June 30, 2017` = numeric(), 
  `Applications completed through July 31, 2017` = numeric(), 
  `Applications submitted through July 31, 2017` = numeric(), 
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
  `Applications completed through January 31, 2017`, 
  `Applications submitted through January 31, 2017`, 
  `Applications completed through February 28, 2017`, 
  `Applications submitted through February 28, 2017`, 
  `Applications completed through March 31, 2017`, 
  `Applications submitted through March 31, 2017`, 
  `Applications completed through April 30, 2017`, 
  `Applications submitted through April 30, 2017`, 
  `Applications completed through May 31, 2017`, 
  `Applications submitted through May 31, 2017`, 
  `Applications completed through June 30, 2017`, 
  `Applications submitted through June 30, 2017`, 
  `Applications completed through July 31, 2017`, 
  `Applications submitted through July 31, 2017`, 
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`)

# End: 
newMerge <- newMerge %>% mutate(`Merge Round` = rep("Round 5"))
goodMerge2017 <- rbind(goodMerge2017, newMerge)
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH-ELSI` %in% goodMerge2017$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge, middleFafsa)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)
rm(zips)

# Special clean-up: "COLORADO EARLY COLLEGES FORT COLLINS" 
goodMerge2017 <- goodMerge2017 %>% filter(duplicated(`Index`)==FALSE)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

#### End #### 

#### Merge: Round 6 ####

newFafsa <- remainingFafsa2017 
newElsi <- remainingElsi2017 

# Middle: 
middleFafsa <- newFafsa %>% select(`Name`, `State`, `City`, `Index`)
middleElsi <- newElsi %>% filter(`G_12_OFFERED`=="Yes") %>% select(`SCH_NAME`, `ST`, `LCITY`, `NCESSCH-ELSI`) %>% rename(`State` = `ST`)
middleMerge <- left_join(x=middleFafsa, y=middleElsi, by="State", relationship="many-to-many")
rm(middleFafsa, middleElsi)
middleMerge <- middleMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`)) %>% filter(`School Name Similarity Index` > 0.92)  
middleMerge <- middleMerge %>% arrange(desc(`School Name Similarity Index`)) %>% filter(duplicated(`Index`)==FALSE)
middleMerge <- middleMerge %>% select(`Index`, `NCESSCH-ELSI`)
middleMerge <- left_join(x=middleMerge, y=newFafsa, by="Index")
middleMerge <- left_join(x=middleMerge, y=newElsi, by="NCESSCH-ELSI")

# End: 
middleMerge <- middleMerge %>% select(
  `NCESSCH`, `Name`, `City`, `State`, 
  `Applications completed through January 31, 2017`, 
  `Applications submitted through January 31, 2017`, 
  `Applications completed through February 28, 2017`, 
  `Applications submitted through February 28, 2017`, 
  `Applications completed through March 31, 2017`, 
  `Applications submitted through March 31, 2017`, 
  `Applications completed through April 30, 2017`, 
  `Applications submitted through April 30, 2017`, 
  `Applications completed through May 31, 2017`, 
  `Applications submitted through May 31, 2017`, 
  `Applications completed through June 30, 2017`, 
  `Applications submitted through June 30, 2017`, 
  `Applications completed through July 31, 2017`, 
  `Applications submitted through July 31, 2017`, 
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`
) %>% mutate(`Merge Round` = rep("Round 6"))
goodMerge2017 <- rbind(goodMerge2017, middleMerge)
remainingFafsa2017 <- fafsaData2017 %>% filter((`Index` %in% goodMerge2017$Index)==FALSE)
remainingElsi2017 <- elsiData2017 %>% filter((`NCESSCH-ELSI` %in% goodMerge2017$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, middleMerge)
# nrow(remainingFafsa2017) + nrow(goodMerge2017)

#### End #### 

#### Load in and merge high school grade 12 enrollment ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
seniors2017 <- fread("ELSI Enrollment 2016-17.csv", header=TRUE, skip=6, nrow=99853)
seniors2017 <- seniors2017 %>% rename(
  `NCESSCH-ELSI` = `School ID - NCES Assigned [Public School] Latest available year`, 
  `Grade 12 students, 2016-17` = `Grade 12 Students [Public School] 2016-17`
) %>% select(
  `NCESSCH-ELSI`, 
  `Grade 12 students, 2016-17`
) %>% mutate(
  `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
) %>% mutate(
  `Grade 12 students, 2016-17` = gsub("‡", "", `Grade 12 students, 2016-17`)
) %>% mutate(
  `Grade 12 students, 2016-17` = gsub("†", "", `Grade 12 students, 2016-17`)
) %>% mutate(
  `Grade 12 students, 2016-17` = gsub("–", "", `Grade 12 students, 2016-17`)
) %>% mutate(
  `Grade 12 students, 2016-17` = as.numeric(`Grade 12 students, 2016-17`)
) 
publicGrade12.2017 <- sum(seniors2017$`Grade 12 students, 2016-17`, na.rm=TRUE) 

seniors2017 <- seniors2017 %>% mutate(`NCESSCH-ELSI` = ifelse(nchar(`NCESSCH-ELSI`)==9, paste("0", `NCESSCH-ELSI`, sep=""), `NCESSCH-ELSI`))
seniors2017 <- seniors2017 %>% mutate(`NCESSCH-ELSI` = ifelse(nchar(`NCESSCH-ELSI`)==10, paste("0", `NCESSCH-ELSI`, sep=""), `NCESSCH-ELSI`))
seniors2017 <- seniors2017 %>% mutate(`NCESSCH-ELSI` = ifelse(nchar(`NCESSCH-ELSI`)==11, paste("0", `NCESSCH-ELSI`, sep=""), `NCESSCH-ELSI`))

goodMerge2017 <- left_join(x=goodMerge2017, y=seniors2017, by="NCESSCH-ELSI")
goodMerge2017$`Grade 12 students, 2016-17`[is.na(goodMerge2017$`Grade 12 students, 2016-17`)] <- 0 
rm(seniors2017)

#### End #### 

#### Merge Census data with FAFSA data ####

goodMerge2017 <- goodMerge2017 %>% mutate(
  `ZCTA5` = paste("ZCTA5 ", substr(`LZIP`, 1, 5), sep="")
)
analysis2017 <- aggregate(data=goodMerge2017, cbind(
  `Grade 12 students, 2016-17`,
  `Applications completed through January 31, 2017`, 
  `Applications submitted through January 31, 2017`, 
  `Applications completed through February 28, 2017`, 
  `Applications submitted through February 28, 2017`, 
  `Applications completed through March 31, 2017`, 
  `Applications submitted through March 31, 2017`, 
  `Applications completed through April 30, 2017`, 
  `Applications submitted through April 30, 2017`, 
  `Applications completed through May 31, 2017`, 
  `Applications submitted through May 31, 2017`, 
  `Applications completed through June 30, 2017`, 
  `Applications submitted through June 30, 2017`, 
  `Applications completed through July 31, 2017`, 
  `Applications submitted through July 31, 2017`
) ~ `ZCTA5`, FUN=sum)

analysis2017 <- left_join(x=analysis2017, y=census, by="ZCTA5")

#### End #### 

#### Group ZCTA5 ####

analysis2017 <- analysis2017 %>% mutate(
  `Black or Latino share` = `Black share` + `Hispanic or Latino share`,
  `Black, Latino, or Native American share` = `Black share` + `Hispanic or Latino share` + `Native American share`,
  `Black, Latino, Native American, or Pacific Islander share` = `Black share` + `Hispanic or Latino share` + `Native American share` + `Pacific Islander share`,
  `No college share` = `Less than 9th grade share` + `High school, no diploma share` + `High school diploma share`, 
  `Associate's or higher share` = `Associate's degree share` + `Bachelor's degree share` + `Graduate degree share`
)

rankInputs2017 <- analysis2017 %>% select(
  `ZCTA5`, 
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
rankOutputs2017 <- analysis2017 %>% select(
  `ZCTA5`, 
  `Grade 12 students, 2016-17`,
  `Applications completed through January 31, 2017`, 
  `Applications submitted through January 31, 2017`, 
  `Applications completed through February 28, 2017`, 
  `Applications submitted through February 28, 2017`, 
  `Applications completed through March 31, 2017`, 
  `Applications submitted through March 31, 2017`, 
  `Applications completed through April 30, 2017`, 
  `Applications submitted through April 30, 2017`, 
  `Applications completed through May 31, 2017`, 
  `Applications submitted through May 31, 2017`, 
  `Applications completed through June 30, 2017`, 
  `Applications submitted through June 30, 2017`, 
  `Applications completed through July 31, 2017`, 
  `Applications submitted through July 31, 2017`
)

nGroups <- 20
for(i in (2:ncol(rankInputs2017))){
  
  tempTiles <- rankInputs2017 %>% select(`ZCTA5`, names(rankInputs2017)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2017)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles20.2017 <- full_join(x=nTiles, y=rankOutputs2017, by="ZCTA5")
rm(nTiles)

nGroups <- 5
for(i in (2:ncol(rankInputs2017))){
  
  tempTiles <- rankInputs2017 %>% select(`ZCTA5`, names(rankInputs2017)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2017)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles5.2017 <- full_join(x=nTiles, y=rankOutputs2017, by="ZCTA5")
rm(nTiles)

nGroups <- 100
for(i in (2:ncol(rankInputs2017))){
  
  tempTiles <- rankInputs2017 %>% select(`ZCTA5`, names(rankInputs2017)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2017)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles100.2017 <- full_join(x=nTiles, y=rankOutputs2017, by="ZCTA5")
rm(nTiles)

#### End #### 

#### Visualize submission rates over time (function6) ####

function6 <- function(tilesVar, printData){
  
  newTiles5.2017 <- nTiles5.2017
  newTiles5.2017 <- newTiles5.2017 %>% select(
    `Applications submitted through July 31, 2017`,
    `Applications submitted through June 30, 2017`,
    `Applications submitted through May 31, 2017`,
    `Applications submitted through April 30, 2017`,
    `Applications submitted through March 31, 2017`, 
    `Applications submitted through February 28, 2017`, 
    `Applications submitted through January 31, 2017`,
    `Grade 12 students, 2016-17`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  )

  newTiles5 <- nTiles5
  newTiles5 <- newTiles5 %>% select(
    `Applications submitted through April 30, 2024`,
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  )
  
  analysis1A <- aggregate(data=newTiles5.2017, cbind(
    `Applications submitted through July 31, 2017`,
    `Applications submitted through June 30, 2017`,
    `Applications submitted through May 31, 2017`,
    `Applications submitted through April 30, 2017`,
    `Applications submitted through March 31, 2017`, 
    `Applications submitted through February 28, 2017`, 
    `Applications submitted through January 31, 2017`,
    `Grade 12 students, 2016-17`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `End of January` = `Applications submitted through January 31, 2017` / `Grade 12 students, 2016-17`, 
    `End of February` = `Applications submitted through February 28, 2017` / `Grade 12 students, 2016-17`, 
    `End of March` = `Applications submitted through March 31, 2017` / `Grade 12 students, 2016-17`,
    `End of April` = `Applications submitted through April 30, 2017` / `Grade 12 students, 2016-17`, 
    `End of May` = `Applications submitted through May 31, 2017` / `Grade 12 students, 2016-17`, 
    `End of June` = `Applications submitted through June 30, 2017` / `Grade 12 students, 2016-17`, 
    `End of July` = `Applications submitted through July 31, 2017` / `Grade 12 students, 2016-17`
  ) %>% select(
    `InterestVar`, 
    `End of January`, 
    `End of February`, 
    `End of March`,
    `End of April`, 
    `End of May`, 
    `End of June`, 
    `End of July`
  ) %>% pivot_longer(cols=c(
    `End of January`, 
    `End of February`, 
    `End of March`,
    `End of April`, 
    `End of May`, 
    `End of June`, 
    `End of July`
  ), names_to="Date", values_to="Submission rate") %>% mutate(
    `Date` = factor(`Date`, levels=c(
      "End of January", 
      "End of February", 
      "End of March", 
      "End of April", 
      "End of May", 
      "End of June", 
      "End of July"
    ))
  ) %>% mutate(
    `Year` = rep("Class of 2017")
  )
  analysis1B <- aggregate(data=newTiles5, cbind(
    `Applications submitted through April 30, 2024`,
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `End of January` = `Applications submitted through January 31, 2024` / `Grade 12 students, 2022-23`, 
    `End of February` = `Applications submitted through February 29, 2024` / `Grade 12 students, 2022-23`, 
    `End of March` = `Applications submitted through March 29, 2024` / `Grade 12 students, 2022-23`,
    `End of April` = `Applications submitted through April 30, 2024` / `Grade 12 students, 2022-23`, 
    `End of May` = rep(NA), 
    `End of June` = rep(NA), 
    `End of July` = rep(NA)
  ) %>% select(
    `InterestVar`, 
    `End of January`, 
    `End of February`, 
    `End of March`,
    `End of April`, 
    `End of May`, 
    `End of June`, 
    `End of July`
  ) %>% pivot_longer(cols=c(
    `End of January`, 
    `End of February`, 
    `End of March`,
    `End of April`, 
    `End of May`, 
    `End of June`, 
    `End of July`
  ), names_to="Date", values_to="Submission rate") %>% mutate(
    `Date` = factor(`Date`, levels=c(
      "End of January", 
      "End of February", 
      "End of March", 
      "End of April", 
      "End of May", 
      "End of June", 
      "End of July"
    ))
  ) %>% mutate(
    `Year` = rep("Class of 2024")
  )
  analysis1 <- rbind(analysis1A, analysis1B)
  analysis1 <- analysis1 %>% filter(
    `InterestVar` %in% c(1, 5)
  )
  figG <- ggplot(data=analysis1, mapping=aes(x=`Date`, y=`Submission rate`, group=interaction(`InterestVar`, `Year`), color=`InterestVar`)) + geom_point(size=2.5) + geom_line(aes(linetype=`Year`), size=1.5) + theme(legend.position="bottom") + scale_y_continuous(limits=c(0, 0.75), labels=percent_format(accuracy=1)) + scale_linetype_manual(values=c("dotted", "longdash")) + labs(color=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  rm(analysis1A, analysis1B, analysis1)
  rm(newTiles5.2017, newTiles5)
  return(figG)
}

#### End #### 

#### Run function6 ####

figF1 <- function6("Groups: Native-born share", FALSE)
figF2 <- function6("Groups: Foreign-born share: AAOA", FALSE)
figF3 <- function6("Groups: Limited English share", FALSE)
figF4 <- function6("Groups: White share", FALSE)
figF5 <- function6("Groups: Black share", FALSE)
figF6 <- function6("Groups: Hispanic or Latino share", FALSE)
figF7 <- function6("Groups: Native American share", FALSE)
figF8 <- function6("Groups: Asian share", FALSE)
figF9 <- function6("Groups: Pacific Islander share", FALSE)
figF10 <- function6("Groups: Other race share", FALSE)
figF11 <- function6("Groups: Two or more races share", FALSE)
figF12 <- function6("Groups: Black or Latino share", FALSE)
figF13 <- function6("Groups: Black, Latino, or Native American share", FALSE)
figF14 <- function6("Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figF15 <- function6("Groups: No college share", FALSE)
figF16 <- function6("Groups: Associate's or higher share", FALSE)
figF17 <- function6("Groups: Households receiving SNAP share", FALSE)
figF18 <- function6("Groups: Share of population in poverty", FALSE)
figF19 <- function6("Groups: Average household income", FALSE)

#### End #### 

####################################
#### Comparison: 2023           ####   
####################################

#### Load FAFSA data #### 

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")

oct22 <- read_excel("HS_ARCHIVE10312022.xls", skip=3, sheet=" 2023-24 Cycle") %>% select("Name", "City", "State", "Applications\nSubmitted\nOct31  2022", "Applications\nComplete\nOct31  2022") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
nov22 <- read_excel("HS_ARCHIVE11302022.xls", skip=3, sheet=" 2023-24 Cycle") %>% select("Name", "City", "State", "Applications\nSubmitted\nNov30  2022", "Applications\nComplete\nNov30  2022") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
dec22 <- read_excel("HS_ARCHIVE12312022.xls", skip=3, sheet=" 2023-24 Cycle") %>% select("Name", "City", "State", "Applications\nSubmitted\nDec31  2022", "Applications\nComplete\nDec31  2022") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
jan23 <- read_excel("HS_ARCHIVE01312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJan31  2023", "Applications\nComplete\nJan31  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
feb23 <- read_excel("HS_ARCHIVE02282023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nFeb28  2023", "Applications\nComplete\nFeb28  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
mar23 <- read_excel("HS_ARCHIVE03312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMar31  2023", "Applications\nComplete\nMar31  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
apr23 <- read_excel("HS_ARCHIVE04302023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nApr30  2023", "Applications\nComplete\nApr30  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
may23 <- read_excel("HS_ARCHIVE05312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMay31  2023", "Applications\nComplete\nMay31  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
jun23 <- read_excel("HS_ARCHIVE06302023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJun30  2023", "Applications\nComplete\nJun30  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)
jul23 <- read_excel("HS_ARCHIVE07312023.xls", skip=3) %>% select("School Code", "Name", "City", "State", "Applications\nSubmitted\nJul31  2023", "Applications\nComplete\nJul31  2023") %>% filter((`State` %in% c("AS", "FC", "FM", "GU", "MH", "MP", "PW", "VI"))==FALSE)

oct22 <- oct22 %>% rename(
  `Applications submitted through October 31, 2022` = `Applications\nSubmitted\nOct31  2022`, 
  `Applications completed through October 31, 2022` = `Applications\nComplete\nOct31  2022`
)
nov22 <- nov22 %>% rename(
  `Applications submitted through November 30, 2022` = `Applications\nSubmitted\nNov30  2022`, 
  `Applications completed through November 30, 2022` = `Applications\nComplete\nNov30  2022`
)
dec22 <- dec22 %>% rename(
  `Applications submitted through December 31, 2022` = `Applications\nSubmitted\nDec31  2022`, 
  `Applications completed through December 31, 2022` = `Applications\nComplete\nDec31  2022`
)
jan23 <- jan23 %>% rename(
  `Applications submitted through January 31, 2023` = `Applications\nSubmitted\nJan31  2023`, 
  `Applications completed through January 31, 2023` = `Applications\nComplete\nJan31  2023`
)
feb23 <- feb23 %>% rename(
  `Applications submitted through February 28, 2023` = `Applications\nSubmitted\nFeb28  2023`, 
  `Applications completed through February 28, 2023` = `Applications\nComplete\nFeb28  2023`
)
mar23 <- mar23 %>% rename(
  `Applications submitted through March 31, 2023` = `Applications\nSubmitted\nMar31  2023`, 
  `Applications completed through March 31, 2023` = `Applications\nComplete\nMar31  2023`
)
apr23 <- apr23 %>% rename(
  `Applications submitted through April 30, 2023` = `Applications\nSubmitted\nApr30  2023`, 
  `Applications completed through April 30, 2023` = `Applications\nComplete\nApr30  2023`
)
may23 <- may23 %>% rename(
  `Applications submitted through May 31, 2023` = `Applications\nSubmitted\nMay31  2023`, 
  `Applications completed through May 31, 2023` = `Applications\nComplete\nMay31  2023`
)
jun23 <- jun23 %>% rename(
  `Applications submitted through June 30, 2023` = `Applications\nSubmitted\nJun30  2023`, 
  `Applications completed through June 30, 2023` = `Applications\nComplete\nJun30  2023`
)
jul23 <- jul23 %>% rename(
  `Applications submitted through July 31, 2023` = `Applications\nSubmitted\nJul31  2023`, 
  `Applications completed through July 31, 2023` = `Applications\nComplete\nJul31  2023`
)

oct22$`Applications completed through October 31, 2022` <- as.numeric(gsub("<5", "0", oct22$`Applications completed through October 31, 2022`))
nov22$`Applications completed through November 30, 2022` <- as.numeric(gsub("<5", "0", nov22$`Applications completed through November 30, 2022`))
dec22$`Applications completed through December 31, 2022` <- as.numeric(gsub("<5", "0", dec22$`Applications completed through December 31, 2022`))
jan23$`Applications completed through January 31, 2023` <- as.numeric(gsub("<5", "0", jan23$`Applications completed through January 31, 2023`))
feb23$`Applications completed through February 28, 2023` <- as.numeric(gsub("<5", "0", feb23$`Applications completed through February 28, 2023`))
mar23$`Applications completed through March 31, 2023` <- as.numeric(gsub("<5", "0", mar23$`Applications completed through March 31, 2023`))
apr23$`Applications completed through April 30, 2023` <- as.numeric(gsub("<5", "0", apr23$`Applications completed through April 30, 2023`))
may23$`Applications completed through May 31, 2023` <- as.numeric(gsub("<5", "0", may23$`Applications completed through May 31, 2023`))
jun23$`Applications completed through June 30, 2023` <- as.numeric(gsub("<5", "0", jun23$`Applications completed through June 30, 2023`))
jul23$`Applications completed through July 31, 2023` <- as.numeric(gsub("<5", "0", jul23$`Applications completed through July 31, 2023`))

oct22$`Applications submitted through October 31, 2022` <- as.numeric(gsub("<5", "0", oct22$`Applications submitted through October 31, 2022`))
nov22$`Applications submitted through November 30, 2022` <- as.numeric(gsub("<5", "0", nov22$`Applications submitted through November 30, 2022`))
dec22$`Applications submitted through December 31, 2022` <- as.numeric(gsub("<5", "0", dec22$`Applications submitted through December 31, 2022`))
jan23$`Applications submitted through January 31, 2023` <- as.numeric(gsub("<5", "0", jan23$`Applications submitted through January 31, 2023`))
feb23$`Applications submitted through February 28, 2023` <- as.numeric(gsub("<5", "0", feb23$`Applications submitted through February 28, 2023`))
mar23$`Applications submitted through March 31, 2023` <- as.numeric(gsub("<5", "0", mar23$`Applications submitted through March 31, 2023`))
apr23$`Applications submitted through April 30, 2023` <- as.numeric(gsub("<5", "0", apr23$`Applications submitted through April 30, 2023`))
may23$`Applications submitted through May 31, 2023` <- as.numeric(gsub("<5", "0", may23$`Applications submitted through May 31, 2023`))
jun23$`Applications submitted through June 30, 2023` <- as.numeric(gsub("<5", "0", jun23$`Applications submitted through June 30, 2023`))
jul23$`Applications submitted through July 31, 2023` <- as.numeric(gsub("<5", "0", jul23$`Applications submitted through July 31, 2023`))

oct22$`Applications completed through October 31, 2022`[is.na(oct22$`Applications completed through October 31, 2022`)] <- 0
nov22$`Applications completed through November 30, 2022`[is.na(nov22$`Applications completed through November 30, 2022`)] <- 0
dec22$`Applications completed through December 31, 2022`[is.na(dec22$`Applications completed through December 31, 2022`)] <- 0
jan23$`Applications completed through January 31, 2023`[is.na(jan23$`Applications completed through January 31, 2023`)] <- 0
feb23$`Applications completed through February 28, 2023`[is.na(feb23$`Applications completed through February 28, 2023`)] <- 0
mar23$`Applications completed through March 31, 2023`[is.na(mar23$`Applications completed through March 31, 2023`)] <- 0
apr23$`Applications completed through April 30, 2023`[is.na(apr23$`Applications completed through April 30, 2023`)] <- 0
may23$`Applications completed through May 31, 2023`[is.na(may23$`Applications completed through May 31, 2023`)] <- 0
jun23$`Applications completed through June 30, 2023`[is.na(jun23$`Applications completed through June 30, 2023`)] <- 0
jul23$`Applications completed through July 31, 2023`[is.na(jul23$`Applications completed through July 31, 2023`)] <- 0

oct22$`Applications submitted through October 31, 2022`[is.na(oct22$`Applications submitted through October 31, 2022`)] <- 0
nov22$`Applications submitted through November 30, 2022`[is.na(nov22$`Applications submitted through November 30, 2022`)] <- 0
dec22$`Applications submitted through December 31, 2022`[is.na(dec22$`Applications submitted through December 31, 2022`)] <- 0
jan23$`Applications submitted through January 31, 2023`[is.na(jan23$`Applications submitted through January 31, 2023`)] <- 0
feb23$`Applications submitted through February 28, 2023`[is.na(feb23$`Applications submitted through February 28, 2023`)] <- 0
mar23$`Applications submitted through March 31, 2023`[is.na(mar23$`Applications submitted through March 31, 2023`)] <- 0
apr23$`Applications submitted through April 30, 2023`[is.na(apr23$`Applications submitted through April 30, 2023`)] <- 0
may23$`Applications submitted through May 31, 2023`[is.na(may23$`Applications submitted through May 31, 2023`)] <- 0
jun23$`Applications submitted through June 30, 2023`[is.na(jun23$`Applications submitted through June 30, 2023`)] <- 0
jul23$`Applications submitted through July 31, 2023`[is.na(jul23$`Applications submitted through July 31, 2023`)] <- 0

#### End #### 

#### Account for repeating schools ####

nov22 <- nov22 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=nov22, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
nov22 <- nov22 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){

  tempData <- nov22 %>% filter(`Name-City-State` == problemCodes[i])
  nov22 <- nov22 %>% filter(`Name-City-State` != problemCodes[i])
  nov22 <- nov22 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through November 30, 2022` = sum(tempData$`Applications submitted through November 30, 2022`, na.rm=TRUE),
    `Applications completed through November 30, 2022` = sum(tempData$`Applications completed through November 30, 2022`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)

}
rm(problemCodes, i)
nov22 <- nov22 %>% select(-(`Name-City-State`))

dec22 <- dec22 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=dec22, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
dec22 <- dec22 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- dec22 %>% filter(`Name-City-State` == problemCodes[i])
  dec22 <- dec22 %>% filter(`Name-City-State` != problemCodes[i])
  dec22 <- dec22 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through December 31, 2022` = sum(tempData$`Applications submitted through December 31, 2022`, na.rm=TRUE),
    `Applications completed through December 31, 2022` = sum(tempData$`Applications completed through December 31, 2022`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)
  
}
rm(problemCodes, i)
dec22 <- dec22 %>% select(-(`Name-City-State`))

# January, February, and March do not need adjustments. 

apr23 <- apr23 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=apr23, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
apr23 <- apr23 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- apr23 %>% filter(`Name-City-State` == problemCodes[i])
  apr23 <- apr23 %>% filter(`Name-City-State` != problemCodes[i])
  apr23 <- apr23 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through April 30, 2023` = sum(tempData$`Applications submitted through April 30, 2023`, na.rm=TRUE),
    `Applications completed through April 30, 2023` = sum(tempData$`Applications completed through April 30, 2023`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)
  
}
rm(problemCodes, i)
apr23 <- apr23 %>% select(-(`Name-City-State`))

may23 <- may23 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=may23, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
may23 <- may23 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- may23 %>% filter(`Name-City-State` == problemCodes[i])
  may23 <- may23 %>% filter(`Name-City-State` != problemCodes[i])
  may23 <- may23 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through May 31, 2023` = sum(tempData$`Applications submitted through May 31, 2023`, na.rm=TRUE),
    `Applications completed through May 31, 2023` = sum(tempData$`Applications completed through May 31, 2023`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)
  
}
rm(problemCodes, i)
may23 <- may23 %>% select(-(`Name-City-State`))

jun23 <- jun23 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=jun23, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
jun23 <- jun23 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- jun23 %>% filter(`Name-City-State` == problemCodes[i])
  jun23 <- jun23 %>% filter(`Name-City-State` != problemCodes[i])
  jun23 <- jun23 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through June 30, 2023` = sum(tempData$`Applications submitted through June 30, 2023`, na.rm=TRUE),
    `Applications completed through June 30, 2023` = sum(tempData$`Applications completed through June 30, 2023`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)
  
}
rm(problemCodes, i)
jun23 <- jun23 %>% select(-(`Name-City-State`))

jul23 <- jul23 %>% mutate(`Count` = rep(1), `Name-City-State` = paste(`Name`, `City`, `State`, sep="-")) 
codeCount <- aggregate(data=jul23, `Count` ~ `Name-City-State`, FUN=sum) %>% filter(`Count` > 1)
jul23 <- jul23 %>% select(-(`Count`))
problemCodes <- codeCount$`Name-City-State`
rm(codeCount)
for(i in (1:length(problemCodes))){
  
  tempData <- jul23 %>% filter(`Name-City-State` == problemCodes[i])
  jul23 <- jul23 %>% filter(`Name-City-State` != problemCodes[i])
  jul23 <- jul23 %>% add_row(
    `Name` = tempData$`Name`[1],
    `City` = tempData$`City`[1],
    `State` = tempData$`State`[1],
    `Applications submitted through July 31, 2023` = sum(tempData$`Applications submitted through July 31, 2023`, na.rm=TRUE),
    `Applications completed through July 31, 2023` = sum(tempData$`Applications completed through July 31, 2023`, na.rm=TRUE), 
    `Name-City-State` = tempData$`Name-City-State`[1]
  )
  rm(tempData)
  
}
rm(problemCodes, i)
jul23 <- jul23 %>% select(-(`Name-City-State`))

#### End #### 

#### Merge FAFSA data ####

fafsaData2023 <- full_join(x=oct22, y=nov22, by=c("Name", "City", "State")) 
fafsaData2023 <- full_join(x=fafsaData2023, y=dec22, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=jan23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=feb23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=mar23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=apr23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=may23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=jun23, by=c("Name", "City", "State"))
fafsaData2023 <- full_join(x=fafsaData2023, y=jul23, by=c("Name", "City", "State"))

rm(oct22, nov22, dec22, jan23, feb23, mar23, apr23, may23, jun23, jul23) 

fafsaData2023 <- fafsaData2023 %>% mutate(
  `Name` = toupper(`Name`), 
  `City` = toupper(`City`)
)

#### End #### 

#### Data cleaning ####

# Remove private schools:
fafsaData2023 <- fafsaData2023 %>% filter((`School Code` %in% privateSchools$`School ID`)==FALSE)

# Remove any other schools with alphabet characters in name (indicating private): 
fafsaData2023 <- fafsaData2023 %>% filter(grepl(paste(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), collapse="|"), `School Code`)==FALSE)

# Remove other schools with religious terms in names:  
fafsaData2023 <- fafsaData2023 %>% filter((grepl(paste(c("ADVENTIST", "ARCHBISHOP", "BAPTIST", "CATHOLIC", "CHRISTIAN", "EPISCOPAL", "HOLY CROSS", "JESUIT", "OUR LADY", "SACRED HEART", "YESHIVA"), collapse="|"), `Name`))==FALSE)

# Remove schools we won't be able to link to Census data: 
fafsaData2023 <- fafsaData2023 %>% filter((`State` %in% c("AS", "DoDEA", "FM", "GU", "MH", "MP", "PR", "PW", "VI"))==FALSE)

# Remove virtual schools 
fafsaData2023 <- fafsaData2023 %>% filter((grepl(paste(c("ONLINE", "VIRTUAL", "ESCHOOL"), collapse="|"), `Name`))==FALSE)

# Index the FAFSA data: 
fafsaData2023 <- fafsaData2023 %>% mutate(`Index` = (1:nrow(fafsaData2023))) %>% rename(`NCESSCH` = `School Code`)

#### End #### 

#### Merge: Round 1 ####

elsiData <- elsiData %>% rename(`NCESSCH` = `NCESSCH-ELSI`)
goodMerge2023 <- inner_join(x=fafsaData2023, y=elsiData, by="NCESSCH") %>% mutate(`Merge Round` = rep("Round 1"))
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH` %in% goodMerge2023$NCESSCH)==FALSE) %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
goodMerge2023 <- goodMerge2023 %>% mutate(`NCESSCH-ELSI` = `NCESSCH`)
elsiData <- elsiData %>% rename(`NCESSCH-ELSI` = `NCESSCH`)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

#### End #### 

#### Merge: Round 2 #### 

# Start: 
newFafsa <- remainingFafsa2023 
newElsi <- remainingElsi2023

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 2"))
goodMerge2023 <- rbind(goodMerge2023, newMerge)
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge2023$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

#### End #### 

#### Merge: Round 3 ####

# Start: 
newFafsa <- remainingFafsa2023 
newElsi <- remainingElsi2023 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `LCITY`, `ST`, sep="-"))

# End: 
newMerge2023 <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 3"))
goodMerge2023 <- rbind(goodMerge2023, newMerge2023)
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge2023$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge2023)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

#### End #### 

#### Merge: Round 4 ####

# Start: 
newFafsa <- remainingFafsa2023 
newElsi <- remainingElsi2023 

# Middle: 
newFafsa <- newFafsa %>% mutate(`NewName` = paste(`Name`, `City`, `State`, sep="-"))
newFafsa <- newFafsa %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))
newElsi <- newElsi %>% mutate(`NewName` = paste(`SCH_NAME`, `MCITY`, `ST`, sep="-"))
newElsi <- newElsi %>% mutate(`NewName` = gsub('[^[:alnum:] ]', '', `NewName`))

# End: 
newMerge <- inner_join(x=newFafsa, y=newElsi, by="NewName") %>% select(-(`NewName`)) %>% mutate(`Merge Round` = rep("Round 4"))
goodMerge2023 <- rbind(goodMerge2023, newMerge)
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge2023$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

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
newFafsa <- remainingFafsa2023 
newElsi <- remainingElsi2023 

# Middle: 
middleFafsa <- data.frame(
  `NCESSCH` = character(),
  `Name` = character(), 
  `City` = character(),
  `State` = character(),
  `Applications completed through October 31, 2022` = numeric(), 
  `Applications submitted through October 31, 2022` = numeric(), 
  `Applications completed through November 30, 2022` = numeric(), 
  `Applications submitted through November 30, 2022` = numeric(), 
  `Applications completed through December 31, 2022` = numeric(), 
  `Applications submitted through December 31, 2022` = numeric(), 
  `Applications completed through January 31, 2023` = numeric(), 
  `Applications submitted through January 31, 2023` = numeric(), 
  `Applications completed through February 28, 2023` = numeric(), 
  `Applications submitted through February 28, 2023` = numeric(), 
  `Applications completed through March 31, 2023` = numeric(), 
  `Applications submitted through March 31, 2023` = numeric(), 
  `Applications completed through April 30, 2023` = numeric(), 
  `Applications submitted through April 30, 2023` = numeric(), 
  `Applications completed through May 31, 2023` = numeric(), 
  `Applications submitted through May 31, 2023` = numeric(), 
  `Applications completed through June 30, 2023` = numeric(), 
  `Applications submitted through June 30, 2023` = numeric(), 
  `Applications completed through July 31, 2023` = numeric(), 
  `Applications submitted through July 31, 2023` = numeric(), 
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
  `Applications completed through October 31, 2022`, 
  `Applications submitted through October 31, 2022`, 
  `Applications completed through November 30, 2022`, 
  `Applications submitted through November 30, 2022`, 
  `Applications completed through December 31, 2022`, 
  `Applications submitted through December 31, 2022`,
  `Applications completed through January 31, 2023`, 
  `Applications submitted through January 31, 2023`, 
  `Applications completed through February 28, 2023`, 
  `Applications submitted through February 28, 2023`, 
  `Applications completed through March 31, 2023`, 
  `Applications submitted through March 31, 2023`, 
  `Applications completed through April 30, 2023`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through May 31, 2023`, 
  `Applications submitted through May 31, 2023`, 
  `Applications completed through June 30, 2023`, 
  `Applications submitted through June 30, 2023`, 
  `Applications completed through July 31, 2023`, 
  `Applications submitted through July 31, 2023`, 
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`)

# End: 
newMerge <- newMerge %>% mutate(`Merge Round` = rep("Round 5"))
goodMerge2023 <- rbind(goodMerge2023, newMerge)
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge2023$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, newMerge, middleFafsa)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)
rm(zips)

goodMerge2023 <- goodMerge2023 %>% filter(duplicated(`Index`)==FALSE)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

#### End #### 

#### Merge: Round 6 ####

newFafsa <- remainingFafsa2023 
newElsi <- remainingElsi2023 

# Middle: 
middleFafsa <- newFafsa %>% select(`Name`, `State`, `City`, `Index`)
middleElsi <- newElsi %>% filter(`G_12_OFFERED`=="Yes") %>% select(`SCH_NAME`, `ST`, `LCITY`, `NCESSCH-ELSI`) %>% rename(`State` = `ST`)
middleMerge <- left_join(x=middleFafsa, y=middleElsi, by="State", relationship="many-to-many")
rm(middleFafsa, middleElsi)
middleMerge <- middleMerge %>% mutate(`School Name Similarity Index` = stringsim(`Name`, `SCH_NAME`)) %>% filter(`School Name Similarity Index` > 0.92)  
middleMerge <- middleMerge %>% arrange(desc(`School Name Similarity Index`)) %>% filter(duplicated(`Index`)==FALSE)
middleMerge <- middleMerge %>% select(`Index`, `NCESSCH-ELSI`)
middleMerge <- left_join(x=middleMerge, y=newFafsa, by="Index")
middleMerge <- left_join(x=middleMerge, y=newElsi, by="NCESSCH-ELSI")

# End: 
middleMerge <- middleMerge %>% select(
  `NCESSCH`, `Name`, `City`, `State`, 
  `Applications completed through October 31, 2022`, 
  `Applications submitted through October 31, 2022`, 
  `Applications completed through November 30, 2022`, 
  `Applications submitted through November 30, 2022`, 
  `Applications completed through December 31, 2022`, 
  `Applications submitted through December 31, 2022`,
  `Applications completed through January 31, 2023`, 
  `Applications submitted through January 31, 2023`, 
  `Applications completed through February 28, 2023`, 
  `Applications submitted through February 28, 2023`, 
  `Applications completed through March 31, 2023`, 
  `Applications submitted through March 31, 2023`, 
  `Applications completed through April 30, 2023`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through May 31, 2023`, 
  `Applications submitted through May 31, 2023`, 
  `Applications completed through June 30, 2023`, 
  `Applications submitted through June 30, 2023`, 
  `Applications completed through July 31, 2023`, 
  `Applications submitted through July 31, 2023`, 
  `Index`, `ST`, `SCH_NAME`, `MCITY`, `MZIP`, `LCITY`, `LZIP`, `G_12_OFFERED`, `NCESSCH-ELSI`
) %>% mutate(`Merge Round` = rep("Round 6"))
goodMerge2023 <- rbind(goodMerge2023, middleMerge)
remainingFafsa2023 <- fafsaData2023 %>% filter((`Index` %in% goodMerge2023$Index)==FALSE)
remainingElsi2023 <- elsiData %>% filter((`NCESSCH-ELSI` %in% goodMerge2023$`NCESSCH-ELSI`)==FALSE)
rm(newFafsa, newElsi, middleMerge)
# nrow(remainingFafsa2023) + nrow(goodMerge2023)

#### End #### 

#### Load in and merge high school grade 12 enrollment ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")
seniors2023 <- fread("ccd_sch_052_2223.csv", header=TRUE, select=c(
  "NCESSCH", 
  "GRADE", 
  "STUDENT_COUNT", 
  "TOTAL_INDICATOR"
)) %>% filter(
  `GRADE` == "Grade 12", 
  `TOTAL_INDICATOR` == "Subtotal 4 - By Grade"
) %>% select(-(`GRADE`)) %>% select(-(`TOTAL_INDICATOR`)) %>% rename(
  `NCESSCH-ELSI` = `NCESSCH`, 
  `Grade 12 students, 2022-23` = `STUDENT_COUNT`
) %>% mutate(
  `NCESSCH-ELSI` = as.character(`NCESSCH-ELSI`)
)
publicGrade12.2023 <- sum(seniors2023$`Grade 12 students, 2022-23`, na.rm=TRUE) 

goodMerge2023 <- left_join(x=goodMerge2023, y=seniors2023, by="NCESSCH-ELSI")
rm(seniors2023)

#### End #### 

#### Merge Census data with FAFSA data ####

goodMerge2023 <- goodMerge2023 %>% mutate(
  `ZCTA5` = paste("ZCTA5 ", substr(`LZIP`, 1, 5), sep="")
)
analysis2023 <- aggregate(data=goodMerge2023, cbind(
  `Grade 12 students, 2022-23`,
  `Applications completed through October 31, 2022`, 
  `Applications submitted through October 31, 2022`, 
  `Applications completed through November 30, 2022`, 
  `Applications submitted through November 30, 2022`, 
  `Applications completed through December 31, 2022`, 
  `Applications submitted through December 31, 2022`,
  `Applications completed through January 31, 2023`, 
  `Applications submitted through January 31, 2023`, 
  `Applications completed through February 28, 2023`, 
  `Applications submitted through February 28, 2023`, 
  `Applications completed through March 31, 2023`, 
  `Applications submitted through March 31, 2023`, 
  `Applications completed through April 30, 2023`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through May 31, 2023`, 
  `Applications submitted through May 31, 2023`, 
  `Applications completed through June 30, 2023`, 
  `Applications submitted through June 30, 2023`, 
  `Applications completed through July 31, 2023`, 
  `Applications submitted through July 31, 2023`
) ~ `ZCTA5`, FUN=sum)

analysis2023 <- left_join(x=analysis2023, y=census, by="ZCTA5")

#### End #### 

#### Group ZCTA5 ####

analysis2023 <- analysis2023 %>% mutate(
  `Black or Latino share` = `Black share` + `Hispanic or Latino share`,
  `Black, Latino, or Native American share` = `Black share` + `Hispanic or Latino share` + `Native American share`,
  `Black, Latino, Native American, or Pacific Islander share` = `Black share` + `Hispanic or Latino share` + `Native American share` + `Pacific Islander share`,
  `No college share` = `Less than 9th grade share` + `High school, no diploma share` + `High school diploma share`, 
  `Associate's or higher share` = `Associate's degree share` + `Bachelor's degree share` + `Graduate degree share`
)

rankInputs2023 <- analysis2023 %>% select(
  `ZCTA5`, 
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
rankOutputs2023 <- analysis2023 %>% select(
  `ZCTA5`, 
  `Grade 12 students, 2022-23`,
  `Applications completed through October 31, 2022`, 
  `Applications submitted through October 31, 2022`, 
  `Applications completed through November 30, 2022`, 
  `Applications submitted through November 30, 2022`, 
  `Applications completed through December 31, 2022`, 
  `Applications submitted through December 31, 2022`,
  `Applications completed through January 31, 2023`, 
  `Applications submitted through January 31, 2023`, 
  `Applications completed through February 28, 2023`, 
  `Applications submitted through February 28, 2023`, 
  `Applications completed through March 31, 2023`, 
  `Applications submitted through March 31, 2023`, 
  `Applications completed through April 30, 2023`, 
  `Applications submitted through April 30, 2023`, 
  `Applications completed through May 31, 2023`, 
  `Applications submitted through May 31, 2023`, 
  `Applications completed through June 30, 2023`, 
  `Applications submitted through June 30, 2023`, 
  `Applications completed through July 31, 2023`, 
  `Applications submitted through July 31, 2023`
)

nGroups <- 20
for(i in (2:ncol(rankInputs2023))){
  
  tempTiles <- rankInputs2023 %>% select(`ZCTA5`, names(rankInputs2023)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2023)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles20.2023 <- full_join(x=nTiles, y=rankOutputs2023, by="ZCTA5")
rm(nTiles)

nGroups <- 5
for(i in (2:ncol(rankInputs2023))){
  
  tempTiles <- rankInputs2023 %>% select(`ZCTA5`, names(rankInputs2023)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2023)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles5.2023 <- full_join(x=nTiles, y=rankOutputs2023, by="ZCTA5")
rm(nTiles)

nGroups <- 100
for(i in (2:ncol(rankInputs2023))){
  
  tempTiles <- rankInputs2023 %>% select(`ZCTA5`, names(rankInputs2023)[i])
  names(tempTiles)[2] <- "InterestVar"
  tempTiles <- tempTiles %>% mutate(`nTile` = ntile(`InterestVar`, nGroups)) %>% select(-(`InterestVar`))
  names(tempTiles)[2] <- paste("Groups: ", names(rankInputs2023)[i], sep="")
  if(i == 2){
    nTiles <- tempTiles
  }else{
    nTiles <- full_join(x=nTiles, y=tempTiles, by="ZCTA5")
  }
  rm(tempTiles)
  
}
rm(i, nGroups)
nTiles100.2023 <- full_join(x=nTiles, y=rankOutputs2023, by="ZCTA5")
rm(nTiles)

#### End #### 

#### Visualize submission rates over time (function7) ####

function7 <- function(tilesVar, printData){
  
  newTiles5.2023 <- nTiles5.2023
  newTiles5.2023 <- newTiles5.2023 %>% select(
    `Applications submitted through July 31, 2023`,
    `Applications submitted through June 30, 2023`,
    `Applications submitted through May 31, 2023`,
    `Applications submitted through April 30, 2023`,
    `Applications submitted through March 31, 2023`, 
    `Applications submitted through February 28, 2023`, 
    `Applications submitted through January 31, 2023`,
    `Applications submitted through December 31, 2022`,
    `Applications submitted through November 30, 2022`,
    `Applications submitted through October 31, 2022`,
    `Grade 12 students, 2022-23`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  )
  
  newTiles5 <- nTiles5
  newTiles5 <- newTiles5 %>% select(
    `Applications submitted through April 30, 2024`,
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`, 
    all_of(tilesVar)
  ) %>% rename(
    `InterestVar` = tilesVar
  )
  
  analysis1A <- aggregate(data=newTiles5.2023, cbind(
    `Applications submitted through July 31, 2023`,
    `Applications submitted through June 30, 2023`,
    `Applications submitted through May 31, 2023`,
    `Applications submitted through April 30, 2023`,
    `Applications submitted through March 31, 2023`, 
    `Applications submitted through February 28, 2023`, 
    `Applications submitted through January 31, 2023`,
    `Applications submitted through December 31, 2022`,
    `Applications submitted through November 30, 2022`,
    `Applications submitted through October 31, 2022`,
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `Oct.` = `Applications submitted through October 31, 2022` / `Grade 12 students, 2022-23`, 
    `Nov.` = `Applications submitted through November 30, 2022` / `Grade 12 students, 2022-23`, 
    `Dec.` = `Applications submitted through December 31, 2022` / `Grade 12 students, 2022-23`, 
    `Jan.` = `Applications submitted through January 31, 2023` / `Grade 12 students, 2022-23`, 
    `Feb.` = `Applications submitted through February 28, 2023` / `Grade 12 students, 2022-23`, 
    `Mar.` = `Applications submitted through March 31, 2023` / `Grade 12 students, 2022-23`,
    `Apr.` = `Applications submitted through April 30, 2023` / `Grade 12 students, 2022-23`, 
    `May` = `Applications submitted through May 31, 2023` / `Grade 12 students, 2022-23`, 
    `Jun.` = `Applications submitted through June 30, 2023` / `Grade 12 students, 2022-23`, 
    `Jul.` = `Applications submitted through July 31, 2023` / `Grade 12 students, 2022-23`
  ) %>% select(
    `InterestVar`, 
    `Oct.`, 
    `Nov.`, 
    `Dec.`, 
    `Jan.`, 
    `Feb.`, 
    `Mar.`,
    `Apr.`, 
    `May`, 
    `Jun.`, 
    `Jul.`
  ) %>% pivot_longer(cols=c(
    `Oct.`, 
    `Nov.`, 
    `Dec.`, 
    `Jan.`, 
    `Feb.`, 
    `Mar.`,
    `Apr.`, 
    `May`, 
    `Jun.`, 
    `Jul.`
  ), names_to="Date", values_to="Submission rate") %>% mutate(
    `Date` = factor(`Date`, levels=c(
      "Oct.", 
      "Nov.", 
      "Dec.", 
      "Jan.", 
      "Feb.", 
      "Mar.", 
      "Apr.", 
      "May", 
      "Jun.", 
      "Jul."
    ))
  ) %>% mutate(
    `Year` = rep("Class of 2023")
  )
  analysis1B <- aggregate(data=newTiles5, cbind(
    `Applications submitted through April 30, 2024`,
    `Applications submitted through March 29, 2024`, 
    `Applications submitted through February 29, 2024`, 
    `Applications submitted through January 31, 2024`,
    `Grade 12 students, 2022-23`
  ) ~ `InterestVar`, FUN=sum) %>% mutate(
    `Oct.` = rep(NA), 
    `Nov.` = rep(NA), 
    `Dec.` = rep(NA), 
    `Jan.` = `Applications submitted through January 31, 2024` / `Grade 12 students, 2022-23`, 
    `Feb.` = `Applications submitted through February 29, 2024` / `Grade 12 students, 2022-23`, 
    `Mar.` = `Applications submitted through March 29, 2024` / `Grade 12 students, 2022-23`,
    `Apr.` = `Applications submitted through April 30, 2024` / `Grade 12 students, 2022-23`, 
    `May` = rep(NA), 
    `Jun.` = rep(NA), 
    `Jul.` = rep(NA)
  ) %>% select(
    `InterestVar`, 
    `Oct.`, 
    `Nov.`, 
    `Dec.`,
    `Jan.`, 
    `Feb.`, 
    `Mar.`,
    `Apr.`, 
    `May`, 
    `Jun.`, 
    `Jul.`
  ) %>% pivot_longer(cols=c(
    `Oct.`, 
    `Nov.`, 
    `Dec.`,
    `Jan.`, 
    `Feb.`, 
    `Mar.`,
    `Apr.`, 
    `May`, 
    `Jun.`, 
    `Jul.`
  ), names_to="Date", values_to="Submission rate") %>% mutate(
    `Date` = factor(`Date`, levels=c(
      "Oct.", 
      "Nov.", 
      "Dec.", 
      "Jan.", 
      "Feb.", 
      "Mar.", 
      "Apr.", 
      "May", 
      "Jun.", 
      "Jul."
    ))
  ) %>% mutate(
    `Year` = rep("Class of 2024")
  )
  analysis1 <- rbind(analysis1A, analysis1B) 
  analysis1 <- analysis1 %>% filter(`InterestVar` %in% c(1, 5))
  figG <- ggplot(data=analysis1, mapping=aes(x=`Date`, y=`Submission rate`, group=interaction(`InterestVar`, `Year`), color=`InterestVar`)) + geom_point(size=2.5) + geom_line(aes(linetype=`Year`), size=1.5) + theme(legend.position="bottom") + scale_y_continuous(limits=c(0, 0.7), labels=percent_format(accuracy=1)) + scale_linetype_manual(values=c("dotted", "longdash")) + labs(color=tilesVar)
  if(printData==TRUE){tempDF <- analysis1}
  return(figG)
  rm(analysis1A, analysis1B, analysis1)
  rm(figG)
  
}

#### End #### 

#### Run function7 ####

figG1 <- function7("Groups: Native-born share", FALSE)
figG2 <- function7("Groups: Foreign-born share: AAOA", FALSE)
figG3 <- function7("Groups: Limited English share", FALSE)
figG4 <- function7("Groups: White share", FALSE)
figG5 <- function7("Groups: Black share", FALSE)
figG6 <- function7("Groups: Hispanic or Latino share", FALSE)
figG7 <- function7("Groups: Native American share", FALSE)
figG8 <- function7("Groups: Asian share", FALSE)
figG9 <- function7("Groups: Pacific Islander share", FALSE)
figG10 <- function7("Groups: Other race share", FALSE)
figG11 <- function7("Groups: Two or more races share", FALSE)
figG12 <- function7("Groups: Black or Latino share", FALSE)
figG13 <- function7("Groups: Black, Latino, or Native American share", FALSE)
figG14 <- function7("Groups: Black, Latino, Native American, or Pacific Islander share", FALSE)
figG15 <- function7("Groups: No college share", FALSE)
figG16 <- function7("Groups: Associate's or higher share", FALSE)
figG17 <- function7("Groups: Households receiving SNAP share", FALSE)
figG18 <- function7("Groups: Share of population in poverty", FALSE)
figG19 <- function7("Groups: Average household income", FALSE)

#### End #### 

####################################
#### Simulate completion rates  ####
#### for 2024 (all, not just    ####
#### publics)                   ####
####################################

#### Load 2017 data, no filters ####

setwd("/Users/peter_granville/FAFSA-2024/FAFSA data")
jan17 <- read_excel("HS_ARCHIVE01312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJan31  2017", "Applications\nComplete\nJan31  2017") 
feb17 <- read_excel("HS_ARCHIVE02282017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nFeb28  2017", "Applications\nComplete\nFeb28  2017") 
mar17 <- read_excel("HS_ARCHIVE03312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMar31  2017", "Applications\nComplete\nMar31  2017") 
apr17 <- read_excel("HS_ARCHIVE04302017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nApr30  2017", "Applications\nComplete\nApr30  2017") 
may17 <- read_excel("HS_ARCHIVE05312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMay31  2017", "Applications\nComplete\nMay31  2017")
jun17 <- read_excel("HS_ARCHIVE06302017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJun30  2017", "Applications\nComplete\nJun30  2017")
jul17 <- read_excel("HS_ARCHIVE07312017.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJul31  2017", "Applications\nComplete\nJul31  2017") 

jan17 <- jan17 %>% rename(
  `Applications submitted through January 31, 2017` = `Applications\nSubmitted\nJan31  2017`, 
  `Applications completed through January 31, 2017` = `Applications\nComplete\nJan31  2017`
)
feb17 <- feb17 %>% rename(
  `Applications submitted through February 28, 2017` = `Applications\nSubmitted\nFeb28  2017`, 
  `Applications completed through February 28, 2017` = `Applications\nComplete\nFeb28  2017`
)
mar17 <- mar17 %>% rename(
  `Applications submitted through March 31, 2017` = `Applications\nSubmitted\nMar31  2017`, 
  `Applications completed through March 31, 2017` = `Applications\nComplete\nMar31  2017`
)
apr17 <- apr17 %>% rename(
  `Applications submitted through April 30, 2017` = `Applications\nSubmitted\nApr30  2017`, 
  `Applications completed through April 30, 2017` = `Applications\nComplete\nApr30  2017`
)
may17 <- may17 %>% rename(
  `Applications submitted through May 31, 2017` = `Applications\nSubmitted\nMay31  2017`, 
  `Applications completed through May 31, 2017` = `Applications\nComplete\nMay31  2017`
)
jun17 <- jun17 %>% rename(
  `Applications submitted through June 30, 2017` = `Applications\nSubmitted\nJun30  2017`, 
  `Applications completed through June 30, 2017` = `Applications\nComplete\nJun30  2017`
)
jul17 <- jul17 %>% rename(
  `Applications submitted through July 31, 2017` = `Applications\nSubmitted\nJul31  2017`, 
  `Applications completed through July 31, 2017` = `Applications\nComplete\nJul31  2017`
)

jan17$`Applications completed through January 31, 2017` <- as.numeric(gsub("<5", "0", jan17$`Applications completed through January 31, 2017`))
feb17$`Applications completed through February 28, 2017` <- as.numeric(gsub("<5", "0", feb17$`Applications completed through February 28, 2017`))
mar17$`Applications completed through March 31, 2017` <- as.numeric(gsub("<5", "0", mar17$`Applications completed through March 31, 2017`))
apr17$`Applications completed through April 30, 2017` <- as.numeric(gsub("<5", "0", apr17$`Applications completed through April 30, 2017`))
may17$`Applications completed through May 31, 2017` <- as.numeric(gsub("<5", "0", may17$`Applications completed through May 31, 2017`))
jun17$`Applications completed through June 30, 2017` <- as.numeric(gsub("<5", "0", jun17$`Applications completed through June 30, 2017`))
jul17$`Applications completed through July 31, 2017` <- as.numeric(gsub("<5", "0", jul17$`Applications completed through July 31, 2017`))

jan17$`Applications submitted through January 31, 2017` <- as.numeric(gsub("<5", "0", jan17$`Applications submitted through January 31, 2017`))
feb17$`Applications submitted through February 28, 2017` <- as.numeric(gsub("<5", "0", feb17$`Applications submitted through February 28, 2017`))
mar17$`Applications submitted through March 31, 2017` <- as.numeric(gsub("<5", "0", mar17$`Applications submitted through March 31, 2017`))
apr17$`Applications submitted through April 30, 2017` <- as.numeric(gsub("<5", "0", apr17$`Applications submitted through April 30, 2017`))
may17$`Applications submitted through May 31, 2017` <- as.numeric(gsub("<5", "0", may17$`Applications submitted through May 31, 2017`))
jun17$`Applications submitted through June 30, 2017` <- as.numeric(gsub("<5", "0", jun17$`Applications submitted through June 30, 2017`))
jul17$`Applications submitted through July 31, 2017` <- as.numeric(gsub("<5", "0", jul17$`Applications submitted through July 31, 2017`))

#### End #### 

#### Load 2023 data, no filters ####

oct22 <- read_excel("HS_ARCHIVE10312022.xls", sheet=" 2023-24 Cycle", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nOct31  2022", "Applications\nComplete\nOct31  2022") 
nov22 <- read_excel("HS_ARCHIVE11302022.xls", sheet=" 2023-24 Cycle", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nNov30  2022", "Applications\nComplete\nNov30  2022") 
dec22 <- read_excel("HS_ARCHIVE12312022.xls", sheet=" 2023-24 Cycle", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nDec31  2022", "Applications\nComplete\nDec31  2022") 
jan23 <- read_excel("HS_ARCHIVE01312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJan31  2023", "Applications\nComplete\nJan31  2023") 
feb23 <- read_excel("HS_ARCHIVE02282023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nFeb28  2023", "Applications\nComplete\nFeb28  2023") 
mar23 <- read_excel("HS_ARCHIVE03312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMar31  2023", "Applications\nComplete\nMar31  2023") 
apr23 <- read_excel("HS_ARCHIVE04302023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nApr30  2023", "Applications\nComplete\nApr30  2023") 
may23 <- read_excel("HS_ARCHIVE05312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMay31  2023", "Applications\nComplete\nMay31  2023")
jun23 <- read_excel("HS_ARCHIVE06302023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJun30  2023", "Applications\nComplete\nJun30  2023")
jul23 <- read_excel("HS_ARCHIVE07312023.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nJul31  2023", "Applications\nComplete\nJul31  2023") 

oct22 <- oct22 %>% rename(
  `Applications submitted through October 31, 2022` = `Applications\nSubmitted\nOct31  2022`, 
  `Applications completed through October 31, 2022` = `Applications\nComplete\nOct31  2022`
)
nov22 <- nov22 %>% rename(
  `Applications submitted through November 30, 2022` = `Applications\nSubmitted\nNov30  2022`, 
  `Applications completed through November 30, 2022` = `Applications\nComplete\nNov30  2022`
)
dec22 <- dec22 %>% rename(
  `Applications submitted through December 31, 2022` = `Applications\nSubmitted\nDec31  2022`, 
  `Applications completed through December 31, 2022` = `Applications\nComplete\nDec31  2022`
)
jan23 <- jan23 %>% rename(
  `Applications submitted through January 31, 2023` = `Applications\nSubmitted\nJan31  2023`, 
  `Applications completed through January 31, 2023` = `Applications\nComplete\nJan31  2023`
)
feb23 <- feb23 %>% rename(
  `Applications submitted through February 28, 2023` = `Applications\nSubmitted\nFeb28  2023`, 
  `Applications completed through February 28, 2023` = `Applications\nComplete\nFeb28  2023`
)
mar23 <- mar23 %>% rename(
  `Applications submitted through March 31, 2023` = `Applications\nSubmitted\nMar31  2023`, 
  `Applications completed through March 31, 2023` = `Applications\nComplete\nMar31  2023`
)
apr23 <- apr23 %>% rename(
  `Applications submitted through April 30, 2023` = `Applications\nSubmitted\nApr30  2023`, 
  `Applications completed through April 30, 2023` = `Applications\nComplete\nApr30  2023`
)
may23 <- may23 %>% rename(
  `Applications submitted through May 31, 2023` = `Applications\nSubmitted\nMay31  2023`, 
  `Applications completed through May 31, 2023` = `Applications\nComplete\nMay31  2023`
)
jun23 <- jun23 %>% rename(
  `Applications submitted through June 30, 2023` = `Applications\nSubmitted\nJun30  2023`, 
  `Applications completed through June 30, 2023` = `Applications\nComplete\nJun30  2023`
)
jul23 <- jul23 %>% rename(
  `Applications submitted through July 31, 2023` = `Applications\nSubmitted\nJul31  2023`, 
  `Applications completed through July 31, 2023` = `Applications\nComplete\nJul31  2023`
)

oct22$`Applications completed through October 31, 2022` <- as.numeric(gsub("<5", "0", oct22$`Applications completed through October 31, 2022`))
nov22$`Applications completed through November 30, 2022` <- as.numeric(gsub("<5", "0", nov22$`Applications completed through November 30, 2022`))
dec22$`Applications completed through December 31, 2022` <- as.numeric(gsub("<5", "0", dec22$`Applications completed through December 31, 2022`))
jan23$`Applications completed through January 31, 2023` <- as.numeric(gsub("<5", "0", jan23$`Applications completed through January 31, 2023`))
feb23$`Applications completed through February 28, 2023` <- as.numeric(gsub("<5", "0", feb23$`Applications completed through February 28, 2023`))
mar23$`Applications completed through March 31, 2023` <- as.numeric(gsub("<5", "0", mar23$`Applications completed through March 31, 2023`))
apr23$`Applications completed through April 30, 2023` <- as.numeric(gsub("<5", "0", apr23$`Applications completed through April 30, 2023`))
may23$`Applications completed through May 31, 2023` <- as.numeric(gsub("<5", "0", may23$`Applications completed through May 31, 2023`))
jun23$`Applications completed through June 30, 2023` <- as.numeric(gsub("<5", "0", jun23$`Applications completed through June 30, 2023`))
jul23$`Applications completed through July 31, 2023` <- as.numeric(gsub("<5", "0", jul23$`Applications completed through July 31, 2023`))

oct22$`Applications submitted through October 31, 2022` <- as.numeric(gsub("<5", "0", oct22$`Applications submitted through October 31, 2022`))
nov22$`Applications submitted through November 30, 2022` <- as.numeric(gsub("<5", "0", nov22$`Applications submitted through November 30, 2022`))
dec22$`Applications submitted through December 31, 2022` <- as.numeric(gsub("<5", "0", dec22$`Applications submitted through December 31, 2022`))
jan23$`Applications submitted through January 31, 2023` <- as.numeric(gsub("<5", "0", jan23$`Applications submitted through January 31, 2023`))
feb23$`Applications submitted through February 28, 2023` <- as.numeric(gsub("<5", "0", feb23$`Applications submitted through February 28, 2023`))
mar23$`Applications submitted through March 31, 2023` <- as.numeric(gsub("<5", "0", mar23$`Applications submitted through March 31, 2023`))
apr23$`Applications submitted through April 30, 2023` <- as.numeric(gsub("<5", "0", apr23$`Applications submitted through April 30, 2023`))
may23$`Applications submitted through May 31, 2023` <- as.numeric(gsub("<5", "0", may23$`Applications submitted through May 31, 2023`))
jun23$`Applications submitted through June 30, 2023` <- as.numeric(gsub("<5", "0", jun23$`Applications submitted through June 30, 2023`))
jul23$`Applications submitted through July 31, 2023` <- as.numeric(gsub("<5", "0", jul23$`Applications submitted through July 31, 2023`))

#### End #### 

#### Load 2024 data, no filters ####

mar24 <- read_excel("HS_ARCHIVE03292024.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nMar29  2024", "Applications\nComplete\nMar29  2024") 
apr24 <- read_excel("HS_ARCHIVE04302024.xls", skip=3) %>% select("Name", "City", "State", "Applications\nSubmitted\nApr30  2024", "Applications\nComplete\nApr30  2024") 

mar24 <- mar24 %>% rename(
  `Applications submitted through March 29, 2024` = `Applications\nSubmitted\nMar29  2024`, 
  `Applications completed through March 29, 2024` = `Applications\nComplete\nMar29  2024`
)
apr24 <- apr24 %>% rename(
  `Applications submitted through April 30, 2024` = `Applications\nSubmitted\nApr30  2024`, 
  `Applications completed through April 30, 2024` = `Applications\nComplete\nApr30  2024`
)

mar24$`Applications completed through March 29, 2024` <- as.numeric(gsub("<5", "0", mar24$`Applications completed through March 29, 2024`))
apr24$`Applications completed through April 30, 2024` <- as.numeric(gsub("<5", "0", apr24$`Applications completed through April 30, 2024`))

mar24$`Applications submitted through March 29, 2024` <- as.numeric(gsub("<5", "0", mar24$`Applications submitted through March 29, 2024`))
apr24$`Applications submitted through April 30, 2024` <- as.numeric(gsub("<5", "0", apr24$`Applications submitted through April 30, 2024`))

mar24$`Applications completed through March 29, 2024`[is.na(mar24$`Applications completed through March 29, 2024`)] <- 0
apr24$`Applications completed through April 30, 2024`[is.na(apr24$`Applications completed through April 30, 2024`)] <- 0

mar24$`Applications submitted through March 29, 2024`[is.na(mar24$`Applications submitted through March 29, 2024`)] <- 0
apr24$`Applications submitted through April 30, 2024`[is.na(apr24$`Applications submitted through April 30, 2024`)] <- 0

#### End #### 

#### Estimate total public and private grade 12 enrollment, no filters ####

setwd("/Users/peter_granville/FAFSA-2024/ELSI data")

private12 <- read.csv("ELSI Private School Grade 12 Enrollment 16 18 20.csv", skip=6, header=TRUE, nrow=29618, check.names=FALSE) %>% rename(
  `Grade 12 2015-16` = `Grade 12 Students [Private School] 2015-16`,
  `Grade 12 2017-18` = `Grade 12 Students [Private School] 2017-18`, 
  `Grade 12 2019-20` = `Grade 12 Students [Private School] 2019-20`
) %>% pivot_longer(
  cols=c(`Grade 12 2015-16`, `Grade 12 2017-18`, `Grade 12 2019-20`), 
  names_to="Year", 
  values_to="Grade 12 Enrollment"
) %>% mutate(
  `Grade 12 Enrollment` = gsub("†", "", `Grade 12 Enrollment`)
) %>% mutate(
  `Grade 12 Enrollment` = gsub("–", "", `Grade 12 Enrollment`)
) %>% mutate(
  `Grade 12 Enrollment` = as.numeric(`Grade 12 Enrollment`)
) 
private12 <- aggregate(data=private12, `Grade 12 Enrollment` ~ `Year`, FUN=sum) %>% mutate(`Control` = rep("Private"))

public12 <- read.csv("ELSI Public School Grade 12 Enrollment 16 18 20.csv", skip=6, header=TRUE, nrow=109082, check.names=FALSE) %>% rename(
  `Grade 12 2015-16` = `Grade 12 Students [Public School] 2015-16`,
  `Grade 12 2017-18` = `Grade 12 Students [Public School] 2017-18`, 
  `Grade 12 2019-20` = `Grade 12 Students [Public School] 2019-20`
) %>% pivot_longer(
  cols=c(`Grade 12 2015-16`, `Grade 12 2017-18`, `Grade 12 2019-20`), 
  names_to="Year", 
  values_to="Grade 12 Enrollment"
) %>% mutate(
  `Grade 12 Enrollment` = gsub("†", "", `Grade 12 Enrollment`)
) %>% mutate(
  `Grade 12 Enrollment` = gsub("–", "", `Grade 12 Enrollment`)
) %>% mutate(
  `Grade 12 Enrollment` = gsub("‡", "", `Grade 12 Enrollment`)
) %>% mutate(
  `Grade 12 Enrollment` = as.numeric(`Grade 12 Enrollment`)
) 
public12 <- aggregate(data=public12, `Grade 12 Enrollment` ~ `Year`, FUN=sum) %>% mutate(`Control` = rep("Public"))

all12 <- rbind(private12, public12) %>% pivot_wider(id_cols=c(`Year`), names_from=`Control`, values_from=`Grade 12 Enrollment`) %>% mutate(`Public share of total` = `Public` / (`Private` + `Public`))
rm(private12, public12)
adjustment <- mean(all12$`Public share of total`)

grade12.2017 <- publicGrade12.2017 / adjustment
grade12.2023 <- publicGrade12.2023 / adjustment
grade12.2024 <- publicGrade12.2024 / adjustment

#### End #### 

#### Combine data ####

analysis0 <- data.frame(
  `Year` = character(), 
  `Oct.` = numeric(), 
  `Nov.` = numeric(),
  `Dec.` = numeric(), 
  `Jan.` = numeric(), 
  `Feb.` = numeric(), 
  `Mar.` = numeric(), 
  `Apr.` = numeric(), 
  `May` = numeric(), 
  `Jun.` = numeric(), 
  `Jul.` = numeric(), 
  check.names=FALSE
)
analysis0 <- analysis0 %>% add_row(
  `Year` = "Class of 2017", 
  `Oct.` = NA,  
  `Nov.` = NA,
  `Dec.` = NA, 
  `Jan.` = sum(jan17$`Applications completed through January 31, 2017`, na.rm=TRUE) / grade12.2017, 
  `Feb.` = sum(feb17$`Applications completed through February 28, 2017`, na.rm=TRUE) / grade12.2017, 
  `Mar.` = sum(mar17$`Applications completed through March 31, 2017`, na.rm=TRUE) / grade12.2017, 
  `Apr.` = sum(apr17$`Applications completed through April 30, 2017`, na.rm=TRUE) / grade12.2017, 
  `May` = sum(may17$`Applications completed through May 31, 2017`, na.rm=TRUE) / grade12.2017, 
  `Jun.` = sum(jun17$`Applications completed through June 30, 2017`, na.rm=TRUE) / grade12.2017, 
  `Jul.` = sum(jul17$`Applications completed through July 31, 2017`, na.rm=TRUE) / grade12.2017
)
analysis0 <- analysis0 %>% add_row(
  `Year` = "Class of 2023", 
  `Oct.` = sum(oct22$`Applications completed through October 31, 2022`, na.rm=TRUE) / grade12.2023,  
  `Nov.` = sum(nov22$`Applications completed through November 30, 2022`, na.rm=TRUE) / grade12.2023,
  `Dec.` = sum(dec22$`Applications completed through December 31, 2022`, na.rm=TRUE) / grade12.2023, 
  `Jan.` = sum(jan23$`Applications completed through January 31, 2023`, na.rm=TRUE) / grade12.2023, 
  `Feb.` = sum(feb23$`Applications completed through February 28, 2023`, na.rm=TRUE) / grade12.2023, 
  `Mar.` = sum(mar23$`Applications completed through March 31, 2023`, na.rm=TRUE) / grade12.2023, 
  `Apr.` = sum(apr23$`Applications completed through April 30, 2023`, na.rm=TRUE) / grade12.2023, 
  `May` = sum(may23$`Applications completed through May 31, 2023`, na.rm=TRUE) / grade12.2023, 
  `Jun.` = sum(jun23$`Applications completed through June 30, 2023`, na.rm=TRUE) / grade12.2023, 
  `Jul.` = sum(jul23$`Applications completed through July 31, 2023`, na.rm=TRUE) / grade12.2023
)
analysis0 <- analysis0 %>% add_row(
  `Year` = "Class of 2024", 
  `Oct.` = NA,  
  `Nov.` = NA,
  `Dec.` = NA, 
  `Jan.` = NA, 
  `Feb.` = NA, 
  `Mar.` = sum(mar24$`Applications completed through March 29, 2024`, na.rm=TRUE) / grade12.2024, 
  `Apr.` = sum(apr24$`Applications completed through April 30, 2024`, na.rm=TRUE) / grade12.2024, 
  `May` = NA, 
  `Jun.` = NA, 
  `Jul.` = NA
)
analysis0 <- analysis0 %>% pivot_longer(cols=c(
  `Oct.`, 
  `Nov.`, 
  `Dec.`,
  `Jan.`, 
  `Feb.`, 
  `Mar.`,
  `Apr.`, 
  `May`, 
  `Jun.`, 
  `Jul.`
), names_to="Date", values_to="Completion rate") %>% mutate(
  `Date` = factor(`Date`, levels=c(
    "Oct.", 
    "Nov.", 
    "Dec.", 
    "Jan.", 
    "Feb.", 
    "Mar.", 
    "Apr.", 
    "May", 
    "Jun.", 
    "Jul."
  ))
)
figI0 <- ggplot(data=analysis0, mapping=aes(x=`Date`, y=`Completion rate`, group=`Year`, color=`Year`)) + geom_point(size=2.5) + geom_line(aes(linetype=`Year`), size=1.5) + theme(legend.position="bottom") + scale_y_continuous(limits=c(0, 0.65), labels=percent_format(accuracy=1)) + scale_linetype_manual(values=c("dotted", "longdash", "twodash"))

#### End #### 

#### Simulate changes #### 

month.number <- data.frame(`Year` = character(), 
                           `Date` = character(), 
                           `Month Number` = numeric(), 
                           check.names=FALSE)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Oct.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Nov.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Dec.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Jan.", `Month Number` = 1)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Feb.", `Month Number` = 2)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Mar.", `Month Number` = 3)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Apr.", `Month Number` = 4)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "May", `Month Number` = 5)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Jun.", `Month Number` = 6)
month.number <- month.number %>% add_row(`Year` = "Class of 2017", `Date` = "Jul.", `Month Number` = 7)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Oct.", `Month Number` = 1)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Nov.", `Month Number` = 2)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Dec.", `Month Number` = 3)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Jan.", `Month Number` = 4)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Feb.", `Month Number` = 5)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Mar.", `Month Number` = 6)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Apr.", `Month Number` = 7)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "May", `Month Number` = 8)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Jun.", `Month Number` = 9)
month.number <- month.number %>% add_row(`Year` = "Class of 2023", `Date` = "Jul.", `Month Number` = 10)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Oct.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Nov.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Dec.", `Month Number` = NA)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Jan.", `Month Number` = 1)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Feb.", `Month Number` = 2)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Mar.", `Month Number` = 3)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Apr.", `Month Number` = 4)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "May", `Month Number` = 5)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Jun.", `Month Number` = 6)
month.number <- month.number %>% add_row(`Year` = "Class of 2024", `Date` = "Jul.", `Month Number` = 7)
analysis2 <- left_join(x=analysis0, y=month.number, by=c("Year", "Date"))
rm(month.number)

changes.2023 <- analysis2 %>% filter(`Year` == "Class of 2023") %>% select(`Month Number`, `Completion rate`) %>% mutate(`Change from prior month, 2023` = rep(NA))
for(i in (2:nrow(changes.2023))){
  changes.2023$`Change from prior month, 2023`[i] <- changes.2023$`Completion rate`[i] - changes.2023$`Completion rate`[i-1]
}
changes.2023 <- changes.2023 %>% select(-(`Completion rate`))

changes.2017 <- analysis2 %>% filter(`Year` == "Class of 2017") %>% select(`Month Number`, `Completion rate`) %>% mutate(`Change from prior month, 2017` = rep(NA))
for(i in (2:nrow(changes.2017))){
  changes.2017$`Change from prior month, 2017`[i] <- changes.2017$`Completion rate`[i] - changes.2017$`Completion rate`[i-1]
}
changes.2017 <- changes.2017 %>% select(-(`Completion rate`)) %>% filter(is.na(`Month Number`)==FALSE)

analysis2A <- analysis2 %>% filter(`Year` == "Class of 2024") %>% mutate(`Projection` = rep("2017 shifted down"))
analysis2B <- analysis2 %>% filter(`Year` == "Class of 2024") %>% mutate(`Projection` = rep("2023 shifted right"))
analysis2 <- rbind(analysis2A, analysis2B) %>% filter(`Date` %in% c("Mar.", "Apr.", "May", "Jun.", "Jul."))

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==5)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==4)
] +  changes.2017$`Change from prior month, 2017`[changes.2017$`Month Number`==5]

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==6)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==5)
] +  changes.2017$`Change from prior month, 2017`[changes.2017$`Month Number`==6]

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==7)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2017 shifted down") & (analysis2$`Month Number`==6)
] +  changes.2017$`Change from prior month, 2017`[changes.2017$`Month Number`==7]

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==5)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==4)
] +  changes.2023$`Change from prior month, 2023`[changes.2023$`Month Number`==5]

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==6)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==5)
] +  changes.2023$`Change from prior month, 2023`[changes.2023$`Month Number`==6]

analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==7)
] <- analysis2$`Completion rate`[
  (analysis2$`Projection`=="2023 shifted right") & (analysis2$`Month Number`==6)
] +  changes.2023$`Change from prior month, 2023`[changes.2023$`Month Number`==7]

analysis2 <- analysis2 %>% select(-(`Month Number`))
analysis2.march <- analysis2$`Completion rate`[(analysis2$`Date`=="Mar.") & (analysis2$Projection=="2023 shifted right")]
analysis2.april <- analysis2$`Completion rate`[(analysis2$`Date`=="Apr.") & (analysis2$Projection=="2023 shifted right")]
analysis2 <- analysis2 %>% add_row(
  `Date` = "Mar.", 
  `Completion rate` = analysis2.march, 
  `Projection` = "2024 actual"
)
analysis2 <- analysis2 %>% add_row(
  `Date` = "Apr.", 
  `Completion rate` = analysis2.april, 
  `Projection` = "2024 actual"
)
analysis2 <- analysis2 %>% add_row(`Date` = "May", `Completion rate` = NA, `Projection` = "2024 actual")
analysis2 <- analysis2 %>% add_row(`Date` = "Jun.", `Completion rate` = NA, `Projection` = "2024 actual")
analysis2 <- analysis2 %>% add_row(`Date` = "Jul.", `Completion rate` = NA, `Projection` = "2024 actual")

analysis0 <- analysis0 %>% filter(`Year` %in% c("Class of 2017", "Class of 2023")) %>% filter(`Date` %in% c("Mar.", "Apr.", "May", "Jun.", "Jul."))
analysis0 <- analysis0 %>% mutate(
  `Date` = factor(`Date`, levels=c(
    "Mar.", 
    "Apr.", 
    "May", 
    "Jun.", 
    "Jul."
)))

analysis2 <- analysis2 %>% mutate(
  `Date` = factor(`Date`, levels=c(
    "Mar.", 
    "Apr.", 
    "May", 
    "Jun.", 
    "Jul."
)))
analysis2$`Projection`[analysis2$`Projection`=="2017 shifted down"] <- "If 2024 follows 2017 trendline"
analysis2$`Projection`[analysis2$`Projection`=="2023 shifted right"] <- "If 2024 follows 2023 trendline"
analysis2 <- analysis2 %>% mutate(
  `Projection` = factor(`Projection`, levels=c(
    "If 2024 follows 2023 trendline", 
    "If 2024 follows 2017 trendline",
    "2024 actual"
  )))

jul2023.rate <- analysis0$`Completion rate`[
  (analysis0$`Year`=="Class of 2023") & (analysis0$`Date`=="Jul.")
]
jul2017.rate <- analysis0$`Completion rate`[
  (analysis0$`Year`=="Class of 2017") & (analysis0$`Date`=="Jul.")
]

figH <- ggplot() + geom_point(
  data=analysis2, mapping=aes(x=`Date`, y=`Completion rate`, group=`Projection`, color=`Projection`), size=2.5
) + geom_line(
  data=analysis2, mapping=aes(x=`Date`, y=`Completion rate`, group=`Projection`, color=`Projection`, linetype=`Projection`), size=1.5
) 
figH <- figH + geom_hline(yintercept=jul2023.rate, linetype="dashed")
figH <- figH + geom_hline(yintercept=jul2017.rate, linetype="dashed")
figH <- figH + theme(legend.position="bottom") + scale_linetype_manual(values=c("longdash", "twodash", "solid", "solid", "solid")) + scale_color_manual(values=c("royalblue4", "deepskyblue1", "blue")) + scale_y_continuous(limits=c(0, 0.65), labels=percent_format(accuracy=1))

rm(analysis0, analysis1)

#### End #### 

