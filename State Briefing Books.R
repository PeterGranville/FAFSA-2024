
#### Setup ####

library(maps)
library(dplyr)
library(scales)
library(readxl)
library(plotly)
library(stringr)
library(viridis)
library(mapdata)
library(zipcodeR)
library(geosphere)
library(tidyverse)
library(colorRamps)
library(data.table)
library(stringdist)

#### End #### 

#### Set state of interest ####

stateLong <- "Texas" 
stateShort <- "TX"

#### End #### 

#### Load in formatted FAFSA data by ZIP ####

setwd("/Users/peter_granville/FAFSA-2024")

analysis <- read.csv("FAFSA by ZIP 2017 to 2024.csv", header=TRUE, check.names=FALSE)

analysis <- analysis %>% filter(
  `State` == stateShort
) %>% select(
  `ZCTA5`,
  `Grade 12 students`, 
  `Submissions`,
  `Completions`, 
  `Year`
)

#### End #### 

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

analysis <- addCounty(analysis)

rm(badZIPs)

#### End #### 

#### Fix problem county names ####

analysis$`County`[analysis$`County`=="Mclean County"] <- "McLean County"
analysis$`County`[analysis$`County`=="LaSalle County"] <- "La Salle County"
analysis$`County`[analysis$`County`=="St. Clair County"] <- "St Clair County" 
analysis$`County`[analysis$`County`=="DeKalb County"] <- "De Kalb County"
analysis$`County`[analysis$`County`=="DuPage County"] <- "Du Page County"
analysis$`County`[analysis$`County`=="DeWitt County"] <- "De Witt County"

#### End #### 

#### Derive city from ZIP ####

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

addCity <- function(analysis0){
  
  analysis0 <- analysis0 %>% mutate(`City` = rep(NA))
  for(i in (1:nrow(analysis0))){
    if((analysis0$`ZCTA5`[i] %in% badZIPs)==FALSE){
      analysis0$`City`[i] <- reverse_zipcode(substr(analysis0$`ZCTA5`[i], 7, 11))$`major_city`
    }
  }
  
  return(analysis0)
}

analysis <- addCity(analysis)

rm(badZIPs)

#### End #### 

########################################
#### High school seniors' FAFSA     ####
#### submission and completion      ####
#### rates by county (with change   ####
#### over time)                     ####
########################################

#### Completion rates by county ####

analysis.county <- aggregate(
  data=analysis, cbind(
    `Grade 12 students`, `Submissions`, `Completions`
  ) ~ `County` + `Year`, FUN=sum
) %>% mutate(
  `Submission rate` = `Submissions` / `Grade 12 students`, 
  `Completion rate` = `Completions` / `Grade 12 students`
) 

analysis.county.submissions <- analysis.county %>% pivot_wider(
  id_cols=c(`County`), 
  names_from=`Year`,
  values_from=`Submission rate`
) %>% mutate(
  `Percentage point change, Class of 2018 to Class of 2023` = `Class of 2023` - `Class of 2018`
)

analysis.county.completions <- analysis.county %>% pivot_wider(
  id_cols=c(`County`), 
  names_from=`Year`,
  values_from=`Completion rate`
) %>% mutate(
  `Percentage point change, Class of 2018 to Class of 2023` = `Class of 2023` - `Class of 2018`
)

#### End #### 

#### Completion rates by city ####

analysis.city <- aggregate(
  data=analysis, cbind(
    `Grade 12 students`, `Submissions`, `Completions`
  ) ~ `City` + `Year`, FUN=sum
) %>% mutate(
  `Submission rate` = `Submissions` / `Grade 12 students`, 
  `Completion rate` = `Completions` / `Grade 12 students`
) 

analysis.city.submissions <- analysis.city %>% pivot_wider(
  id_cols=c(`City`), 
  names_from=`Year`,
  values_from=`Submission rate`
) %>% mutate(
  `Percentage point change, Class of 2018 to Class of 2023` = `Class of 2023` - `Class of 2018`
)

analysis.city.completions <- analysis.city %>% pivot_wider(
  id_cols=c(`City`), 
  names_from=`Year`,
  values_from=`Completion rate`
) %>% mutate(
  `Percentage point change, Class of 2018 to Class of 2023` = `Class of 2023` - `Class of 2018`
)

#### End #### 

#### Write output files #### 

setwd("/Users/peter_granville/FAFSA-2024/Outputs for State Briefing Books")

filename.county.submissions <- paste("High School FAFSAs - County level - Submission rate - ", stateLong, ".csv", sep="")
filename.city.submissions <- paste("High School FAFSAs - City level - Submission rate - ", stateLong, ".csv", sep="")

filename.county.completions <- paste("High School FAFSAs - County level - Completion rate - ", stateLong, ".csv", sep="")
filename.city.completions <- paste("High School FAFSAs - City level - Completion rate - ", stateLong, ".csv", sep="")

write.csv(analysis.county.submissions, filename.county.submissions, row.names=FALSE)
write.csv(analysis.city.submissions, filename.city.submissions, row.names=FALSE)
write.csv(analysis.county.completions, filename.county.completions, row.names=FALSE)
write.csv(analysis.city.completions, filename.city.completions, row.names=FALSE)

rm(
  filename.county.submissions,
  filename.city.submissions, 
  filename.county.completions,
  filename.city.completions
)

#### End #### 

#### Create heatmap functions ####

createHeatmap <- function(fafsaClass){
  
  analysis.county.completions.import <- analysis.county.completions %>% select(
    `County`,
    all_of(fafsaClass)
  ) 
  names(analysis.county.completions.import)[2] <- "FAFSA completion rate"
  
  analysis.county.completions.import <- analysis.county.completions.import %>% mutate(
    `County` = tolower(gsub(" County", "", `County`))
  ) %>% rename(
    `subregion` = `County`
  ) 
  
  states <- map_data("state")
  counties <- map_data("county")
  state_df <- subset(states, region == tolower(stateLong))
  state_county <- subset(counties, region == tolower(stateLong))
  fafsaCountyForMap <- left_join(x=state_county, y=analysis.county.completions.import, by = "subregion")
  
  state_base <- ggplot(
    data = state_df, mapping = aes(x = long, y = lat, group = group)
  ) + coord_fixed(
    1.3
  ) + geom_polygon(
    data = fafsaCountyForMap, mapping = aes(fill = `FAFSA completion rate`), color = "white", linewidth=0.1
  ) + scale_fill_continuous(
    labels=percent_format(accuracy=1), limits=c(0, 1)
  ) + theme(
    axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()
  ) + labs(
    title=paste("Public high school FAFSA completion rate in ", stateLong, " counties: ", fafsaClass, sep="")
  )
  
  return(ggplotly(state_base, tooltip="fill"))
  
  rm(
    analysis.county.completions.import, 
    state_base, 
    state_county, 
    state_df,
    states, 
    fafsaCountyForMap, 
    counties
  )
  
}

#### End #### 

#### Create comparison heatmap function #### 

createHeatmapComp <- function(fafsaClass1, fafsaClass2){
  
  analysis.county.completions.import <- analysis.county.completions %>% select(
    `County`,
    all_of(fafsaClass1), 
    all_of(fafsaClass2)
  ) 
  names(analysis.county.completions.import)[2] <- "FAFSA completion rate 1"
  names(analysis.county.completions.import)[3] <- "FAFSA completion rate 2"
  
  analysis.county.completions.import <- analysis.county.completions.import %>% mutate(
    `Percentage point change` = `FAFSA completion rate 2` - `FAFSA completion rate 1`
  ) %>% select(
    -(`FAFSA completion rate 1`), 
    -(`FAFSA completion rate 2`)
  )
  
  analysis.county.completions.import <- analysis.county.completions.import %>% mutate(
    `County` = tolower(gsub(" County", "", `County`))
  ) %>% rename(
    `subregion` = `County`
  ) 
  
  states <- map_data("state")
  counties <- map_data("county")
  state_df <- subset(states, region == tolower(stateLong))
  
  state_county <- subset(counties, region == tolower(stateLong))
  fafsaCountyForMap <- left_join(x=state_county, y=analysis.county.completions.import, by = "subregion")
  
  state_base <- ggplot(
    data = state_df, mapping = aes(x = long, y = lat, group = group)
  ) + coord_fixed(
    1.3
  ) + geom_polygon(
    data = fafsaCountyForMap, mapping = aes(fill = `Percentage point change`), color = "white", linewidth=0.1
  ) + scale_fill_continuous(
    labels=percent_format(accuracy=1)
  ) + theme(
    axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()
  ) + labs(
    title=paste("Percentage point change in public high school FAFSA completion rate in ", stateLong, " counties: ", fafsaClass1, " to ", fafsaClass2, sep="")
  )
  
  return(ggplotly(state_base, tooltip="fill"))
  
  rm(
    analysis.county.completions.import, 
    state_base, 
    state_county, 
    state_df,
    states, 
    fafsaCountyForMap, 
    counties
  )
  
}

#### End ####

#### Run heatmap functions #### 

createHeatmap("Class of 2017")
createHeatmap("Class of 2018")
createHeatmap("Class of 2019")
createHeatmap("Class of 2020")
createHeatmap("Class of 2021")
createHeatmap("Class of 2022")
createHeatmap("Class of 2023")
createHeatmap("Class of 2024")

createHeatmapComp("Class of 2019", "Class of 2023")

#### End #### 

########################################
#### Total number of FAFSAs         ####
#### processed at each of the       ####
#### state's colleges (with change  ####
#### over time)                     ####
########################################

#### Load and format datasets ####

setwd("/Users/peter_granville/FAFSA-2024/College FAFSAs")

colleges19 <- read_excel(
  "2018-2019-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2018-19")
)
colleges20 <- read_excel(
  "2019-2020-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2019-20")
)
colleges21 <- read_excel(
  "2020-2021-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2020-21")
)
colleges22 <- read_excel(
  "2021-2022-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2021-22")
)
colleges23 <- read_excel(
  "2022-2023-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2022-23")
)
colleges24 <- read_excel(
  "2023-2024-app-data-by-school-q6.xls", sheet="App by School", skip=5, col_names=TRUE
) %>% mutate(
  `College Year` = rep("2023-24")
)

colleges <- rbind(
  colleges19, 
  colleges20, 
  colleges21, 
  colleges22, 
  colleges23, 
  colleges24
)

rm(
  colleges19, 
  colleges20, 
  colleges21, 
  colleges22, 
  colleges23, 
  colleges24
)

colleges <- colleges %>% select(
  -(`Dependent Students...6`), 
  -(`Independent Students...7`), 
  -(`Quarterly Total`)
) %>% rename(
  `Dependent Students` = `Dependent Students...9`, 
  `Independent Students` = `Independent Students...10`
) %>% filter(
  `State` == stateShort
) %>% mutate(
  `Zip Code` = substr(`Zip Code`, 1, 5)
) %>% rename(
  `Total` = `Award Year To Date Total`
) %>% mutate(
  `Dependent Students` = as.numeric(`Dependent Students`),
  `Independent Students` = as.numeric(`Independent Students`),
  `Total` = as.numeric(`Total`)
)

colleges.ind <- colleges %>% pivot_wider(
  id_cols=c(`OPE ID`, `School`, `State`, `Zip Code`, `School Type`), 
  names_from=`College Year`, 
  values_from=`Independent Students`
)

colleges.dep <- colleges %>% pivot_wider(
  id_cols=c(`OPE ID`, `School`, `State`, `Zip Code`, `School Type`), 
  names_from=`College Year`, 
  values_from=`Dependent Students`
)

colleges.tot <- colleges %>% pivot_wider(
  id_cols=c(`OPE ID`, `School`, `State`, `Zip Code`, `School Type`), 
  names_from=`College Year`, 
  values_from=`Total`
)

#### End #### 

#### Calculate change, 2018-19 to 2023-24 ####

colleges.dep <- colleges.dep %>% mutate(
  `Percent change, 2018-19 to 2023-24` = (`2023-24` - `2018-19`) / `2018-19`
)

colleges.ind <- colleges.ind %>% mutate(
  `Percent change, 2018-19 to 2023-24` = (`2023-24` - `2018-19`) / `2018-19`
)

colleges.tot <- colleges.tot %>% mutate(
  `Percent change, 2018-19 to 2023-24` = (`2023-24` - `2018-19`) / `2018-19`
)

#### End #### 

#### Write output files #### 

setwd("/Users/peter_granville/FAFSA-2024/Outputs for State Briefing Books")

filename.dep <- paste("College FAFSAs - Dependent Students - ", stateLong, ".csv", sep="")
filename.ind <- paste("College FAFSAs - Independent Students - ", stateLong, ".csv", sep="")
filename.tot <- paste("College FAFSAs - Total Students - ", stateLong, ".csv", sep="")

write.csv(colleges.dep, filename.dep, row.names=FALSE)
write.csv(colleges.ind, filename.ind, row.names=FALSE)
write.csv(colleges.tot, filename.tot, row.names=FALSE)

rm(
  filename.dep,
  filename.ind,
  filename.tot
)

#### End #### 

########################################
#### Share of enrolled college      ####
#### students who receive Pell, for ####
#### each of the state's colleges   ####
#### (with change over time)        ####
########################################

#### Load and format data #### 

setwd("/Users/peter_granville/Various-TCF-Projects")

sfa22 <- read.csv(
  "sfa2122_rv.csv", header=TRUE
) %>% select(
  `UNITID`, `UPGRNTP`
) %>% mutate(
  `Year` = rep("2021-22")
)
sfa21 <- read.csv(
  "sfa2021_rv.csv", header=TRUE
) %>% select(
  `UNITID`, `UPGRNTP`
) %>% mutate(
  `Year` = rep("2020-21")
)
sfa20 <- read.csv(
  "sfa1920_rv.csv", header=TRUE
) %>% select(
  `UNITID`, `UPGRNTP`
) %>% mutate(
  `Year` = rep("2019-20")
)

sfa <- rbind(
  sfa22, 
  sfa21, 
  sfa20
) %>% mutate(
  `UPGRNTP` = `UPGRNTP` / 100
)

sfa <- sfa %>% pivot_wider(
  id_cols=c(`UNITID`), 
  names_from=`Year`, 
  values_from=`UPGRNTP`
) %>% select(
  `UNITID`, 
  `2019-20`, 
  `2020-21`, 
  `2021-22`
)

rm(
  sfa22, 
  sfa21, 
  sfa20
)

hd22 <- read.csv(
  "hd2022.csv", header=TRUE
) %>% select(
  `UNITID`, `INSTNM`, `CITY`, `STABBR`, `SECTOR`
) 

sector.names <- data.frame(
  `SECTOR` = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
  `Sector` = c(
    "Administrative Unit",
    "Public, 4-year or above",
    "Private not-for-profit, 4-year or above",
    "Private for-profit, 4-year or above",
    "Public, 2-year",
    "Private not-for-profit, 2-year",
    "Private for-profit, 2-year",
    "Public, less-than 2-year",
    "Private not-for-profit, less-than 2-year",
    "Private for-profit, less-than 2-year"
  )
)
hd22 <- left_join(
  x=hd22, y=sector.names, by="SECTOR"
) %>% select(
  -(`SECTOR`)
) %>% rename(
  `State` = `STABBR`, 
  `Name` = `INSTNM`, 
  `City` = `CITY`
)

sfa <- right_join(
  x=hd22, y=sfa, by="UNITID"
) %>% filter(
  `State` == stateShort
)

rm(
  sector.names, 
  hd22
)

#### End #### 

#### Write CSVs ####

setwd("/Users/peter_granville/FAFSA-2024/Outputs for State Briefing Books")

filename <- paste("Pell share of undergraduates - ", stateLong, ".csv", sep="")
write.csv(sfa, filename, row.names=FALSE)

rm(sfa, filename)

#### End #### 

########################################
#### Share of enrolled college      ####
#### students in the state who      ####
#### complete the FAFSA, by         ####
#### demographics                   ####
########################################

#### Overall at 2- and 4-years ####

setwd("/Users/peter_granville/FAFSA-2024/NPSAS data")

overall2 <- read.csv(
  "pfiphb.csv", skip=12, nrow=52, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% rename(
  `State` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

overall4 <- read.csv(
  "brpbbk.csv", skip=12, nrow=52, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% rename(
  `State` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

overall4I <- read.csv(
  "wafruu.csv", skip=12, nrow=52, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% rename(
  `State` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

overall <- full_join(x=overall2, y=full_join(
  x=overall4, y=overall4I, by="State"
), by="State")

rm(
  overall2,
  overall4, 
  overall4I
)

#### End #### 

#### By race/ethnicity: 2-years ####

race2.us <- read.csv(
  "ufwouj.csv", skip=13, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

race2.ca <- read.csv(
  "ufwouj.csv", skip=77, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

race2.il <- read.csv(
  "ufwouj.csv", skip=141, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

race2.tx <- read.csv(
  "ufwouj.csv", skip=205, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

race2 <- rbind(
  race2.us, 
  race2.ca, 
  race2.il, 
  race2.tx
)

rm(
  race2.us, 
  race2.ca, 
  race2.il, 
  race2.tx
)

#### End #### 

#### By race/ethnicity: 4-years ####

race4.us <- read.csv(
  "nrpods.csv", skip=13, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

race4.ca <- read.csv(
  "nrpods.csv", skip=77, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

race4.il <- read.csv(
  "nrpods.csv", skip=141, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

race4.tx <- read.csv(
  "nrpods.csv", skip=205, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

race4 <- rbind(
  race4.us, 
  race4.ca, 
  race4.il, 
  race4.tx
)

rm(
  race4.us, 
  race4.ca, 
  race4.il, 
  race4.tx
)

#### End #### 

#### By race/ethnicity: 4-years ####

race4I.us <- read.csv(
  "frfesf.csv", skip=13, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

race4I.ca <- read.csv(
  "frfesf.csv", skip=77, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

race4I.il <- read.csv(
  "frfesf.csv", skip=141, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

race4I.tx <- read.csv(
  "frfesf.csv", skip=205, nrow=7, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

race4I <- rbind(
  race4I.us, 
  race4I.ca, 
  race4I.il, 
  race4I.tx
)

rm(
  race4I.us, 
  race4I.ca, 
  race4I.il, 
  race4I.tx
)

#### End #### 

#### Combine stats by race ####

race <- full_join(x=race2, y=full_join(
  x=race4, y=race4I, by=c("Group", "State")
), by=c("Group", "State"))

rm(race2, race4, race4I)

#### End #### 

#### By age: 2-years ####

age2.us <- read.csv(
  "nnclwk.csv", skip=13, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

age2.ca <- read.csv(
  "nnclwk.csv", skip=57, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

age2.il <- read.csv(
  "nnclwk.csv", skip=101, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

age2.tx <- read.csv(
  "nnclwk.csv", skip=145, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 2-year students who complete FAFSA` = `V3`
)

age2 <- rbind(
  age2.us, 
  age2.ca, 
  age2.il, 
  age2.tx
)

rm(
  age2.us, 
  age2.ca, 
  age2.il, 
  age2.tx
)

#### End #### 

#### By age: 4-years ####

age4.us <- read.csv(
  "goqqrp.csv", skip=13, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

age4.ca <- read.csv(
  "goqqrp.csv", skip=57, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

age4.il <- read.csv(
  "goqqrp.csv", skip=101, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

age4.tx <- read.csv(
  "goqqrp.csv", skip=145, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA` = `V3`
)

age4 <- rbind(
  age4.us, 
  age4.ca, 
  age4.il, 
  age4.tx
)

rm(
  age4.us, 
  age4.ca, 
  age4.il, 
  age4.tx
)

#### End #### 

#### By age: 4-years ####

age4I.us <- read.csv(
  "vgrjxu.csv", skip=13, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("U.S. Overall")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

age4I.ca <- read.csv(
  "vgrjxu.csv", skip=57, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("California")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

age4I.il <- read.csv(
  "vgrjxu.csv", skip=101, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Illinois")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

age4I.tx <- read.csv(
  "vgrjxu.csv", skip=145, nrow=3, header=FALSE
) %>% select(
  `V1`, 
  `V3`
) %>% mutate(
  `V3` = as.numeric(`V3`) / 100
) %>% mutate(
  `State` = rep("Texas")
) %>% rename(
  `Group` = `V1`, 
  `Share of public 4-year students who complete FAFSA (in-state enrollment only)` = `V3`
)

age4I <- rbind(
  age4I.us, 
  age4I.ca, 
  age4I.il, 
  age4I.tx
)

rm(
  age4I.us, 
  age4I.ca, 
  age4I.il, 
  age4I.tx
)

#### End #### 

#### Combine stats by age ####

age <- full_join(x=age2, y=full_join(
  x=age4, y=age4I, by=c("Group", "State")
), by=c("Group", "State"))

rm(age2, age4, age4I)

#### End #### 

#### Write output files ####

setwd("/Users/peter_granville/FAFSA-2024/Outputs for State Briefing Books")

write.csv(overall, "NPSAS data - Overall.csv", row.names=FALSE)
write.csv(race, "NPSAS data - Race.csv", row.names=FALSE)
write.csv(age, "NPSAS data - Age.csv", row.names=FALSE)

#### End #### 


