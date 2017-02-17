#R - File for Propensity Score Matching and finding the most probable matches

library(xlsx)
library(plyr)

#Path where the Data is stored.
DataDirectory <- "/Volumes/CCWB/Title_IV_E_Waiver_DCS/Data/Raw Data"
#Path when using CCWB system
DataDirectory2 <- "Z:/Title_IV_E_Waiver_DCS/Data/Raw Data" 
setwd(DataDirectory2)

#Reading the csv data
Base_Pop <- read.csv("ASU AFCARS Base-Pop.csv", header = T)
Participant_File <- read.csv("RPT187_PARTICIPANT_FILE_20170106_09231498.csv", header = T)
Person_File <- read.csv("RPT187_PERSON_FILE_20170106_09231498.csv", header = T)
Placement_File <- read.csv("RPT187_PLACEMENTS_FILE_20170106_09231498.csv", header = T)
Removal_File <- read.csv("RPT187_REMOVAL_FILE_20170106_09231498.csv", header = T)
Report_File <- read.csv("RPT187_REPORT_FILE_20170106_09231498.csv", header = T)
Services_File <- read.csv("RPT187_SERVICES_FILE_20170106_09231498.csv", header = T)

#Setting dates as dates in the tables
#Base_Pop$FC.21.LATEST.REMOVAL.DATE <- as.character(Base_Pop$FC.21.LATEST.REMOVAL.DATE)
Base_Pop$FC.21.LATEST.REMOVAL.DATE <- as.Date(Base_Pop$FC.21.LATEST.REMOVAL.DATE, format = "%m/%d/%y")
Base_Pop$FC.56.DATE.OF.DISCHARGE.FROM.CARE <- as.Date(Base_Pop$FC.56.DATE.OF.DISCHARGE.FROM.CARE, format = "%m/%d/%Y")
Base_Pop$FC.06.DATE.OF.BIRTH <- as.Date(Base_Pop$FC.06.DATE.OF.BIRTH, format = "%m/%d/%y")
Base_Pop$FC.23.CURRENT.PLACEMENT.DATE <- as.Date(Base_Pop$FC.23.CURRENT.PLACEMENT.DATE, format = "%m/%d/%Y")

Participant_File$Part.Run.Date <- as.Date(Participant_File$Part.Run.Date, format = "%m/%d/%Y")

Person_File$Part.Run.Date <- as.Date(Person_File$Part.Run.Date, format = "%m/%d/%Y")
Person_File$PERS.DOB <- as.Date(Person_File$PERS.DOB, format = "%m/%d/%Y")

Placement_File$Placem.Eff.Date <- as.Date(Placement_File$Placem.Eff.Date, format = "%m/%d/%Y")
Placement_File$Placem.End.Date <- as.Date(Placement_File$Placem.End.Date, format = "%m/%d/%Y")
Placement_File$Placem.Run.Date <- as.Date(Placement_File$Placem.Run.Date, format = "%m/%d/%Y")

Removal_File$REMVL.DATE <- as.Date(Removal_File$REMVL.DATE, format = "%m/%d/%Y")
Removal_File$REMVL.END.DATE <- as.Date(Removal_File$REMVL.END.DATE, format = "%m/%d/%Y")
Removal_File$REMVL.RUN.DT <- as.Date(Removal_File$REMVL.RUN.DT, format = "%m/%d/%Y")

Report_File$REPORT.Rprt.Date <- as.Date(Report_File$REPORT.Rprt.Date, format = "%m/%d/%Y")
Report_File$REPORT.Run.Date <- as.Date(Report_File$REPORT.Run.Date, format = "%m/%d/%Y")

Services_File$SVC.Eff.Date <- as.Date(Services_File$SVC.Eff.Date, format = "%m/%d/%Y")
Services_File$SVC.End.Date <- as.Date(Services_File$SVC.End.Date, format = "%m/%d/%Y")
Services_File$SVC.Run.Date <- as.Date(Services_File$SVC.Run.Date, format = "%m/%d/%Y")

#Subset Group home and sheter as congregate care type
Base_Pop2 <- Base_Pop[Base_Pop$CONGREGATE.CARE.TYPE == "SHELTER" | Base_Pop$CONGREGATE.CARE.TYPE=="GROUP HOME",]

#Subset the 1131 cases on which we have the information
common_cases <- intersect(Base_Pop2$FC.CASE.IDX, Removal_File$REMVL.CASE.ID)
Base_Pop3 <- Base_Pop2[Base_Pop2$FC.CASE.IDX %in% common_cases,]

#unit numbers that belong to avondale and tempe
Tempe_Unit_Numbers <- c('0010 00 07 000','0010 00 07 070','0010 00 07 071','0010 00 07 072', '0010 00 07 073',
                        '0010 00 07 074','0010 00 07 075','0010 00 07 076','0010 00 07 077','0010 00 07 078')

Avondale_Unit_Numbers <- c('0050 00 07 000','0050 00 07 070','0050 00 07 071','0050 00 07 072','0050 00 07 073',
                           '0050 00 07 074','0050 00 07 075','0050 00 07 076','0050 00 07 077')

#subset the records from Tempe and Avondale
Base_Pop4 <- Base_Pop3[Base_Pop3$FC.UNIT %in% Tempe_Unit_Numbers | Base_Pop3$FC.UNIT %in% Avondale_Unit_Numbers,]

#unit numbers that belong to regions in Maricopa other than Tempe and Avondale
Peoria_Unit_Numbers <- c('0050 00 05 000','0050 00 05 050','0050 00 05 051','0050 00 05 052','0050 00 05 053',
                         '0050 00 05 054','0050 00 05 055','0050 00 05 056')
Glendale_Unit_Numbers <- c('0050 00 06 000','0050 00 06 060','0050 00 06 061','0050 00 06 062','0050 00 06 063',
                           '0050 00 06 064','0050 00 06 065','0050 00 06 066','0050 00 06 067','0050 00 06 068',
                           '0050 00 06 069')
#unit numbers for Thunderbird, Mesa Mcdonald, South Mesa, District One
Thund_Mesa_Distone <- c('0050 00 08 000','0050 00 08 080','0050 00 08 081','0050 00 08 082','0050 00 08 083',
                        '0050 00 08 084','0050 00 08 085','0050 00 08 086','0050 00 08 087')
Thunderbird_Unit_Numbers <- c('0050 00 04 000','0050 00 04 040','0050 00 04 041','0050 00 04 042','0050 00 04 043',
                              '0050 00 04 044','0050 00 04 045','0050 00 04 046')
Phoenix_unit_Numbers <- c('0010 00 03 000','0010 00 03 030','0010 00 03 031','0010 00 03 032','0010 00 03 033',
                          '0010 00 03 034','0010 00 03 035','0010 00 03 036')
Mesa_Mcdonald_Unit_Numbers <- c('0010 00 05 000','0010 00 05 050','0010 00 05 051','0010 00 05 052','0010 00 05 053',
                                '0010 00 05 054','0010 00 05 055','0010 00 05 056','0010 00 05 057','0010 00 05 058')
South_Mountain <- c('0010 00 08 000','0010 00 08 080','0010 00 08 081','0010 00 08 082','0010 00 08 083',
                    '0010 00 08 084','0010 00 08 085','0010 00 08 086','0010 00 08 087','0010 00 08 088',
                    '0010 00 08 089')
University_Unit_Numbers <- c('0010 00 10 000','0010 00 10 100','0010 00 10 101','0010 00 10 102','0010 00 10 103',
                             '0010 00 10 104','0010 00 10 105','0010 00 10 106','0010 00 10 107')
South_Mesa_Unit_Numbers <- c('0010 00 06 000','0010 00 06 060','0010 00 06 061','0010 00 06 062','0010 00 06 063',
                             '0010 00 06 064','0010 00 06 065','0010 00 06 066','0010 00 06 067','0010 00 06 068')
Twntieth_Street <- c('0010 00 09 000','0010 00 09 090','0010 00 09 091','0010 00 09 092','0010 00 09 093',
                     '0010 00 09 094','0010 00 09 095','0010 00 09 096','0010 00 09 097','0010 00 09 098',
                     '0010 00 09 099')
#subset the records from regions other than Tempe and Avondale but those that belong to Maricopa
Base_Pop5 <- Base_Pop3[Base_Pop3$FC.UNIT %in% Peoria_Unit_Numbers | Base_Pop3$FC.UNIT %in% Glendale_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% Thund_Mesa_Distone | Base_Pop3$FC.UNIT %in% Thunderbird_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% Phoenix_unit_Numbers | Base_Pop3$FC.UNIT %in% Mesa_Mcdonald_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% South_Mountain | Base_Pop3$FC.UNIT %in% University_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% South_Mesa_Unit_Numbers | Base_Pop3$FC.UNIT %in% Twntieth_Street,]

#Base_Pop4 is split one - cases that have recieved treatment
#Base_Pop5 is split two - cases that have not received treatment
#We need to build variables for the two files - 
#1) Age at First Removal
#2) Race
#3) Gender
#4) Primary Language of Caretaker
#5) Number of Removals
#6) Hispanic (Yes/No)
#7) Marital STatus of Caretaker

#Merging Removal File with Person FIle
#Consider columnns for Removal and Person necessary for table construction
Removal_Filter <- Removal_File[,c(1, 2, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 37, 38)]
Person_Filter <- Person_File[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
#Merge tables Removal_File and Person Detail Record
Removal_Person_merger <-  merge(x = Removal_Filter, by.x = "REMVL.CHILD.ID", y = Person_Filter, 
                                by.y="PERS.ID", all.x = TRUE)

#Merging Base_Pop4 with the Removal File - First Population
Base_Pop4_Removal <- merge(x = Base_Pop4, 
                           y = Removal_Person_merger, 
                           by.x = c("FC.CASE.IDX","FC.21.LATEST.REMOVAL.DATE", "FC.06.DATE.OF.BIRTH"),
                           by.y = c("REMVL.CASE.ID","REMVL.DATE","PERS.DOB"))

#Merging Base_Pop5 with the Removal File - Second Population
Base_Pop5_Removal <- merge(x = Base_Pop5, 
                           y = Removal_Person_merger, 
                           by.x = c("FC.CASE.IDX","FC.21.LATEST.REMOVAL.DATE", "FC.06.DATE.OF.BIRTH"),
                           by.y = c("REMVL.CASE.ID","REMVL.DATE","PERS.DOB"))

#Creating the two populations for the Matching by choosing required columns.
#Out of the 144 records in Population1, only 122 match for Maricopa county.
#Included only variables that could be potentially useful for matching
#Care needs to be taken to exclude missing variables
#Variables chosen in a way that no variables repeat
#FC.REMOVAL.COUNTY represents county of treatment and
#REMVL.COUNTY represents county of removal - assumption
#Age in days at removal in years given - which should suffice, age in days will make it more granular


Population1 <- Base_Pop4_Removal[,c(1,43,2,3,8,10,11,12,13,14,15,16,17,18,19,20,22,23,25,27,28,29,39,40,41,42,45,47,48,
                                    52,53,54,58,59,60,62)]
#we add a new column Sample_Type and declare population1 as treated.
Population1$Sample_Type <- as.factor('Treated')
Population2 <- Base_Pop5_Removal[,c(1,43,2,3,8,10,11,12,13,14,15,16,17,18,19,20,22,23,25,27,28,29,39,40,41,42,45,47,48,
                                    52,53,54,58,59,60,62)]
#we add a new column Sample_Type and declare population2 as non-treated.
Population2$Sample_Type <- as.factor('Non-Treated')

#Before we match the samples, we need to merge the two dataframes. 
#We create a new variable GROUP - of type logic - based on tge variable Sample_Type
Population <- rbind(Population1,Population2)
Population$Sample_Type<- as.logical(Population$Sample_Type == 'Treated')

#We will exclude the ones that do not belong to Maricopa County
Population_Maricopa <- Population[Population$FC.REMOVAL.COUNTY == 'MARICOPA',]
#check for any missing values
#FC.11.MENTAL.RETARDATION, FC.12.VISUAL.HEARING.IMPAIRED, FC.13.PHYS.DISABLED have 6 NAs each
#Family Structure Description has 53 blank strings, thus we will not consider these 4 covariates 
#in matching. We now have 122 treated and 651 non-treated cases

Population_Maricopa1 <- Population_Maricopa[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23,24,25,27,28,29,
                                              30,31,32,33,34,35,36,37)]
 
#Anirudh's addition
#Removing column (29,30,31) from population 1 as they are only names

Population_Maricopa1 <- Population_Maricopa1[,-c(29,30,31)]













