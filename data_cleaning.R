
########################################################
# COVID-19 Modelling
# Script for cleaning CHESS data
# Requires the packages tidyverse, lubridate, readxl, readr, foreign
# CHESS COVID19 CaseReport data released on 26-05-2020
########################################################
# Authors:
# Andrea Aparicio Castro, Diego Perez Ruiz, Arkadiusz Wisniowski
#
########################################################

`%notin%` <- Negate(`%in%`)

########################################################
#Importing cases reports and creating unique identifiers
########################################################

CHESS_CaseReport_Data <- read.csv(file = 'Data/Data - Processed/26th May data processed/CHESS COVID19 CaseReport 20200526 deduplicated complete.csv') %>% 
  group_by(caseid, trustcode) %>%
  mutate(
    caseid_obsno = paste0(caseid, "_", obsid)) %>% 
  as.data.frame(ungroup())

CHESS_CaseReport_Data[CHESS_CaseReport_Data==""] <- NA
report_date <- as.Date("05/26/2020", format = "%m/%d/%Y")

####################################################
#Dates as dates and initial filtering 
####################################################
CHESS_CaseReport_Data <- CHESS_CaseReport_Data %>%
  mutate(
  dateupdated = dateupdated %>% dmy_hms()) %>%
  separate(dateupdated,into = c("dateup", "timeup"), sep = " ",remove=FALSE) %>%
  mutate(
    dateup = ymd(as.character(dateup)),
    timeup = hms::as_hms(timeup),
    estimateddateonset = dmy(as.character(estimateddateonset)),
    infectionswabdate	= dmy(as.character(infectionswabdate)),
    labtestdate = dmy(as.character(labtestdate)),
    hospitaladmissiondate = dmy(as.character(hospitaladmissiondate)),
    dateadmittedicu = dmy(as.character(dateadmittedicu)) %>% na_if(dmy("01/01/1900")),
    dateleavingicu = dmy(as.character(dateleavingicu)) %>% na_if(dmy("01/01/1900")),
    sbdate  = dmy(as.character(sbdate)) %>% na_if(dmy("01/01/1900")),
    finaloutcomedate = dmy(as.character(finaloutcomedate)) %>% na_if(dmy("01/01/1900")),
    wasthepatientadmittedtoicu = as.character(wasthepatientadmittedtoicu) %>% fct_explicit_na("Unknown"),
    wasthepatientadmittedtoicu = ifelse(wasthepatientadmittedtoicu=="No" & !is.na(dateadmittedicu), "Yes",as.character(wasthepatientadmittedtoicu))) %>%
    filter(weekofadmission>=12 & weekofadmission<=21 &
           ageyear>=18 & 
           sex!="Unknown" & 
           wasthepatientadmittedtoicu!="Unknown") %>%
    mutate(bad_trusts = case_when(
      trustcode=="RF4" ~1, 
      trustcode=="RDD" ~1,
      trustcode=="RMC" ~1,
      trustcode=="RVV" ~1,
      trustcode=="RYJ" ~1,
      trustcode=="RNS" ~1,
      trustcode=="RXW" ~1,
      trustcode=="RJ7" ~1,
      trustcode=="RAS" ~1,
      trustcode=="RRJ" ~1,
      trustcode=="RRK" ~1,
      trustcode=="RRF" ~1, TRUE~ 0))%>%
    filter(bad_trusts==0 & wasthepatientadmittedtoicu=="Yes") 

####################################################
#Educated guessing
####################################################
####################################################
#finaloutcomedate
####################################################
# assuming date of update as final outcome date

CHESS_CaseReport_Data = CHESS_CaseReport_Data %>%
  mutate(
    finaloutcomedate1 = ifelse((finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered") & is.na(finaloutcomedate), dateup, finaloutcomedate) %>% as_date(),
    finaloutcome1 = finaloutcome %>% fct_explicit_na("Censored") %>% recode(
      "Death" = "Death",
      "Discharged" = "Discharged",
      "Transfered" = "Discharged",
      "N/A - still on unit" = "Censored" ))

####################################################
#Admission to ICU and hospital + Date of leaving ICU
####################################################
CHESS_CaseReport_Data <- CHESS_CaseReport_Data %>%
                         mutate( #admitted to icu
                                 dateadmittedicu1 = as.Date(ifelse( !is.na(dateadmittedicu) == FALSE & wasthepatientadmittedtoicu=="Yes" |
                                                          !is.na(dateleavingicu)==TRUE, hospitaladmissiondate, dateadmittedicu), origin = "1970-01-01"),
                                 #admission to hospital
                                 hospitaladmissiondate1 = as.Date(ifelse( !is.na(hospitaladmissiondate)==FALSE & !is.na(dateadmittedicu1)==TRUE, 
                                                                          dateadmittedicu, hospitaladmissiondate), origin = "1970-01-01"),
                                 #leavingicu
                                 dateleavingicu1 = as.Date(ifelse( !is.na(dateleavingicu)==FALSE & 
                                                                   wasthepatientadmittedtoicu=="Yes" &
                                                                   finaloutcome1!="Censored", finaloutcomedate1, dateleavingicu), origin = "1970-01-01"),
                                 dateleavingicu2 = as.Date(ifelse( !is.na(dateleavingicu1) == FALSE & 
                                                                   wasthepatientadmittedtoicu=="Yes" & 
                                                                   finaloutcome1!="Censored", dateup, dateleavingicu1), origin = "1970-01-01"))

####################################################
# Duration and censoring variables
####################################################

CHESS_CaseReport_Data <- CHESS_CaseReport_Data  %>%
  mutate( ##last date for duration (either outcome or date of update or sbdate)
          lastdateup=as_date(ifelse(!is.na(finaloutcomedate1),finaloutcomedate1,
                                    ifelse(!is.na(dateup),dateup, sbdate))),
          
          #duration from icu admission to leaving icu 
          durationiculeavingicu = case_when(!is.na(dateleavingicu2) ~ as.double(dateleavingicu2 - dateadmittedicu1, units = "days"),
                                             wasthepatientadmittedtoicu=="Yes" & !is.na(dateadmittedicu1) & is.na(dateleavingicu2) ~ as.double(lastdateup - dateadmittedicu1, units = "days")),
          #censoring variable
          dummy_outcome_icu = ifelse(!is.na(dateadmittedicu1) & (durationiculeavingicu>0) & finaloutcome%in%c("Death","Discharged", "Transfered"), 1, 0)) %>%
  filter(durationiculeavingicu>0)

##################################################
# Joining Rasch scores
###################################################
RaschScores_MP <- read.csv('Data/Data - Processed/26th May data processed/SOST paper/data_forRaschScores_MP_2020-05-26with scores.csv') %>%
  rename(entry=AA) %>%
  select(entry,RaschComor)

CHESS_CaseReport_Data <- CHESS_CaseReport_Data %>%
  mutate(entry = row_number()) %>%
  left_join(RaschScores_MP, by=c("entry")) %>%
  select(-entry)

####################################################
### Creating variable: nhs region of trust
####################################################

trust_geographies <- read_csv("Data/Data - Other/trust_geographies.csv") %>%
  rename(nhser_code = trust_nhser, 
         trustcode = trust_code) %>%
  mutate(
    nhser_name = case_when(
      nhser_code=="E40000003" ~	"London",
      nhser_code=="E40000005" ~	"South East",
      nhser_code=="E40000006" ~	"South West",
      nhser_code=="E40000007" ~	"East of England",
      nhser_code=="E40000008" ~	"Midlands",
      nhser_code=="E40000009" ~	"North East and Yorkshire",
      nhser_code=="E40000010" ~	"North West",
      TRUE ~ nhser_code),
    nhser_name = factor(nhser_name, levels = c("London","South East","South West","East of England","Midlands","North East and Yorkshire","North West"), ordered = TRUE))%>%
  select(trustcode,nhser_code,nhser_name)

CHESS_CaseReport_Data = CHESS_CaseReport_Data %>%
  left_join(trust_geographies, by = "trustcode")

####################################################
# Demographics
####################################################
CHESS_CaseReport_Data <- CHESS_CaseReport_Data  %>%
  mutate(
    #Sex
    female = ifelse(sex=="Female",1,0),
    
    #Age
    ageyear4 = dplyr::case_when(
      ageyear%in%c(18:49) ~ "<50",
      ageyear%in%c(50:64) ~ "50-64",
      ageyear%in%c(65:74) ~ "65-74",
      ageyear%in%c(75:150) ~ "75+"),
    ageyear4 = factor(ageyear4, levels = c("<50", "50-64","65-74","75+"), ordered = TRUE),
    age18_49 = ifelse(ageyear4=="<50",1,0),
    age50_64 = ifelse(ageyear4=="50-64",1,0),
    age65_74 = ifelse(ageyear4=="65-74",1,0),
    age75up = ifelse(ageyear4=="75+",1,0),
    
    #Ethnicity
    ethnicity = ethnicity %>% gdata::trim() %>% na_if("Unknown"),
    ethnicity5= dplyr::case_when(
      ethnicity %in% c("British", "Irish", "White British", "White Irish", "Other White") ~ "White",
      ethnicity%in%c("Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian") ~ "Asian",
      ethnicity%in%c("Black Caribbean", "Black African", "Other Black") ~ "Black",
      ethnicity%in%c("White and Asian", "White and Black African", "White and Black Caribbean","Other mixed") ~ "Mixed",
      ethnicity%in%c("other") ~ "Other", 
      ethnicity%in%c("Unknown") ~ "Unknown"),
    ethnicity5 = factor(ethnicity5, levels = c("White", "Asian","Black","Mixed","Other", "Unknown"), ordered = TRUE),
    ethnicity2= dplyr::case_when(
      ethnicity5%in% c("White") ~ "White",
      ethnicity5%in%c("Asian", "Black", "Mixed", "Other") ~ "non-white",
      ethnicity%in%c("Unknown") ~ "Unknown"),
    ethnicity2 = factor(ethnicity2, levels = c("White","non-white", "Unknown"), ordered = TRUE),
    
    #Rash scores
    RaschComor1 = case_when(
      RaschComor <= -2.67 ~ 1,
      RaschComor > -2.67 & RaschComor <= -1.910 ~ 2,
      RaschComor > -1.910 & RaschComor <= -1.030 ~ 3,
      RaschComor > -1.030 ~ 4),
    
    #HAI patients
    HAI = case_when(
      issecondarybacterialpneumoniacom=="Yes" ~ 1,
      isothercoinfectionscomplication=="Yes" ~ 1,
      TRUE ~ 0),
    
    #ICU policy change variable
    icupolicychange = case_when(
      dateadmittedicu1<=ymd("2020-03-24") ~ 1,
      dateadmittedicu1>=ymd("2020-03-25") & dateadmittedicu1<=ymd("2020-04-07") ~ 2,
      dateadmittedicu1>=ymd("2020-04-08") ~ 3 ),
    
    #Week of admission
    admit12_13 = ifelse((weekofadmission==12|weekofadmission==13),1,0),
    admit14_15 = ifelse((weekofadmission==14|weekofadmission==15),1,0),
    admit16_17 = ifelse((weekofadmission==16|weekofadmission==17),1,0),
    admit18_19 = ifelse((weekofadmission==18|weekofadmission==19),1,0),
    admit20_21 = ifelse((weekofadmission==20|weekofadmission==21),1,0))
    
