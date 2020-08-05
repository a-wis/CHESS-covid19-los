
########################################################
# COVID-19 Modelling
# Script for getting descriptive statistics of LoS calculated based on CHESS data
# Requires the packages tidyverse
# Use after running "Code_Data_Cleaning_Master_File"
# CHESS COVID19 CaseReport data released on 26-05-2020
########################################################

##########################################################
# durationiculeavingicu to either Death or Discharged
##########################################################
sex1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(sex) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup() 

age1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(ageyear4) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()

ethnicity1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(ethnicity2) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()

irt1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(RaschComor1) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()

ecmo1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(respiratorysupportecmo) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()

HAI1 <-  CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(HAI) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()
  
icupolicychange1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(icupolicychange) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()

region1 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  group_by(nhser_name) %>% 
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu)) %>% ungroup()



write.table(sex1,file = "clipboard",row.names = F,col.names = F)
write.table(age1,file = "clipboard",row.names = F,col.names = F)
write.table(ethnicity1,file = "clipboard",row.names = F,col.names = F)
write.table(ecmo1,file = "clipboard",row.names = F,col.names = F)
write.table(irt1,file = "clipboard",row.names = F,col.names = F)
write.table(HAI1,file = "clipboard",row.names = F,col.names = F)
write.table(icupolicychange1,file = "clipboard",row.names = F,col.names = F)
write.table(region1,file = "clipboard",row.names = F,col.names = F)
write.table(total1,file = "clipboard",row.names = F,col.names = F)


##########################################################
# durationiculeavingicu for censored cases
##########################################################
sex2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(sex) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

age2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(ageyear4) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

ethnicity2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(ethnicity2) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

irt2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(RaschComor1) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

ecmo2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(respiratorysupportecmo) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

HAI2 <-  CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(HAI) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

icupolicychange2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(icupolicychange) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()

region2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  group_by(nhser_name) %>% 
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu)) %>% ungroup()



######################################################################################
# durationiculeavingicu for totals
######################################################################################
total1 <-  CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome=="Death"|finaloutcome=="Discharged"|finaloutcome=="Transfered")) %>%
  summarise(n1 = n(),
            mean1 = mean(durationiculeavingicu),
            sd1 = sd(durationiculeavingicu),
            median1 = median(durationiculeavingicu),
            min1 = min(durationiculeavingicu),
            max1 = max(durationiculeavingicu))

total2 <- CHESS_CaseReport_Data %>%
  filter(durationiculeavingicu>0 & (finaloutcome1=="Censored")) %>%
  summarise(n2 = n(),
            mean2 = mean(durationiculeavingicu),
            sd2 = sd(durationiculeavingicu),
            median2 = median(durationiculeavingicu),
            min2 = min(durationiculeavingicu),
            max2 = max(durationiculeavingicu))


write.table(sex1,file = "clipboard",row.names = F,col.names = F)
write.table(age1,file = "clipboard",row.names = F,col.names = F)
write.table(ethnicity1,file = "clipboard",row.names = F,col.names = F)
write.table(ecmo1,file = "clipboard",row.names = F,col.names = F)
write.table(irt2[,-1],file = "clipboard",row.names = F,col.names = F)
write.table(HAI1,file = "clipboard",row.names = F,col.names = F)
write.table(icupolicychange1,file = "clipboard",row.names = F,col.names = F)
write.table(region1,file = "clipboard",row.names = F,col.names = F)
write.table(total1,file = "clipboard",row.names = F,col.names = F)



