########################################################
# COVID-19 Modelling
# Script for plotting CHESS data
# Requires the packages tidyverse, ggplot, ggpubr
# Use after running "Code_Data_Cleaning_Master_File"
# CHESS COVID19 CaseReport data released on 26-05-2020
########################################################

#########################################################################
# LoS by final outcome, sex, and age. ECMO and HAI are also plotted
#########################################################################
ggplot(CHESS_CaseReport_Data%>% filter(durationiculeavingicu>0), aes(x = sex, y = durationiculeavingicu, fill = sex)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  geom_point(aes(shape=(respiratorysupportecmo==c("Yes"))), size = 3, color = c("darkred")) + 
  geom_point(aes(shape=(HAI==1)), shape = 2, size = 5, color = c("darkgoldenrod4")) +
  facet_wrap(.~finaloutcome1 + ageyear4, nrow = 1) + 
  scale_fill_brewer(palette=3) +
  theme_bw(base_size = 21.5) + theme(legend.position="none",
                                     axis.text.x = element_text(angle = 45, vjust = 1, size = 22, hjust = 1)) +
  ylab("LoS from ICU admission to leaving ICU") + xlab("Sex")
ggsave("LOS_descr_ASO.png", plot = last_plot(), device = "png", width = 45, height = 25, unit = "cm", limitsize = FALSE) 

################################################################################
# LoS by final outcome and ethnicity of patients. ECMO and HAI are also plotted
################################################################################
ggplot(CHESS_CaseReport_Data%>% filter(durationiculeavingicu>0 ), aes(x = ethnicity2, y = durationiculeavingicu, fill = ethnicity2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  geom_point(aes(shape=(respiratorysupportecmo==c("Yes"))), size = 3, color = c("darkred")) + 
  geom_point(aes(shape=(HAI==1)), shape = 2, size = 5, color = c("goldenrod4")) +
  facet_wrap(.~finaloutcome1, nrow =1) + 
  scale_fill_brewer(palette=6) +
  theme_bw(base_size = 30) + theme(legend.position="none",
                                     axis.text.x = element_text(angle = 45, vjust = 1, size = 25, hjust = 1)) +
  ylab("LoS from ICU admission to leaving ICU") + xlab("Ethnicity")
ggsave("LOS_descr_EthO.png", plot = last_plot(), device = "png", width = 45, height = 25, unit = "cm", limitsize = FALSE) 
