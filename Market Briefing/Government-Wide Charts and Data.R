### Quarter comparisons for Contract Obligations - Civilian/Defense Breakout####

library(readr)
data <- read_csv("~/Market Briefings/Data/Government-Wide data/Civilian and Defense Data by quarter.csv")

data<-rename(data, civ_def = "Civ/Def", total_obligations = "Contract Obligations (in Billions)")
data$`Contract Obligations (in Billions)`

data$Year = as.character(data$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def", 
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>% 
  filter(Year == 2014|Year ==2015| Year == 2016 | Year == 2017 | Year == 2018) %>% 
  group_by(Year, civ_def) %>% 
  mutate(label_y = cumsum(total_obligations))

data.civdef$Year = as.character(data.civdef$Year)



ggplot(data.civdef, aes(x = civ_def, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5)+
  scale_fill_manual(name = "none", values = c("Q4" = "rosybrown3", "Q3" = "burlywood1", "Q2" = "skyblue3", "Q1" = "turquoise4")) +
  facet_grid(~Year, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions") + #, 
  #      title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())


####################################################################################################

####Total contract spending - services categories + products####

## Government-Wide Total Contract Spending####

data_GW <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Government Wide.csv")


data_GW$`DPAP Category` <- factor(data_GW$`DPAP Category`, 
                               levels = c("Products", "Construction Services", 
                                          "Electronic & Communication Services",
                                          "Equipment Related Services",
                                          "Facility Related Services",
                                          "Knowledge Based Services",
                                          "Logistics Management Services",
                                          "Medical Services",
                                          "Research and Development",
                                          "Transportation Services"), 
                               ordered = is.ordered(data_GW$`DPAP Category`))

DPAP_GW <- data_GW %>% 
  group_by(`Fiscal Year`) %>% 
  arrange(desc(`DPAP Category`)) %>% 
  mutate(label_y = cumsum(`$ billions`))




ggplot(DPAP_GW, aes(x = `Fiscal Year`, y = `$ billions`, 
                     fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = round(`$ billions`, digits = 1), y = label_y), size = 3, vjust = 1.5)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
       title = "Government Wide Total Contract Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())




  

##DoD Total Contract Spending####
data_DoD <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - DoD.csv")

data_DoD$`DPAP Category` <- factor(data_DoD$`DPAP Category`, 
                                   levels = c("Products", "Construction Services", 
                                              "Electronic & Communication Services",
                                              "Equipment Related Services",
                                              "Facility Related Services",
                                              "Knowledge Based Services",
                                              "Logistics Management Services",
                                              "Medical Services",
                                              "Research and Development",
                                              "Transportation Services"), 
                               ordered = is.ordered(data_DoD$`DPAP Category`))

DPAP_DoD <- data_DoD %>% 
  group_by(`Fiscal Year`) %>% 
  arrange(desc(`DPAP Category`)) %>% 
  mutate(label_y = cumsum(`$ billions`))




ggplot(DPAP_DoD, aes(x = `Fiscal Year`, y = `$ billions`, 
                 fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = round(`$ billions`, digits = 1), y = label_y), size = 3, vjust = 1.5)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
      title = "DoD Total Contract Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())




## Civilian total contract spending####

data_civ <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Civilian.csv")


data_civ$`DPAP Category` <- factor(data_civ$`DPAP Category`, 
                                  levels = c("Products", "Construction Services", 
                                             "Electronic & Communication Services",
                                             "Equipment Related Services",
                                             "Facility Related Services",
                                             "Knowledge Based Services",
                                             "Logistics Management Services",
                                             "Medical Services",
                                             "Research and Development",
                                             "Transportation Services"), 
                                  ordered = is.ordered(data_civ$`DPAP Category`))

DPAP_civ <- data_civ %>% 
  group_by(`Fiscal Year`) %>% 
  arrange(desc(`DPAP Category`)) %>% 
  mutate(label_y = cumsum(`$ billions`))




ggplot(DPAP_civ, aes(x = `Fiscal Year`, y = `$ billions`, 
                    fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = round(`$ billions`, digits = 1), y = label_y), size = 3, vjust = 1.5)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
       title = "Civilian Total Contract Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())
