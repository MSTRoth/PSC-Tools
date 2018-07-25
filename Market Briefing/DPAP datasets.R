construction_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Construction Services 7-19-18 FY14-17.csv")
electronic_and_communication_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Technology Services 7-19-18 FY14-17.csv")
equipment_related_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Equipment Related Services 7-19-18 FY14-17.csv")
knowledge_based_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Knowledge Based Services 7-19-18 FY14-17.csv")
Logistics_management_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Logistics Management Services 7-19-18 FY14-17.csv")
medical_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Medical Services 7-19-18 FY14-17.csv")
Total <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Total 7-19-18 FY14-17.csv")
transportation_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Transportation Services 7-19-18 FY14-17.csv")
facility_related_services <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Source Data/Visible Facility Related Services 7-19-18 FY14-17.csv")


construction_services_data <- construction_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Construction Services") 

electronic_and_communication_services_data <- electronic_and_communication_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Electronic & Communication Services")

equipment_related_services_data <- equipment_related_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Equipment Related Services")

knowledge_based_services_data <- knowledge_based_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Knowledge Based Services")

Logistics_management_services_data <- Logistics_management_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Logistics Management Services")

medical_services_data <- medical_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Medical Services")

total_data <- Total %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Total")

transportation_services_data <- transportation_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Transportation Services")

facility_related_services_data <- facility_related_services %>% 
  select(-"FY 2009 - 2018 Total", -"X2") %>% 
  gather("FY","amount", "2014":"2018") %>% 
  mutate(DPAP_category = "Facility Related Services")


DPAP_Categories <- rbind(construction_services_data, electronic_and_communication_services_data, 
                         equipment_related_services_data, facility_related_services_data, 
                         knowledge_based_services_data, Logistics_management_services_data, medical_services_data,
                         transportation_services_data, total_data)
DPAP_Categories <- DPAP_Categories %>% 
  filter(`Funding Agency` != "Total")
DPAP_Categories <- DPAP_Categories %>% 
  filter(funding_agency != "Total")


DPAP_Categories$amount = as.numeric(gsub("[\\$,]", "", DPAP_Categories$amount))

write.csv(DPAP_Categories, "C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Visible all Services 7-19-18 FY14-18.csv")  

DPAP_Categories <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Visible all Services 7-19-18 FY14-18.csv")

data.agency.year.pf <- DPAP_Categories %>%
  # filter(Service == "Air Force") %>% 
  dplyr::rename(fiscal_year = "FY", 
                funding_agency = "Funding Agency", 
                transaction_value = "amount",
                portfolio_group = "DPAP_category") %>% 
  filter(fiscal_year != 2018) %>% 
 # filter(funding_agency == "DOD - Department of Defense") %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  #filter(funding_agency == "GSA - General Services Administration") %>% 
  #filter(funding_agency == "Department of Health and Human Services (HHS)") %>% 
  #filter(funding_agency == "Department of Transportation (DOT)") %>% 
  #filter(funding_agency == "Department of Veterans Affairs (VA)") %>% 
  #filter(funding_agency == "DHS - Department of Homeland Security") %>% 
  filter(funding_agency == "DOJ - Department of Justice") %>% 
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000) %>% 
  # filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|fiscal_year == "FY17") %>% 
  filter(portfolio_group != "Total")

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year,
                                x = fiscal_year,
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "FY18" = "olivedrab3")) +
  labs(y = "Contract Obligations (in Millions)")+
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

data.agency.year.pf1 <- data.agency.year.pf %>% 
  #filter(fiscal_year != 2018) %>% 
  filter(portfolio_group ==  "Electronic & Communication Services"| portfolio_group == "Facility Related Services"|
           portfolio_group ==  "Knowledge Based Services"| portfolio_group == "Medical Services")

data.agency.year.pf2 <- data.agency.year.pf %>% 
  #filter(fiscal_year != 2018) %>% 
  filter(portfolio_group == "Construction Services" |portfolio_group == "Equipment Related Services" | 
           portfolio_group ==  "Logistics Management Services" | 
           portfolio_group == "Transportation Services")




plot.first <- ggplot(data.agency.year.pf1, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Millions") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  


  
  plot.last <- ggplot(data.agency.year.pf2, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
      scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
      facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), 
            axis.title.y = element_blank())

  
  plot<-grid.arrange(plot.first, plot.last, nrow = 1, widths = c(4,4)) 
  


