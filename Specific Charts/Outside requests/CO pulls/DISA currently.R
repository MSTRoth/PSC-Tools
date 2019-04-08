data <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/DoD_DISA.csv") 
# data.disa <- data %>%
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>% 
#   select(`Product Service Code (PSC) / Federal Supply Code (FSC)`, 
#          fiscal_year, funding_agency, transaction_value, )
#   # dplyr::group_by(funding_agency, fiscal_year) %>% 
#   # dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) %>% 
#   # mutate(agency = "all")
#   #filter(`Funding Office Level 3` == "Defense Information Systems Agency (DISA)") %>% 
#   #filter(`Fiscal Year`!= 2018)
# 
# data.disa$`Funding Office Level 3`[data.disa$`Funding Office Level 3`!= "Defense Information Systems Agency (DISA)"] = "Other"
#   
# NAICS.edit$NAICS2[NAICS.edit$NAICS2 %in% c(31,32,33)] = "31-33"  
  
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")

data.disa <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group",
                PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(!is.na(portfolio_group))

agency1 <- unique(data.disa$portfolio_group)[c(2,3,4)]
agency2 <- unique(data.disa$portfolio_group)[c(1,5,6,7,8)]



  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000) 


data.disa$fiscal_year = as.character(data.disa$fiscal_year)

plot<-ggplot(data.disa, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+  
    facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
    labs(x="Fiscal Year", y = "Contract Obligations (in) Millions",
         title = "DISA Services Contracts by Category FY14-FY17")+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())



ggsave("DISA Services Contracts by Category FY14-FY17-1.jpg", plot, 
       width = 15, height = 6, units = "in")







PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")

data.disa <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group",
                PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(!is.na(portfolio_group))

disa1 <- unique(data.disa$portfolio_group[data.disa$portfolio_group == "Electronic & Communication Services"|data.disa$portfolio_group == "Knowledge Based Services"])
disa2 <- unique(data.disa$portfolio_group[data.disa$portfolio_group != "Electronic & Communication Services" & data.disa$portfolio_group != "Knowledge Based Services"])
  
disa.top<- data.disa %>% 
  filter(portfolio_group %in% disa1) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000)  


disa.bottom<- data.disa %>% 
  filter(portfolio_group %in% disa2) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000)  


disa.top$fiscal_year = as.character(disa.top$fiscal_year)
disa.bottom$fiscal_year = as.character(disa.bottom$fiscal_year)



plot1 <- ggplot(disa.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = ("Contract Obligations (in) Millions")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")


plot2 <- ggplot(disa.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())


plot3<-grid.arrange(plot1, plot2, widths = c(2, 6), 
                    top = "DISA Services Contracats by Category FY14-FY17", bottom = "Fiscal Year") 



data <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/DoD Services, by DPAP.csv")

army.data <- data %>% 
  filter(Year == "FY14" | Year == "FY15"|Year == "FY16"|Year == "FY17") %>% 
  filter(Service == "Army") %>% 
  filter(Type!= "Products") %>% 
  rename(total = `Total (in millions)`) %>% 
  mutate(total_bil = total/1000)

army.data$Year[army.data$Year == "FY14"] = 2014
army.data$Year[army.data$Year == "FY15"] = 2015
army.data$Year[army.data$Year == "FY16"] = 2016
army.data$Year[army.data$Year == "FY17"] = 2017


plot <- ggplot(army.data, aes(x = Year, y = total_bil, fill = Year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_bil, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~Type, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
       title = "Army Services Contracts by Category FY14-FY17")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())


ggsave("Army Services Contracts by Category FY14-FY17.jpg", plot, 
       width = 11, height = 6, units = "in")

  
#   filter(funding_agency == top_n_agencies[chart_no]) %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
#   
# select(`Product Service Code (PSC) / Federal Supply Code (FSC)`, 
#        fiscal_year, funding_agency, transaction_value, 
#   
#   
#   
#   
# data.agency.year <- data %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>% 
#   filter(fiscal_year != FY) %>% 
#   filter(funding_agency %in% top_n_agencies) %>%
#   dplyr::group_by(funding_agency, fiscal_year) %>% 
#   dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) 
# 
#   
# 
# ggplot(data.disa, aes(x = fiscal_year, y = transaction_value, fill = fiscal_year)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
#   facet_grid(~facet, labeller = label_wrap_gen(20))+
#   labs(x="Fiscal Year", y = "Contract Obligations (in)", 
#        title = "Contract Obligations by Agency ")+
#   theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
#         axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot, 
#        width = w, height = h, units = "in")
