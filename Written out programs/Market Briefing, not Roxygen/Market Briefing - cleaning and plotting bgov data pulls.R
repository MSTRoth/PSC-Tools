####Market Briefings Graphics####


#install.packages("tidyverse")
#install.packages("svglite")
library(tidyverse)
library(svglite)
library(ggrepel)
library(grid)
library(gridExtra)
options(scipen = 999)

setwd("S:/1 Marielle Folder/Visualizations/Vendor Specific")

####read dataset into environment####
data <- read_csv("S:/1 Marielle Folder/Data Sets/Vendor Specific/FMP Company Profile.csv")

#colnames(data)

####Prime Contract Obligations By Agency and Year####
top_6_agencies <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2014) %>% 
  group_by(funding_agency) %>% 
  dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
  arrange(desc(grand_total_transaction_value)) %>% 
  top_n(6) 

top_n_agencies <- top_6_agencies$funding_agency

data.agency.year <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2014 & fiscal_year != 2019) %>% 
  filter(funding_agency %in% top_n_agencies) %>%
  dplyr::group_by(funding_agency, fiscal_year) %>% 
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000)) 

data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)
data.agency.year$facet = factor(data.agency.year$funding_agency, levels = c(top_n_agencies))



plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2018" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Thousands",
       title = "FMP Consulting Contract Obligation by Top Agencies",
       subtitle = "FY2015-FY2018") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())


plot


ggsave(paste("FMP", " Contract Obligations by Agency 15-18.jpg", sep = ""), plot, 
       width = 11, height = 5, units = "in")












###if two dif

top_n_agencies <- top_6_agencies$funding_agency
# top_agencies <- top_n_agencies[1]
# bottom_agencies <- top_n_agencies[c(2,3,6)]

data.agency.year.top <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2014) %>% 
  filter(funding_agency %in% top_agencies) %>%
  dplyr::group_by(funding_agency, fiscal_year) %>% 
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000)) 

data.agency.year.top$fiscal_year = as.character(data.agency.year.top$fiscal_year)
data.agency.year.top$facet = factor(data.agency.year.top$funding_agency, levels = c(top_agencies))

  

data.agency.year.bottom <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2014) %>% 
  filter(funding_agency %in% bottom_agencies) %>% 
  dplyr::group_by(fiscal_year, funding_agency) %>% 
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))

data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)
data.agency.year.bottom$facet = factor(data.agency.year.bottom$funding_agency, levels = c(bottom_agencies))

plot1 <- ggplot(data.agency.year.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2018" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  labs(y = ("Contract Obligations (in) Millions")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")


plot2 <- ggplot(data.agency.year.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 2), vjust = 1.5), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2018" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())

plot3<-grid.arrange(plot1, plot2, nrow = 1, widths = c(1,4),
                    top = textGrob(paste("Govplace ", "Contract Obligations by Agency ", "2015-2018", sep = ""), gp = gpar(fontsize = 24))) 

ggsave(paste("Govplace", " Contract Obligations by Agency 15-18.jpg", sep = ""), plot3, 
       width = 11, height = 5, units = "in")


























### Plot: By Agency, By Year####
# 
# ggplot(data.agency.year, aes(fill = fiscal_year, 
#                              x = funding_agency, 
#                              y = total_transaction_value))+
#   geom_bar(stat = "identity", position = position_dodge()) +
#   scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
#   labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "COMPANY NAME Contract Obligations by Agency FY14-FY17")

### Plot: By Agency, By Year, faceted ####
plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("Fiscal Year", values = c("2018" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in Millions)", 
       title = "Serco Contract Obligations by Agency FY15-FY18") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  #scale_x_continuous(limits = )
ggsave("Serco Contract Obligation by Agency top 5.jpg", plot, 
        width = 11, height = 5, units = "in")




####By Agency, Year, and services Portfolio group Category####
data <- 
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")

data.agency.year.pf <- data %>% 
 left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(funding_agency == "Department of Defense (DOD)") %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  #filter(funding_agency == "Department of Health and Human Services (HHS)") %>% 
  #filter(funding_agency == "Department of Transportation (DOT)") %>% 
  #filter(funding_agency == "Department of Veterans Affairs (VA)") %>% 
  #filter(funding_agency == "Department of Homeland Security (DHS)") %>% 
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000000)

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year, 
                             x = portfolio_group, 
                             y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
plot <- ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
       title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

ggsave(paste("Contract Obligations by Agency top.jpg"), plot, 
       width = 11, height = 6, units = "in")





# ####By Agency####
# data.agency <- data %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>%
#   filter(fiscal_year != 2018) %>% 
#   dplyr::group_by(funding_agency) %>% 
#   dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))
# 
# 
# 
# ####By Year####
# data.year <- data %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>% 
#   filter(fiscal_year != 2018) %>% 
#   filter(funding_agency = "Department of Defense (DOD)"|
#            funding_agency = "Department of Health and Human Services (HHS)"|
#            funding_agency = "Department of Transportation (DOT)"|
#            funding_agency = "Department of Veterans Affairs (VA)" |
#            funding_agency = "Department of Homeland Security (DHS)") %>% 
#   dplyr::group_by(fiscal_year) %>% 
#   dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))



####Year, and services Portfolio group Category####

PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")

data.agency.year.pf <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(funding_agency == "Department of Defense (DOD)") %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  #filter(funding_agency == "Department of Health and Human Services (HHS)") %>% 
  #filter(funding_agency == "Department of Transportation (DOT)") %>% 
  #filter(funding_agency == "Department of Veterans Affairs (VA)") %>% 
  #filter(funding_agency == "Department of Homeland Security (DHS)") %>% 
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000000)

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year, 
                                x = portfolio_group, 
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
plot <- ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
       title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

ggsave(paste("Contract Obligations by Agency top.jpg"), plot, 
       width = 11, height = 6, units = "in")




 