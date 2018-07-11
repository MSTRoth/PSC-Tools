# install.packages("ggrepel")
#install.packages("gridExtra")
library(gridExtra)
library(grid)
library(ggrepel)
library(tidyverse)
library(svglite)
options(scipen = 999)

setwd("C:/Users/Roth/Documents/Market Briefings/Data/Contract Obligations by Agency Charts")
bar_primeob_by_agency(company_name = "AINS - USAID, DOS data",
                      FY = 2018, 
                      n_agencies = 5,
                      scale = 1000000, 
                      scale_text = "Millions",
                      FY_range = "FY14-FY17",
                      h = 7,
                      w = 13)

###if some charts are too small

bar_primeob_by_agency_scaling(company_name = "Leidos", 
                                          FY = 2018,
                                          n_agencies = 2,
                                          scale = 1000000,
                                          scale_text = "Millions",
                                          FY_range = "FY14-FY17",
                                          top_range = c(1:2,4),
                                          bottom_range = c(3,5),
                                          grid_division = c(3,2),
                                          num_size = 3,
                                          h = 6,
                                          w = 11)

###choosing specific agencies or subsets

bar_primeob_by_agency_choosing(company_name = "AINS - USAID, DOS data", 
                                           funding_agency_type = "Funding Agency",
                                           funding_agency_name = "Agency for International Development (USAID)",
                                           FY = 2018,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           FY_range = "FY14-FY17",
                                           num_size = 3,
                                           h = 6,
                                           w = 11)

##several different specific agencies

bar_primeob_by_agency_scaling <- function(company_name, 
                                          FY = 1,
                                          n_agencies = 6,
                                          scale = 1000000000,
                                          scale_text = "Billions",
                                          FY_range,
                                          top_range,
                                          bottom_range,
                                          grid_division,
                                          num_size = 4,
                                          h = 6,
                                          w = 11){
  
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/", company_name,
                         " Company Profile.csv", sep = ""))
  ###Get top n agencyies by obligation
  top_n_agencies <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != FY) %>% 
    group_by(funding_agency) %>% 
    dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
    arrange(desc(grand_total_transaction_value)) %>% 
    top_n(n_agencies)
  
  top_n_agencies <- top_n_agencies$funding_agency
  top_agencies <- top_n_agencies[top_range]
  bottom_agencies <- top_n_agencies[bottom_range]
  
  ###Process Data to get total transaction value by year
  
  data.agency.year <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != FY) %>% 
    filter(funding_agency %in% top_n_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) 
  
  data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)
  data.agency.year$facet = factor(data.agency.year$funding_agency, levels = c(top_n_agencies))
  
  ###Get top n agencyies by obligation
  top_n_agencies <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != 2018) %>% 
    group_by(funding_agency) %>% 
    dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
    arrange(desc(grand_total_transaction_value)) %>% 
    top_n(6)
  
  ##Seperate Out differentt scales
  top_n_agencies <- top_n_agencies$funding_agency
  top_agencies <- top_n_agencies[top_range]
  bottom_agencies <- top_n_agencies[bottom_range]
  
  ###Process Data to get total transaction value by year - for top and bottom agencies
  
  data.agency.year.top <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != FY) %>% 
    filter(funding_agency %in% top_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) 
  
  data.agency.year.top$fiscal_year = as.character(data.agency.year.top$fiscal_year)
  data.agency.year.top$facet = factor(data.agency.year.top$funding_agency, levels = c(top_agencies))
  
  
  data.agency.year.bottom <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != FY) %>% 
    filter(funding_agency %in% bottom_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) 
  
  data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)
  data.agency.year.bottom$facet = factor(data.agency.year.bottom$funding_agency, levels = c(bottom_agencies))
  ###Create Barplot and Save as JPG
  plot1 <- ggplot(data.agency.year.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~facet, labeller = label_wrap_gen(20))+
    labs(y = paste("Contract Obligations (in) ", scale_text, sep = "")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  
  
  plot2 <- ggplot(data.agency.year.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~facet, labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())
  
  
  plot3<-grid.arrange(plot1, plot2, nrow = 1, widths = grid_division, 
                      top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = "")), gp = gpar(fontsize = 24), bottom = "Fiscal Year") 
  
  
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot3, 
         width = w, height = h, units = "in")
  
  
  plot
  
}


-------------------------
  
bar_funding_agency_services_by_category(funding_agency_name = "AINS - USAID, DOS data Company Profile",
                                        "Funding Agency",
                                        FY = 2018,
                                        scale = 1000000,
                                        scale_text = "Millions",
                                        "FY14-FY17",
                                        num_size = 3,
                                        h = 6,
                                        w = 11)


bar_funding_agency_services_by_category_scaling(funding_agency_name = "AINS - USAID, DOS data Company Profile",
                                                "Funding Agency",
                                                FY = 2018,
                                                scale = 1000000,
                                                scale_text = "Millions",
                                                "FY14-FY17",
                                                            top_categories = c(1,3,4,6,7),
                                                            bottom_categories = c(2,5,8,9),
                                                            grid_division = c(5,4),
                                                            num_size = 3,
                                                            h = 6,
                                                            w = 15)



bar_funding_agency_services_by_category_scaling2("AINS - USAID, DOS data Company Profile",
                                                             "Funding Agency",
                                                             FY = 2018,
                                                             scale = 1000000,
                                                             scale_text = "Millions",
                                                             "FY14-FY17",
                                                             one_categories = 1,
                                                             two_categories = 2,
                                                             grid_division = c(1,1),
                                                             num_size = 4,
                                                             h = 6,
                                                             w = 11)




bar_funding_agency_services_by_category_scaling(funding_agency_name = "HHS_CMS",
                                                "Funding Bureau",
                                                FY = 2018, 
                                                scale = 1000000,
                                                scale_text = "Millions",
                                                "FY14-FY17",
                                                one_categories = c(1,3,4,6,7),
                                                            two_categories = c(5,8),
                                                            three_categories = c(2),
                                                            grid_division = c(5,2,1),
                                                            num_size = 4,
                                                            h = 10,
                                                            w = 20)

data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/", "DHS_CBP",
                       ".csv", sep = ""))
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv") 

data.agency <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Bureau", "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Bureau", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group",
                PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(!is.na(portfolio_group))


agency1 <- sort(unique(data.agency$portfolio_group))[c(2,3,4,5)]
agency2 <- sort(unique(data.agency$portfolio_group))[bottom_categories]

agency.top <- data.agency %>% 
  filter(portfolio_group %in% agency1) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  


agency.bottom<- data.agency %>% 
  filter(portfolio_group %in% agency2) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  


agency.top$fiscal_year = as.character(agency.top$fiscal_year)
agency.bottom$fiscal_year = as.character(agency.bottom$fiscal_year)

plot1 <- ggplot(agency.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = paste("Contract Obligations (in) ", scale_text, sep = "")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")


plot2 <- ggplot(agency.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())


plot3<-grid.arrange(plot1, plot2, widths = grid_division, 
                    top = paste(funding_agency_name, " Services Contracts by Category FY14-FY17", sep = ""), bottom = "Fiscal Year") 



ggsave(paste(funding_agency_name, " Services Contracts by Category FY14-FY17-scaled.jpg", sep = ""), plot3, 
       width = w, height = h, units = "in")


plot3
