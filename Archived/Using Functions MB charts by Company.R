# install.packages("ggrepel")
#install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggpubr")
install.packages("lemon")
library(lemon)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(ggrepel)
library(tidyverse)
library(svglite)
library(PSCmb)
library(RColorBrewer)
options(scipen = 999)


plot.one(data.agency.year, "facet", 4, scale_text = "Millions", "KBRwyle", "Millions)")

setwd("X:/1 Marielle Folder/Data Sets/By Agency")

bar_primeob_by_agency("Prospecta",
                                  FY = 2018,
                                  n_agencies = 6,
                                  scale = 1000000,
                                  scale_text = "Millions",
                                  FY_range = "FY2014 - FY2017",
                                  num_size = 3,
                                  h = 6,
                                  w = 11)




setwd("C:/Users/Roth/Documents/Market Briefings/Data/Contract Obligations by Agency Charts")
setwd("C:/Users/Roth/Documents/Market Briefings/Data/Contract Obligations by Agency Charts")
bar_primeob_by_agency(company_name = "Prospecta",
                      FY = 2018,
                      n_agencies = 6,
                      scale = 1000000,
                      scale_text = "Millions",
                      FY_range = "FY14-FY17",
                      h = 6,
                      w = 13)






###if some charts are too small####

bar_primeob_by_agency_scaling(company_name = "HII",
                                          FY = 2018,
                                          n_agencies = 4,
                                          scale = 1000000,
                                          scale_text = "Millions",
                                          FY_range = "FY14-FY17",
                                          top_range = c(1),
                                          bottom_range = c(2:4),
                                          grid_division = c(1,3),
                                          num_size = 3,
                                          h = 6,
                                          w = 11)

###choosing specific agencies or subsets####

bar_primeob_by_agency_choosing(company_name = "Halfaker and Associates",
                                           funding_agency_type = "Funding Agency",
                                           funding_agency_name = "Agency for International Development (USAID)",
                                           FY = 2018,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           FY_range = "FY14-FY17",
                                           num_size = 3,
                                           h = 6,
                                           w = 11)


bar_primeob_by_agency_choosing(company_name = "Halfaker and Associates",
                                           funding_agency_type1 = "Funding Agency",
                                           funding_agency_name1 = "Department of Defense (DOD)",
                                           funding_agency_type2 = "Funding Agency",
                                           funding_agency_name2 = "Department of Veterans Affairs (VA)",
                                           funding_agency_type3 = "Funding Bureau",
                                           funding_agency_name3 = "Centers for Medicare and Medicaid Services (CMS)",
                                           funding_agency_type4 = "Funding Office Level 3",
                                           funding_agency_name4 = "Defense Health Agency (DHA)",
                                           funding_agency_type5 = NULL,
                                           funding_agency_name5 = NULL,
                                           FY = 2018,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           grid_division = NULL,
                                           FY_range = "FY14-FY17",
                                           num_size = 3,
                                           h = 6,
                                           w = 11)
plot.all
leg <- get_legend(plot)
bar_primeob_by_agency_scaling("HII",
                                          FY = 2018,
                                          n_agencies = 4,
                                          scale = 1000000,
                                          scale_text = "Millions",
                                          FY_range = "FY14-FY17",
                                          top_range = c(1),
                                          bottom_range = c(3,4),
                                          optional_third_range = c(2),
                                          grid_division = c(1,1,2),
                                          num_size = 3,
                                          h = 6,
                                          w = 13)

##several different specific agencies####

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
  Desktop
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/", "KBR",
                         " Company Profile.csv", sep = ""))
  
  data <- read_csv(paste("C:/Users/Roth/Desktop/", "KBR",
                         " Company Profile.csv", sep = ""))
  
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
    top_n(2)
  
top_n_agencies <- top_n_agencies$funding_agency
  
 
  top_agencies <- top_n_agencies[top_range]
  bottom_agencies <- top_n_agencies[bottom_range]

  ###Process Data to get total transaction value by year

  data.agency.year <- data %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != 2018) %>%
    filter(funding_agency %in% top_n_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))
  
  
  all_else <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value") %>%
    filter(fiscal_year != 2018) %>%
    filter(!funding_agency %in% top_n_agencies) %>%
    dplyr::group_by(fiscal_year) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))
  
  all_else$funding_agency <- "Other"
  
  all_else <- all_else[,c(3,1,2)]
  

  
data.agency.year.wo <- bind_rows(data.agency.year, all_else)

  data.agency.year.wo$fiscal_year = as.character(data.agency.year.wo$fiscal_year)
  data.agency.year.wo$facet = factor(data.agency.year.wo$funding_agency, levels = c(top_n_agencies, "Other"))

  
  
  plot.one(data.agency.year.wo, "facet", 4, scale_text = "Millions", "KBRwyle", "(FY14 - FY17)")
  
  
  
  
  
  
  
  
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
    filter(fiscal_year != 2018) %>%
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
    filter(fiscal_year != FY) %>%
    filter(funding_agency %in% bottom_agencies) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale))

  data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)
  data.agency.year.bottom$facet = factor(data.agency.year.bottom$funding_agency, levels = c(bottom_agencies))
  ###Create Barplot and Save as JPG
  plot1 <- ggplot(data.agency.year.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 4)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~facet, labeller = label_wrap_gen(20))+
    labs(y = paste("Contract Obligations (in ", "millions)", sep = "")) +
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



bar_funding_agency_services_by_category(funding_agency_name = "DOE",
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

data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/",
                       "USPS",
                       ".csv", sep = ""))

PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")
USPS<- data %>%
  filter(`Funding Agency`=="Postal Service (USPS)")

AO<- data %>%
  filter(`Funding Agency` == "Judicial Branch (JUDICIAL)")


data.agency.usps <- USPS %>%
  left_join(PSC_portfolio,
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>%
  select("Fiscal Year", "Funding Bureau", "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>%
  dplyr::rename(fiscal_year = "Fiscal Year",
                funding_agency = "Funding Bureau",
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group",
                PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>%
  filter(fiscal_year != 2018) %>%
  mutate(ifelse(is.na(portfolio_group), "products", "services")) %>%
  #filter(!is.na(portfolio_group)) %>%
  dplyr::group_by(portfolio_group, fiscal_year) %>%
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000))


data.agency.usps$fiscal_year <- as.character(data.agency.usps$fiscal_year)

ggplot(data.agency.usps, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  #scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = paste("Contract Obligations (in) ", "Thousands", sep = "")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())



data.agency.ao <- AO %>%
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
  # dplyr::group_by(portfolio_group, fiscal_year) %>%
  # dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))

data.agency.ao$fiscal_year <- as.character(data.agency.ao$fiscal_year)

ggplot(data.agency.ao, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = paste("Contract Obligations (in) ", "Millions", sep = "")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())


agency1 <- sort(unique(data.agency.usps$portfolio_group))[c(2)]
agency2 <- sort(unique(data.agency.usps$portfolio_group))[c(1,3,4,5)]

agency.top <- data.agency.usps %>%
  filter(portfolio_group %in% agency1) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>%
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000)


agency.bottom<- data.agency.usps %>%
  filter(portfolio_group %in% agency2) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>%
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000)


agency.top$fiscal_year = as.character(agency.top$fiscal_year)
agency.bottom$fiscal_year = as.character(agency.bottom$fiscal_year)

plot1 <- ggplot(agency.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  #scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = paste("Contract Obligations (in) ", "Millions", sep = "")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")


plot2 <- ggplot(agency.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 2), vjust = 1.5), size = 3)+
  #scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())


plot3<-grid.arrange(plot1, plot2, widths = c(2,4),
                    top = paste("Administrative Office of the US Courts", " Services Contracts by Category FY14-FY17", sep = ""), bottom = "Fiscal Year")



ggsave(paste(funding_agency_name, " Services Contracts by Category FY14-FY17-scaled.jpg", sep = ""), plot3,
       width = w, height = h, units = "in")


plot3

########################################################################

##Contracts by services/products####
install.packages(PSCmb)
library(PSCmb)
PSCmb::total_contract_spending("Civilian",
                                    num_size = 4,
                                    FY_range = "2009-2018",
                                    h = 6,
                                    w = 11,
                                    file_ext = ".jpg")



contract_obs_by_quarter <- function(year, num_size = 3, notyear_prop,
                                    FY_range, title = paste("Contract Obligations by Quarter: ", FY_range, sep = ""),
                                    subtitle = NULL, h = 6, w = 11, file_ext = ".jpg")
  
  #Location for saving charts
  setwd("S:/1 Marielle Folder/Visualizations/Government-Wide") #location charts are saved

  data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Civilian and Defense Data by quarter.csv")
  
  data$Year = as.character(data$Year)
  
  data.civdef_total <- data %>%
    rename(civ_def = "Civ/Def",
           total_obligations = "Contract Obligations (in Billions)") %>%
    group_by(Year, civ_def) %>%
    mutate(label_y = cumsum(total_obligations))
  
  data.civdef_total$Year = as.character(data.civdef_total$Year)
  
  data.civdef <- data %>%
    rename(civ_def = "Civ/Def",
           total_obligations = "Contract Obligations (in Billions)") %>%
    group_by(Year, civ_def) %>%
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations)) %>%
    filter(Year %in% c(2016, 2017, 2018)) %>% 
    mutate(FYYear = paste("FY", Year, sep = ""))
  
  
  plot <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(data.civdef, Year != 0), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
    facet_grid(~civ_def, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20), 
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
  
  
  ggsave("All Contract Obligations Civ-Def FY16-FY18 by quarter.jpg", plot,
         width = 13, height = 6.5, units = "in")
  
  
###readable chart
chart <- data %>% 
  spread('Civ/Def', `Contract Obligations (in Billions)`)

write.csv(chart, "C:/Users/Roth/Documents/Other Requests (Co-workers)/David/readable civ-def quarterly chart (as of 1-17-19).csv")
  
#--------------------------------------

  total_contract_spending <- function(type,
                                      num_size = 4,
                                      FY_range,
                                      h = 6,
                                      w = 11,
                                      file_ext = ".jpg")
    
    data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP (services and total) Data - Government Wide.csv")
    
    
  setwd("S:/1 Marielle Folder/Visualizations/Government-Wide")
    
    data$`DPAP Category` <- factor(data$`DPAP Category`,
                                   levels = c("Products", "Construction Services",
                                              "Electronic & Communication Services",
                                              "Equipment Related Services",
                                              "Facility Related Services",
                                              "Knowledge Based Services",
                                              "Logistics Management Services",
                                              "Medical Services",
                                              "Research and Development",
                                              "Transportation Services"),
                                   ordered = is.ordered(data$`DPAP Category`))
    
    DPAP_all <- data %>%
      filter(`DPAP Category` != "Total") %>% 
      group_by(`Fiscal Year`) %>%
      arrange(desc(`DPAP Category`)) %>%
      #mutate(label_y = cumsum(`$ billions`))
      mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
      group_by(`Fiscal Year`, pors) %>%
      mutate(`pors$` = sum(`$_billions_all`))
    
    
    label_height_all <- DPAP_all %>%
      group_by(`Fiscal Year`, pors) %>%
      summarize(`pors$` = sum(`$_billions_all`)) %>%
      group_by(`Fiscal Year`) %>%
      arrange(desc(`pors`)) %>%
      mutate(label_y2 = cumsum(`pors$`)) %>%
      left_join(DPAP_all, by = c("Fiscal Year", "pors", "pors$") )
    
    DPAP_dod <- data %>%
      filter(`DPAP Category` != "Total") %>% 
      group_by(`Fiscal Year`) %>%
      arrange(desc(`DPAP Category`)) %>%
      #mutate(label_y = cumsum(`$ billions`))
      mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
      group_by(`Fiscal Year`, pors) %>%
      mutate(`pors$` = sum(`$_billions_DoD`))
    
    
    label_height_dod <- DPAP_dod %>%
      group_by(`Fiscal Year`, pors) %>%
      summarize(`pors$` = sum(`$_billions_DoD`)) %>%
      group_by(`Fiscal Year`) %>%
      arrange(desc(`pors`)) %>%
      mutate(label_y2 = cumsum(`pors$`)) %>%
      left_join(DPAP_dod, by = c("Fiscal Year", "pors", "pors$") )
    
    DPAP_civ <- data %>%
      filter(`DPAP Category` != "Total") %>% 
      group_by(`Fiscal Year`) %>%
      arrange(desc(`DPAP Category`)) %>%
      #mutate(label_y = cumsum(`$ billions`))
      mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
      group_by(`Fiscal Year`, pors) %>%
      mutate(`pors$` = sum(`$_billions_Civilian`))
    
    
    label_height_civ <- DPAP_civ %>%
      group_by(`Fiscal Year`, pors) %>%
      summarize(`pors$` = sum(`$_billions_Civilian`)) %>%
      group_by(`Fiscal Year`) %>%
      arrange(desc(`pors`)) %>%
      mutate(label_y2 = cumsum(`pors$`)) %>%
      left_join(DPAP_civ, by = c("Fiscal Year", "pors", "pors$") )
    
    
    
    plot_all <- ggplot(label_height_all, aes(x = `Fiscal Year`, y = `$_billions_all`,
                                     fill = `DPAP Category`)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
      scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
      labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
           title = paste("Government-Wide", " Total Contract Spending", sep = ""))+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 
    
    plot_plot_dod <- ggplot(label_height_dod, aes(x = `Fiscal Year`, y = `$_billions_DoD`,
                                         fill = `DPAP Category`)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
      scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
      labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
           title = paste("DoD", " Total Contract Spending", sep = ""))+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())   
    
    plot_civ <- ggplot(label_height_civ, aes(x = `Fiscal Year`, y = `$_billions_Civilian`,
                                         fill = `DPAP Category`)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
      scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
      labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
           title = paste("Civilian", " Total Contract Spending", sep = ""))+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 
    
    
    ggsave(paste("Civilian", " Total Contract Spending Service Product ", "FY16-FY18.jpg", sep = ""), plot_civ,
           width = 13, height = 6.5, units = "in")
    
    ggsave(paste("Defense", " Total Contract Spending Service Product ", "FY16-FY18.jpg", sep = ""), plot_dod,
           width = 13, height = 6.5, units = "in")
    
    ggsave(paste("Government-Wide", " Total Contract Spending Service Product ", "FY16-FY18.jpg", sep = ""), plot_all,
           width = 13, height = 6.5, units = "in")
    

  
  
  
  
  
  