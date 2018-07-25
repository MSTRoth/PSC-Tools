data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/FCI Enterprises Company Profile.csv", sep = ""))

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
  





  
  ###Process Data to get total transaction value by year - for top and bottom agencies
  
  data.agency.year.top <- data %>% 
    select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Agency", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != 2018) %>% 
    filter(funding_agency == "Department of Defense (DOD)") %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000)) 
  
  data.agency.year.top$fiscal_year = as.character(data.agency.year.top$fiscal_year)

  
  
  data.agency.year.bottom <- data %>% 
    select("Fiscal Year", "Funding Office Level 3", "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = "Funding Office Level 3", 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != 2018) %>% 
    filter(funding_agency == "Defense Information Systems Agency (DISA)") %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000)) 
  
  data.agency.year.bottom$fiscal_year = as.character(data.agency.year.bottom$fiscal_year)

  ###Create Barplot and Save as JPG
  plot1 <- ggplot(data.agency.year.top, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    labs(y = ("Contract Obligations (in) Millions")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  
  
  plot2 <- ggplot(data.agency.year.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())
  
  
  plot3<-grid.arrange(plot1, plot2, nrow = 1, widths = c(1,1), bottom = "Fiscal Year") 
  
  plot3<-grid.arrange(plot1, plot2, nrow = 1, widths = c(grid_division), 
                      top = textGrob("Contract Obligations by Agency "), gp = gpar(fontsize = 24), bottom = "Fiscal Year") 
  
  
  ggsave("FCI Enterprises Contract Obligations by Agency - DoD, DISA.jpg", plot3, width = 11, height = 6, units = "in")
  