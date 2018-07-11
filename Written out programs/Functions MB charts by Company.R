bar_primeob_by_agency <- function(company_name, 
                                  FY = 1,
                                  n_agencies = 6,
                                  scale = 1000000000,
                                  scale_text = "Billions",
                                  FY_range,
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

 ###Create Barplot and Save as JPG
plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = paste("Contract Obligations (in) ", scale_text, sep = ""), 
       title = paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot, 
       width = w, height = h, units = "in")
 plot
 
}

bar_primeob_by_agency_scaling <- function(company_name, 
                                  FY = 1,
                                  n_agencies = 6,
                                  scale = 1000000,
                                  scale_text = "Millions",
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
  
    ##Seperate Out different scales
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
                      top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year") 
  
  
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot3, 
         width = w, height = h, units = "in")


  plot
  
}



###Choosing agencies or subsections

bar_primeob_by_agency_choosing <- function(company_name, 
                                  funding_agency_type,
                                  funding_agency_name,
                                  FY = 1,
                                  scale = 1000000,
                                  scale_text = "Millions",
                                  FY_range,
                                  num_size = 3,
                                  h = 6,
                                  w = 11){
  
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/", company_name,
                         " Company Profile.csv", sep = ""))
  
  
  ###Process Data to get total transaction value by year
  
  data.agency.year <- data %>% 
    select("Fiscal Year", funding_agency_type, "Transaction Value") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = funding_agency_type, 
                  transaction_value = "Transaction Value") %>% 
    filter(fiscal_year != FY) %>% 
    filter(funding_agency == funding_agency_name) %>%
    dplyr::group_by(funding_agency, fiscal_year) %>% 
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/scale)) 
  
  data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)
  
  ###Create Barplot and Save as JPG
  plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    labs(x="Fiscal Year", y = paste("Contract Obligations (in) ", scale_text, sep = ""), 
         title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot, 
         width = w, height = h, units = "in")
  plot


}



--------------------------------------
##Converting PSC to DPAP category, chart

bar_funding_agency_services_by_category <- function(funding_agency_name,
                                                    funding_agency_subset,
                                            FY = 1,
                                            scale = 1000000,
                                            scale_text = "Millions",
                                            FY_range,
                                            num_size = 4,
                                            h = 6,
                                            w = 11){  
  
  
data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/", funding_agency_name,
                         ".csv", sep = ""))
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv") 

data.agency <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", funding_agency_subset, "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = funding_agency_subset, 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group",
                PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
  filter(fiscal_year != FY) %>% 
  filter(!is.na(portfolio_group))%>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale) 


data.agency$fiscal_year = as.character(data.agency$fiscal_year)

plot<-ggplot(data.agency, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size) +  
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20)) +
  labs(x="Fiscal Year", y = paste("Contract Obligations (in) ", scale_text, sep = ""),
       title = paste(funding_agency_name, " Services Contracts by Category ", FY_range, sep = "")) +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())



ggsave(paste(funding_agency_name, " Services Contracts by Category ", FY_range, ".jpg", sep = ""), plot, 
       width = w, height = h, units = "in")

plot

}



##Converting PSC to DPAP category, chart


bar_funding_agency_services_by_category_scaling2 <- function(funding_agency_name,
                                                             funding_agency_subset,
                                                             FY = 1,
                                                             scale = 1000000,
                                                             scale_text = "Millions",
                                                             FY_range,
                                                             one_categories,
                                                             two_categories,
                                                             grid_division,
                                                             num_size = 4,
                                                             h = 6,
                                                             w = 11){  
  
  
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/", funding_agency_name,
                         ".csv", sep = ""))
  PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv") 
  
  data.agency <- data %>% 
    left_join(PSC_portfolio, 
              by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
    select("Fiscal Year", funding_agency_subset, "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = funding_agency_subset, 
                  transaction_value = "Transaction Value",
                  portfolio_group = "Portfolio Group",
                  PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
    filter(fiscal_year != FY) %>% 
    filter(!is.na(portfolio_group))
  
  
  agency1 <- sort(unique(data.agency$portfolio_group))[one_categories]
  agency2 <- sort(unique(data.agency$portfolio_group))[two_categories]
  
  
  agency.one <- data.agency %>% 
    filter(portfolio_group %in% agency1) %>%
    dplyr::group_by(fiscal_year, portfolio_group) %>% 
    dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  
  
  
  agency.two<- data.agency %>% 
    filter(portfolio_group %in% agency2) %>%
    dplyr::group_by(fiscal_year, portfolio_group) %>% 
    dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  
  
  
  
  agency.one$fiscal_year = as.character(agency.one$fiscal_year)
  agency.two$fiscal_year = as.character(agency.two$fiscal_year)

  
  plot1 <- ggplot(agency.one, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
    labs(y = paste("Contract Obligations (in) ", scale_text, sep = "")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  
  
  plot2 <- ggplot(agency.two, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())
  
  plot_arrange<-grid.arrange(plot1, plot2, widths = grid_division, 
                             top = paste(funding_agency_name, " Services Contracts by Category FY14-FY17", sep = ""), bottom = "Fiscal Year") 
  
  
  
  ggsave(paste(funding_agency_name, "Services Contracts by Category FY14-FY17_scaled.jpg", sep = ""), plot_arrange, 
         width = w, height = h, units = "in")
  
  
  plot_arrange
  
}




bar_funding_agency_services_by_category_scaling3 <- function(funding_agency_name,
                                                    funding_agency_subset,
                                                    FY = 1,
                                                    scale = 1000000,
                                                    scale_text = "Millions",
                                                    FY_range,
                                                    one_categories,
                                                    two_categories,
                                                    three_categories,
                                                    grid_division,
                                                    num_size = 4,
                                                    h = 6,
                                                    w = 11){  
  
  
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/", funding_agency_name,
                         ".csv", sep = ""))
  PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv") 
  
  data.agency <- data %>% 
    left_join(PSC_portfolio, 
              by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
    select("Fiscal Year", funding_agency_subset, "Transaction Value", "Portfolio Group", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
    dplyr::rename(fiscal_year = "Fiscal Year", 
                  funding_agency = funding_agency_subset, 
                  transaction_value = "Transaction Value",
                  portfolio_group = "Portfolio Group",
                  PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
    filter(fiscal_year != FY) %>% 
    filter(!is.na(portfolio_group))

  
agency1 <- sort(unique(data.agency$portfolio_group))[one_categories]
agency2 <- sort(unique(data.agency$portfolio_group))[two_categories]
agency3 <- sort(unique(data.agency$portfolio_group))[three_categories]

  
agency.one <- data.agency %>% 
  filter(portfolio_group %in% agency1) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  


agency.two<- data.agency %>% 
  filter(portfolio_group %in% agency2) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  

agency.three<- data.agency %>% 
  filter(portfolio_group %in% agency3) %>%
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/scale)  



agency.one$fiscal_year = as.character(agency.one$fiscal_year)
agency.two$fiscal_year = as.character(agency.two$fiscal_year)
agency.three$fiscal_year = as.character(agency.three$fiscal_year)

  
plot1 <- ggplot(agency.one, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = paste("Contract Obligations (in) ", scale_text, sep = "")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")


plot2 <- ggplot(agency.two, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())+ guides(fill="none")


plot3 <- ggplot(agency.three, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())

plot_arrange<-grid.arrange(plot1, plot2, plot3, widths = grid_division, 
                    top = paste(funding_agency_name, " Services Contracts by Category FY14-FY17", sep = ""), bottom = "Fiscal Year") 
         
         
         
ggsave(paste(funding_agency_name, "Services Contracts by Category FY14-FY17_scaled.jpg", sep = ""), plot_arrange, 
                width = w, height = h, units = "in")
         

plot_arrange
 
}







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
    labs(y = ("Contract Obligations (in) Millions")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  
  
  plot2 <- ggplot(data.agency.year.bottom, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
    facet_grid(~facet, labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), axis.title.y = element_blank())
  
  
  plot3<-grid.arrange(plot1, plot2, nrow = 1, widths = c(grid_division), 
                      top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = "")), gp = gpar(fontsize = 24), bottom = "Fiscal Year") 
  
  
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot3, 
         width = w, height = h, units = "in")
  
  
  plot
  
}

  
  
  
  
  
  



























































  

# bar_serv_DPAP_by_agency<- function(company_name, 
#                                   FY = 1,
#                                   n_agencies = 6,
#                                   scale = 1000000000,
#                                   scale_text = "Billions",
#                                   FY_range,
#                                   h = 6,
#                                   w = 11,
#                                   chart_no){
#   data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/", company_name,
#                          " Company Profile.csv", sep = ""))
#   ###Get top n agencyies by obligation
#   top_n_agencies <- data %>% 
#     select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#     dplyr::rename(fiscal_year = "Fiscal Year", 
#                   funding_agency = "Funding Agency", 
#                   transaction_value = "Transaction Value") %>% 
#     filter(fiscal_year != FY) %>% 
#     group_by(funding_agency) %>% 
#     dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
#     arrange(desc(grand_total_transaction_value)) %>% 
#     top_n(n_agencies)
#   
#   top_n_agencies <- top_n_agencies$funding_agency
# 
# PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")
# 
# data.agency.year.pf <- data %>% 
#   left_join(PSC_portfolio, 
#             by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value",
#                 portfolio_group = "Portfolio Group") %>% 
#   #filter(fiscal_year != FY) %>% 
#   #filter(funding_agency == top_n_agencies[chart_no]) %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
#   dplyr::group_by(fiscal_year, portfolio_group) %>% 
#   dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000)
# 
# data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs
# 
# data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)
# 
# 
# ### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
# ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, 
#                                 fill = fiscal_year))+
#   geom_bar(stat = "identity") +
#   scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", 
#                                          "2016" = "grey70", "2017" = "orange")) +
#   facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
#   labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
#        title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")
# }
# 
# years[1]
# years[5]
