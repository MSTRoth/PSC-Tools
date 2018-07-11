##Functions for graphics 

##processed data function?
process.data.get.sum <- function(funding_agency_name, funding_agency_type, FY, scale){
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
    
    data.agency.year
  }
  
plot.one <- function (data.agency.year, num_size, scale_text, company_name, FY_range){
  plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    labs(x="Fiscal Year", y = paste("Contract Obligations (in) ", scale_text, sep = ""), 
         title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  plot
}

plot.first <- function (data.agency.year, num_size, scale_text){
  plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    labs(y = paste("Contract Obligations (in) ", scale_text, sep = "")) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")
  
  plot
}

plot.last <- function (data.agency.year, num_size){
  plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
    scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
    facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(), 
          axis.title.y = element_blank())
  
  plot
}

plot.middle <- function (data.agency.year, num_size){  
  plot <- ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = num_size)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
  facet_grid(~funding_agency, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(),
        axis.title.y = element_blank())+ guides(fill="none")
  
  plot
  
}

bar_primeob_by_agency_choosing <- function(company_name, 
                                           funding_agency_type1,
                                           funding_agency_name1,
                                           funding_agency_type2 = NULL,
                                           funding_agency_name2 = NULL,
                                           funding_agency_type3 = NULL,
                                           funding_agency_name3 = NULL,
                                           # funding_agency_type4 = NULL,
                                           # funding_agency_name4 = NULL,
                                           # funding_agency_type5 = NULL,
                                           # funding_agency_name5 = NULL,
                                           FY = 1,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           grid_division,
                                           FY_range,
                                           num_size = 3,
                                           h = 6,
                                           w = 11){
  
  data <- read_csv(paste("C:/Users/Roth/Documents/Market Briefings/Data/", company_name,
                         " Company Profile.csv", sep = ""))
  
  
  
    
  ###Process Data to get total transaction value by year
if(is.null(funding_agency_name2)){
    data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
    ###Create Barplot and Save as JPG
    plot.all <- plot.one(data.agency.year1, num_size, scale_text, funding_agency_name1, FY_range)
    
    plot.all
    
  }  
  else{
    if(is.null(funding_agency_name3)){
      data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
      
      data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
      
      ###Create Barplot and Save as JPG
      plot1 <- plot.first(data.agency.year1, num_size, scale_text)
      
      plot2 <- plot.last(data.agency.year2, num_size)
      
      plot.all<-grid.arrange(plot1, plot2, nrow = 1, widths = grid_division, 
                             top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year") 
    }
    else{
      data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
      
      data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
      
      data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
      
      ###Create Barplot and Save as JPG
      plot1 <- plot.first(data.agency.year1, num_size, scale_text)
      
      plot2 <- plot.middle(data.agency.year2, num_size)
      
      plot3 <- plot.last(data.agency.year3, num_size)
      
      plot.all<-grid.arrange(plot1, plot2, plot3, nrow = 1, widths = grid_division, 
                             top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""), gp = gpar(fontsize = 24)), bottom = "Fiscal Year") 
    }

    }
  
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot.all, 
         width = w, height = h, units = "in")
  plot.all
  
}

  
bar_primeob_by_agency_choosing(company_name = "Leidos", 
                                           funding_agency_type1 = "Funding Bureau",
                                           funding_agency_name1 = "Customs and Border Protection (CBP)",
                                           funding_agency_type2 = "Funding Agency",
                                           funding_agency_name2 = "Department of Defense (DOD)",
                                           funding_agency_type3 = "Funding Agency",
                                           funding_agency_name3 = "Department of Homeland Security (DHS)",
                                           # funding_agency_type4 = NULL,
                                           # funding_agency_name4 = NULL,
                                           # funding_agency_type5 = NULL,
                                           # funding_agency_name5 = NULL,
                                           # FY = 2018,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                          grid_division = c(1,1,1),
                                           FY_range = "FY14-FY17",
                                           num_size = 3,
                                           h = 6,
                                           w = 11)


data.agency.year1 <- process.data.get.sum("Customs and Border Protection (CBP)", "Funding Bureau", 2018, 1000000)
plot.all <- plot.one(data.agency.year1, 3, "Millions", "Leidos", "FY14-FY17")
