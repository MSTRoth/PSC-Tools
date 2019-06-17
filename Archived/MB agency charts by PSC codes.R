data <- read_csv(paste("X:/1 Marielle Folder/Data For R/Contract Obligation by Agency/Company Profiles/", "Halfaker and Associates",
                       " Company Profile.csv", sep = ""))

setwd("~/Reference Tables")
crosswalk <- read_csv("PSC RandD, S, P.csv")

dataH <- data %>% 
  select(`Fiscal Year`, `Transaction Value`, 
         `Funding Agency`, `Funding Bureau`, `Funding Office Level 3`,
         `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  rename(PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  filter(`Fiscal Year`>=2014 & `Fiscal Year`<=2017) %>% 
  left_join(crosswalk, by = c("PSC"="PSC CODE"))


  data.agency.year1 <- dataH %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value", "Product or Service") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value",
                  PSC = "Product or Service") %>%
    filter(fiscal_year != 2018) %>%
    filter(funding_agency == "Department of Defense (DOD)") %>%
    dplyr::group_by(funding_agency, fiscal_year, PSC) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))
  
  data.agency.year1$fiscal_year = as.character(data.agency.year1$fiscal_year)
  
  data.agency.year2 <- dataH %>%
    select("Fiscal Year", "Funding Agency", "Transaction Value", "Product or Service") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Agency",
                  transaction_value = "Transaction Value",
                  PSC = "Product or Service") %>%
    filter(fiscal_year != 2018) %>%
    filter(funding_agency == "Department of Veterans Affairs (VA)") %>%
    dplyr::group_by(funding_agency, fiscal_year, PSC) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))

  data.agency.year2$fiscal_year = as.character(data.agency.year2$fiscal_year)
  
  data.agency.year3 <- dataH %>%
    select("Fiscal Year", "Funding Bureau", "Transaction Value", "Product or Service") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Bureau",
                  transaction_value = "Transaction Value",
                  PSC = "Product or Service") %>%
    filter(fiscal_year != 2018) %>%
    filter(funding_agency == "Centers for Medicare and Medicaid Services (CMS)") %>%
    dplyr::group_by(funding_agency, fiscal_year, PSC) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))
  
  data.agency.year3$fiscal_year = as.character(data.agency.year3$fiscal_year)

  data.agency.year4 <- dataH %>%
    select("Fiscal Year", "Funding Office Level 3", "Transaction Value", "Product or Service") %>%
    dplyr::rename(fiscal_year = "Fiscal Year",
                  funding_agency = "Funding Office Level 3",
                  transaction_value = "Transaction Value",
                  PSC = "Product or Service") %>%
    filter(fiscal_year != 2018) %>%
    filter(funding_agency == "Defense Health Agency (DHA)") %>%
    dplyr::group_by(funding_agency, fiscal_year, PSC) %>%
    dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000))
  
  data.agency.year4$fiscal_year = as.character(data.agency.year4$fiscal_year)

  

###Create Barplot and Save as JPG
plot1 <- plot.first(data.agency.year1, "funding_agency", 3, "Millions")
plot2 <- plot.middle(data.agency.year2, "funding_agency", 3)
plot3 <- plot.middle(data.agency.year3, "funding_agency", 3)
plot4 <- plot.middle(data.agency.year4, "funding_agency", 3)


plot1<-ggplot(data.agency.year1, aes(x = fiscal_year, y = total_transaction_value, fill = PSC)) +
    geom_bar(position = "stack", stat = "identity") +
    #geom_text_repel(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
    scale_fill_manual("Products and Services", values = c("Products" = "seagreen1", 
                                                          "Services" = "goldenrod1", 
                                                          "R&D" = "firebrick1")) +
    #facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in Millions)") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())
  



ggsave("Contract Obligations by Agency.jpg", plot1,
       width = 10, height = 6, units = "in")










####add variabelf or choosing which chart to get legend from 
legend = gtable_filter(ggplot_gtable(ggplot_build(plot1)), "guide-box")

plot.all<-grid.arrange(plot1 + theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
                       plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), legend, ncol = 5,bottom = "Fiscal Year")
data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3, data.agency.year4)



bar_primeob_by_agency_choosing <- function(company_name,
                                           funding_agency_type1,
                                           funding_agency_name1,
                                           funding_agency_type2 = NULL,
                                           funding_agency_name2 = NULL,
                                           funding_agency_type3 = NULL,
                                           funding_agency_name3 = NULL,
                                           funding_agency_type4 = NULL,
                                           funding_agency_name4 = NULL,
                                           funding_agency_type5 = NULL,
                                           funding_agency_name5 = NULL,
                                           FY = 1,
                                           scale = 1000000,
                                           scale_text = "Millions",
                                           FY_range,
                                           num_size = 3,
                                           h = 6,
                                           w = 11){
  
  data <- read_csv(paste("X:/1 Marielle Folder/Data For R/Contract Obligation by Agency/Company Profiles/", company_name,
                         " Company Profile.csv", sep = ""))
  
  
  ###Process Data to get total transaction value by year
  if(is.null(funding_agency_name2)){
    data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
    ###Create Barplot and Save as JPG
    plot.all <- plot.one(data.agency.year1, "funding_agency", num_size, scale_text, funding_agency_name1, FY_range)
    
    data.agency.year.all <- data.agency.year1
    
  }
  else{
    if(is.null(funding_agency_name3)){
      data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
      data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
      
      ###Create Barplot and Save as JPG
      plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
      plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
      
      legend <- gtable_filter(ggplot_gtable(ggplot_build(plot1)), "guide-box")
      
      plot.all<-grid.arrange(plot1 + theme(legend.position='hidden'), 
                             plot2 + theme(legend.position='hidden'), 
                             legend, ncol = 3,
                             top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""), 
                                            gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
      data.agency.year.all <- rbind(data.agency.year1, data.agency.year2)
      
    }
    else{
      if(is.null(funding_agency_name4)){
        data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
        data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
        data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
        
        ###Create Barplot and Save as JPG
        plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
        plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
        plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
        
        legend <- gtable_filter(ggplot_gtable(ggplot_build(plot1)), "guide-box")
        
        plot.all<-grid.arrange(plot1 + theme(legend.position='hidden'), 
                               plot2 + theme(legend.position='hidden'), 
                               plot3 + theme(legend.position='hidden'), 
                               legend, ncol = 4,
                               top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                              gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
        data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3)
        
      }
      else{
        if(is.null(funding_agency_name5)){
          data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
          data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
          data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
          data.agency.year4 <- process.data.get.sum(data, funding_agency_name4, funding_agency_type4, FY, scale)
          
          ###Create Barplot and Save as JPG
          plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
          plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
          plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
          plot4 <- plot.middle(data.agency.year4, "funding_agency", num_size)
          
          legend <- gtable_filter(ggplot_gtable(ggplot_build(plot1)), "guide-box")
          
          plot.all<-grid.arrange(plot1 + theme(legend.position='hidden'), 
                                 plot2 + theme(legend.position='hidden'), 
                                 plot3 + theme(legend.position='hidden'), 
                                 plot4 + theme(legend.position='hidden'), 
                                 legend, ncol = 5,
                                 top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                                gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
          data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3, data.agency.year4)
          
        }
        else{
          data.agency.year1 <- process.data.get.sum(data, funding_agency_name1, funding_agency_type1, FY, scale)
          data.agency.year2 <- process.data.get.sum(data, funding_agency_name2, funding_agency_type2, FY, scale)
          data.agency.year3 <- process.data.get.sum(data, funding_agency_name3, funding_agency_type3, FY, scale)
          data.agency.year4 <- process.data.get.sum(data, funding_agency_name4, funding_agency_type4, FY, scale)
          data.agency.year5 <- process.data.get.sum(data, funding_agency_name5, funding_agency_type5, FY, scale)
          
          ###Create Barplot and Save as JPG
          plot1 <- plot.first(data.agency.year1, "funding_agency", num_size, scale_text)
          plot2 <- plot.middle(data.agency.year2, "funding_agency", num_size)
          plot3 <- plot.middle(data.agency.year3, "funding_agency", num_size)
          plot4 <- plot.middle(data.agency.year4, "funding_agency", num_size)
          plot5 <- plot.middle(data.agency.year5, "funding_agency", num_size)
          
          plot.all<-grid.arrange(plot1 + theme(legend.position='hidden'),
                                 plot2 + theme(legend.position='hidden'),
                                 plot3 + theme(legend.position='hidden'),
                                 plot4 + theme(legend.position='hidden'),
                                 plot5 + theme(legend.position='hidden'),
                                 legend, ncol = 6, 
                                 top = textGrob(paste(company_name, "Contract Obligations by Agency ", FY_range, sep = ""),
                                                gp = gpar(fontsize = 24)), bottom = "Fiscal Year")
          
          data.agency.year.all <- rbind(data.agency.year1, data.agency.year2, data.agency.year3,
                                        data.agency.year4, data.agency.year5)
          
        }
        
      }
      
    }
    
  }
  
  
  ggsave(paste(company_name, " Contract Obligations by Agency.jpg", sep = ""), plot.all,
         width = w, height = h, units = "in")
  plot.all
  data.agency.year.all