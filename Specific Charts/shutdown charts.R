library(tidyverse)
library(RColorBrewer)

#Location for saving charts
setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/FY19 Shutdown") #location charts are saved

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/shutdown agencies by quarter.csv")

data$Year = as.character(data$Year)
Total <- data %>% 
  mutate(total_obligations = round((`Total Obligation`), digits=0)) %>% 
  filter(Agency != "FTC", Agency != "FCC") %>% 
  group_by(Year) %>% 
  summarize(sum = sum(total_obligations))

Shutdown_list<- split(data, data$Agency)

shutdown_data<-lapply(Shutdown_list, function(x) {
  shutdown_data <- x %>%  
    mutate(total_obligations = round((`Total Obligation`)/1000000, digits=2)) %>% 
    group_by(Year, Agency) %>% 
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations))%>% 
    mutate(FYYear = paste("FY", Year, sep = ""))

   } )

plot<-lapply(shutdown_data, function(xy) {
ggplot(xy, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(xy, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
    facet_grid(~Agency, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Millions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20), 
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
  
} )

print(plot)
list2env(shutdown_data,envir=.GlobalEnv) 

lapply(names(plot), function(x) {
ggsave(filename = paste(x," Contract Obligations FY17-19Q1 by quarter.jpg", sep = ""), plot=plot[[x]],
       width = 13, height = 6.5, units = "in")
})



by_agency <- lapply(shutdown_data, function(x) {
  shutdown_data <- x %>%  
    group_by(Agency,FYYear) %>% 
    summarize(sum = sum(total_obligations))
    
})

big_df$Agency
big_df <- do.call(rbind,by_agency)

fil_bid_df<- filter(big_df, Agency != "FTC", Agency != "FCC")

plot<-ggplot(fil_bid_df, aes(x = reorder(Agency, -sum), y = sum, fill = FYYear)) +
    geom_bar(stat = "identity", color = "Black", position = "dodge") +
    geom_text(aes(label = round(sum, digits = 0)), size = 4, vjust = -.7, fontface = "bold", position = position_dodge(width = 1))+
    # facet_grid(~Agency, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Millions",
         title = "Contract Obligations Comparison") +
    scale_fill_manual(name = "Fiscal year", values = c("FY2017" = "darkolivegreen", "FY2018" = "greenyellow")) +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20), 
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))

  
  ggsave(filename = "Shutdown agencies one chart Contract Obligations FY17-18.jpg", plot,
         width = 13, height = 6.5, units = "in")  
  