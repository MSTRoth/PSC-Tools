##OTA


library(scales)
library(tidyverse)
library(RColorBrewer)
options(scipen=999)

setwd("S:/1 Marielle Folder/Visualizations/OTA")
data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/OTA/Other_Transaction_Actions_and_Dollars FY13-18.csv")

OTA_data <- data %>% 
  filter(`Contract Fiscal Year` %in% c(2013,2014,2015,2016,2017,2018)) %>% 
  group_by(`Contract Fiscal Year`) %>% 
  summarize(total_dollars = sum(`Total Dollars`)/1000000,
            total_actions = sum(`Total Actions`))

OTA_data$`Contract Fiscal Year` <- as.character(OTA_data$`Contract Fiscal Year`)
# ggplot(OTA_data) +
#   geom_bar(mapping = aes(x = `Contract Fiscal Year`, y = total_dollars), stat = "identity", fill = "dodgerblue")+
#   geom_line(mapping = aes(x = `Contract Fiscal Year`, y = total_actions*1000), size = 2, color = "navyblue")+
#   scale_y_continuous(name = "Total Obligations (in Thousands)", labels = dollar,
#                      sec.axis = sec_axis(~./1000, name = "Total Number of Actions"))
  
plot <- ggplot(OTA_data, aes(x = `Contract Fiscal Year`, y = total_dollars)) +
  geom_bar(stat = "identity", fill = "dodgerblue")+
  scale_y_continuous(name = "Total Obligations (in Millions)", labels = dollar) +
  geom_text(aes(label = dollar(round(total_dollars, digits = 0))), vjust = -1, size = 6)+
  labs(title = "Total Combined OTA Obligations", subtitle = "FY2013 - FY2018", caption = "Source: FPDS-NG")+
theme(plot.title = element_text(hjust = 0.5, size = 34, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
      axis.text.x = element_text(face = "bold", size = 16, color = "black"),
      panel.background = element_blank(),
      plot.background = element_rect(colour = NA, fill = NA),
      #panel.spacing = unit(1, "lines"),
      # panel.border = element_rect(colour = NA, fill = NA),
      axis.title.x = element_blank(),
      axis.title.y = element_text(angle=90,vjust =2, face = "bold",size = 22),
      axis.line = element_line(colour="#bcbcbc"),
      axis.ticks.y = element_line(),
      axis.text.y = element_text(face = "bold", size = 16, color = "black"),
      panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.minor = element_line(colour="#f0f0f0"),
      plot.caption = element_text(size = 14),
      # legend.key = element_rect(colour = NA),
      # legend.position = "bottom",
      # legend.direction = "horizontal",
      # legend.key.size= unit(0.2, "cm"),
      # legend.margin = unit(0, "cm"),
      legend.title = element_text(face="bold"),
      #legend.text = element_text(size = 13),
      #plot.margin=unit(c(10,5,5,5),"lines"),
      strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
      strip.text.x = element_text(face="bold", size = 13),
      panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
)+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)



ggsave("OTA combined obligations chart FY13-FY18.jpg", plot,
       width = 12, height = 10.5, units = "in") 


OTA_data2 <- data %>% 
  filter(`Contract Fiscal Year` %in% c(2014,2015,2016,2017,2018)) %>% 
  group_by(`Contract Fiscal Year`) %>% 
  summarize(total_dollars = sum(`Total Dollars`)/1000000,
            total_actions = sum(`Total Actions`))

plot <- ggplot(OTA_data2, aes(x = `Contract Fiscal Year`, y = total_dollars)) +
  geom_bar(stat = "identity", fill = "dodgerblue")+
  scale_y_continuous(name = "Total Obligations (in Millions)", labels = dollar) +
  geom_text(aes(label = dollar(round(total_dollars, digits = 0))), vjust = -1, size = 6)+
  labs(title = "Total Combined OTA Obligations", subtitle = "FY2014 - FY2018", caption = "Source: FPDS-NG")+
  theme(plot.title = element_text(hjust = 0.5, size = 34, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_text(face = "bold", size = 16, color = "black"),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        #panel.spacing = unit(1, "lines"),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle=90,vjust =2, face = "bold",size = 22),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(face = "bold", size = 16, color = "black"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        plot.caption = element_text(size = 14),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #legend.text = element_text(size = 13),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)

ggsave("OTA combined obligations chart FY14-FY18.jpg", plot,
       width = 12, height = 10.5, units = "in") 
