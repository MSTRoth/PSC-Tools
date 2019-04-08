library(tidyverse)
DPAP_Categories <- read_csv("X:/1 Marielle Folder/Data For R/Government-Wide and DPAP Visualizations/Visible all Services 7-19-18 FY14-18.csv")
data <- DPAP_Categories %>% 
  select(-X1) %>% 
  #unite(FY, C)
  spread(DPAP_category, amount)


HHS<- read_csv("X:/1 Marielle Folder/Data Sets/Vendor Specific/ARS Company Profile.csv")
PSC <- read_csv("~/Reference Tables/PSC RandD, S, P.csv")

x<- c("Services","R&D","Products")

data<- HHS %>% 
  rename(PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  left_join(PSC, by= c("PSC" = "PSC CODE")) %>% 
  select(`Transaction Value`, `Fiscal Year`, `Product or Service`) %>% 
  filter(`Fiscal Year` %in% c(2015, 2016, 2017)) %>% 
  group_by(`Fiscal Year`, `Product or Service`) %>% 
  summarise(sum = sum(`Transaction Value`/1000000)) %>% 
  filter(!is.na(`Product or Service`)) %>% 
  mutate(category = factor(`Product or Service`, levels = x)) %>% 
  arrange(category) %>% 
  mutate(label_y = cumsum(sum),
         prop = 100*sum/sum(sum)) 

ggplot(data, aes(x = `Fiscal Year`, y = sum, fill = `Product or Service`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(sum, digits = 1), ", ", sprintf('%.0f%%', prop)), y = sum), size = 3, position = position_stack(vjust = .5), fontface = "bold")+
  # geom_text(aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 2.5)+
  stat_summary(fun.y = sum, aes(label = round( ..y.., digits = 1), group = `Fiscal Year`),
               geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual("Product/Service/R&D", values = c("Products" = "seagreen2", "R&D" = "gold","Services" = "steelblue3")) +
  labs(y = paste("Contract Obligations (in) ", "millions", sep = ""))


VA<- read_csv("X:/1 Marielle Folder/Data For R/Government-Wide and DPAP Visualizations/Funding Agencies and Subsets/VA SPRD.csv")

x<- c("Services","R&D","Products")

data<- VA %>% 
  gather("Fiscal Year","sum",2:4) %>% 
  rename("Product or Service" = DPAP) %>% 
  filter(`Product or Service` %in% x) %>% 
  mutate(category = factor(`Product or Service`, levels = x)) %>% 
  arrange(category)

data<- data %>% 
  group_by(`Product or Service`) %>% 
  mutate(label_y = cumsum(sum),
         prop = 100*sum/sum(sum))
  
  
  
ggplot(data, aes(x = `Fiscal Year`, y = sum, fill = `Product or Service`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(sum, digits = 2), ", ", sprintf('%.0f%%', prop)), y = sum), size = 4, position = position_stack(vjust = .5), fontface = "bold")+
  # geom_text(aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 2.5)+
  stat_summary(fun.y = sum, aes(label = round( ..y.., digits = 1), group = `Fiscal Year`),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual("Product/Service/R&D", values = c("Products" = "seagreen2", "R&D" = "gold","Services" = "steelblue3")) +
  labs(y = paste("Contract Obligations (in) ", "millions", sep = ""))


