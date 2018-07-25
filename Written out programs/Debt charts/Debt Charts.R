library(tidyverse)
# install.packages("colorRamps")
# library(colorRamps)
install.packages("randomcoloR")
library(randomcoloR)


setwd("~/Other Requests/Debt/TIC")
data<- read_csv("Foreign debt investors by country by type JUN 02- JUN 17.csv")

data_long <- data %>% 
  select(-`Country code`) %>% 
  rename(Country = `Countries and Regions`) %>% 
  gather(key = year_type_subtype, value = securities_in_mil, -Country) %>% 
  separate(col = "year_type_subtype", into = c("year", "type", "subtype"), sep = "_") %>% 
  filter(securities_in_mil != "*" & securities_in_mil != "n.a.") %>%
  filter(Country != "Total") %>% 
  filter(subtype == "Total Securities")

data_long$securities_in_mil <- as.numeric(gsub(",", "", data_long$securities_in_mil))


countries.top.10.total <- data_long %>% 
  select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  group_by(Country) %>% 
  summarise(sum = sum(securities_in_mil)) %>% 
  top_n(10) %>% 
  arrange(desc(sum))

countries.top.5.total <- data_long %>% 
  select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  group_by(Country) %>% 
  summarise(sum = sum(securities_in_mil)) %>% 
  top_n(5) %>% 
  arrange(desc(sum))

top_10 <- data_long %>% 
  select(Country, year, securities_in_mil) %>% 
  select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  filter(Country %in% c("Belgium ","Canada","Cayman Islands ","China ",
                        "Ireland","Japan","Luxembourg ","Netherlands",    
                        "Switzerland","United Kingdom")) 

top_10$Country <- factor(top_10$Country, levels = countries.top.10.total$Country)

top_5 <- data_long %>% 
  select(Country, year, securities_in_mil) %>% 
  select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  filter(Country %in% c("Cayman Islands ","China ",
                        "Japan","Luxembourg ","United Kingdom")) 

top_5$Country <- factor(top_5$Country, levels = countries.top.5.total$Country)

###Plotting####


ggplot(top_10, aes(x=year, y = securities_in_mil, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Millions)", title = "Total Foreign Holdings of U.S. Securities")


ggplot(top_5, aes(x=year, y = securities_in_mil, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Millions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")
  


ggplot(top_5, aes (x=year, y= securities_in_mil, color = Country))+
  geom_bar(stat = "identity")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(~Country, labeller = label_wrap_gen(20))+
  labs(x = "Year", y= "Total Securities Held (in Millions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")

  

####China and Japan ####

data_long <- data %>% 
  select(-`Country code`) %>% 
  rename(Country = `Countries and Regions`) %>% 
  gather(key = year_type_subtype, value = securities_in_mil, -Country) %>% 
  separate(col = "year_type_subtype", into = c("year", "type", "subtype"), sep = "_") %>% 
  filter(securities_in_mil != "*" & securities_in_mil != "n.a.") %>%
  filter(Country != "Total") 

data_long$securities_in_mil <- as.numeric(gsub(",", "", data_long$securities_in_mil))

data_long <- data_long %>% 
  group_by(Country, year, subtype) %>% 
  summarize(sum = sum(securities_in_mil)) %>% 
  filter(subtype == "Treasury Debt")






countries.top.10.total <- data_long %>% 
  # select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>%
  select(-delete) %>%
  group_by(Country) %>%
  summarise(sum = sum(sum)) %>%
  top_n(10, sum)

countries.top.5.total <- data_long %>% 
  # select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>%
  select(-delete) %>%
  group_by(Country) %>%
  summarise(sum = sum(sum)) %>%
  top_n(5)

top_10 <- data_long %>% 
  #select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  filter(Country %in% c(countries.top.10.total$Country)) 

top_10$Country <- factor(top_10$Country, levels = countries.top.10.total$Country)

top_5 <- data_long %>% 
#  select(Country, year, securities_in_mil) %>% 
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
  select(-delete) %>% 
  filter(Country %in% c(countries.top.5.total$Country)) 

top_5$Country <- factor(top_5$Country, levels = countries.top.5.total$Country)



###Plotting####
ggplot(top_10, aes(x=year, y = sum, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Treasury Debt Held (in Millions)", title = "Foreign Holdings of U.S. Securities for Overall Top 10 Investors")


ggplot(top_5, aes(x=year, y = sum, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Treasury Debt Held (in Millions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")


  
  # scale_colour_manual(values=c("grey", "blue", "green","purple","red"))
# countries.all.years.sum <- data_long %>% 
#   select(Country, year, securities_in_mil) %>% 
#   separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
#   select(-delete) %>% 
#   group_by(Country) %>% 
#   summarise(count = n()) %>% 
#   filter(count == 16)



# all <- data_long %>% 
#   select(Country, year, securities_in_mil) %>% 
#   separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>% 
#   select(-delete) 
# 
# all <- all[all$Country %in% countries.all.years.sum$Country,]



# ggplot(all, aes(x=year, y = securities_in_mil, color = Country, group = Country))+
#   geom_line()+
#   scale_color_discrete(name = "Top 10 Countries", 
#                     breaks= c("Belgium ","Canada","Cayman Islands ","China ",
#                                "Ireland","Japan","Luxembourg ","Netherlands",    
#                                "Switzerland","United Kingdom"), h = c(0,174))+
#   labs(x = "Year", y= "Total Securities Held (in Millions)", title = "Total Foreign Holdings of U.S. Securities")

ggplot()

countries.all.years.total$Country
unique(top_5$Country)

color <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
unlist(color[1:174])

palette <- distinctColorPalette(174)

