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
  filter(Country %in% c("Cayman Islands (7)","China (21)",
                        "Japan","Luxembourg (5)","United Kingdom")) %>%
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>%
  select(-delete)
  #filter(subtype == "Total Securities")

data_long_total <- data %>%
  select(-`Country code`) %>%
  rename(Country = `Countries and Regions`) %>%
  gather(key = year_type_subtype, value = securities_in_mil, -Country) %>%
  separate(col = "year_type_subtype", into = c("year", "type", "subtype"), sep = "_") %>%
  filter(securities_in_mil != "*" & securities_in_mil != "n.a.") %>%
  # filter(Country != "Total") %>%
  filter(Country == "Total")
#filter(subtype == "Total Securities")


data_long$securities_in_mil <- as.numeric(gsub(",", "", data_long$securities_in_mil))
data_long$securities_in_tril <- (data_long$securities_in_mil)/1000000


data_long_total$securities_in_mil <- as.numeric(gsub(",", "", data_long_total$securities_in_mil))
data_long_total$securities_in_tril <- (data_long_total$securities_in_mil)/1000000

total <- data_long_total %>%
  group_by(year, subtype) %>%
  mutate(subtype_sec = sum(securities_in_tril))


by_type_LT <- total %>%
  filter(type == "Long-term", subtype != "Total long-term Debt") %>%
  select(-subtype_sec) %>%
  arrange(desc(subtype)) %>%
  group_by(year) %>%
  mutate(label_y = cumsum(securities_in_tril),
         prop = 100*securities_in_tril/sum(securities_in_tril))

ggplot(by_type_LT, aes(x=year, y = securities_in_tril, fill = subtype, group = subtype))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(securities_in_tril, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = by_type_LT, aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 2.75, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = round(..y.., digits = 1), group = year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Debt Type", values = brewer.pal(11, "Spectral")[c(2, 10, 4, 9)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Long Term Foreign Holdings of U.S. Securities by Type")


by_type_total <- total %>%
  filter(subtype != "Total long-term Debt" & subtype != "Total short-term Debt" & subtype != "Total Securities") %>%
  select(-type, -securities_in_mil, -securities_in_tril)

by_type_total <- unique(by_type_total)

by_type_total<- by_type_total %>%
  rename(securities_in_tril = subtype_sec) %>%
  arrange(desc(subtype)) %>%
  group_by(year) %>%
  mutate(label_y = cumsum(securities_in_tril),
         prop = 100*securities_in_tril/sum(securities_in_tril))

ggplot(by_type_total, aes(x=year, y = securities_in_tril, fill = subtype, group = subtype))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(securities_in_tril, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = by_type_total, aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = round(..y.., digits = 1), group = year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Debt Type", values = brewer.pal(11, "Spectral")[c(2, 10, 4, 9)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Total Foreign Holdings of U.S. Securities by Type")



LT_ST <- total %>%
  filter(subtype == "Equity" | subtype == "Total long-term Debt" | subtype == "Total short-term Debt") %>%
  select(-type, -securities_in_mil) %>%
  filter(subtype == "Total long-term Debt"| subtype == "Total short-term Debt" |subtype == "Equity")

LT_ST$subtype[LT_ST$subtype == "Total long-term Debt"] <- "Long-term"
LT_ST$subtype[LT_ST$subtype == "Equity"] <- "Long-term"
LT_ST$subtype[LT_ST$subtype == "Total short-term Debt"] <- "Short-term"

LT_ST <- LT_ST %>%
  group_by(year, subtype) %>%
  mutate(subtype_2 = sum(securities_in_tril)) %>%
  select(year, subtype, subtype_2)

LT_ST <- unique(LT_ST)

LT_ST <- LT_ST %>%
  arrange(subtype_2) %>%
  group_by(year) %>%
  mutate(label_y = cumsum(subtype_2),
         prop = 100*subtype_2/sum(subtype_2)) %>%
  select(year, subtype, subtype_2, label_y, prop) %>%
  rename(securities_in_tril = subtype_2) %>%
  mutate(type = subtype)



ggplot(LT_ST, aes(x=year, y = securities_in_tril, fill = type, group = type))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(securities_in_tril, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
 # geom_text(data = by_type_LT, aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 2.75, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = round(..y.., digits = 1), group = year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Debt Type", values = brewer.pal(9, "Set1")[c(1,2)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Long Term vs. Short term Foreign Holdings of U.S. Securities")





top_5_LT <- data_long %>%
  filter(type == "Long-term", subtype != "Total long-term Debt") %>%
  group_by(Country, year) %>%
  dplyr::summarize(LT_total = sum(securities_in_tril)) %>%
  arrange(Country)

ggplot(top_5_LT, aes(x=year, y = LT_total, color = Country, group = Country))+
  geom_line()+
  scale_color_manual(name = "Country", values = brewer.pal(9, "Set1")[c(3, 1, 2, 5, 4)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Long Term Foreign Holdings of U.S. Securities for Overall Top 5 Investors")


top_5_total <- data_long %>%
  filter(type == "Total") %>%
  select(Country, year, securities_in_tril) %>%
  arrange(Country)

ggplot(top_5_total, aes(x=year, y = securities_in_tril, color = Country, group = Country))+
  geom_line()+
  scale_color_manual(name = "Country", values = brewer.pal(9, "Set1")[c(3, 1, 2, 5, 4)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Total Foreign Holdings of U.S. Securities for Overall Top 5 Investors")



top_5_Treasury <- data_long %>%
  filter(subtype == "Treasury Debt") %>%
  group_by(Country, year) %>%
  dplyr::summarize(TD_total = sum(securities_in_tril)) %>%
  arrange(Country)

ggplot(top_5_Treasury, aes(x=year, y = TD_total, color = Country, group = Country))+
  geom_line()+
  scale_color_manual(name = "Country", values = brewer.pal(9, "Set1")[c(3, 1, 2, 5, 4)])+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Treasury Debt Foreign Holdings of U.S. Securities for Overall Top 5 Investors")




ggplot(top_10, aes(x=year, y = securities_in_mil/1000, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Billions)", title = "Total Foreign Holdings of U.S. Securities")


ggplot(top_5, aes(x=year, y = securities_in_mil/1000, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Billions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")



ggplot(top_5, aes (x=year, y= securities_in_mil, color = Country))+
  geom_bar(stat = "identity")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(~Country, labeller = label_wrap_gen(20))+
  labs(x = "Year", y= "Total Securities Held (in Millions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")











countries.top.10.total <- data_long %>%
  select(Country, year, securities_in_mil) %>%
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>%
  select(-delete) %>%
  group_by(Country) %>%
  summarise(sum = sum(securities_in_mil)/1000) %>%
  top_n(10) %>%
  arrange(desc(sum))

countries.top.5.total <- data_long %>%
  select(Country, year, securities_in_mil) %>%
  separate(col = "Country", into = c("Country", "delete"), sep = "[(]") %>%
  select(-delete) %>%
  group_by(Country) %>%
  summarise(sum = sum(securities_in_mil)/1000) %>%
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


ggplot(top_10, aes(x=year, y = securities_in_mil/1000, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Billions)", title = "Total Foreign Holdings of U.S. Securities")


ggplot(top_5, aes(x=year, y = securities_in_mil/1000, color = Country, group = Country))+
  geom_line()+
  scale_color_brewer(palette = "Paired")+
  labs(x = "Year", y= "Total Securities Held (in Billions)", title = "Foreign Holdings of U.S. Securities for Overall Top 5 Investors")



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
  stat_summary(fun.y = sum, aes(label = round(..y.., 1), group = Year_y),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+
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

