#DoD_Branch_DPAP <- function(){}
#

library(tidyverse)


data <- read_csv("S:/1 Marielle Folder/Data Sets/By Agency/DOD/DoD services, by DPAP.csv")

data_edit <- data %>%
  rename(amount = `Total (in millions)`) %>% 
  filter(Type != "Products") %>% 
  filter(Year != "FY18") %>% 
  mutate(amountB = amount/1000) %>% 
  group_by(Service) %>% 
  mutate(prop = 100*amountB/sum(amountB))

data_edit$amountB = as.numeric(data_edit$amountB)
data_edit$Type = factor(data_edit$Type, levels = c("Knowledge Based Services", "Research and Development",
                                                   "Equipment Related Services","Facility Related Services",
                                                   "Electronic & Communication Services", "Transportation Services",
                                                   "Logistics Management Services", "Construction Services",
                                                   "Medical Services"))
data_edit$Service[data_edit$Service == "Defense-wide"] = "DoD, Other"
data_edit$Service = factor(data_edit$Service, levels = c("Air Force", "Army", "Navy", "DoD, Other"))

army <- data_edit %>% 
  filter(Service == "Army") 

navy <- data_edit %>%
  filter(Service == "Navy")

air_force <- data_edit %>%
  filter(Service == "Air Force")

defense_other <- data_edit %>%
  filter(Service == "DoD, Other")

# ggplot(data_edit, aes(x = factor(-amountB) , y = amountB, fill = Type)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = round(amountB, digit = 1), vjust = -.5), size = 3, fontface = "bold")+
#   geom_text(aes(label = sprintf('%.0f%%', prop)), size = 3, vjust = 3, fontface = "bold")+
#   scale_fill_manual(data_edit$Type, values = c("Knowledge Based Services" = "mediumorchid3", "Research and Development" = "mediumseagreen",
#                                                "Facility Related Services" = "goldenrod2","Equipment Related Services" = "firebrick1",  
#                                                "Electronic & Communication Services" = "steelblue3", "Medical Services" = "orangered1",
#                                                "Construction Services" = "olivedrab2",  "Transportation Services" = "palevioletred1",
#                                                "Logistics Management Services" = "lightseagreen")) +
#   #scale_fill_manual(Type, palette = "Dark1") +
#   facet_grid(~Service, scales = "free_x", drop = TRUE) +#labeller = label_wrap_gen(20)) +
#   #scale_x_discrete(labels = molten[, setNames(as.character(id), ord)])+
#   labs(y="Amount (in Billions)", title = "DoD Services Taxonomy by Branch")+
#   theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.title.x = element_blank(),
#         axis.text.x = element_blank(), axis.ticks.x = element_blank())


ggplot(data_edit, aes(x = year , y = amountB, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf('%0.1f', round(amountB, digit = 1))), vjust = -.5, size = 3.5, fontface = "bold")+
  geom_text(aes(label = sprintf('%0.1f%%', prop)), vjust = 1.25, size = 3, fontface = "bold") +
  scale_fill_manual(name = "Services Taxonomy", data_edit$Type, values = c("Knowledge Based Services" = "mediumorchid3", "Research and Development" = "mediumseagreen",
                                               "Equipment Related Services" = "firebrick1","Facility Related Services" = "goldenrod2",
                                               "Electronic & Communication Services" = "steelblue3", "Transportation Services" = "palevioletred1",
                                               "Logistics Management Services" = "lightseagreen", "Construction Services" = "olivedrab2",
                                               "Medical Services" = "orangered1")) +
  #scale_fill_manual(Type, palette = "Dark1") +
  facet_grid(~Service, scales = "free_x", drop = TRUE) +   #labeller = label_wrap_gen(20)) +
  #scale_x_discrete(labels = molten[, setNames(as.character(id), ord)])+
  labs(y="Amount (in Billions)", title = "DoD Services Taxonomy by Branch")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.title.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())




setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/Defense")

ggsave("DPAP DOD by service FY18 (as of 8-27).jpg", plot,
       width = 15, height = 8, units = "in") 



unique(data_edit$Type)

data_amt <- data_edit %>% 
  group_by(Type) %>% 
  summarise(sum = sum(amount)) %>% 
  arrange(desc(sum))

