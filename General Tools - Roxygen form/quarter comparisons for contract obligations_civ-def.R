### Contract Obligations by Quarter

library(readr)
data <- read_csv("~/Market Briefings/Data/Government-Wide data/Civilian and Defense Data by quarter.csv")

data<-rename(data, civ_def = "Civ/Def", total_obligations = "Contract Obligations (in Billions)")
data$`Contract Obligations (in Billions)`

data$Year = as.character(data$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def", 
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>% 
  filter(Year == 2014|Year ==2015| Year == 2016 | Year == 2017 | Year == 2018) %>% 
  group_by(Year, civ_def) %>% 
  mutate(label_y = cumsum(total_obligations))

data.civdef$Year = as.character(data.civdef$Year)



ggplot(data.civdef, aes(x = civ_def, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5)+
  scale_fill_manual(name = "none", values = c("Q4" = "rosybrown3", "Q3" = "burlywood1", "Q2" = "skyblue3", "Q1" = "turquoise4")) +
  facet_grid(~Year, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions") + #, 
  #      title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())
