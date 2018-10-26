### Quarter comparisons for Contract Obligations - Civilian/Defense Breakout####
library(colorspace)
library(readr)
library(RColorBrewer)
data <- read_csv("~/Market Briefings/Data/Government-Wide data/Civilian and Defense Data by quarter.csv")

#data<-rename(data, civ_def = "Civ/Def", total_obligations = "Contract Obligations (in Billions)")

data$Year = as.character(data$Year)

# data.civdef <- data %>%
#   rename(civ_def = "Civ/Def",
#          total_obligations = "Contract Obligations (in Billions)") %>%
#   #filter(Year!=2019) %>%
#   filter(Year == 2014|Year ==2015| Year == 2016 | Year == 2017 | Year == 2018) %>%
#   group_by(Year, civ_def) %>%
#   mutate(label_y = cumsum(total_obligations))
#
# data.civdef$Year = as.character(data.civdef$Year)

data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year == 2016 | Year == 2017 | Year == 2018) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

display.brewer.all()



plotyr <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity") +
 geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
 geom_text(data = subset(data.civdef, Year != 2018), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
 #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
# geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
                         #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
 #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
 # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
                                      #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
 facet_grid(~civ_def, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", title = "Contract Obligations Comparison FY16-FY18",
        subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))
# 
# install.packages("png")
# library(png)
# 
# img <- readPNG("C:/Users/Roth/Desktop/logo.png")


ggsave("Contract Obligations by Quarter- FY16-FY18.jpg", plotyr,
       width = 13, height = 7, units = "in")

ggsave("Contract Obligations by Quarter- FY16-FY18.pdf", plotyr,
       width = 13, height = 7, units = "in")

library(svglite)
# plotrg <- ggplot(data.civdef, aes(x = civ_def, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
#   geom_text(data = subset(data.civdef, Year != 2018), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+
#   stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
#                geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
#   #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
#   # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
#   #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
#   #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
#   # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
#   #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
#   #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
#   scale_fill_manual(name = "Quarter", values = brewer.pal(11, "Spectral")[c(2,4,5,8)])+
#   facet_grid(~Year, labeller = label_wrap_gen(20))+
#   labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
#        title = "Contract Obligations by Quarter: FY16-FY18") +
#   theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank(),
#         strip.text = element_text(face = "bold"))

####################################################################################################

####Total contract spending - services categories + products####

## Government-Wide Total Contract Spending####

data_GW <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Government Wide.csv")


data_GW$`DPAP Category` <- factor(data_GW$`DPAP Category`,
                               levels = c("Products", "Construction Services",
                                          "Electronic & Communication Services",
                                          "Equipment Related Services",
                                          "Facility Related Services",
                                          "Knowledge Based Services",
                                          "Logistics Management Services",
                                          "Medical Services",
                                          "Research and Development",
                                          "Transportation Services"),
                               ordered = is.ordered(data_GW$`DPAP Category`))

DPAP_GW <- data_GW %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$ billions`))


label_height <- DPAP_GW %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$ billions`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_GW, by = c("Fiscal Year", "pors", "pors$") )



ggplot(label_height, aes(x = `Fiscal Year`, y = `$ billions`,
                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
  #            fill = pors,
  #              colour = "black")+
  # scale_color_manual(values = c("black", "black"))+
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Spectral") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "Government Wide Total Contract Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())




##DoD Total Contract Spending####
data_DoD <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - DoD.csv")

data_DoD$`DPAP Category` <- factor(data_DoD$`DPAP Category`,
                                   levels = c("Products", "Construction Services",
                                              "Electronic & Communication Services",
                                              "Equipment Related Services",
                                              "Facility Related Services",
                                              "Knowledge Based Services",
                                              "Logistics Management Services",
                                              "Medical Services",
                                              "Research and Development",
                                              "Transportation Services"),
                               ordered = is.ordered(data_DoD$`DPAP Category`))

DPAP_DoD <- data_DoD %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
 # mutate(label_y = cumsum(`$ billions`)) %>%
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
    group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$ billions`))


label_height <- DPAP_DoD %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$ billions`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_DoD, by = c("Fiscal Year", "pors", "pors$") )





ggplot(label_height, aes(x = `Fiscal Year`, y = `$ billions`,
                 fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
 # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
 #            fill = pors,
 #              colour = "black")+
 # scale_color_manual(values = c("black", "black"))+
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
      title = "DoD Total Contract Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())




## Civilian total contract spending####

data_civ <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Civilian.csv")


data_civ$`DPAP Category` <- factor(data_civ$`DPAP Category`,
                                  levels = c("Products", "Construction Services",
                                             "Electronic & Communication Services",
                                             "Equipment Related Services",
                                             "Facility Related Services",
                                             "Knowledge Based Services",
                                             "Logistics Management Services",
                                             "Medical Services",
                                             "Research and Development",
                                             "Transportation Services"),
                                  ordered = is.ordered(data_civ$`DPAP Category`))

DPAP_civ <- data_civ %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
 # mutate(label_y = cumsum(`$ billions`)) %>%
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$ billions`))


label_height <- DPAP_civ %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$ billions`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
 mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_civ, by = c("Fiscal Year", "pors", "pors$") )


ggplot(label_height, aes(x = `Fiscal Year`, y = `$ billions`,
                    fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "Civilian Total Contract Spending")+
  ylim(0, 200) +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())

#################### wo products #####

data_civ <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Civilian.csv")
data_dod <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - DoD.csv")
total_civ <- data_civ %>%
  group_by(`DPAP Category`) %>% 
  summarise(sum = sum(`$ billions`)) %>% 
  arrange(desc(sum))

colnames(data_dod)

total_dod <- data_dod %>%
  filter(`Fiscal Year`!="FY18") %>% 
  group_by(`DPAP Category`) %>% 
  summarise(sum = sum(`$ billions`)) %>% 
  arrange(desc(sum))

data_civ$`DPAP Category` <- factor(data_civ$`DPAP Category`,
                                   levels = c(total_civ$`DPAP Category`),
                                   ordered = is.ordered(total_civ$`DPAP Category`))

DPAP_civ <- data_civ %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  # mutate(label_y = cumsum(`$ billions`)) %>%
  filter(`DPAP Category`!="Products") %>%
  filter(`Fiscal Year` %in% c("FY14", "FY15", "FY16", "FY17", "FY18")) %>% 
  group_by(`Fiscal Year`, `DPAP Category`) %>%
  mutate(`pors$` = sum(`$ billions`))

palette <- palette(brewer.pal(n = 10, name = "Set3")[2:10])

ggplot(DPAP_civ, aes(x = `Fiscal Year`, y = `$ billions`,
                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(x = `Fiscal Year`, label = round(total, digits = 2)), size = 4, vjust = 1.5, check_overlap = TRUE)+
  stat_summary(fun.y = sum, aes(label = round(..y.., digits = 2), group = `Fiscal Year`), geom = "text", vjust = -.5, fontface = "bold")+
  scale_fill_manual(name = "Services Contract Category", values = c("Construction Services" = "#FFFFB3",
                    "Electronic & Communication Services" = "#BEBADA",
                    "Equipment Related Services" = "#FB8072",
                    "Facility Related Services" = "#80B1D3",
                    "Knowledge Based Services" = "#FDB462",
                    "Logistics Management Services" = "#B3DE69",
                    "Medical Services" = "#FCCDE5",
                    "Research and Development" = "gray85",
                    "Transportation Services" = "#BC80BD")) +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "Civilian Services Contract Spending")+
  ylim(0, 170) +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())


data_dod$`DPAP Category` <- factor(data_dod$`DPAP Category`,
                                   levels = c(total_dod$`DPAP Category`),
                                   ordered = is.ordered(data_dod$`DPAP Category`))

DPAP_dod <- data_dod %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  # mutate(label_y = cumsum(`$ billions`)) %>%
  filter(`DPAP Category`!="Products") %>%
  filter(`Fiscal Year` %in% c("FY13", "FY14", "FY15", "FY16", "FY17")) %>% 
  group_by(`Fiscal Year`, `DPAP Category`) %>%
  mutate(`pors$` = sum(`$ billions`))

palette <- palette(brewer.pal(n = 10, name = "Set3")[2:10])

ggplot(DPAP_dod, aes(x = `Fiscal Year`, y = `$ billions`,
                     fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(x = `Fiscal Year`, label = round(total, digits = 2)), size = 4, vjust = 1.5, check_overlap = TRUE)+
  stat_summary(fun.y = sum, aes(label = round(..y.., digits = 2), group = `Fiscal Year`), geom = "text", vjust = -.5, fontface = "bold")+
  scale_fill_manual(name = "Services Contract Category", values = c("Construction Services" = "#FFFFB3",
                                                                    "Electronic & Communication Services" = "#BEBADA",
                                                                    "Equipment Related Services" = "#FB8072",
                                                                    "Facility Related Services" = "#80B1D3",
                                                                    "Knowledge Based Services" = "#FDB462",
                                                                    "Logistics Management Services" = "#B3DE69",
                                                                    "Medical Services" = "#FCCDE5",
                                                                    "Research and Development" = "gray85",
                                                                    "Transportation Services" = "#BC80BD")) +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "Defense Services Contract Spending")+
  ylim(0, 170) +
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())



##############################
###Services/Products by Agency####

data <- read_csv("C:/Users/Roth/Documents/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services/Civilian agencies DPAP Categories.csv")
colnames(data)
setwd("~/Market Briefings/Data/Government-Wide data/FY14-FY18 Agency Charts")
setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services")

data$`DPAP Category` <- factor(data$`DPAP Category`, levels=c("Construction Services", 
                                                              "Electronic & Communication Services", 
                                                              "Equipment Related Services",
                                                              "Facility Related Services",
                                                              "Knowledge Based Services",
                                                              "Logistics Management Services",
                                                              "Medical Services",
                                                              "Research & Development",
                                                              "Transportation Services",
                                                              "Products"))

data.DPAP.agencies <- data %>%
  dplyr::rename("DPAP_Category" = `DPAP Category`)%>% 
  gather("fiscal_year","amount",4:13) %>%
  filter(DPAP_Category != "Total") %>% 
  # filter(Agency == "Department of Homeland Security (DHS)") %>%
  # filter(Agency == "Department of Commerce (DOC)") %>%
  # filter(Agency == "Department of Energy (DOE)") %>%
  # filter(Agency == "Department of Justice (DOJ)") %>%
  # filter(Agency == "Department of State (DOS)") %>%
  # filter(Agency == "Department of Transportation (DOT)") %>%
  # filter(Agency == "Environmental Protection Agency (EPA)") %>%
  # filter(Agency == "Department of Health and Human Services (HHS)") %>%
  # filter(Agency == "National Aeronautics and Space Administration (NASA)") %>%
  # filter(Agency == "Department of Treasury (TREAS)") %>%
  # filter(Agency == "Agency for International Development (USAID)") %>%
# filter(Agency == "Department of Agriculture (USDA)") %>%
filter(Agency == "Department of Veterans Affairs (VA)") %>%
  dplyr::mutate(total_transaction_value = amount/1000000000) %>% 
  filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|fiscal_year == "FY17"|fiscal_year == "FY18")

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))

#data.DPAP.agencies$DPAP_Category <- droplevels(data.DPAP.agencies$DPAP_Category)

#data.DPAP.agencies <- melt(data.DPAP.agencies,id.vars=c("Abbrev","Agency","DPAP_Category","fiscal_year","amount","total_transaction_value"))



plot <- ggplot(data.DPAP.agencies, aes(fill = fiscal_year, x = fiscal_year,
                                       y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual(values = cc) +
  labs(y = "Contract Obligations (in Billions)", title = unique(data.DPAP.agencies$Agency), 
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~DPAP_Category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(size = 13, face="bold"),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))+
  guides(fill = FALSE)


ggsave(paste(unique(data.DPAP.agencies$Abbrev)," DPAP FY14-FY18 - blue spectrum.jpg", sep = ""), plot,
       width = 15, height = 7, units = "in") 

############################

data <- read_csv("C:/Users/Roth/Documents/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services/Civilian agencies DPAP Categories.csv")


##agencies stacked bar by DPAP#####
library(colorRamps)

data.DPAP.agencies <- data %>%
  dplyr::rename("DPAP_Category" = `DPAP Category`)%>% 
  gather("fiscal_year","amount",4:13) %>%
  filter(DPAP_Category != "Total") %>% 
  dplyr::mutate(total_transaction_value = amount/1000000000) %>% 
  filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|
           fiscal_year == "FY17"|fiscal_year == "FY18") 

cc <- primary.colors(16, steps = 3, no.white = T)


ggplot(data.DPAP.agencies, aes(fill = Abbrev, x = fiscal_year,
                                       y = total_transaction_value))+
  geom_bar(stat = "identity") +
  #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual(values = cc, name = "Agencies") +
  labs(y = "Contract Obligations (in Billions)", title = "Top Civilian Agencies", 
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~DPAP_Category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(size = 13, face="bold"),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))

ggsave(paste("Top Civilian Agencies by DPAP FY14-FY18.jpg", sep = ""), plot,
       width = 15, height = 7, units = "in") 

#########################
data <- read_csv("C:/Users/Roth/Documents/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services/Civilian agencies DPAP Categories.csv")

data.DPAP.agencies <- data %>%
  dplyr::rename("DPAP_Category" = `DPAP Category`)%>% 
  gather("fiscal_year","amount",4:13) %>%
  filter(DPAP_Category == "Total") %>% 
  dplyr::mutate(total_transaction_value = amount/1000000000) %>% 
  filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|
           fiscal_year == "FY17"|fiscal_year == "FY18")%>% 
  arrange(Abbrev, Agency)

cc <- primary.colors(16, steps = 3, no.white = T)


ggplot(data.DPAP.agencies)+
  geom_bar(stat = 'identity', aes(x = Agency,
                               y = total_transaction_value, fill = Abbrev), position = 'stack')+
  #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual(values = cc, name = "Agencies") +
  labs(y = "Contract Obligations (in Billions)", title = "Top Civilian Agencies", 
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~fiscal_year, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(size = 13, face="bold"),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))

ggsave(paste("Top Civilian Agencies total FY14-FY18.jpg", sep = ""), plot,
       width = 15, height = 7, units = "in") 

ggplot(data.DPAP.agencies, aes(y = total_transaction_value, x = Agency, fill = Abbrev))+
  geom_bar(stat = 'identity', position = 'stack')
  