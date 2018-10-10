
setwd("~/Other Requests/Bradley")

data<- read_csv("DHS BGOV Contract FY17-FY18 9-18.csv")


data.USA <- read_csv("DHS Contract FY17-FY18 9-18.csv")


data.cs <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Construction Services.csv")
data.ecs <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Electronic & Communication Services.csv")
data.ers <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Equipment Related Services.csv")
data.frs <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Facility Related Services.csv")
data.kbs <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Knowledge Based Services.csv")
data.lms <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Logistics Services.csv")
data.ms <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Medical Services.csv")
data.ts <- read_csv("DHS BGOV Contract FY17-FY18 9-18 Transportation Services.csv")
data.rd <- read_csv("DHS BGOV Contract FY17-FY18 9-18 RD.csv")

list <- list("Construction Services" = data.cs, "Electronic & Communication Services" = data.ecs, 
             "Equipment Related Services" = data.ers, "Facility Related Services" = data.frs, 
             "Knowledge Based Services" = data.kbs, "Logistics Management Services" = data.lms, 
             "Medical Services" = data.ms, "Transportation Services" = data.ts, "Research & Development" = data.rd)
data.total_services <- rbind(data.cs, data.ecs, data.ers, data.frs, data.kbs, data.lms, data.ms, data.ts, data.rd)



data.total_services_title <- list %>%
  lapply(function(x) mutate_all(x, funs('as.character'))) %>%
  bind_rows(.id = "Service Type")

write.csv(data.total_services, "Total Services DHS FY17-18 as of 9-20.csv")

data.total_services_title$`Transaction Value` = as.numeric(data.total_services_title$`Transaction Value`)

DHS_services <- data.total_services_title %>% 
  group_by(`Service Type`, `Fiscal Year`) %>% 
  summarise(sum = sum(`Transaction Value`))

DHS_services$Service_Type_Factor = factor(DHS_services$`Service Type`, 
                                                     levels = c("Facility Related Services",
                                                                "Electronic & Communication Services", "Knowledge Based Services", 
                                                                "Equipment Related Services", "Transportation Services", "Research & Development",
                                                                "Medical Services",
                                                                "Construction Services", "Logistics Management Services"))

ggplot(DHS_services, aes(x = Service_Type_Factor, y = `sum`, fill = `Fiscal Year`
                         )) +
  geom_bar(stat = "identity", position = "dodge") +
  # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
  #            fill = pors,
  #              colour = "black")+
  # scale_color_manual(values = c("black", "black"))+
  #geom_text(aes(x = `Fiscal Year`, label = round(`sum`, digits = 2)), size = 4, vjust = 1.5, check_overlap = TRUE)+
  #facet_wrap(~`Service Type`)+
  scale_color_manual(name = "Services Contract Category", values = brewer.pal(n = 11, "RdBu")[2,11]) +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "DHS Contract Services Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
    #    axis.ticks.x = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10), axis.title.x = element_blank())+
  scale_x_discrete("Service_Type_Factor", 
                     labels = c("Facility Related\nServices",
                                "Electronic &\nCommunication Services", 
                                "Knowledge Based \nServices", 
                                "Equipment Related\nServices", 
                                "Transportation\nServices",
                                "Research &\nDevelopment",
                                "Medical Services",
                                "Construction\nServices", 
                                "Logistics Management\nServices"))
############


DHS_services_components <- data.total_services_title %>% 
  group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  filter(`Funding Bureau` %in% c("Coast Guard (USCG)", "Customs and Border Protection (CBP)",
                                 "Federal Emergency Management Agency (FEMA)", "Secret Service (USSS)",
                                 "Transportation Security Administration (TSA)", "Citizenship and Immigration Services (USCIS)",
                                 "Immigration and Customs Enforcement (ICE)"))

DHS_services_components$Service_Type_Factor = factor(DHS_services_components$`Service Type`, 
                                                     levels = c("Facility Related Services",
                                                                "Electronic & Communication Services", "Knowledge Based Services", 
                                                                "Equipment Related Services", "Transportation Services", "Research & Development",
                                                                "Medical Services",
                                                                "Construction Services", "Logistics Management Services"))



# DHS_services_USCG <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Coast Guard (USCG)")
#          
# DHS_services_CBP <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Customs and Border Protection (CBP)")
# 
# DHS_services_FEMA <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Federal Emergency Management Agency (FEMA)")
# 
# DHS_services_USSS <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Secret Service (USSS)")
# 
# DHS_services_TSA <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Transportation Security Administration (TSA)")
# 
# DHS_services_USCIS <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Citizenship and Immigration Services (USCIS)")
#  
# DHS_services_ICE <- data.total_services_title %>% 
#   group_by(`Fiscal Year`, `Service Type`, `Funding Bureau`) %>% 
#   summarise(sum = sum(`Transaction Value`)/1000000) %>% 
#   filter(`Funding Bureau`  == "Immigration and Customs Enforcement (ICE)")


# ggplot(DHS_services_USCG, aes(x = `Fiscal Year`, y = `sum`, 
#                               fill = Service_Type_Factor)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
#   #            fill = pors,
#   #              colour = "black")+
#   # scale_color_manual(values = c("black", "black"))+
#   geom_text(aes(x = `Fiscal Year`, label = round(`sum`, digits = 1)), 
#             size = 4, vjust = -.5, hjust = 0,
#             position = position_dodge(width = .9))+
#   scale_fill_brewer(name = "Services Contract Category", palette = "RdYlBu") +
#   labs(x="Fiscal Year", y = "Contract Obligations (in) Millions",
#        title = "USCG Contract Services Spending")+
#   theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
#         axis.ticks.x = element_blank(), legend.position = "bottom", 
#         axis.title.x = element_blank())
# 




ggplot(DHS_services_components, aes(x = `Fiscal Year`, y = `sum`, 
                                    fill = `Funding Bureau`)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
  #            fill = pors,
  #              colour = "black")+
  # scale_color_manual(values = c("black", "black"))+
  # geom_text(aes(x = `Fiscal Year`, label = round(`sum`, digits = 1)), 
  #           size = 3, vjust = -1.6, hjust = 0,
  #           position = position_dodge(width = 0.9))+
  facet_wrap(~Service_Type_Factor, scales = "free_x")+
  scale_fill_brewer(name = "Services Contract Category", palette = "RdYlBu") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Millions",
       title = "DHS Contract Services Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        axis.ticks.x = element_blank(), legend.position = "bottom", 
        axis.title.x = element_blank(), axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), strip.text.x = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10))


ggplot(DHS_services_components, aes(x = `Service Type`, y = `sum`, fill = `Fiscal Year`
)) +
  geom_bar(stat = "identity", position = "dodge") +
  # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
  #            fill = pors,
  #              colour = "black")+
  # scale_color_manual(values = c("black", "black"))+
  #geom_text(aes(x = `Fiscal Year`, label = round(`sum`, digits = 2)), size = 4, vjust = 1.5, check_overlap = TRUE)+
  facet_wrap(~`Funding Bureau`)+
  scale_fill_brewer(name = "Services Contract Category", palette = "RdYlBu") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "DHS Contract Services Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        axis.ticks.x = element_blank(), legend.position = "bottom", axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45))

#################

DHS_USA <- read_csv("DHS Contract FY17-FY18 9-18.csv")

crosswalk <- read_csv("C:/Users/Roth/Documents/Reference Tables/DPAP Crosswalk.csv")



colnames(DHS_USA_services)

DHS_USA_services <- DHS_USA %>% 
  left_join(crosswalk, by = c("product_or_service_code" = "PSC Code")) %>% 
  select(last_modified_date, `Portfolio Group`, awarding_sub_agency_name, federal_action_obligation)

DHS_USA_services$date = as.Date(DHS_USA_services$last_modified_date, format = '%m/%d/%Y')

DHS_USA_service <- DHS_USA_services %>% 
  mutate(fiscal_year = ifelse(last_modified_date>"2016-10-01" & last_modified_date<"2017-09-30", 2017, 2018)) %>% 
  group_by(fiscal_year, `Portfolio Group`) %>% 
  summarise(sum = sum(federal_action_obligation)/1000000000)



###########################
install.packages("colorRampPalette")
library(colorRampPalette)

year_span <-read_csv("DPAP DHS FY13-FY18 9-20.csv")

DHS_service_years <- year_span %>%
  gather("fiscal_year", "contract_obs", 2:7) %>% 
  mutate("sumb" = contract_obs/1000000000)
  
DHS_service_years$Service_Type_Factor = factor(DHS_service_years$`Service Type`, 
                                                     levels = c("Facility Related Services",
                                                                "Electronic & Communication Services", "Knowledge Based Services", 
                                                                "Equipment Related Services", "Transportation Services", "Research & Development",
                                                                "Medical Services",
                                                                "Construction Services", "Logistics Management Services"))


colfunc <- colorRampPalette(c("firebrick2", "royalblue2"))
colfunc(6)


ggplot(DHS_service_years, aes(x = Service_Type_Factor, y = sumb, fill = fiscal_year
)) +
  geom_bar(stat = "identity", position = "dodge") +
  # stat_summary(aes(x = `Fiscal Year`, y = `$ billions`),
  #            fill = pors,
  #              colour = "black")+
  # scale_color_manual(values = c("black", "black"))+
  #geom_text(aes(x = `Fiscal Year`, label = round(`sum`, digits = 2)), size = 4, vjust = 1.5, check_overlap = TRUE)+
  #facet_wrap(~`Service Type`)+
  scale_fill_brewer(name = "Services Contract Category", palette = "RdBu")+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = "DHS Contract Services Spending")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        #    axis.ticks.x = element_blank(), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10), axis.title.x = element_blank())+
  scale_x_discrete("Service_Type_Factor", 
                   labels = c("Facility Related\nServices",
                              "Electronic &\nCommunication Services", 
                              "Knowledge Based \nServices", 
                              "Equipment Related\nServices", 
                              "Transportation\nServices",
                              "Research &\nDevelopment",
                              "Medical Services",
                              "Construction\nServices", 
                              "Logistics Management\nServices"))
