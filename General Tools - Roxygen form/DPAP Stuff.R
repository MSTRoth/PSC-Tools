#DoD_Branch_DPAP <- function(){}
#
library(scales)
library(tidyverse)
library(RColorBrewer)


setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories") #location charts are saved

data <- read_csv("S:/1 Marielle Folder/Data Sets/By Agency/DOD/DoD services, by DPAP.csv")

data_edit <- data %>%
  rename(amount = `Total (in millions)`) %>% 
  filter(Type != "Products") %>% 
  #filter(Year != "FY18") %>% 
  #mutate(amountB = amount/1000) %>% 
  #group_by(Service) %>% 
  mutate(prop = 100*amount/sum(amount))

data_edit$amount = as.numeric(data_edit$amount)
# data_edit$Type = factor(data_edit$Type, levels = c("Knowledge Based Services", "Research and Development",
#                                                    "Equipment Related Services","Facility Related Services",
#                                                    "Electronic & Communication Services", "Transportation Services",
#                                                    "Logistics Management Services", "Construction Services",
#                                                    "Medical Services"))
data_edit$Service[data_edit$Service == "Defense-wide"] = "DoD, Other"
data_edit$Service = factor(data_edit$Service, levels = c("Air Force", "Army", "Navy", "DoD, Other"))

services_list<- split(data_edit, data_edit$Service)
list2env(services_list,envir=.GlobalEnv) 


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


plot<-lapply(services_list, function(xy){
 ggplot(xy, aes(x = Year , y = amount, fill = Type)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = sprintf('%0.1f', round(amount, digit = 1))), vjust = -.5, size = 3.5, fontface = "bold")+
  #geom_text(aes(label = sprintf('%0.1f%%', prop)), vjust = 1.25, size = 3, fontface = "bold") +
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+
  scale_fill_manual(name = "Services Taxonomy", data_edit$Type, values = c("Knowledge Based Services" = "mediumorchid3", "Research and Development" = "mediumseagreen",
                                               "Equipment Related Services" = "firebrick1","Facility Related Services" = "goldenrod2",
                                               "Electronic & Communication Services" = "steelblue3", "Transportation Services" = "palevioletred1",
                                               "Logistics Management Services" = "lightseagreen", "Construction Services" = "olivedrab2",
                                               "Medical Services" = "orangered1")) +
  #scale_fill_manual(Type, palette = "Dark1") +
  facet_grid(~Service, scales = "free_x", drop = TRUE) +   #labeller = label_wrap_gen(20)) +
  #scale_x_discrete(labels = molten[, setNames(as.character(id), ord)])+
  labs(y="Amount (in Millions)", title = "DoD Services Taxonomy by Branch")+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.title.x = element_blank(),
        axis.title.y = element_text (size = 14), axis.text = element_text(size = 12), axis.ticks.x = element_blank())

}
)

print(plot)

lapply(names(plot), function(x) {
  ggsave(filename = paste(x," DPAP DOD by service.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 6.5, units = "in")
})

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=9))
plot2 <-lapply(services_list, function(xy){
  
  ggplot(xy, aes(fill = Year,
                                   x = Year,
                                   y = amount))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = xy$Service,
       subtitle = "FY10-FY18", x = NULL)+
  facet_grid(~Type, labeller = label_wrap_gen(10), scales = "free")+
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
        #legend.text = element_text(size = 11),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)

})


lapply(names(plot2), function(x) {
  ggsave(filename = paste(x," DPAP - spectrum.jpg", sep = ""), plot=plot2[[x]],
         width = 13, height = 6.5, units = "in")
})

print(plot2)


setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/Defense")

ggsave("DPAP DOD by service FY18 (as of 8-27).jpg", plot,
       width = 15, height = 8, units = "in") 



unique(data_edit$Type)

data_amt <- data_edit %>% 
  group_by(Type) %>% 
  summarise(sum = sum(amount)) %>% 
  arrange(desc(sum))

