DOJ_DOD <- read_csv("DHS and DOJ Comparison by Quarter 2017-2018.csv")

DOJ_DOD$Year = as.character(DOJ_DOD$Year)

ggplot(DOJ_DOD, aes(x=Quarter, y = `Total Obligations`, fill = factor(Year)))+
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = `Total Obligations`), vjust = -.25, position = position_dodge(0.9), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2017" = "steelblue1", "2018" = "orangered")) +
  facet_grid(~Agency, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions", x = element_blank()) 


HHS_VA <- read_csv("HHS and VA Comparison by Quarter 2017-2018.csv")

HHS_VA$Year = as.character(HHS_VA$Year)

ggplot(HHS_VA, aes(x=Quarter, y = `Total Obligations`, fill = factor(Year)))+
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = `Total Obligations`), vjust = -.25, position = position_dodge(0.9), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2017" = "steelblue1", "2018" = "orangered")) +
  facet_grid(~Agency, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions", x = element_blank()) 
