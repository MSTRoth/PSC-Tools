###Top vendors
setwd("C:/Users/Roth/Documents/Charts/Internal Requests/Top Vendor Charts")


data.HUD <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/HUD data 2014-2018.csv") 

colnames(data.HUD)

data.top.10 <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  arrange(desc(sum)) %>% 
  head(10) %>% 
  mutate(round.sum = round(sum, digits = 1)) %>% 
  mutate(factor.vendor = factor(`Parent Vendor`, `Parent Vendor`))

levels(data.top.10$factor.vendor) <- gsub(" ", "\n", levels(data.top.10$factor.vendor))


plot <- ggplot(data.top.10, aes(x = factor.vendor, y = round.sum)) +
  geom_bar(stat = "identity", fill = "steelblue1") +
  geom_text(aes(label = round.sum, vjust = 1.5), size = 3) +
  labs(x="Top 10 Vendors", y = "Contract Obligations (in) Millions", 
       title = "Top 10 Vendors for HUD - FY14-FY18") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
         axis.ticks.x = element_blank())


ggsave("HUD top 10 vendors FY14-FY18.jpg", plot, 
       width = 11, height = 6, units = "in")

data.top.10.txt <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  filter(`Fiscal Year` != 2018) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)) %>% 
  arrange(desc(sum)) %>% 
  head(10) 

##Top Vendors in DPAP Categories


data.top.10.IT <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  filter(str_detect(BGOVMarkets, "Technology Services")) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  arrange(desc(sum)) %>% 
  head(10) %>% 
  mutate(round.sum = round(sum, digits = 1)) %>% 
  mutate(factor.vendor = factor(`Parent Vendor`, `Parent Vendor`))

levels(data.top.10.IT$factor.vendor) <- gsub(" ", "\n", levels(data.top.10.IT$factor.vendor))




plot.IT <- ggplot(data.top.10.IT, aes(x = factor.vendor, y = round.sum)) +
  geom_bar(stat = "identity", fill = "palegreen") +
  geom_text(aes(label = round.sum, vjust = 1.5), size = 3) +
  labs(x="Top 10 Vendors", y = "Contract Obligations (in) Millions", 
       title = "Top 10 Vendors for HUD - FY14-FY18") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
        axis.ticks.x = element_blank())

ggsave("HUD, IT top 10 vendors FY14-FY18.jpg", plot.IT, 
       width = 11, height = 6, units = "in")


data.top.10.IT.txt <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  filter(`Fiscal Year` != 2018) %>% 
  filter(str_detect(BGOVMarkets, "Technology Services")) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)) %>% 
  arrange(desc(sum)) %>% 
  head(10) 



data.top.10.kbs <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  filter(str_detect(BGOVMarkets, "Knowledge-Based Services")) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  arrange(desc(sum)) %>% 
  head(10) %>% 
  mutate(round.sum = round(sum, digits = 1)) %>% 
  mutate(factor.vendor = factor(`Parent Vendor`, `Parent Vendor`))

levels(data.top.10.kbs$factor.vendor) <- gsub(" ", "\n", levels(data.top.10.kbs$factor.vendor))


plot.kbs <- ggplot(data.top.10.kbs, aes(x = factor.vendor, y = round.sum)) +
  geom_bar(stat = "identity", fill = "firebrick1") +
  geom_text(aes(label = round.sum, vjust = 1.5), size = 3) +
  labs(x="Top 10 Vendors", y = "Contract Obligations (in) Millions", 
       title = "Top 10 Vendors for HUD - FY14-FY18") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), 
         axis.ticks.x = element_blank())


ggsave("HUD, KBS top 10 vendors  FY14-FY17.jpg", plot.kbs, 
       width = 11, height = 6, units = "in")


data.top.10.kbs.txt <- data.HUD %>% 
  select(c(7, 8, 14, 23, 36, 53, 61, 62, 63)) %>% 
  filter(`Fiscal Year` != 2018) %>% 
  filter(str_detect(BGOVMarkets, "Knowledge-Based Services")) %>% 
  group_by(`Parent Vendor`) %>% 
  summarise(sum = sum(`Transaction Value`)) %>% 
  arrange(desc(sum)) %>% 
  head(10) 
            
            