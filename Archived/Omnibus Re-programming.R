###Omnibus Reprogramming

data <- read_csv("C:/Users/Roth/Documents/Analyzed Datasets/Omnibus Reprogramming Data/Omnibus Reprogramming Data.csv")

data$`Amount: Reprogramming Action` = as.numeric(data$`Amount: Reprogramming Action`)

by_branch <- data %>% 
  na.omit(A) %>% 
  group_by(`Increase/Decrease`,Branch) %>% 
  summarize(total_sum = sum(`Amount: Reprogramming Action`))
