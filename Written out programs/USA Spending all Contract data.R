#current storage capacity
memory.limit()
#to increase storage capacity
memory.limit(size = 56000)

###2014 Data USA Spending####

x2014_1 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2014_all_Contracts_Full_20180618/contracts_prime_transactions_1.csv")
x2014_2 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2014_all_Contracts_Full_20180618/contracts_prime_transactions_2.csv")
x2014_3 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2014_all_Contracts_Full_20180618/contracts_prime_transactions_3.csv")

x2014_total <- rbind(x2014_1, x2014_2, x2014_3)
write.csv(x2014_total, "2014_all_contracts_full_20180618_prime_transactions.csv")
x2014 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2014_all_contracts_full_20180618_prime_transactions.csv")


###2015 Data USA Spending####

x2015_1 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2015_all_Contracts_Full_20180618/contracts_prime_transactions_1.csv")
x2015_2 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2015_all_Contracts_Full_20180618/contracts_prime_transactions_2.csv")
x2015_3 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2015_all_Contracts_Full_20180618/contracts_prime_transactions_3.csv")
x2015_4 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2015_all_Contracts_Full_20180618/contracts_prime_transactions_4.csv")
x2015_5 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2015_all_Contracts_Full_20180618/contracts_prime_transactions_5.csv")


x2015_total <- rbind(x2015_1, x2015_2, x2015_3, x2015_4, x2015_5)
write.csv(x2015_total, "2015_all_contracts_full_20180618_prime_transactions.csv")


###2016 Data USA Spending####

x2016_1 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2016_all_Contracts_Full_20180618/contracts_prime_transactions_1.csv")
x2016_2 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2016_all_Contracts_Full_20180618/contracts_prime_transactions_2.csv")
x2016_3 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2016_all_Contracts_Full_20180618/contracts_prime_transactions_3.csv")
x2016_4 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2016_all_Contracts_Full_20180618/contracts_prime_transactions_4.csv")
x2016_5 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2016_all_Contracts_Full_20180618/contracts_prime_transactions_5.csv")


x2016_total <- rbind(x2016_1, x2016_2, x2016_3, x2016_4, x2016_5)
write.csv(x2016_total, "2016_all_contracts_full_20180618_prime_transactions.csv")


###2017 Data USA Spending####

x2017_1 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2017_all_Contracts_Full_20180618/contracts_prime_transactions_1.csv")
x2017_2 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2017_all_Contracts_Full_20180618/contracts_prime_transactions_2.csv")
x2017_3 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2017_all_Contracts_Full_20180618/contracts_prime_transactions_3.csv")
x2017_4 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2017_all_Contracts_Full_20180618/contracts_prime_transactions_4.csv")
x2017_5 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2017_all_Contracts_Full_20180618/contracts_prime_transactions_5.csv")



x2017_total <- rbind(x2017_1, x2017_2, x2017_3, x2017_4, x2017_5)
write.csv(x2017_total, "2017_all_contracts_full_20180618_prime_transactions.csv")



###2018 Data USA Spending####

x2018_1 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2018_all_Contracts_Full_20180618/contracts_prime_transactions_1.csv")
x2018_2 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2018_all_Contracts_Full_20180618/contracts_prime_transactions_2.csv")
x2018_3 <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/USA Spending/2018_all_Contracts_Full_20180618/contracts_prime_transactions_3.csv")


x2018_total <- rbind(x2018_1, x2018_2, x2018_3)
write.csv(x2018_total, "2018_all_contracts_full_20180618_prime_transactions.csv")