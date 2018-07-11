#### Market Briefing Visualizations: general graphs#####

####Contract Obligations Comparison by Quarter, civilian vs Defense####
cont.oblig.comp <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", ) %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(funding_agency == "Department of Defense (DOD)"|
           funding_agency == "Department of Health and Human Services (HHS)"|
           funding_agency == "Department of Transportation (DOT)"|
           funding_agency == "Department of Veterans Affairs (VA)" |
           funding_agency == "Department of Homeland Security (DHS)") %>% 
  dplyr::group_by(fiscal_year, funding_agency) %>% 
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))

data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)