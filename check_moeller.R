
dat_moeller <- read_csv('moeller_data.csv')

unique(dat_moeller$ID)

dat_m_test <- dat_moeller %>%
  select(ID, trial,risky, typeOfChoice) %>%
  filter(risky %in% c(0,1)) 

c_risky_tr <- dat_m_test %>% 
  count(ID)






