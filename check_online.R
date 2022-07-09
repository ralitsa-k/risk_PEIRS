
library(tidyverse)
setwd('Y:/TOOLS/runBEHAV/')
temp = list.files(pattern="*.csv")
myfiles = data.frame(lapply(temp[grepl('s023',temp)], read_csv))

# Prep behav data big file ---------------------------------
temp = list.files(pattern="*.csv",include.dirs = TRUE)
myfiles = data.frame(lapply(temp[grepl('s023',temp)], read_csv))

d1<- myfiles   %>%
  select(stim1, stim2, trigger_integer, typeOfChoice, key_resp_3.keys) %>%
  mutate(correct = ifelse(trigger_integer %in% c(13,14,23,24) & key_resp_3.keys == 'left', 1,
                          ifelse(trigger_integer %in% c(31,41,32,42) & key_resp_3.keys == 'right',1, 
                                 ifelse(trigger_integer %in% c(13,14,23,24) & key_resp_3.keys == 'right', 0,
                                        ifelse(trigger_integer %in% c(31,41,32,42) & key_resp_3.keys == 'left',0,NA))))) %>%
  mutate(bin = rep(1:4, each = 20)) %>%
  na.omit() %>%
  group_by(bin) %>%
  summarise(mean_correct = mean(correct))

ggplot(d1, aes(x = factor(bin), y = mean_correct)) +
  geom_bar(stat = 'identity')
  






