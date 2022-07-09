
library(tidyverse)

# Imports psychopy file and creates events 

subj_id = 1
subject = 'sub-001'
project_folder= 'Z:/socPEIRS/'

# Import psychopy file
raw_dat <- read_csv(paste0(project_folder,'DATA/sourcedata/',subject,'/',subject,'_task_socPEIRS_events.csv'))

original = read_csv(paste0(project_folder,'DATA/sourcedata/',subject,'/',subject,'_events_for_psychopy.csv'))
original_prep = original %>%
  group_by(left) %>%
  mutate(mean_l = mean(left_rew_dbl), sd_l = sd(left_rew_dbl)) %>%
  group_by(right) %>%
  mutate(mean_r = mean(right_rew_dbl), sd_r = sd(right_rew_dbl)) %>%
  group_by(block) %>%
  mutate(left_tr = ifelse(mean_l > 50 & sd_l > 10, 1,
                          ifelse(mean_l > 50 & sd_l < 10, 2,
                                 ifelse(mean_l < 50 & sd_l > 10, 3,
                                        ifelse(mean_l < 50 & sd_l < 10, 4, 0))))) %>%
  mutate(right_tr = ifelse(mean_r > 50 & sd_r > 10, 1,
                           ifelse(mean_r > 50 & sd_r < 10, 2,
                                  ifelse(mean_r < 50 & sd_r > 10, 3,
                                         ifelse(mean_r < 50 & sd_r < 10, 4, 0)))))

right_averages <- original_prep %>%
  select(trigger_integer,right, mean_r, sd_r) %>%
unique()
left_averages <- original_prep %>%
  select(trigger_integer,left, mean_l, sd_l) %>%
  unique()

original_prep %>%  mutate(color1 = substr(left, -1,1))  %>%
  ggplot(aes(x = trial_n, y = left_rew_dbl, color = color1)) + geom_point() + geom_smooth() + facet_wrap(vars(block)) +
  labs(y = 'left reward', color = 'stimulus') +
  scale_color_manual(values = c('#8198c1', '#5ea358', '#db7d7d', '#9f9a35'))

original_prep %>%  mutate(color1 = substr(left, -1,1))  %>%
  ggplot(aes(x = trial_n, y = left_rew_dbl, color = color1)) + geom_violin() + facet_wrap(vars(block)) +
  labs(y = 'left reward', color = 'stimulus') +
  scale_color_manual(values = c('#8198c1', '#5ea358', '#db7d7d', '#9f9a35'))

unique(raw_dat$trigger_integer)

dt_temp <- raw_dat %>%
  mutate(ID = subj_id) %>%
  mutate(typeOfChoice = case_when(grepl("12|21", trigger_integer) ~ 1,
                                  grepl("13|31", trigger_integer) ~ 2,
                                  grepl("14|41", trigger_integer) ~ 3,
                                  grepl("23|32", trigger_integer) ~ 4,
                                  grepl("24|42", trigger_integer) ~ 5,
                                  grepl("34|43", trigger_integer) ~ 6)) %>%
  separate(trigger_integer, into = c('stim1' ,'stim2'), sep = -1, remove = FALSE) %>%
  mutate(stim_chosen = ifelse(buttonBox_2.keys == 0, stim1,
                              ifelse(buttonBox_2.keys == 1, stim2, NA)),
         reward = ifelse(buttonBox_2.keys == 0, left_rew_dbl,
                         ifelse(buttonBox_2.keys == 1, right_rew_dbl, NA))) %>%
  select(ID, block, stim1, stim2, stim_chosen, reward, trigger_integer, typeOfChoice)


dt_temp %>%
  na.omit() %>%
  ggplot(aes(x = as.factor(stim_chosen), y = reward)) +
  geom_violin() +
  geom_boxplot(width = 0.08) +
  scale_y_continuous(limits = c(0, 100))

