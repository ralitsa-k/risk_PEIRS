
# This version does not include gathering of csv files and creating the big file 

library(tidyverse)
library(see)
library(ggsignif)
library(wesanderson)
library(lme4)
library("RColorBrewer")

color1 = "#4d9699"
color2 = '#ff0659'
color3 = '#5f6c11'
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = wes_palette('GrandBudapest1', 3, type = c("discrete"))
)
} 

a = setdiff(c(1:32),c(11,12,16,20)) #, c(11,12,16,20))

# Add social ----------------------------------------
fix_big_data <- read_csv('full_data.csv')

# Get behav 
big_data <- fix_big_data %>%
  mutate(ID = as.numeric(as.factor(ID)),
         trialIndex = trial_cont) 

# Leave only both-high and both-low 
dat_group_EV <- big_data %>%
  filter(response != 'None') %>%
  mutate(pair_type = ifelse(typeOfChoice == 1, 'both_high',
                            ifelse(typeOfChoice == 6, 'both_low',
                                   'different'))) %>%
  ungroup() %>%
  filter(pair_type != 'different')

# Check both-hgih both-low per block 
count_EV_per_block = dat_group_EV %>%
  group_by(ID, block, pair_type) %>%
  count(block)

# Get social blocks info 
sort_social = big_data %>%
  mutate(first_observer = ifelse(group_1 == perceptual_result, 'in-group','out-group'),
         second_observer = ifelse(first_observer == 'in-group', 'out-group', 'in-group'),
         block_soc = ifelse(block == 2, first_observer,
                            ifelse(block == 3, second_observer, 'baseline'))) %>%
  mutate(pair_type = ifelse(typeOfChoice == 1, 'both_high',
                            ifelse(typeOfChoice == 6, 'both_low',
                                   'different')))


# All trial with NAs (get percentages with mean, ignore NAs, keep number of trials per bin)
dat_soc_order <- sort_social %>%
  group_by(ID, block) 


# Plot Moeller --------------------
#Fig 2 B
data_2b = sort_social %>%
  filter(response != 'None') 
  
data_2b_test = data_2b %>%
  select(ID, pair_type, risky, trial) %>%
  filter(risky %in% c(0,1)) 

count_risky_trials <- data_2b_test %>%
  count(ID)

risk_probs <- data_2b_test %>%
  group_by(pair_type, ID) %>%
  summarise(risk_chosen = mean(risky, na.rm = TRUE)) %>%
  filter(pair_type != 'different') %>%
  pivot_wider(names_from=pair_type, values_from = risk_chosen)
  
# Statistical analysis
# all blocks in one
t_r = t.test(risk_probs$both_high, risk_probs$both_low, paired = TRUE)
t_v = as.character(round(t_r$statistic,3))
t_p_v = round(t_r$p.value,3)

# block-wise
risk_probs_by_block = data_2b %>% group_by(pair_type, ID, block_soc) %>%
  summarise(risk_chosen = mean(risky, na.rm = TRUE)) %>%
  filter(pair_type != 'different') %>%
  pivot_wider(names_from=pair_type, values_from = risk_chosen)

lm_data = risk_probs_by_block %>%
  pivot_longer(c('both_high', 'both_low'), names_to= 'pair_type', values_to = 'perc_corr')

mod1 = aov(data = lm_data, perc_corr ~ pair_type*block_soc)
summary(mod1)



# Plot single
ggplot(risk_probs, aes(x = both_high, y = both_low, color = factor(ID), label = factor(ID))) +
  geom_point(size = 4) +
  geom_text(vjust = -0.5, hjust = -0.5) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0, 1), name = 'P(risky|both-high)') +
  scale_y_continuous(limits = c(0, 1), name = 'P(risky|both-low)') +
  annotate(size = 6, geom="text", x=0.6, y=0, label=paste('p = ', t_p_v), color="black")+ 
  theme(legend.position = 'none',text = element_text(size = 20))

r_1 = risk_probs_by_block %>%
  filter(block_soc == 'in-group')
t_r1 = t.test(r_1$both_high, r_1$both_low, paired = TRUE)
t_v1 = as.character(round(t_r$statistic,3))
t_p_v1 = round(t_r$p.value,3)

# Plot by block
ggplot(risk_probs_by_block, aes(x = both_high, y = both_low, color = factor(ID), label = factor(ID))) +
  geom_point(size = 4) +
  facet_grid(~block_soc) + 
  geom_text(vjust = -0.5, hjust = -0.5) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0, 1), name = 'P(risky|both-high)') +
  scale_y_continuous(limits = c(0, 1), name = 'P(risky|both-low)') +
  #annotate(size = 6, geom="text", x=0.6, y=0, label=paste('p = ', t_p_v), color="black")+ 
  theme(legend.position = 'none',text = element_text(size = 20))
  
# Fig 2 C
bins = 5
fill_b = 180/bins
bin_split <- sort_social %>%
  filter(block != is.na(block)) %>%
  select(ID,pair_type, risky,response, block,trial_id,trial_n, block_soc) %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(pair_type != 'different') %>%
  group_by(ID, pair_type, trial_bin) %>%
  summarise(perc_risky = mean(risky, na.rm = TRUE),
         sd = sd(risky, na.rm = TRUE)) 

bin_split %>%
  filter(ids == 1) %>%
ggplot(aes(x = factor(trial_bin), y = perc_risky, color = pair_type)) + 
#  geom_point(size = 4) +
  geom_abline(intercept = 0.5, slope = 0, size = 1) +
  geom_boxplot()
  geom_errorbar(aes(ymin=perc_risky-sd, ymax=perc_risky+sd), width=.1)

count_bins_per_EV = sort_social %>%
  filter(block != is.na(block)) %>%
  select(ID,pair_type, risky,response, block,trial_id,trial_n, block_soc) %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(response != is.na(response)) %>%
  ungroup() %>%
  na.omit() %>%
  count(pair_type, trial_bin) %>%
  filter(pair_type != 'different')

bin_split <- sort_social %>%
  filter(block != is.na(block)) %>%
  select(ID,pair_type, risky,response, block,trial_id,trial_n, block_soc) %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(response != is.na(response)) %>%
  group_by(ID,pair_type, trial_bin) %>%
  summarise(perc_risky = mean(risky, na.rm = TRUE)) %>%
  filter(pair_type != 'different') %>%
  mutate(trial_bin = as.factor(trial_bin))

mod11 <- aov(data = bin_split, perc_risky ~ pair_type * trial_bin + Error(ID))
summary(mod11 )
# plot risk taking both high both low across trials -----------------------
sort_social %>%
  filter(block != is.na(block)) %>%
  select(ID,pair_type, risky,response, block,trial_id,trial_n, block_soc) %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(response != is.na(response)) %>%
  group_by(block_soc, pair_type, trial_bin) %>%
  summarise(perc_risky = mean(risky, na.rm = TRUE),
            sd = sd(risky, na.rm = TRUE)) %>%
  filter(pair_type != 'different') %>%
  full_join(count_bins_per_EV) %>%
  mutate(se = sd/sqrt(n)) %>%
ggplot(aes(x = factor(trial_bin), y = perc_risky, color = pair_type)) + 
  geom_point(size = 4) +
  geom_line(aes(group = pair_type), size = 1) +
  scale_y_continuous(limits = c(0,1)) +
  geom_abline(intercept = 0.5, slope = 0) +
  facet_grid(~block_soc) +
  labs(x = 'Trial bins', y = 'P(risky)', color = 'Pair type') +
  theme(text = element_text(size = 20)) +
  geom_signif(aes(group = pair_type))
  

box_plot_dat <- sort_social %>%
  filter(block != is.na(block)) %>%
  ungroup() %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(pair_type != 'different') %>%
  filter(response != is.na(response)) %>%
  ungroup() %>%
  dplyr::group_by(ID,block_soc) %>%
  summarise(perc_risky = mean(risky, na.rm = TRUE))
  


# Logistic regression on choosing risky depending on EV and social block -------
  glm_Dat <- sort_social %>%
    filter(block != is.na(block)) %>%
    filter(pair_type != 'different') %>%
    filter(response != is.na(response)) %>%
    filter(block_soc == 'out-group')

mo1 <- glmer(data = glm_Dat, risky ~ pair_type +  (1 | ID), family = 'binomial')
summary(mo1)
plot(mo1)
test_reg <- subset(soc_risk_perc, split == "FALSE")

predict_reg <- predict(mo1, )


ggplot(box_plot_dat,aes(x = pair_type, y = perc_risky, fill = pair_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  facet_grid(~block_soc) +
  labs(x = 'Pair type', y = 'P(risky)', fill = 'Pair type') +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(angle = 20),
        legend.position = 'none')

# risky plot only social 
sort_social %>%
  filter(block != is.na(block)) %>%
  ungroup() %>%
  mutate(trial_bin = rep(rep(1:bins, each = fill_b, times = 3), times = length(subjects))) %>%
  filter(pair_type != 'different') %>%
  filter(pair_type == 'both_high') %>%
  filter(response != is.na(response)) %>%
  ungroup() %>%
  dplyr::group_by(ID,block_soc) %>%
  summarise(perc_risky = mean(risky, na.rm = TRUE)) %>%
  ggplot(aes(x = block_soc, y = perc_risky)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  labs(x = 'Pair type', y = 'P(risky)', fill = 'Pair type') +
  theme(text = element_text(size=20), 
        axis.text.x = element_text(angle = 20),
        legend.position = 'none')
  


# Plot modeled values---------------------------------

dt_latent = read_csv('dt_latent_PEIRS.csv')

dt_stat <- dt_latent %>%
  select(1:23, PEIRS_PE_s, PEIRS_abs_PE_s, PEIRS_abs_PE_o, PEIRS_PE_o) %>%
  mutate(pair_type = ifelse(typeOfChoice == 1, 'both_high',
                            ifelse(typeOfChoice == 6, 'both_low',
                                   'different')))


dt_stat %>%
  ggplot(aes(x = factor(typeOfChoice), y = PEIRS_PE_s)) +
  geom_violin()

dt_stat %>%
  ggplot(aes(x = reward, y = PEIRS_PE_o, color = factor(stim_chosen))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~block) +
  ggtitle('RK + MGP')


stat_stim_PE <- dt_stat %>%
  ungroup() %>%
  group_by(block, ID, pair_type, stim_chosen, typeOfChoice) %>%
  summarise(mean_PE = mean(PEIRS_PE_s)) 

stat_stim_PE %>%
  ggplot(aes(x = factor(pair_type, levels = c('both_high', 'different', 'both_low')), y = mean_PE, fill = pair_type)) +
  geom_violin() +
  ggtitle('RK + MGP') +
  scale_fill_manual(values = c('#999999','#e69f00','#56b4e9')) +
  geom_jitter(alpha = 0.2) +
  labs(x = 'EV type', y = 'stim PE PEIRS modelled') +
  theme(legend.position = 'none')






stat_stim_PE_ABS <- dt_stat %>%
  ungroup() %>%
  group_by(block, ID, pair_type, stim_chosen, typeOfChoice) %>%
  summarise(mean_PE = mean(PEIRS_abs_PE_s)) 
    
stat_stim_PE_ABS %>%
  ggplot(aes(x = factor(pair_type, levels = c('both_high', 'different', 'both_low')), y = mean_PE, fill = pair_type)) +
  geom_violin() +
  ggtitle('RK + MGP') +
  scale_fill_manual(values = c('#999999','#e69f00','#56b4e9')) +
  geom_jitter(alpha = 0.2) +
  labs(x = 'EV type', y = 'stim PE PEIRS modelled ABS') +
  theme(legend.position = 'none')    
    






dt_latent_M = read_csv('dt_latent_PEIRS_Moeller.csv')

dt_statM <- dt_latent_M %>%
  select(1:23, PEIRS_PE_s, PEIRS_abs_PE_s, PEIRS_abs_PE_o, PEIRS_PE_o) %>%
  mutate(pair_type = ifelse(typeOfChoice == 1, 'both_high',
                            ifelse(typeOfChoice == 6, 'both_low',
                                   'different')))

dt_statM %>%
  ggplot(aes(x = factor(typeOfChoice), y = PEIRS_PE_s)) +
  geom_violin()

dt_statM %>%
  ggplot(aes(x = reward, y = PEIRS_PE_o, color = factor(stim_chosen))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~block) +
  ggtitle('Moeller')


# Prep EEG----------------------
eeg_dt <- dt_statM %>%
  mutate(cond = ifelse(pair_type == 'both_low', 0,
                          ifelse(pair_type == 'both_high', 1, 2))) %>%
  mutate(trials_model = trial) %>%
  mutate(ID1 = subjects[ID]) %>%
  select(ID1, trial) %>%
  na.omit()

big_data_tr <- big_data %>%
  select(ID, trial) %>%
  na.omit
# Validation of the two datasets in terms of number of trials and trials to delete
ddd <- setdiff(big_data_tr$trial,eeg_dt$trial)


id_s = c(c(1:10), c(13:15), 17)

indx_no_resp = big_data %>%
  filter(response == 'None') %>%
  select(ID, trial_id)


eeg_ready_dt <- dt_statM %>%
  mutate(cond = ifelse(pair_type == 'both_low', 0,
                       ifelse(pair_type == 'both_high', 1, 2))) %>%
  mutate(ID1 = subjects[ID]) %>%
  select(ID1, cond, PEIRS_PE_s, block, trial,pair_type)

# Plot modelled stim PEs
all_colors = c("#9E9E9E",'#F3A712','#5978A9','#5978A9')

ggplot(eeg_ready_dt, aes(x = factor(pair_type, levels = c('both_high', 'different', 'both_low')), y = PEIRS_PE_s, color = pair_type)) +
  geom_violinhalf(alpha = 0.7, aes(fill = pair_type)) +
  xlab('') +
  ylab('Modelled PEIRS stim PE') +
  scale_color_manual(values = all_colors) +
  scale_fill_manual(values = all_colors) +
  theme_bw() +
  theme(text = element_text(size = 20), legend.position = 'none')

ggplot(eeg_ready_dt, aes(x = pair_type, y = PEIRS_PE_s)) +
  geom_violin() 


full_dat = tibble(ID1 = rep(subjects, each = 540),
                  trial_id = rep(1:540, times = 14),
                  trial = rep(1:180, times = 14*3),
                  block = rep(1:3, times = 14, each  =180))

new_eeg <- full_join(full_dat, eeg_ready_dt, by = c('ID1', 'trial', 'block'))%>%
  mutate(cond = ifelse(cond %in% c(1,2,0), cond, 5))

write.csv(id_s, 'sj_ids_to_test.csv')
write.csv(indx_no_resp, 'indx_no_resp.csv')
write.csv(new_eeg, 'eeg_ready_dt_full.csv')


ee1 = eeg_ready_dt %>%
  filter(ID == 1)



yys = read_csv('yy_values.csv', col_names = c('both_low', 'both_low1', 'both_high','diff_means')) %>%
  mutate(id = 1:11) %>%
  select(both_low1, diff_means, both_high,id) %>%
  rename('both_low'= 'both_low1') %>%
  pivot_longer(1:3)
  


all_colors = c("#9E9E9E",'#F3A712','#5978A9','#5978A9')
ggplot(yys, aes(x = factor(name, levels = c('both_high', 'diff_means', 'both_low')), y = value, color = name)) +
  geom_violinhalf(alpha = 0.7, aes(fill = name)) +
  geom_line(aes(group = id), alpha = 0.5) +
  geom_point(alpha = 0.9) +
  xlab('') +
  ylab('Y (EEG amplitude)') +
  scale_color_manual(values = all_colors) +
  scale_fill_manual(values = all_colors) +
  theme_bw() +
  theme(text = element_text(size = 20, color =' black'), legend.position = 'none')
  

geom_violinhalf(alpha = 0.3, aes(fill = risk_level), color = NA) +
  geom_line(aes(group = id)) +
  theme_minimal() +
  geom_point(alpha = 0.3) +
  theme(legend.position = 'none') +
  xlab('RiPE level') +
  ylab('Y') +
  ggtitle(paste(var_n[1], ' Y`s at best points across levels N = ',as.character(nrow(dat1)))) +
  annotate(geom="text", x=2, y=2, label=paste(" Y~risk_level+error(id), Anova F(", df1 ,",", df2 ,") = ", F_v ,", p = ", p_v_text ,"\n low_neg vs. zero ", t_v ,", p = ", t_p_v_text),
           color="red") +
  geom_point(aes(x = factor(risk_level, levels = c('high_neg', 'low_neg', 'zero', 'pos')), mean_risk), size = 4, color = 'black') 




  
  











#####