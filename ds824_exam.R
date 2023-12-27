#### DS824 rapport ####

library(haven)
library(dplyr)
library(magrittr)
library(knitr)
library(ggplot2)
library(stargazer)
library(plm)
library(data.table)
library(modelsummary)
library(lmtest)

# Hent data
data <- read_dta("divorce data.dta") %>%
  as_tibble() %>%
  rename(first_treat = `_nfd`)

#### Opg. 2 ####
opg2_data <- data %>%
  filter(first_treat == 1973 | is.na(first_treat)) %>%
  mutate(treated = if_else(is.na(first_treat), 0, 1),
         treated = as.factor(treated),
         sub_grp = if_else(treated == 1, 1, if_else(treated == 0 & post == 1, 2, 0)),
         after_73 = if_else(year >= 1973, 1, 0)
         )

# Table 1 (middel asmrs værdier pr gruppe)
t1.1 <- opg2_data %>%
  group_by(sub_grp, after_73) %>%
  summarise(mean_asmrs = mean(asmrs))

t1.2 <- opg2_data %>%
  group_by(treated, after_73) %>%
  summarise(mean_asmrs = mean(asmrs))

# DiD model
model1_DiD <- lm(asmrs ~ treated + after_73 + treated:after_73, data = opg2_data)
summary(model1_DiD)
confint(model1_DiD)

# Figure 1 (middel asmrs pr year pr gruppe og DiD fitted values)
fig_1 <- opg2_data %>%
  mutate(y_hat = predict(model1_DiD, opg2_data)) %>%
  group_by(treated, year, y_hat) %>%
  summarise(mean_asmrs = mean(asmrs)) %>%
  ggplot(aes(x = year, y = mean_asmrs, group = treated)) +
  geom_line(aes(linetype = treated)) +
  scale_linetype_manual(values=c("twodash", "dashed")) +
  labs(linetype = 'Gruppe') +
  scale_linetype_discrete(labels = c('Control', 'Treatment')) +
  geom_vline(xintercept = 1973, linetype = 'dashed') +
  ylab("middel asmrs") +
  geom_line(aes(x = year, y = y_hat, color = 'red')) +
  labs(colour = 'Fitted values') +
  scale_color_discrete(labels=c('DiD-model'))

# Falsifikationstest
falsitest_data <- opg2_data %>%
  filter(year < 1973) %>%
  mutate(
    after_68 = if_else(year >= 1968, 1, 0),
    after_70 = if_else(year >= 1970, 1, 0)
  )

falsification_68 <- lm(asmrs ~ treated + after_68 + treated:after_68, data = falsitest_data)
summary(falsification_68); confint(falsification_68)

falsification_70 <- lm(asmrs ~ treated + after_70 + treated:after_70, data = falsitest_data)
summary(falsification_70); confint(falsification_70)


#### Opg. 3 ####
always_treated_states <- data %>%
  filter(is.na(first_treat) & post == 1) %>%
  distinct(stfips)

opg3_data <- data %>%
  mutate(
    year_numeric = year,
    year = as.factor(year),
    stfips = as.factor(stfips),
    first_treat = if_else(is.na(first_treat), 0, first_treat),
    time_to_treat = if_else(first_treat != 0, year_numeric- first_treat, NA_real_),
    time_to_treat_f = as.factor(time_to_treat),
    time_to_treat_f = relevel(time_to_treat_f, ref = 21),
    treated = as.factor(if_else(first_treat == 0, 0, 1))
  )


# Figure 2 (distribution ad rollouts for treatment stater)
fig_2 <- opg3_data %>%
  filter(treated == 1 & year == 1964) %>%
  mutate(first_treat = as.factor(first_treat)) %>%
  group_by(first_treat) %>%
  summarise(n = n()) %>%
  mutate(n = as.factor(n)) %>%
  ggplot(aes(x = first_treat, y = n)) +
  geom_bar(stat = 'identity') +
  xlab(label = 'År for indførelse af ny skilsmisselovgivning') +
  ylab(label = 'n stater')


# Table 2 (t_0> and t_0<=  middel asmrs for treatment stater)
t2 <- opg3_data %>%
  filter(treated == 1) %>%
  mutate(period = if_else(time_to_treat < 0, 0, 1)) %>% # 0 = pre, 1 = post
  group_by(period) %>%
  summarise(mean_asmrs = mean(asmrs))

# Figure 3 (event study graph)
fig3_model <- lm(asmrs ~ time_to_treat_f + stfips + year, data = opg3_data)

add_rows <- data.frame(
  term = "time_to_treat_f-1",
  model = "(1)",
  estimate = 0,
  std.error = 0,
  conf.low = 0,
  conf.high = 0
)

attr(add_rows, "position") = 21

x_scale_new <- sapply(seq(-21,27), function(x) paste0('t',x))

fig_3 <- modelsummary::modelplot(fig3_model,
                        coef_omit = "^year|^stfips|Intercept",
                        add_rows = add_rows) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = "time_to_treat_f-1", linetype = 'dashed', col = 'red') +
  xlab(label = 'ATT') +
  ylab(label = 'Relativ position ift. indførelse af ny skilsmisselovgivning') +
  scale_y_discrete(labels=x_scale_new)

# 2WFE model

# Undersøg om det har nogen effekt at ændre post = 1 til post = 0
# for always treated stater.
opg3_data <- opg3_data %>%
  mutate(post_new = if_else(first_treat == 0 & post == 1, 0, post))

model2_2wfe <- plm(asmrs ~ post + year,
                      effect = 'twoway',
                      model = 'within',
                      index = c('stfips','year'),
                      data = opg3_data)

summary(model2_2wfe)
confint(model2_2wfe)

























