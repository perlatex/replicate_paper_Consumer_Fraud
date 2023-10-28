library(tidyverse)

d <- haven::read_sav("./data/Study 3.sav")
d
sjPlot::view_df(d)

# CEISStd  : emotional intelligence
# GSR      : Physiological Arousal
# Cond     : Selfishness (1 =high, -1 = low)
# PriceTag : Willingness to switch price Tag
# SelfCks  : selfishness manipulation check

d %>% 
  group_by(Cond) %>% 
  summarise(
    n  = n(),
    M  = mean(SelfCks),
    SD = sd(SelfCks)
  )


d %>% 
  rstatix::t_test(SelfCks ~ Cond)



mod <- lm(PriceTag ~ Cond + CEISStd + Cond * CEISStd, data = d)
summary(mod)


mod %>% 
  gtsummary::tbl_regression()


# 作者此图有问题，柱状图要从0开始
library(marginaleffects)

mod %>% 
  marginaleffects::predictions(
    newdata = datagrid(Cond    = c(-1, 1), 
                       CEISStd = c(-1, 1))
  ) %>% 
  as_tibble() %>% 
  
  mutate(across(c(Cond, CEISStd), as.factor)) %>% 
  ggplot(aes(x = CEISStd, y = estimate)) +
  geom_col(
    aes(fill = Cond), 
    position = position_dodge(width = 0.6),
    width = 0.5
  ) +
  scale_x_discrete(
    name =  "Selfishness Condition",
    breaks = c(-1, 1),
    labels = c("Low selfishness", "High selfishness")
  ) +
  scale_y_continuous(
    name =  "Intentions to Switch Price Tag",
    limits = c(0, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("gray85", "black"),
    labels = c("Low EI(-1 SD)", "High EI(+1 SD)")
  ) +
  theme_classic(base_size = 16) +
  theme(axis.ticks.x = element_blank())








mod %>% 
  marginaleffects::slopes(
    variables = "CEISStd",
    by        = "Cond"
  ) %>% 
  as_tibble()



##################################################################
library(lavaan)

model <- '

  GSR      ~ a1 * Cond + a2 * CEISStd + a3 * Cond : CEISStd 
  PriceTag ~ c1 * Cond + c2 * CEISStd + c3 * Cond : CEISStd + b * GSR

  # Index of Moderated Mediation
  IndexOfModMed := a3*b

  # Simple Slopes
  aSSLow  := a2 + a3 * (-1)    
  aSSHigh := a2 + a3 * (1)     
   
  # Conditional Indirect Effects
  abLow  := aSSLow*b
  abHigh := aSSHigh*b

'




fit <- sem(model, 
           data      = d, 
           estimator = "ML", 
           se        = "bootstrap",
           bootstrap = 1000,
           mimic     = "Mplus")


fit %>% 
  parameterEstimates(ci = TRUE) %>% 
  filter(op %in% c("~", ":=")) %>%  
  select(-lhs, -op, -rhs) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()
##################################################################



##################################################################
# run PROCESS
# put the process.R in the same folder as this RMD file.
source("process.R")

process(data = d, y = "PriceTag", x = "Cond", m = "GSR", w = "CEISStd",
        model = 8)
##################################################################


