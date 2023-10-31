library(tidyverse)

d <- haven::read_sav("./data/Study5b.sav")
d %>% glimpse()

sjPlot::view_df(d)

# d_b
# Cond         : 0 = control / 1 = Guilt
# Return       : 1 = Kepp / 2 = Return
# ZCEIS        : EI
# ZSF          : selfishness
# GuiltMC_All  : guilt manipulation check




d %>% 
  summarise(
    n  = n(),
    M  = mean(GuiltMC_All, na.rm = TRUE),
    SD = sd(GuiltMC_All, na.rm = TRUE),
    .by = Cond
  ) %>% 
  flextable::flextable()



d %>% 
  mutate(Cond = factor(Cond, level = c(1, 0))) %>% 
  rstatix::t_test(GuiltMC_All ~ Cond)




library(processR)
processR::pmacroModel(3)
processR::statisticalDiagram(3)



d1 <- d %>% mutate(Return = Return - 1)

mod1 <- glm(Return ~ ZSF * ZCEIS * Cond,
           data = d1,
           family = binomial(link = "logit")
)

broom::tidy(mod1)
mod1 %>% 
  gtsummary::tbl_regression()

mod1 %>% 
  flextable::as_flextable()


cor(d$ZCEIS, d$ZSF)
d %>% 
  rstatix::cor_test(vars = c(ZCEIS, ZSF))


library(interactions)

mod1 %>%
  interactions::johnson_neyman(
    pred = "ZCEIS",
    modx = "ZSF",
    alpha = 0.05
  )




