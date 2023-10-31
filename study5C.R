library(tidyverse)

d <- haven::read_sav("./data/Study 5c.sav")

d %>% glimpse()
d %>% sjPlot::view_df()




# d_c
# Return       : 1 = Kepp / 2 = Return
# ShameMC      : 
# Cond         : 0 = control / 1 = Guilt
# ZCEIS        : EI
# ZSF          : selfishness




d %>% 
  summarise(
    n  = n(),
    M  = mean(ShameMC, na.rm = TRUE),
    SD = sd(ShameMC, na.rm = TRUE),
    .by = Cond
  ) %>% 
  flextable::flextable()


d %>% 
  mutate(Cond = factor(Cond, level = c(1, 0))) %>% 
  rstatix::t_test(ShameMC ~ Cond)




library(processR)
processR::pmacroModel(3)
processR::statisticalDiagram(3)







d1 <- d %>% mutate(Return = Return - 1)

mod1 <- glm(Return	~ ZSF * ZCEIS * Cond,
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


mod1 %>%
  interactions::johnson_neyman(
    pred = "ZCEIS",
    modx = "ZSF",
    alpha = 0.05
  )



