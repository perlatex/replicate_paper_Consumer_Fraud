library(tidyverse)

d <- haven::read_sav("./data/Study 5a.sav")
d
d %>% glimpse()
sjPlot::view_df(d)


# Embarrass : 0/1
# Return    : 1/2
# Age       :
# Gender    :
# EmbManip  : 
# ZEI       :
# ZSelfish  :


d %>% 
  filter(if_any(everything(), is.na))

d %>% 
  group_by(Embarrass) %>% 
  summarise(
    n  = n(),
    M  = mean(EmbManip, na.rm = TRUE),
    SD = sd(EmbManip, na.rm = TRUE)
  ) %>% 
  flextable::flextable()


d %>% 
  drop_na() %>% 
  mutate(Embarrass = factor(Embarrass, level = c(1, 0))) %>% 
  rstatix::t_test(EmbManip ~ Embarrass)

library(processR)
processR::pmacroModel(1)
processR::statisticalDiagram(3)


library(processR)
processR::pmacroModel(3)
processR::statisticalDiagram(3)


cor(d$ZEI, d$ZSelfish)
d %>% 
  rstatix::cor_test(vars = c(ZEI, ZSelfish))




d1 <- d %>% mutate(Return = Return - 1)

mod1 <- glm(Return	~ ZSelfish * ZEI * Embarrass,
           data = d1,
           family = binomial(link = "logit")
)

broom::tidy(mod1)
mod1 %>% 
  gtsummary::tbl_regression()

mod1 %>% 
  flextable::as_flextable()






library(marginaleffects)

mod1 %>%
  marginaleffects::predictions(
    newdata = datagrid(
      ZSelfish  = seq(-2, 2, by = 0.1), 
      ZEI       = c(-1, 1),
      Embarrass = c(0, 1)
    ),
    type = "response"
  ) %>%
  as_tibble() %>% 
  
  mutate(across(c(ZEI, Embarrass), as.factor)) %>% 
  ggplot(aes(x = ZSelfish, y = estimate, group = ZEI)) +
  geom_line(aes(linetype = ZEI), linewidth = 2)  +
  scale_linetype_manual(
    name = NULL,
    values = c("dotted", "solid"),
    labels = c("Low EI", "High EI"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(
    name   = "Selfishness",
    breaks = c(-2, 2),
    labels = c("-2SD", "+2 SD")
  ) +
  scale_y_continuous(
    name   = "Probability of lying about product Damage",
    limits = c(0, 0.8),
    expand = c(0, 0)
  ) +
  facet_wrap(
    vars(Embarrass), 
    labeller = labeller(Embarrass = c("0" = "Control Condition", 
                                      "1" =  "High Embarrassment"))
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  )


#############################################################
# 作者把ZSelfish_x_ZEI 作为独立变量考察
d2 <- d %>%
  mutate(
    ZSelfish_x_ZEI           = ZSelfish * ZEI,
    ZSelfish_x_Embarrass     = ZSelfish * Embarrass,
    ZEI_x_Embarrass          = ZEI * Embarrass
  ) %>%
  mutate(Return = Return - 1)
d2 %>% glimpse()

mod2 <- glm(Return ~ ZSelfish + ZEI + Embarrass + ZSelfish_x_ZEI + 
             ZSelfish_x_Embarrass + ZEI_x_Embarrass + 
             ZSelfish_x_ZEI:Embarrass,
           data = d2,
           family = binomial(link = "logit")
)

broom::tidy(mod2)
mod2 %>% 
  gtsummary::tbl_regression()

#############################################################


mod2 %>% 
  marginaleffects::slopes(
    variables = "ZSelfish_x_ZEI",
    newdata   = datagrid(Embarrass = 1),
    type      = "link"
  ) %>% 
  as_tibble()


mod2 %>% 
  marginaleffects::avg_slopes(
    variables = "ZSelfish_x_ZEI",
    by        = "Embarrass",
    type      = "link"
  ) %>% 
  as_tibble()







#########################################################
library(interactions)

mod1 %>%
  interactions::johnson_neyman(
    pred = "ZEI",
    modx = "ZSelfish",
    alpha = 0.05
  )
#########################################################



