library(tidyverse)

d <- haven::read_sav("./data/Study 2.sav")
d %>% glimpse()
sjPlot::view_df(d)

# ZSelfish : selfishness manipulation(1 =high, 1 = low)
#  ZEI      : Standardized EI scores
# Fraud : whether participants stole(1 = yes, 0 = no)



mod <- glm(Fraud ~ ZSelfish*ZEI,
           data = d,
           family = binomial(link = "logit")
)

mod %>% 
  gtsummary::tbl_regression()




cor(d$ZEI, d$ZSelfish)
d %>% 
  rstatix::cor_test(vars = c(ZEI, ZSelfish))



library(rstatix)

d %>% 
  select(ZEI, ZSelfish, Fraud) %>% 
  rstatix::cor_mat(method = "pearson") %>% 
  rstatix::cor_mark_significant() %>% 
  flextable::flextable() %>% 
  flextable::autofit()

library(corrplot)
d %>% 
  select(ZEI, ZSelfish, Fraud) %>% 
  corrplot::cor.mtest(conf.level = 0.95) %>% 
  purrr::pluck("p") %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  flextable::flextable() %>% 
  flextable::autofit()







library(marginaleffects)

mod %>% 
  marginaleffects::predictions(
    newdata = datagrid(ZSelfish = seq(-2, 2, by = 0.1), 
                       ZEI      = c(-1, 1)),
    type    = "response"
  ) %>% 
  as_tibble()


mod %>% 
  marginaleffects::predictions(
    newdata = datagrid(ZSelfish = seq(-2, 2, by = 0.1), 
                       ZEI      = c(-1, 1)),
    type    = "response"
  ) %>% 
  as_tibble() %>% 
  
  mutate(across(ZEI, as.factor)) %>% 
  ggplot(aes(x = ZSelfish, y = estimate,group = ZEI)) +
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
    name   = "Probability of stealing pen",
    limits = c(0, 0.5),
    expand = c(0, 0)
  ) +
  theme_classic(base_size = 16) +
  theme(axis.ticks.x = element_blank())







