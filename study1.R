library(tidyverse)

d <- haven::read_sav("./data/Study 1.sav")
d %>% glimpse()
sjPlot::view_df(d)

d %>% 
  count(CoinFlip2, SelfManip) 
# SelfManip : selfishness manipulation(1 =high, 1 = low)
#  ZEI      : Standardized EI scores
# CoinFlip2 : whether participants lied(1 =lied, 0 = not lie)
d %>% 
  group_by(SelfManip) %>% 
  summarise(
    M = mean(ZEI)
  )


d %>% 
  mutate(SelfManip = factor(SelfManip, level = c(1, -1))) %>% 
  rstatix::t_test(ZEI ~ SelfManip)


mod <- glm(CoinFlip2 ~ SelfManip*ZEI,
           data = d,
           family = binomial(link = "logit")
)

broom::tidy(mod)
mod %>% 
  gtsummary::tbl_regression()


library(equatiomatic)
equatiomatic::extract_eq(mod)


# selfmanip 变化一个单位，引起log odd 变化 0.01个 单位



# 通常来说，如果VIF大于10，就表明存在多重共线性的问题
car::vif(mod, type = 'terms') # vif支持glm模型？







library(effsize)
effsize::cohen.d(ZEI ~ SelfManip, data = d)
effectsize::effectsize(mod)


library(pwr)
?pwr.f2.test()
pwr.f2.test(u=5,v=89,f2=0.1/(1-0.1),sig.level=0.05)







d %>% 
  ggplot(aes(x = CoinFlip2,  y = ZEI, color = SelfManip)) +
  geom_jitter()




library(marginaleffects)

mod %>% 
  marginaleffects::predictions(
    newdata   = datagrid(SelfManip = c(-1, 1)),
    variables = list(ZEI = "threenum"),                  
    type      = "response"
  ) %>% 
  as_tibble() %>% 
  filter(abs(ZEI) > 0.01)



sd(d$ZEI)

mod %>% 
  marginaleffects::predictions(
    newdata = datagrid(SelfManip = c(-1, 1), ZEI = c(-1, 1)),
    type    = "response"
  ) %>% 
  as_tibble()



mod %>% 
  marginaleffects::predictions(
    newdata = datagrid(SelfManip = c(-1, 1), ZEI = c(-1, 1)),
    type    = "response"
  ) %>% 
  as_tibble() %>% 
  
  mutate(across(c(SelfManip, ZEI), as.factor)) %>% 
  ggplot(aes(x = SelfManip, y = estimate)) +
  geom_col(
    aes(fill = ZEI), 
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_x_discrete(
    name =  "Selfishness Condition",
    breaks = c(-1, 1),
    labels = c("Low selfishness", "High selfishness")
  ) +
  scale_y_continuous(
    name =  "Probability to Lie in contest",
    limits = c(0, 0.4),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("gray85", "black"),
    labels = c("Low EI", "High EI"),
    guide = guide_legend(reverse = TRUE)
  ) +
  theme_classic()




mod %>% 
  marginaleffects::slopes(
    variables = "ZEI",
    by        = "SelfManip",
    type      = "link"
  ) %>% 
  as_tibble()


