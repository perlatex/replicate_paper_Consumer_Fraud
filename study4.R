library(tidyverse)

d <- haven::read_sav("./data/Study 4.sav")
d
sjPlot::view_df(d)



d %>% 
  group_by(Selfish) %>% 
  summarise(
    n  = n(),
    M  = mean(SelfCheck),
    SD = sd(SelfCheck)
  )


d %>% 
  rstatix::t_test(SelfCheck ~ Selfish)



mod <- glm(Accident	 ~ Selfish*ZEI,
           data = d,
           family = binomial(link = "logit")
)

broom::tidy(mod)
mod %>% 
  gtsummary::tbl_regression()



mod <- lm(PriceTag ~ Cond + CEISStd + Cond * CEISStd, data = d)
summary(mod)



mod %>% 
  gtsummary::tbl_regression()




##################################################################
library(lavaan)

model <- '

     GSR      ~ a1 * Cond + a2 * CEISStd + a3 * Cond : CEISStd 
     PriceTag ~ c1 * Cond + c2 * CEISStd + c3 * Cond : CEISStd + b * GSR


    
'


fit <- sem(model, data = d)



fit %>% 
  parameterEstimates(ci = TRUE) %>% 
  filter(op %in% c("~")) %>%  
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





mod %>% 
  marginaleffects::slopes(
    variables = "CEISStd",
    by        = "Cond"
  ) %>% 
  as_tibble()


