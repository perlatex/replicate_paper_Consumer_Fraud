library(tidyverse)

d <- haven::read_sav("./data/Study 4.sav")
d
d %>% glimpse()
sjPlot::view_df(d)

# Selfish   :
# Accident  : whether participants intended to commit insurance fraud(1 = Yes, 0 = No)

# Embarrass :
# Guilt
# Anxiety
# Shame
# Threat
# Risk perceptions
# Emotion reappraisal

# SelfCheck	: selfishness manipulation check
# ZEI       :



d %>% 
  group_by(Selfish) %>% 
  summarise(
    n  = n(),
    M  = mean(SelfCheck),
    SD = sd(SelfCheck)
  )


d %>% 
  mutate(Selfish = factor(Selfish, level = c(1, -1))) %>% 
  rstatix::t_test(SelfCheck ~ Selfish)




mod <- glm(Accident	 ~ Selfish*ZEI,
           data = d,
           family = binomial(link = "logit")
)

broom::tidy(mod)
mod %>% 
  gtsummary::tbl_regression()

mod %>% 
  flextable::as_flextable()




library(marginaleffects)

mod %>%
  marginaleffects::predictions(
    newdata = datagrid(
      Selfish = c(-1, 1),
      ZEI     = c(-1, 1)
    ),
    type = "response"
  ) %>%
  as_tibble() %>% 
  
  mutate(across(c(Selfish, ZEI), as.factor)) %>% 
  ggplot(aes(x = Selfish, y = estimate)) +
  geom_col(
    aes(fill = ZEI), 
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_x_discrete(
    name   = "Selfishness Condition",
    breaks = c(-1, 1),
    labels = c("Low selfishness", "High selfishness")
  ) +
  scale_y_continuous(
    name   = "Probability to commit insurance fraud",
    limits = c(0, 0.5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c("gray80", "black"),
    labels = c("Low EI(-1 SD)", "High EI(+1 SD)"),
    guide  = guide_legend(reverse = TRUE)
  ) +
  theme_classic(base_size = 16) +
  theme(axis.ticks.x = element_blank())





mod %>% 
  marginaleffects::slopes(
    variables = "ZEI",
    by        = "Selfish",
    type      = "link"
  ) %>% 
  as_tibble()





##################################################################
# 以下错误， lavaan 似乎搞不定这种类型
library(lavaan)

model <- '

  Embarrass ~ a1 * Selfish + a2 * ZEI + a3 * Selfish : ZEI 
  Accident  ~ b * Embarrass + c1 * Selfish + c2 * ZEI + c3 * Selfish : ZEI

  # Index of Moderated Mediation
  IndexOfModMed := a3 * b

  # Simple Slopes
  aSSLow  := a2 + a3 * (-1)    
  aSSHigh := a2 + a3 * (1)     
   
  # Conditional Indirect Effects
  abLow  := aSSLow * b
  abHigh := aSSHigh* b

'


d1 <- d %>% mutate(Accident = as.factor(Accident))
fit <- sem(model, 
           data      =  d1, 
           link      = "probit"
)



fit %>% 
  parameterEstimates(ci = TRUE) %>% 
  filter(op %in% c("~", ":=")) %>%  
  select(-lhs, -op, -rhs) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()
##################################################################





###########################################################################
Mediation_function_binary_outcome <- function(data_used, i) {
  # Sample a data
  data_temp <- data_used[i, ]
  
  # a path
  result_a <- lm(Embarrass ~ Selfish + ZEI + Selfish:ZEI, 
                 data = data_temp)
  
  a0 <- result_a$coefficients[1]
  a1 <- result_a$coefficients[2]
  a2 <- result_a$coefficients[3]
  a3 <- result_a$coefficients[4]
  
  # b path
  result_b <- glm(Accident ~ Embarrass + Selfish + ZEI + Selfish:ZEI, 
                  data = data_temp, 
                  family = binomial(link = "logit"))
  
  c0 <- result_b$coefficients[1]
  b  <- result_b$coefficients[2]
  c1 <- result_b$coefficients[3]
  c2 <- result_b$coefficients[4]
  c3 <- result_b$coefficients[5]

  

  # Index of Moderated Mediation
  IndexOfModMed <- a3 * b
  
  # Simple Slopes of Embarrass on ZEI
  aSSLow  <- a2 + a3 * (-1)    
  aSSHigh <- a2 + a3 * (1)     
  
  # Conditional Indirect Effects
  abLow  <- aSSLow  * b
  abHigh <- aSSHigh * b
  
   return(
     c(a1, a2, a3, c1, c2, c3, b, 
       IndexOfModMed, 
       aSSLow, aSSHigh, abLow, abHigh)
   ) 
}



library(boot)
# use boot() to do bootstrapping mediation analysis
boot_mediation <- boot(d, 
                       Mediation_function_binary_outcome, 
                       R = 5000)



rr <- boot::boot.ci(boot_mediation, index = 7, conf = 0.95, type = "perc")


####################################################
xcoef <- c("a1", "a2", "a3", "c1", "c2", "c3", "b", 
        "IndexOfModMed", "aSSLow", "aSSHigh", "abLow", "abHigh")

tibble(
  var = xcoef
) %>% 
  rowwise() %>% 
  mutate(
    across(var, fun, .unpack = TRUE)
  )

fun <- function(.x) {
  aa <- boot::boot.ci(boot_mediation, index = which(xcoef == .x), conf = 0.95, type = "perc")

  tibble(
    mean      = aa$t0,
    ci.lower  = aa$percent[4],
    ci.higher = aa$percent[5]
  )
}
####################################################


####################################################
# using
xcoef <- c("a1", "a2", "a3", "c1", "c2", "c3", "b", 
           "IndexOfModMed", "aSSLow", "aSSHigh", "abLow", "abHigh")

fun <- function(i) {
  aa <- boot::boot.ci(boot_mediation, index = i, conf = 0.95, type = "perc")
  
  tibble(
    mean      = aa$t0,
    ci.lower  = aa$percent[4],
    ci.higher = aa$percent[5]
  )
}

tibble(
  idx = 1:12,
  label = xcoef
) %>% 
  rowwise() %>% 
  mutate( fun(idx) ) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()
####################################################



####################################################
boot_mediation$t %>% 
  as.data.frame() %>% 
  set_names(
    c("a1", "a2", "a3", "c1", "c2", "c3", "b", 
      "IndexOfModMed",  "aSSLow", "aSSHigh", "abLow", "abHigh")
  ) %>% 
  as_tibble() %>% 
  map( 
    ~ tibble(
      mean      = mean(.x), 
      ci.lower  = quantile(.x, 0.025),
      ci.higher = quantile(.x, 0.975)
    )
  ) %>% 
  list_rbind(names_to = "variable") %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()
####################################################





# Normal CI:
bias <- mean(boot_mediation$t) - boot_mediation$t0
print(boot_mediation$t0 - bias + c(-1, 1) * 1.96 * sd(boot_mediation$t))


# Percentile CI:
quantile(boot_mediation$t, c(0.025, 0.975))
###########################################################################



###########################################################################
library(processR)
processR::pmacroModel(8)

# 第二种方法，来源于https://github.com/tidydatayt/binary_mediation
# 这段process.R代码 https://haskayne.ucalgary.ca/CCRAM/resource-hub
# 它竟然可以识别Y = binary，并使用logit 回归
# 如何规整结果呢？

# run PROCESS
# put the process.R in the same folder as this RMD file.
source("process.R")

process(data = d, x = "Selfish", y = "Accident", m = "Embarrass", w = "ZEI",
        model = 8)
##################################################################





