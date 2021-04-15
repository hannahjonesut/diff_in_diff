library(tidyverse)
library(haven)
library(estimatr)

read_data <- function(df)
{
  full_path <- paste("https://github.com/hannahjonesut/Causal-Inference/blob/main/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw_cpscontrol <- read_data("nsw_cps_mixtape.dta") %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         educcube = educ*educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         re74sq = re74^2,
         re74cube = re74^3,
         re75sq = re75^2,
         re75cube = re75^3)


# estimating LOGIT 
quad_logit_nsw <- glm(treat ~ age + agesq + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + 
                   re74sq + re75 + re75sq + u74 + u75, 
                 family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

cube_logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                        marr + nodegree + black + hisp + re74 +
                        re74sq + re74cube + re75 + re75sq + re75cube + u74 + u75, 
                      family = binomial(link = "logit"), 
                      data = nsw_dw_cpscontrol)

logit_nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(quadpscore = quad_logit_nsw$fitted.values, cubepscore = cube_logit_nsw$fitted.values)

# mean pscore 
quad_logit_pscore_control <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(quadpscore) %>% 
  mean()

quad_logit_pscore_treated <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(quadpscore) %>% 
  mean()

cube_logit_pscore_control <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(cubepscore) %>% 
  mean()

cube_logit_pscore_treated <- logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(cubepscore) %>% 
  mean()

# histogram
logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

logit_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))

logit_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))



# estimating ols
quad_ols_nsw <- lm(treat ~ age + agesq + educ + educsq + 
                       marr + nodegree + black + hisp + re74 + 
                       re74sq + re75 + re75sq +
                       u74 + u75, data = nsw_dw_cpscontrol)

cube_ols_nsw <- lm(treat ~ age + agesq + agecube + educ + educsq + educcube +
                            marr + nodegree + black + hisp + re74 +
                            re74sq + re74cube + re75 + re75sq + 
                            re75cube + u74 + u75, data = nsw_dw_cpscontrol)

ols_nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(quadpscore = quad_ols_nsw$fitted.values, cubepscore = cube_ols_nsw$fitted.values)



# mean pscore 
quad_ols_pscore_control <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(quadpscore) %>% 
  mean()

quad_ols_pscore_treated <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(quadpscore) %>% 
  mean()

cube_ols_pscore_control <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(cubepscore) %>% 
  mean()

cube_ols_pscore_treated <- ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(cubepscore) %>% 
  mean()

# histogram
ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

ols_nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))

ols_nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))

#drop pscores <0.1 and >0.9
quad_cut_logit_data<- logit_nsw_dw_cpscontrol %>%
  filter(quadpscore > 0.1 & quadpscore < 0.9)

cube_cut_logit_data<- logit_nsw_dw_cpscontrol %>%
  filter(cubepscore > 0.1 & cubepscore < 0.9)

quad_cut_ols_data<- ols_nsw_dw_cpscontrol %>%
  filter(quadpscore > 0.1 & quadpscore < 0.9)

cube_cut_ols_data<- ols_nsw_dw_cpscontrol %>%
  filter(cubepscore > 0.1 & cubepscore < 0.9)

#repeat 1c

quad_cut_logit_data%>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

quad_cut_logit_data%>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))

cube_cut_logit_data %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))

cube_cut_logit_data %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))

#logit averages

cut_quad_logit_pscore_control <- quad_cut_logit_data  %>% 
  filter(treat == 0) %>% 
  pull(quadpscore) %>% 
  mean()

cut_quad_logit_pscore_treated <- quad_cut_logit_data %>% 
  filter(treat == 1) %>% 
  pull(quadpscore) %>% 
  mean()

cut_cube_logit_pscore_control <- cube_cut_logit_data %>% 
  filter(treat == 0) %>% 
  pull(cubepscore) %>% 
  mean()

cut_cube_logit_pscore_treated <- cube_cut_logit_data %>% 
  filter(treat == 1) %>% 
  pull(cubepscore) %>% 
  mean()


#ols

quad_cut_ols_data %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))+
  labs(title = "Propensity Score for Untreated", caption = "using Logit regression with quadratic terms")

quad_cut_ols_data %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = quadpscore))+
  labs(title = "Propensity Score for Treated", caption = "using Logit regression with quadratic terms")

cube_cut_ols_data %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))+
  labs(title = "Propensity Score for Untreated", caption = "using linear regression with quadratic terms")

cube_cut_ols_data %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = cubepscore))+
  labs(title = "Propensity Score for Treated", caption = "using linear regression with quadratic terms")

#ols averages

cut_quad_ols_pscore_control <- quad_cut_ols_data  %>% 
  filter(treat == 0) %>% 
  pull(quadpscore) %>% 
  mean()

cut_quad_ols_pscore_treated <- quad_cut_ols_data %>% 
  filter(treat == 1) %>% 
  pull(quadpscore) %>% 
  mean()

cut_cube_ols_pscore_control <- cube_cut_ols_data %>% 
  filter(treat == 0) %>% 
  pull(cubepscore) %>% 
  mean()

cut_cube_ols_pscore_treated <- cube_cut_ols_data %>% 
  filter(treat == 1) %>% 
  pull(cubepscore) %>% 
  mean()

