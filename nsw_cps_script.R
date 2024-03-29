library(tidyverse)
library(haven)
library(estimatr)

read_data <- function(df)
{
  full_path <- paste("https://github.com/hannahjonesut/diff_in_diff/blob/main/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw_cpscontrol <- read_dta("nsw_cps_mixtape.dta") %>% 
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
logit_sumstat<- logit_nsw_dw_cpscontrol %>% 
  group_by(treat)%>% 
  summarize(quad_mean = mean(quadpscore), cube_mean = mean(cubepscore),
            quad_max = max(quadpscore), cube_max = max(cubepscore),
            quad_min = min(quadpscore), cube_min = min(cubepscore))
logit_sumstat

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
ols_sumstat<- ols_nsw_dw_cpscontrol %>% 
  group_by(treat)%>% 
  summarize(ols_quad_mean = mean(quadpscore), ols_cube_mean = mean(cubepscore),
            ols_quad_max = max(quadpscore), ols_cube_max = max(cubepscore),
            ols_quad_min = min(quadpscore), ols_cube_min = min(cubepscore))
ols_sumstat

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

quad_cut_logit_data %>% 
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


sumstat_quad_cut_logit_data<- quad_cut_logit_data %>% 
  group_by(treat)%>% 
  summarize(logit_quad_mean = mean(quadpscore), 
            logit_quad_max = max(quadpscore), 
            logit_quad_min = min(quadpscore))
sumstat_quad_cut_logit_data

sumstat_cube_cut_logit_data<- cube_cut_logit_data %>% 
  group_by(treat)%>% 
  summarize(logit_cube_mean = mean(cubepscore), 
            logit_cube_max = max(cubepscore), 
            logit_cube_min = min(cubepscore))
sumstat_cube_cut_logit_data


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

sumstat_quad_cut_ols_data<- quad_cut_ols_data %>% 
  group_by(treat)%>% 
  summarize(ols_quad_mean = mean(quadpscore), 
            ols_quad_max = max(quadpscore), 
            ols_quad_min = min(quadpscore))
sumstat_quad_cut_ols_data

sumstat_cube_cut_ols_data<- cube_cut_ols_data %>% 
  group_by(treat)%>% 
  summarize(ols_cube_mean = mean(cubepscore), 
            ols_cube_max = max(cubepscore), 
            ols_cube_min = min(cubepscore))
sumstat_cube_cut_ols_data


#number 2: Using trimmed Logit with quadratic covariates

#LOGIT

cube_cut_logit_data  %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1 <- cube_cut_logit_data%>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

cube_cut_logit_data$y1 <- mean1

cube_cut_logit_data%>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0 <- cube_cut_logit_data%>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

cube_cut_logit_data$y0 <- mean0

ate <- unique(cube_cut_logit_data$y1 - cube_cut_logit_data$y0)

cube_cut_logit_data <- cube_cut_logit_data%>% 
  select(-y1, -y0)

#OLS

quad_cut_ols_data %>% 
  filter(treat == 1) %>% 
  summary(re78)

mean1 <- quad_cut_ols_data %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

quad_cut_ols_data$y1 <- mean1

quad_cut_ols_data %>% 
  filter(treat == 0) %>% 
  summary(re78)

mean0 <- quad_cut_ols_data%>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

quad_cut_ols_data$y0 <- mean0

ate <- unique(quad_cut_ols_data$y1 - quad_cut_ols_data$y0)

quad_cut_ols_data <- quad_cut_ols_data %>% 
  select(-y1, -y0)


#number 3: Construct a Weighted Difference in Difference
  
#LOGIT
#continuation
N <- nrow(cube_cut_logit_data )
#- Manual with non-normalized weights using all data
  cube_cut_logit_data  <- cube_cut_logit_data  %>% 
  mutate(d1 = treat/cubepscore,
         d0 = (1-treat)/(1-cubepscore))

s1 <- sum(cube_cut_logit_data$d1)
s0 <- sum(cube_cut_logit_data$d0)


cube_cut_logit_data <- cube_cut_logit_data %>% 
  mutate(y1 = treat * re78/cubepscore,
         y0 = (1-treat) * re78/(1-cubepscore),
         ht = y1 - y0)

#- Manual with normalized weights
cube_cut_logit_data  <- cube_cut_logit_data  %>% 
  mutate(y1 = (treat*re78/cubepscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-cubepscore))/(s0/N),
         norm = y1 - y0)

cube_cut_logit_data  %>% 
  pull(ht) %>% 
  mean()

cube_cut_logit_data  %>% 
  pull(norm) %>% 
  mean()

#OLS
N <- nrow(quad_cut_ols_data )
#- Manual with non-normalized weights using all data
quad_cut_ols_data <- quad_cut_ols_data %>% 
  mutate(d1 = treat/quadpscore,
         d0 = (1-treat)/(1-quadpscore))

s1 <- sum(quad_cut_ols_data$d1)
s0 <- sum(quad_cut_ols_data$d0)


quad_cut_ols_data <- quad_cut_ols_data %>% 
  mutate(y1 = treat * re78/quadpscore,
         y0 = (1-treat) * re78/(1-quadpscore),
         ht = y1 - y0)

#- Manual with normalized weights
quad_cut_ols_data  <- quad_cut_ols_data  %>% 
  mutate(y1 = (treat*re78/quadpscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-quadpscore))/(s0/N),
         norm = y1 - y0)

quad_cut_ols_data %>% 
  pull(ht) %>% 
  mean()

quad_cut_ols_data  %>% 
  pull(norm) %>% 
  mean()

