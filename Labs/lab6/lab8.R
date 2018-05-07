#############################
#  Sergio I Garcia-Rios
#  Lab: Bianary models
#  sub:  data wrangling
############################


library(tidyverse)
library(haven)
library(margins)
library(magrittr)


df <-   read_dta("anes_timeseries_2016.dta")


df_tidy <- zap_labels(df)





df_tidy <- 
  df_tidy %>% 
  mutate(
    vote_trump = case_when(
      V161031 == 2 ~  1,
      TRUE ~ 0),
    male = case_when(
      V161342 == 2 ~ 0,
      V161342 == 1 ~ 1,
      TRUE ~ NA_real_),
    income = recode(V161361x, 
                    `-9` = NA_real_, 
                    `-5` = NA_real_),
    year = recode(V161267c, 
                  `-9` = NA_real_, 
                  `-8` = NA_real_),
    age  =  
      2016 - year,
    conservative = recode(V161126, 
                          `-9` = NA_real_, 
                          `-8` = NA_real_, 
                          `99` = 4),
    conservative = case_when(
      conservative == 4 & V161127 == 1 ~ 3,
      conservative == 4 & V161127 == 2 ~ 5,
      TRUE ~ as.double(conservative)))


df_tidy %>% 
  count(age) 

df %>% 
  count(V161342) 








df_tidy <- 
  df_tidy %>% 
  mutate(
    vote_trump = case_when(
      V161031 == 2 ~  1,
      TRUE ~ 0),
    male = case_when(
      V161342 == 2 ~ 0,
      V161342 == 1 ~ 1,
      TRUE ~ NA_real_),
    income = recode(V161361x, 
                    `-9` = NA_real_, 
                    `-5` = NA_real_),
    year = recode(V161267c, 
                  `-9` = NA_real_, 
                  `-8` = NA_real_),
    age  =  
      2016 - year,
    conservative = recode(V161126, 
                          `-9` = NA_real_, 
                          `-8` = NA_real_, 
                          `99` = 4), 
    conservative = case_when(
      conservative == 4 & V161127 == 1 ~ 3,
      conservative == 4 & V161127 == 2 ~ 5,
      TRUE ~ as.double(conservative)),
    white = recode( V161310a, 
                    `-9` = NA_real_, 
                    `-8` = NA_real_),
    education = recode(V161270, 
                       `-9` = NA_real_, 
                       `-8` = NA_real_, 
                       `95` = NA_real_))

df_tidy %>% 
  count(age) 

df %>% 
  count(V161361x) 



model <- glm(vote_trump ~ male + income + age + conservative +  
               education +  white,  
             family = "binomial", data = df_tidy)
summary(model)



df_tidy <- 
  df_tidy %>% 
  mutate(
    vote_trump = case_when(
        V161031 == 2 ~  1,
        TRUE ~ 0), 
    male = case_when(
      V161342 == 2 ~ 0,
      V161342 == 1 ~ 1,
      TRUE ~ NA_real_),
    income = recode(V161361x, 
                    `-9` = NA_real_, 
                    `-5` = NA_real_),
    year = recode(V161267c, 
                  `-9` = NA_real_, 
                  `-8` = NA_real_),
    age  =  
      2016 - year,
    education = recode(V161270, 
                       `-9` = NA_real_, 
                       `-8` = NA_real_, 
                       `95` = NA_real_),
    white = recode( V161310a, 
                    `-9` = NA_real_, 
                    `-8` = NA_real_),
    conservative = recode(V161126, 
                          `-9` = NA_real_, 
                          `-8` = NA_real_, 
                          `99` = 4), 
    conservative = case_when(
      conservative == 4 & V161127 == 1 ~ 3,
      conservative == 4 & V161127 == 2 ~ 5,
      TRUE ~ as.double(conservative)),
    sexism = recode(V161508, 
                    `-9` = NA_real_, 
                    `-5` = NA_real_),
    sexism = abs(6-sexism),
    aff_action_oppose = recode(V161204,
                        `-9` = NA_real_,
                        `-8` = NA_real_,
                        `1` = 1,
                        `2` = 3,
                        `3` = 2),
    econ_anx = recode(V162134,
                      `-9` = NA_real_,
                      `-8` = NA_real_,
                      `-7` = NA_real_,
                      `-6` = NA_real_),
    immig_anx = recode(V162157,
                       `-9` = NA_real_,
                       `-8` = NA_real_,
                       `-7` = NA_real_,
                       `-6` = NA_real_))


df_tidy

df_tidy %>% 
  count(immig_anx) 
df %>% 
  count(V162157) 


model <- glm(vote_trump ~ male + income + age + conservative +  education +  white,  
             family = "binomial", data = df_tidy)
summary(model)

summary(model <- 
          glm(vote_trump ~ 
                male + 
                income + 
                age + 
                conservative + 
                education + 
            sexism +
            aff_action_oppose +
              immig_anx +
            econ_anx +
            white,
            family = "binomial", data = df_tidy))



ame <- summary(margins(model)) 



ame_minmax <- summary(margins(model, change = "minmax"))

ggplot(ame_minmax, aes(x = factor, y = AME,
                ymin = lower,
                ymax = upper)) + 
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw()



ame_minmax <- summary(margins(model, change = "minmax")) 



summary(model <- 
          glm(vote_trump ~ 
                male +
                income + 
                age +
                conservative*immig_anx +
                education + 
                white +
                sexism +
                aff_action_oppose +
                econ_anx,
              family = "binomial", data = df_tidy))


xhyp <- df_tidy %$%
  expand.grid(
    conservative = seq(1,7, .01),
    immig_anx = c(1,5), 
    male = mean(male, na.rm = T),
    income = mean(income, na.rm = T), 
      age = mean(age, na.rm = T),
      education = mean(education, na.rm = T), 
      white = mean(white, na.rm = T),
      sexism = mean(sexism, na.rm = T),
      aff_action_oppose = mean(aff_action_oppose, na.rm = T),
      econ_anx = mean(econ_anx, na.rm = T)
  )


preds <- predict(model, xhyp, type = "response", se.fit = T)

preds_df <- cbind(xhyp, preds)


ggplot(preds_df, aes(x = conservative, y = fit, 
                     fill = factor(immig_anx),
                     ymin = fit - (1.96*se.fit),
                     ymax = fit + (1.96*se.fit))) +
  geom_ribbon() +
  theme_bw()


V161508 #women appreciate
V161204 # aff action
V162134 # get ahead
V162157# num of immig
