#############################
#  Sergio I Garcia-Rios
#  Lab: Bianary models
#  sub:  data wrangling
############################



# Load libraries


library(tidyverse)
library(haven)
library(margins)
library(magrittr)

# Read Data

df <-   read_dta("anes_timeseries_2016.dta")



# Remove labels from Stata

df_tidy <- zap_labels(df)



# Recode variables of interest, note the different ways to recode 

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


# I like to check my recodes as I go to make sure that I am doing it right

df_tidy %>% 
  count(age) 

df %>% 
  count(V161342)  

# We are now ready to fit a  model with basic controls

model <- glm(vote_trump ~ male + income + age + conservative +  
               education +  white,  
             family = "binomial", data = df_tidy)
summary(model)




# Let's now add other vaiables of interest



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


# Now Average marginal effects


ame <- summary(margins(model)) 


# And also min-max probabilty changes


ame_minmax <- summary(margins(model, change = "minmax"))


# the summary of margins outputs a very tidy dataframe thrat we can put into ggplot easily

ggplot(ame_minmax, aes(x = factor, y = AME,
                       ymin = lower,
                       ymax = upper)) + 
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_bw()



# We can also get predicted probabilities using predict the way we have done it before



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



# And of course, plot it with ggplot

ggplot(preds_df, aes(x = conservative, y = fit, 
                     fill = factor(immig_anx),
                     ymin = fit - (1.96*se.fit),
                     ymax = fit + (1.96*se.fit))) +
  geom_ribbon() +
  theme_bw()

