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
library(stargazer)


# Read Data
df <-   read_dta("anes_timeseries_2016.dta")

# Remove labels from Stata

df2 <- zap_labels(df)



# Recode variables of interest, note the different ways to recode 


df_tidy <- 
  df2 %>% 
  mutate(
    vote_trump = case_when(
      V161031 == 2 ~  1,
      V161031 == -1 ~ NA_real_,
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

model1 <- glm(vote_trump ~ male + 
               income + 
               age + 
               conservative +  
               education +  
               white,  
             family = "binomial", data = df_tidy)


summary(model1)

# Let's now add other vaiables of interest like econ anx

summary(model2 <- 
          glm(vote_trump ~ 
                male + 
                income + 
                age + 
                conservative + 
                education + 
                econ_anx +
                white,
              family = "binomial", data = df_tidy))


# Now a full model

summary(model3 <- 
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


# WE are done with models but before we move on let's create a nice table for our paper/poster

df_tab <- df_tidy %>%
  select("vote_trump",
  "male", 
  "income", 
  "age",
  "conservative", 
  "education",
  "sexism",
  "aff_action_oppose",
  "immig_anx",
  "econ_anx",
  "white"
  ) %>% 
  as.data.frame()

var_names_dec <- c("Vote for Trump", 
               "Male",
               "Income", 
               "Age", 
               "Conservative", 
               "Education",
               "Sexism",
               "Oppose Aff Action",
               "Immigration Axiety",
               "Economic Anxiety",
               "White")

# When you give stargazer a data-frame instead of a lm-object
# it will create a nice table with desicriptive statistics

stargazer(df_tab, 
          covariate.labels = var_names_dec,
          title = "Descriptive Statistics for Variables Used",
          out = "descriptives.tex", 
          label = "tab: desc",
          single.row = TRUE)




var_names <- c( "Male",
                "Income", 
                "Age", 
                "Conservative", 
                "Education",
                "Sexism",
                "Oppose Aff Action",
                "Immigration Axiety",
                "Economic Anxiety",
                "White")



stargazer(model1, model2, model3, 
          covariate.labels = var_names, 
          dep.var.labels =  "Vote for Trump",
          title = "Logistic Regression: Trump something something",
          out = "regressions.tex", 
          label = "tab: regs",
          single.row = TRUE
          )

# Now Average marginal effects


ame <- summary(margins(model)) 


# And also min-max probabilty changes


ame_minmax <- summary(margins(model3, change = "minmax"))


# the summary of margins outputs a very tidy dataframe thrat we can put into ggplot easily

ggplot(ame_minmax, aes(x = factor %>% 
                         fct_relevel( "econ_anx", "immig_anx", "sexism") %>% 
                         fct_rev(), 
                       y = AME,
                       ymin = lower,
                       ymax = upper)) + 
  scale_x_discrete(labels=c(male = "Male",
                            income = "Income", 
                            age  = "Age", 
                            conservative  = "Conservative", 
                            education = "Education",
                            sexism   = "Sexism",
                            aff_action_oppose  = "Oppose Aff Action",
                            immig_anx  = "Immigration Axiety",
                            econ_anx = "Economic Anxiety",
                            white  = "White")) +
  geom_pointrange(size = 1, alpha = .7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "") +
  coord_flip() + 
  theme_bw() 








ggplot(ame_minmax, aes(x = factor %>%
                         fct_relevel( "econ_anx", "immig_anx", "sexism") %>% 
                         fct_rev(), 
                       y = AME,
                       ymin = lower,
                       ymax = upper)) + 
  geom_pointrange(size = 1, alpha = .7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  scale_x_discrete(labels=c(male = "Male",
                            income = "Income", 
                            age  = "Age", 
                            conservative  = "Conservative", 
                            education = "Education",
                            sexism   = "Sexism",
                            aff_action_oppose  = "Oppose Aff Action",
                            immig_anx  = "Immigration Axiety",
                            econ_anx = "Economic Anxiety",
                            white  = "White")) +
  labs(y = "Average Marginal Effect (Min-Max Change): Vote for Trump",
       x = "") + 
  theme_bw() +
  ggsave("ame_plot.pdf", width = 8, height = 7) 




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
    conservative = seq(1, 7, .01),
    immig_anx = c(1, 5), 
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
                     color = factor(immig_anx),
                     ymin = fit - (1.96*se.fit),
                     ymax = fit + (1.96*se.fit))) +
  geom_ribbon(alpha = .7) +
  geom_line() +
  scale_fill_brewer(palette = "Accent",
                   labels  = c("Low", "High")) +
  scale_color_brewer(palette = "Accent",
                    labels  = c("Low", "High")) +
  labs(x= "Consevative Level", 
       fill = "Immig Anxiety",
       color = "Immig Anxiety",
       y = "(Pr) Voting for Trump") +
  theme_bw() +
  ggsave("preds.pdf", width = 8, height = 7)






ggplot(preds_df, aes(x = conservative, y = fit, 
                     color = factor(immig_anx),
                     ymin = fit - (1.96*se.fit),
                     ymax = fit + (1.96*se.fit))) +
  geom_pointrange(size = 1) +
  theme_bw()




# Now let's make a map


states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault)) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5)

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault / murder)) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5)



