set.seed(42) #for consistent results


library(dplyr) # to wrangle our data
library(tidyr) # to wrangle our data - pivot_longer()
library(ggplot2) # to render our graphs
library(readr) # for loading the .csv data
library(kableExtra) # to render better formatted tables
library(stargazer) # for formatting your model output

library(lmtest) # to gather our clustered standard errors - coeftest()
library(plm)  # to gather our clustered standard errors - vcovHC()

#read csv
soda_tax <- readr::read_csv("/Users/lucapennella/Desktop/Master Unifi/Policy Evaluation/Soda_Tax.csv")

soda_tax %>% head(10)

#format table
soda_tax_new<- 
  soda_tax %>% # the wide format df
  tidyr::pivot_longer(cols = c(pre_tax, post_tax),
                      names_to = "period", 
                      values_to = "soda_drank") %>% 
  dplyr::mutate(after_tax = ifelse(period == "post_tax", 1, 0)) # create dummy for period

head(soda_tax_new, 10)

summary(soda_tax)

sum(soda_tax$treatment)

# plot of cons pre-tax
ggplot(soda_tax, aes(x = pre_tax, fill = factor(treatment))) + 
  geom_density(alpha = 0.3) + # density plot with transparency
  scale_fill_manual(name = " ", 
                    values = c("#a7a8aa", "#cc0055"),
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of soft-drink consumption before the tax",
       x = "consumtion (oz)",
       y = "Density")

#plot of cons post-tax
ggplot(soda_tax, aes(x = post_tax, fill = factor(treatment))) + 
  geom_density(alpha = 0.3) + # density plot with transparency (alpha = 0.5)
  scale_fill_manual(name = " ",
                    values = c("#a7a8aa", "#cc0055"),
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of oft-drink consumption after the tax",
       x = "consumtion (oz)",
       y = "Density")

#evaluation only after-tax
after_model <- lm(post_tax ~ treatment, data = soda_tax)
stargazer(after_model, type = "text")

#insert the varaibles change 
soda_tax <- soda_tax %>%
  dplyr::mutate(change = post_tax - pre_tax) 

#did model 
did_model <- lm(change ~ treatment, data = soda_tax)
stargazer(did_model, after_model, type = "text")


did_long <- lm(soda_drank ~ treatment + after_tax + treatment*after_tax, data = soda_tax_new) #running our model

did_long_clustered_se <- coeftest(did_long, vcov=vcovHC(did_long,type="HC0",cluster="district")) #clustering out standard errors at the district level

stargazer::stargazer(did_long_clustered_se, type = "text")

#chart

soda_tax_new %>%
  dplyr::group_by(period, treatment) %>% # group to extract means of each group at each time
  dplyr::mutate(group_mean = mean(soda_drank)) %>%
  ggplot(aes(x = after_tax, y = group_mean, color = factor(treatment))) +
  geom_point() +
  geom_line(aes(x = after_tax, y = group_mean)) +
  scale_x_continuous(breaks = c(0,1)) +
  scale_color_manual(name = " ", # changes to color dimension
                     values = c("#a7a8aa", "#cc0055"),
                     labels = c("Control", "Treatment")) +
  labs(x = "Time periods", y = "Ounces per week", color = "Treatment group")+
  theme_minimal() 