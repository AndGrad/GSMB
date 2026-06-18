##################################################################
##                    Donations in session 1                    ##
##################################################################

## load environment
source("scripts/load_environment.R")

## mean and sd for reporting
mean(data_donations$prop_donations)
sd(data_donations$prop_donations)

## plot proportion of individual donations
ggplot(data = data_donations, aes(x = prop_donations)) +
  geom_histogram(
    aes(#y = stat(density)/length(unique(prop_donations$ID)),
      #fill = study
    ),
    bins = 20,
    color = 'black',
    fill = 'orange'
  ) +
  labs(
    # subtitle = 'Each participants starts with 24 tokens in pilot, 30 in other 2 studies',
    x = 'proportion of donated tokens',
    y = 'count')+
  theme_classic() +
  #facet_wrap(~study) +
  guides(fill = 'none')+
  theme(text = element_text(size = 20)) 

ggsave("plots/donations_w1.png", height = 4)


## distribution of donations across age groups
data_donations %>% 
  ggplot(., aes(x = factor(age), y = prop_donations)) +
  geom_half_point(
    aes(#y = stat(density)/length(unique(prop_donations$ID)),
      #fill = study
    ),
    color = 'orange',
    #fill = 'orange',
    alpha = .6
  ) +
  geom_half_boxplot(
    aes(#y = stat(density)/length(unique(prop_donations$ID)),
      #fill = study
    ),
    color = 'black',
    fill = 'orange'
  ) +
  stat_summary()+
  labs(#title = 'Age difference in donations',
    # subtitle = 'Each participants starts with 24 tokens in pilot, 30 in other 2 studies',
    x = 'age',
    y = 'proprtion donated tokens')+
  theme_classic() +
  #facet_wrap(~study) +
  guides(fill = 'none')+
  theme(text = element_text(size = 20)) 

## save the plot
ggsave("plots/age_donations_w1.png", height = 4)


## make a table
## donation rates by age
table_age_donations <- data_donations %>% 
  group_by(age) %>% 
  summarise(n_participants = n(),
            mean_donations = round(mean(prop_donations),2),
            sd = round(sd(prop_donations),2)) 

## make a table
ft <- flextable(head(table_age_donations)) 

ft <- set_header_labels(ft, age = "Age", 
                        n_participants = "N", mean_donations = "Mean Donations", sd = "SD")

ft <- set_table_properties(ft, layout = "autofit")
save_as_docx(ft, path = "tables/donations_by_age.docx" )

## run a regression

if (file.exists('modelfits/donations_by_age.RData')){
  
  base::load('modelfits/donations_by_age.RData')
  
} else {
  
  donations_by_age <- data_donations %>% 
    brm(prop_donations ~ age_scaled , data = .)
  
  save("donations_by_age", file = "modelfits/donations_by_age.RData")
}

tab_model(donations_by_age)

