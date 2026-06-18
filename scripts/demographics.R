##################################################################
##                         Demographics                         ##
##################################################################

## load environment
source("scripts/load_environment.R")

# create dataset only with demographic variables
data_demographics <- 
  all_data %>% 
  dplyr::select(IDself, 
                gender,
                age,
                classNr) %>% 
  distinct() %>%  
  mutate(gender_dummy = ifelse(gender == "male", 1, 0))

# make a table for overall sample stats
table_dem_total <- data_demographics %>% 
  summarise(min_age = min(age, na.rm = TRUE ),
            max_age = max(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE),
            perc_male = paste(round(sum(gender_dummy, na.rm = TRUE)/n()*100,2), "%"),
            #range = c(min(age), max(age)),
            n = n()
  ) 

# make a table
table_dem <- data_demographics %>% 
  group_by(classNr) %>% 
  summarise(min_age = min(age, na.rm = TRUE ),
            max_age = max(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE),
            perc_male = paste(round(sum(gender_dummy, na.rm = TRUE)/n()*100,2), "%"),
            #range = c(min(age), max(age)),
            n_wave2 = n()
  ) 
  
  ## add column with wave 1 data
  wave1 <- map_dfr(full_edge_list, ~data.frame(
    classNr = unique(.x$classNr),
    n_wave1 = length(unique(c(.x$IDself)))
  ))
 
 table_dem <- left_join(table_dem, wave1, by = "classNr") |> 
  ## add column of total classroom members, based on names provided from teachers
  mutate(n_total = c(20, 31, 28, 17, 25, 19, 28, 29, 29, 23, 27, 23, 26, 31, 21, 28, 27, 26, 30, 48, 27, 27, 29)) |> 
  mutate(perc_class = paste(round(((n_wave1/n_total)*100),2), "%"))

  library(flextable)

## @andrea do this also for social network data; i.e. how many participants per class
flextable_df <- flextable(table_dem)

## apply APA style customization
flextable_df <- flextable_df %>%
  ## remove all borders
  border_remove() %>%
  ## add top border for the header and bottom border for the entire table
  hline_top(part = "header", border = fp_border_default(width = 2)) %>%
  hline_bottom(part = "body", border = fp_border_default(width = 1)) %>%
  hline_bottom(part = "header", border = fp_border_default(width = 1)) %>%
  ## align text to the left
  align(align = "left", part = "all") %>%
  ## set text size (optional)
  fontsize(size = 9, part = "all") %>% 
  autofit()

## export as word
save_as_docx(flextable_df, path =  "tables/demographics.docx")

## make a plot of the age and gender in the sample
all_data %>%
  select(IDself, age) %>%
  filter(age < 20) %>%
  distinct() %>%
  ggplot(aes(x = factor(age))) +
  geom_bar(color = 'black', fill = 'lightgray') +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    #   position = position_dodge2(preserve = "single", width = 1),
    vjust = -0.5,
    # hjust = -.5,
    size = 4
  ) +
  #  geom_text(stat = 'count', aes(x = factor(age),label = ..count.., vjust = -0.5,   position = position_dodge(width = 0.9),) ) +
  #scale_fill_viridis_d(option = 'plasma', name = 'gender') +
  labs(y = "Count",
       x = "Age") +
  theme_tidybayes() +
  theme(text = element_text(size = 20))+
  ylim(c(0,95))

ggsave(filename = "plots/demographics_basic.png", height = 3, width = 4)

## make a plot of the age and gender in the sample
all_data %>% 
  mutate(gender_f = ifelse(gender == "male", "Male",ifelse(gender == "female", "Female", "Other"))) %>% 
  select(IDself, age, gender_f) %>% 
  filter(age < 20) %>% 
  filter(!is.na(gender_f)) %>% 
  distinct() %>% 
  ggplot(aes(x = factor(age),
             fill = factor(gender_f))) +
  geom_bar(position = position_dodge2(preserve = "single"), width = 1) + 
  geom_text( aes(label = ..count..),
             stat = "count",
             position = position_dodge2(preserve = "single", width = 1),
             vjust = -0.5,
             # hjust = -.5,
             size = 4) +
  #  geom_text(stat = 'count', aes(x = factor(age),label = ..count.., vjust = -0.5,   position = position_dodge(width = 0.9),) ) +
  scale_fill_viridis_d(option ='plasma',name = 'Gender')+
  labs(x="Age")+
  theme_tidybayes() +
  theme(text = element_text(size = 20)) +
  ylim(c(0,55))

ggsave(filename = "plots/demographics_full.png", height = 3.5, width = 7)

