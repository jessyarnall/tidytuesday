library(tidyverse)
library(tidytuesdayR)
library(skimr)

#read in data manually
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


stressor_state <- stressor %>% drop_na(stress_pct) %>%
                            group_by(year, months, state) %>%
                            arrange(year, months, state, -stress_pct) %>%
                            mutate( my_rank = row_number()) %>%
                            group_by(state, stressor) %>%
                            summarise(avg_rank = mean(my_rank)) %>%
                            arrange(state, avg_rank) %>%
                            mutate( rank = row_number()) %>%
                            filter( rank == 1)
                            
          



colony_state <- colony %>% drop_na(colony_lost_pct) %>%
                    group_by(state) %>%
                    summarise(avg_col_lost_pct = mean(colony_lost_pct))


colony_stressor_state <- colony_state %>% inner_join(stressor_state, by = "state") %>%
                                       select(state, avg_col_lost_pct, stressor) %>%
                                      filter(state != 'United States')



colony_stressor_state %>% ggplot(aes(x = avg_col_lost_pct, y = reorder(state, +avg_col_lost_pct), fill = stressor)) + 
  geom_col(stat = "identity") +
  ggtitle("Average Bee Colony Loss by State Since 2015") + 
  scale_x_continuous(name = "Average Colony Loss Percentage", labels = scales::percent_format(scale = 1, accuracy=1)) +
  ylab("State") +
  labs(fill = "Top Stressor") +
  labs(caption = "Source: USDA")

ggsave("colony_stressor_state.png")

