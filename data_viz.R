library(tidyverse)
library(paletteer)
library(extrafont)
#fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE)

# Theme
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Karla", size = 13),
                      title = element_text("Franklin Gothic", size = 24, color = "gray20"),
                      plot.title = element_text("Karla", face = "bold", size = 24, color = "gray20"),
                      plot.title.position = "plot",
                      plot.subtitle = element_text("Inter",size = 26, color = "gray20"),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 14),
                      axis.text.x = element_blank(),
                      axis.line.x = element_blank(),
                      axis.line.y = element_line(color = "gray80"),
                      plot.margin = margin(15, 25, 15,0 ))

# Source: https://data.census.gov/cedsci/table?g=0100043US_0400000US06&hidePreview=false&tid=ACSDT1Y2018.B02018&table=DP05&vintage=2018&cid=HCT011001&t=Asian&layer=VT_2010_040_00_PY_D1
asians_pop <- readxl::read_xlsx("data/race_data.xlsx",col_names = c("race","population"),
                               range = "C4:D26") %>% 
  mutate(group = "asians")

# https://data.census.gov/cedsci/table?t=Native Hawaiian and Pacific Islander&tid=ACSDT1Y2018.B02016&hidePreview=false&vintage=2018
islander_pop <- readxl::read_xlsx("data/race_islander_data.xlsx",col_names = c("race","population"),
                                   range = "D5:E17") %>% 
  mutate(group = "islander")

# Data Prep
pop_data <- rbind(asians_pop,islander_pop) %>%  
  drop_na(population) %>% 
  mutate(population = as.numeric(str_replace_all(population,",","")),
         #race = if_else(population < 61416,"Other",race),
         race = case_when(str_detect(race,"Other") & group == "islander" ~ "Other Pacific Islander",
                          str_detect(race,"Other") & group == "asians" ~ "Other Asian",
                          TRUE ~ race),
         race = as_factor(race)) %>% 
  group_by(race,group) %>% 
  summarise(pop = sum(population)) %>% 
  ungroup()
  
# Plot
pop_data %>% 
  ggplot(aes(fct_reorder(race,pop),pop, fill = group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(race,pop, label = scales::comma(pop)),nudge_y = 50000, family = "Inconsolata", size = 4.5, hjust = 0) +
  labs(title = "    Asian and Pacific Islanders Populations", x = "", y = "") +
  scale_y_continuous(expand = expansion(0,0),labels = scales::comma, limits = c(0,5600000)) +
  scale_fill_paletteer_d("rcartocolor::Pastel") +
  coord_flip() +
  ggsave("asian_americans.png", device = "png", type = "cairo", width = 17, height = 9, dpi = 300)

