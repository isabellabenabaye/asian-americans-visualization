library(tidyverse)
library(cowplot)
library(ggtext)
library(ggrepel)
library(paletteer)
library(extrafont)
#fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE)

# Theme
theme_set(theme_minimal())
theme <- theme_update(text = element_text(family = "Karla", size = 13),
                      title = element_text("Karla", size = 24, color = "gray20"),
                      plot.title = element_text("Karla", face = "bold", size = 24, color = "gray20"),
                      plot.title.position = "plot",
                      plot.subtitle = element_text("Inter",size = 26, color = "gray20"),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 14),
                      axis.text.x = element_blank(),
                      axis.line.x = element_blank(),
                      axis.line.y = element_line(color = "gray80"),
                      plot.margin = margin(15, 25, 5,0 ))

# Source: https://data.census.gov/cedsci/table?g=0100043US_0400000US06&hidePreview=false&tid=ACSDT1Y2018.B02018&table=DP05&vintage=2018&cid=HCT011001&t=Asian&layer=VT_2010_040_00_PY_D1
asians_pop <- readxl::read_xlsx("data/race_data.xlsx",col_names = c("race","population"),
                               range = "C4:D26") %>% 
  mutate(group = "asians")

# Source: https://data.census.gov/cedsci/table?t=Native Hawaiian and Pacific Islander&tid=ACSDT1Y2018.B02016&hidePreview=false&vintage=2018
islander_pop <- readxl::read_xlsx("data/race_islander_data.xlsx",col_names = c("race","population"),
                                   range = "D5:E17") %>% 
  mutate(group = "islander")

# Source: https://data.census.gov/cedsci/table?q=S0201&t=031 - Asian alone or in combination with one or more other races  (400-499) %26 (100-299) or (300, A01-Z99) or (400-999)%3ARace and Ethnicity&tid=ACSSPP1Y2018.S0201&hidePreview=true&tp=false
# Asian alone or in combination with one or more other races
# Total population: 22,137,269
# One race	83.2%
# Two races	14.4%
# Three races	2.1%
# Four or more races	0.4%
pob <- readxl::read_xlsx("data/place_of_birth_citizenship.xlsx",col_names = c("place_of_birth","population"),
                                  range = "A2:B11") %>% 
  filter(!(place_of_birth %in% c("Male","Female",	"Foreign born"))) %>% 
  mutate(label = "Place of Birth") %>% 
  mutate(pct = prop.table(population))

entered <- readxl::read_xlsx("data/place_of_birth_citizenship.xlsx",col_names = c("entered","pct"),
                         range = "A15:B17") %>% 
  mutate(entered = fct_relevel(entered,c("Entered before 2000","Entered 2000 to 2009","Entered 2010 or later")))



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
pop_plot <- pop_data %>% 
  ggplot(aes(fct_reorder(race,pop),pop, fill = group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(race,pop, label = scales::comma(pop)),nudge_y = 50000, family = "Inconsolata", size = 4.5, hjust = 0) +
  labs(
    #title = "Asian and Pacific Islander American Heritage Month", 
    title = "Asian and Pacific Islander Populations", x = "", y = "") +
  scale_y_continuous(expand = expansion(0,0),labels = scales::comma, limits = c(0,5600000)) +
  scale_fill_paletteer_d("rcartocolor::Pastel") +
  coord_flip()

pob_plot <- pob %>% 
  ggplot(aes(label, population, fill = fct_reorder(place_of_birth,population))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Of the 22.1 million Asians in the US, 57.1% are foreign born",x = "", y = "") + 
  geom_text(aes(label = scales::percent(pct)), 
            position = position_stack(vjust = 0.5), family = "Karla", size = 4) + 
  #geom_label(aes(label = place_of_birth), 
             #position = position_dodge(0.9),
            #position = position_stack(vjust = 0.2),
            #family = "Karla", size = 4) +
  scale_fill_manual(values = c("#DCB0F2","#F89C74","#8BE0A4")) +
  #scale_fill_paletteer_d("rcartocolor::Pastel") +
  coord_flip() +
  theme_nothing()+
  theme(axis.line.y = element_blank(),
        plot.margin = margin(0),
        axis.text.y = element_blank(),
        plot.title = element_text("Karla", face = "bold", size = 16, color = "gray20", hjust = 0.16))

entered_plot <- entered %>% 
  ggplot(aes(entered, pct)) +
  geom_segment( aes(x=entered, xend=entered, y=0, yend=pct), size = 2, show.legend = FALSE, color = "#C9DB74") +
  geom_point(size = 5, show.legend = FALSE, color = "#C9DB74") +
  labs(title = "   Most Asians in America have been\nhere for more than 10 years",x = "", y = "") + 
  scale_x_discrete(labels = c("Entered\nbefore 2000","Entered\n2000 to 2009","Entered\n2010 or later")) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text("Karla", face = "bold", size = 14),
        plot.title = element_text("Karla", face = "bold", size = 16, color = "gray20", hjust = 0.16))

pop_plot %>% 
  ggdraw() +
  draw_plot(pob_plot,.4,.05,.5,.15) +
  draw_plot(entered_plot,.55,.25,.3,.5) +
  ggsave("asian_americans.png", device = "png", type = "cairo", width = 17, height = 9, dpi = 300)
