library(tidyverse)
library(cowplot)
library(ggtext)
library(paletteer)
library(extrafont)
#fonttable <- fonttable()
loadfonts(device = "win", quiet = TRUE)

# Theme
theme_set(theme_minimal())
theme <- theme_update(title = element_text("Karla", size = 24, color = "gray20"),
                      plot.title = element_text("Karla", face = "bold", size = 26, color = "gray50", hjust = 0.05),
                      #plot.title.position = "plot",
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 14),
                      axis.text.x = element_blank(),
                      axis.line.x = element_blank(),
                      axis.line.y = element_line(color = "gray80"),
                      plot.margin = margin(35, 30, 5, 10),
                      plot.background = element_rect(fill = "#F3F4F6", color = "#F3F4F6"),
                      plot.caption = element_text(size = 12))

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
  mutate(label = 1) %>% 
  mutate(pct = prop.table(population))

entered <- readxl::read_xlsx("data/place_of_birth_citizenship.xlsx",col_names = c("entered","pct"),
                         range = "A15:B17") %>% 
  mutate(entered = fct_relevel(entered,c("Entered before 2000","Entered 2000 to 2009","Entered 2010 or later")))



# Data Prep
pop_data <- rbind(asians_pop,islander_pop) %>%  
  drop_na(population) %>% 
  mutate(population = as.numeric(str_replace_all(population,",","")),
         race = case_when(str_detect(race,"Other") & group == "islander" ~ "Other Pacific Islander",
                          str_detect(race,"Other") & group == "asians" ~ "Other Asian",
                          TRUE ~ race),
         race = as_factor(race)) %>% 
  group_by(race,group) %>% 
  summarise(pop = sum(population)) %>% 
  ungroup()
  

# Bar chart - main plot
pop_plot <- pop_data %>% 
  ggplot(aes(fct_reorder(race,pop),pop, fill = group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(race,pop, label = scales::comma(pop)),nudge_y = 50000, family = "Inconsolata", size = 4.5, hjust = 0) +
  labs(title = "<span style = 'color:#18377A;'>Asian</span> and <span style = 'color:#4DC9CC;'>Pacific Islander</span> Populations in the United States", x = "", y = "",
    caption = "Source: Estimates from the U.S. Census Bureau’s 2018 American Community Survey") +
  scale_y_continuous(expand = expansion(0,0),labels = scales::comma, limits = c(0,5600000)) +
  scale_fill_manual(values = c("#18377A","#55DDE0")) +
  coord_flip() +
  theme(plot.title = element_markdown(),
        plot.title.position = "plot")

# Stacked bar plot
pob_plot <- pob %>% 
  ggplot(aes(label, population, fill = fct_reorder(place_of_birth,population))) +
  geom_col(show.legend = FALSE) +
  # 22.6M based on the census.gov site: https://www.census.gov/newsroom/facts-for-features/2020/aian.html
  labs(title = "Of the <span style = 'color:#353535;'>22.6 million</span><span style = 'color:#18377A;'> Asians</span>  in the US, <span style = 'color:#353535;'>57.1% are foreign born</span>",x = "", y = "") + 
  ## percent labels
  geom_text(aes(label = scales::percent(pct)),
            color = "#F3F4F6",
            fontface = "bold",
            position = position_stack(vjust = 0.5), family = "Karla", size = 5) + 
  ## category labels
  geom_label(aes(x = label, y = c(0,7462176+2000000,16963096+50000),
                 label = c("U.S. born","Foreign born;\nnaturalized U.S. citizen","Foreign born;\nnot a U.S. citizen"), 
                 color = fct_reorder(place_of_birth,population)),
             nudge_x = -0.55,
             hjust = 0,
             vjust = 1,
             fontface = "bold",
             fill = "#F3F4F6",
             family = "Karla", size = 5) +
  scale_x_continuous(expand = expansion(0,0),limits=c(-0.2,1.5)) +  ## adjusting the limits to include the labels in the plot area
  scale_fill_paletteer_d("jcolors::pal9") +   ## for the colors of the bar
  scale_color_paletteer_d("jcolors::pal9") +  ## for the color of the labels
  coord_flip() +
  theme_nothing()+
  theme(axis.line.y = element_blank(),
        plot.margin = margin(0),
        axis.text.y = element_blank(),
        plot.title = element_markdown("Karla", face = "bold", size = 20, color = "gray50", hjust = 0.22, lineheight = 1.3))

# Lollipop plot
entered_plot <- entered %>% 
  ggplot(aes(entered, pct)) +
  geom_segment( aes(x=entered, xend=entered, y=0, yend=pct), size = 2, show.legend = FALSE, color = "#FF7844") +
  geom_point(size = 5, show.legend = FALSE, color = "#FF7844") +
  geom_text(aes(label = scales::percent(pct)), 
            vjust = -1.6,
            family = "Karla", size = 5) + 
  labs(title = "Most <span style = 'color:#18377A;'>foreign born Asians</span> in America<br>have been here for more than <span style = 'color:#FF7844;'>10 years</span>",x = "", y = "") + 
  scale_x_discrete(labels = c("Entered\nbefore 2000","Entered\n2000 to 2009","Entered\n2010 or later")) +
  scale_y_continuous(expand = expansion(0,0), limits = c(0,.57)) +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text("Karla", face = "bold", size = 14),
        plot.title = element_markdown("Karla", face = "bold", size = 20, color = "gray50", hjust = 0.55, lineheight = 1.3))

pop_plot %>% 
  ggdraw() +
  draw_plot(pob_plot,.40,.045,.57,.2) +
  draw_plot(entered_plot,.5,.29,.425,.5) +
  ggsave("asian_americans.png", device = "png", type = "cairo", width = 17, height = 10, dpi = 300)