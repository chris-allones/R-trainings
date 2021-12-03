
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggrepel)
library(viridis)
library(readxl)

extrafont::font_import()
extrafont::loadfonts(device = "win")

data <- read_csv("Economist_Assignment_Data.csv") %>% 
  clean_names() %>% 
  select(-x1)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", 
                   "Japan", "New Zealand", "Singapore", "Philippines")

clr <- c("tomato",  "steelblue2",  "indianred1", "seagreen", "lightskyblue1", "brown")

ggplot(data = data, 
       mapping = aes(x = cpi, y = hdi)) +
  geom_point(size = 3, fill = "white", 
             shape = 21, stroke = 1.3,
             aes(color = region)) +
  geom_smooth(aes(group = 1, fill = "R^2= 52%"),
              method = "lm",
              se = FALSE,
              formula = y ~ log(x),
              color = "tomato") +
  geom_text_repel(aes(label = country),
                  data = subset(data, country %in% pointsToLabel)) + 
  scale_color_manual(values = clr) +
  scale_x_continuous(breaks = seq(1, 10, 1),
                     limits = c(1, 10)) +
  scale_y_continuous(breaks = seq(0.2, 1.0, 0.1),
                     limits = c(0.2, 1.0)) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 1),
        legend.position = "top",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0),
        axis.ticks.x = element_line(),
        axis.line.x = element_line(color = "gray", lineend = "round")) +
  guides(color = guide_legend(nrow = 1)) +
  labs(title = "Corruption and human development",
       caption = "Sources: Transparancy International; UN Development Report",
       x = expression(italic("Corruption Perception Index, 2011 (10 = least corrup)")),
       y = expression(italic("Human Development Index, 2011 (1 = best"))
  )


# PSA Plot

psa_data <- read_excel("psa_gdp_rate.xlsx") %>% 
  mutate(quarter_year = paste(quarter, year)) %>% 
  select(quarter_year, gdp_rate) %>% 
  mutate(row = row_number())
ggplot(data = psa_data,
       mapping = aes(x = reorder(quarter_year, row), y  = gdp_rate, group = 1)) +
  geom_hline(yintercept = 0, color = "deepskyblue", size = 0.7) +
  geom_point(size = 4, color = "dodgerblue3") +
  geom_segment(aes(y = 0, yend = gdp_rate, xend = quarter_year),
               data = subset(psa_data, gdp_rate < 0),
               color = "dodgerblue3",
               linetype = "dashed") +
  geom_line(size = 0.7, color = "dodgerblue3") +
  geom_text_repel(aes(label = gdp_rate),
                  data = subset(psa_data, gdp_rate > 0),
                  nudge_y = 1.4) +
  geom_text(aes(label = gdp_rate),
                  data = subset(psa_data, 
                                gdp_rate <= 0),
                  nudge_y = -1.7,
                  nudge_x = -0.1) +
  theme_minimal(base_family = "serif") +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 20, 
                                  colour = "dodgerblue4",
                                  margin = margin(b = 10),
                                  face = "bold"),
        plot.margin = margin(0.5, 1, 1, 1, "cm"),
        panel.grid = element_blank(),
        axis.text.x  = element_text(vjust = 119, size = 8),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing = unit(c(2, 0.5, 0.5, 0.5), "cm"),
        plot.caption = element_text(hjust = 0, size = 14)) +
  labs(title = "GROSS DOMESTIC PRODUCT (AT CONSTANT 2018 PRICES)\nYEAR-ON-YEAR GROWTH RATES \nQ1 2018-2019 TO Q3 2020-2021")
































