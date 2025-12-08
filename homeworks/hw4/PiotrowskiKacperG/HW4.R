library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(scales)
library(ggdist)
library(tidyr)
library(patchwork)
library(showtext)
library(glue)
library(ggtext)

font_add_google("Roboto Condensed", "roboto_c")
font_add_google("Roboto", "roboto")
font_add_google("Merriweather", "merriweather")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2025, week = 44)

flint_mdeq <- tuesdata$flint_mdeq
flint_vt <- tuesdata$flint_vt

df_mdeq <- flint_mdeq %>% 
  mutate(
    source = "MDEQ"
  )

df_vt <- flint_vt %>% 
  mutate(
    source = "Virginia Tech"
  )

combined_data <- bind_rows(df_mdeq, df_vt)

pct90_values <- combined_data %>%
  group_by(source) %>%
  summarise(p90 = round(quantile(lead, 0.9), 1)) %>% 
  mutate(lab = paste0(source, ": ", p90, " ppb"))

plot_theme <- theme(
  plot.margin = margin(t = 80, r = 40, b = 80, l = 80),
  
  plot.title.position = "plot",
  plot.caption.position = "plot",
  
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_line(color = "gray80", linetype = 1),
  

  axis.text.x = element_text(
    family = "roboto_c", 
    size = 36,
    color = "gray30", 
    margin = margin(t = 0)
  ),
  
  axis.title.y = element_text(
    family = "roboto_c",
    size = 35,
    margin = margin(l = 20, r = 5),
    face = "bold",
    color = "gray30",
    vjust = -5
  ),
  
  axis.title.x = element_text(
    family = "roboto_c",
    size = 35,
    margin = margin(r = 20, t = 20),
    face = "bold",
    color = "gray30"
  ),
  
  axis.ticks.y = element_line(color = "transparent"),
  axis.ticks.length.y = unit(1.5, "cm"),
  axis.ticks.x = element_line(color = "transparent"),
  axis.ticks.length.x = unit(1.5, "cm"),
  
  plot.title = element_text(
    family = "merriweather", 
    face = "bold",
    color = "black",
    size = 44, 
    hjust = 0, 
    margin = margin(b = 20)
  ),
  
  plot.subtitle = element_markdown(
    family = "roboto", 
    face = "italic",
    color = "gray30",
    size = 32, 
    hjust = 0, 
    margin = margin(b = 60),
    lineheight = 1.4
  ),
  legend.position = "none"
  
)

epa_line <- geom_vline(xintercept = 15, linetype = "dashed", color = "#D32F2F", size = 2)

colors <- c("MDEQ" = "#E66100", "Virginia Tech" = "#2C7BB6")

p1 <- ggplot(combined_data, aes(x = lead, y = source)) +
  
  coord_cartesian(
    clip = "off",
    ylim = c(1, 2.4)
  ) + 
  
  scale_y_discrete(expand = expansion(mult = c(0.3, 0.5))) +
  
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  
  geom_jitter(
    aes(color = source),
    height = 0.02,
    width = 0,
    size = 10,
    alpha = 0.4
  ) +
  
  geom_boxplot(
    aes(color = source, fill = source),
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.5,
    position = position_nudge(y = 0.12)
  ) +
  
  
  stat_halfeye(
    aes(fill = source),
    adjust = 0.5,
    .width = 0,
    point_colour = NA,
    scale = 0.6,
    alpha = 0.6,
    position = position_nudge(y = 0.2) 
  ) +
  
  epa_line +
  
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 1),
    breaks = c(0, 1, 5, 15, 50, 100),
    labels = c("0", "1", "5", "15", "50", "100"),
    expand = c(0.01, 0)
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    title = "Comparative Analysis of Lead Concentrations in Water Samples",
    subtitle = "Distribution of measurements by source: Virginia Tech vs. MDEQ"
    ) +
  
  theme_minimal() +
  plot_theme +
  theme(
    axis.text.y = element_text(
      family = "roboto_c", 
      size = 32,
      color = "gray30",
      vjust = -2.10,
      margin = margin(r = 0)
    )
  )

p2 <- ggplot(combined_data, aes(lead, color = source)) +
  
  coord_cartesian(
    clip = "off",
    ylim = c(0, 0.95)
  ) +
  
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  
  stat_ecdf(
    aes(color = source),
    linewidth = 5,
    alpha = 0.7
    ) +
  
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 1),
    breaks = c(0, 1, 5, 15, 50, 100),
    labels = c("0", "1", "5", "15", "50", "100"),
    expand = c(0.01, 0)
  ) +
  
  scale_y_continuous(labels = label_percent(),
                     breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0, 0.1))
  ) +
  
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "gray40", linewidth = 2) +
  
  geom_point(
    data = pct90_values,
    aes(p90, 0.9, fill = source, color = source),
    shape = 21, size = 9, stroke = 1.3, show.legend = FALSE
  ) +
  
  geom_label_repel(
    data = pct90_values,
    aes(
      x = p90,
      y = 0.9,
      label = lab,
      color = source
    ),
    nudge_y = c(-0.07, -0.07),
    nudge_x = c(0.2, 0.5),
    label.padding = unit(0.5, "lines"),
    label.size = 0,
    show.legend = FALSE,
    size = 10,
    segment.size = 2,
    family = "roboto_c"
    ) +
  
  epa_line +
  
  annotate("text",
           x = 16, y = 0.7,
           label = "EPA Action Level:\n15 ppb (requires\nintervention)",
           color = "#D32F2F", size = 10, hjust = 0, fontface = "bold", family = "roboto_c"
  ) +
  
  annotate("text",
           x = 0.5, y = 0.93,
           label = "90% of homes had lead levels below this line",
           color = "gray30", size = 10, hjust = 0, family = "roboto_c"
  ) +
  
  labs(
    x = "Lead Concentration (ppb)",
    y = NULL,
    title = "Cumulative Distribution of Lead Levels",
    subtitle = "Shows the percentage of samples below a specific concentration threshold"
  ) +
  
  theme_minimal() +
  plot_theme +
  theme(
    axis.text.y = element_text(
      family = "roboto_c", 
      size = 32,
      color = "gray30",
      margin = margin(r = 0)
    )
  )

combined_plots <- p1 / p2

combined_plots <- combined_plots +
  plot_annotation(
    title = "Discrepancies in Reported Lead Levels in Flint, Michigan (2015)",
    subtitle = "Comparison of water samples collected by Virginia Tech (n = 271) and MDEQ (n = 71)",
    caption = glue::glue(
      "**Note:** EPA health goal is 0 ppb. The red dashed line represents the EPA Action Level of 15 ppb. Samples exceeding this threshold indicate potentially unsafe water.<br><br>",
      "**Unit:** ppb (parts per billion) on a pseudo-logarithmic scale.<br><br>",
      "#TidyTuesday: 2025 Week 44 **&bull;** Source: Loux and Gibson (2018)."
      ),
    
    theme = theme(
      plot.title = element_text(
        family = "merriweather", 
        face = "bold", 
        size = 56,
        hjust = 0,
        margin = margin(b = 15, t = 15)
      ),
      plot.subtitle = element_text(
        family = "roboto",
        face = "italic",
        size = 36, 
        color = "gray30",
        hjust = 0,
        margin = margin(b = 30)
      ),
      plot.caption = element_markdown(
        family = "roboto_c", 
        size = 24, 
        color = "gray50", 
        hjust = 0,
        margin = margin(t = 30)
      ),
      plot.margin = margin(t = 80, r = 40, b = 80, l = 80),
    )
  )

ggsave("final.png", plot = combined_plots,
       width = 3000, height = 4000,
       dpi = 72, units="px",
       bg = "white", limitsize = FALSE)

