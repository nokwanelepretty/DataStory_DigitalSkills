# Digital skills: retrieve Eurostat data
library(tidyverse)
library(eurostat)

skills_raw <- get_eurostat(
  "isoc_sk_dskl_i21",
  time_format = "num",
  filters = list(
    indic_is = "I_DSK2_BAB",
    unit = "PC_IND"
  )
)

print(head(skills_raw))


# Select our age groups and EU geography
age_codes <- c(
  "Y16_24", "Y25_34", "Y35_44",
  "Y45_54", "Y55_64", "Y65_74"
)

eu_codes <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK",
  "EE", "FI", "FR", "DE", "EL", "HU", "IE",
  "IT", "LV", "LT", "LU", "MT", "NL", "PL",
  "PT", "RO", "SK", "SI", "ES", "SE"
)

skills_age <- skills_raw |>
  filter(
    ind_type %in% age_codes,
    geo %in% c(eu_codes, "EU27_2020"),
    time %in% c(2021, 2023, 2025)
  )

print(
  skills_age |>
    group_by(time) |>
    summarise(
      rows = n(),
      geographies = n_distinct(geo),
      missing_values = sum(is.na(values))
    )
)

# Identify missing estimates
print(
  skills_age |>
    filter(is.na(values)) |>
    select(geo, ind_type, time, values)
)

# Save data for reuse
dir.create("data", showWarnings = FALSE)

write_csv(skills_raw, "data/digital_skills_raw.csv")
write_csv(skills_age, "data/digital_skills_age_eu.csv")

print(file.exists(
  c("data/digital_skills_raw.csv",
    "data/digital_skills_age_eu.csv")
))

# Redesign: one clear visual story
story_colours <- c(
  "Estonia" = "#55DCCE",
  "Germany" = "#FFD080",
  "Romania" = "#EF9A91",
  "EU-27 benchmark" = "#AAB7CA"
)

end_labels <- plot_data |>
  filter(ind_type == "Y65_74") |>
  mutate(
    label = paste0(country, "  ", sprintf("%.1f%%", values))
  )

age_story <- ggplot(
  plot_data,
  aes(
    x = as.numeric(age), y = values,
    group = country, colour = country
  )
) +
  geom_line(
    data = filter(plot_data, geo == "EU27_2020"),
    linewidth = 0.9, linetype = "dashed"
  ) +
  geom_line(
    data = filter(plot_data, geo != "EU27_2020"),
    linewidth = 1.6
  ) +
  geom_point(size = 3.2) +
  geom_text(
    data = end_labels,
    aes(x = 6.25, label = label),
    hjust = 0, size = 4.3, fontface = "bold"
  ) +
  annotate(
    "text", x = 2, y = 94,
    label = "Estonia · 86.0%",
    colour = "#55DCCE", size = 4.5,
    fontface = "bold"
  ) +
  scale_colour_manual(values = story_colours) +
  scale_x_continuous(
    breaks = 1:6,
    labels = levels(plot_data$age),
    limits = c(0.8, 8.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  labs(
    title = "The digital advantage\nchanges with age.",
    subtitle = paste0(
      "Estonia exceeds Germany at ages 16–54. ",
      "Germany leads at ages 55–74.\n",
      "Share with basic or above-basic digital skills · 2025"
    ),
    x = "AGE GROUP",
    y = NULL,
    caption = paste0(
      "SOURCE  Eurostat · isoc_sk_dskl_i21 · I_DSK2_BAB\n",
      "Survey estimates based on reported digital activities, ",
      "not a direct skills test.\n",
      "Six age groups, ages 16–74. People aged 75+ are excluded. ",
      "Lines connect age groups, not changes over time."
    )
  ) +
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    plot.background = element_rect(
      fill = "#111D2E", colour = NA
    ),
    panel.background = element_rect(
      fill = "#111D2E", colour = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "#2B3749", linewidth = 0.4
    ),
    legend.position = "none",
    axis.text = element_text(colour = "#C7CFDB", size = 12),
    axis.title.x = element_text(
      colour = "#AAB7CA", size = 10,
      margin = margin(t = 18), hjust = 0
    ),
    plot.title = element_text(
      family = "serif", size = 36,
      colour = "#F8F3E9", lineheight = 1.05,
      margin = margin(b = 18)
    ),
    plot.subtitle = element_text(
      colour = "#C7CFDB", size = 14,
      lineheight = 1.35, margin = margin(b = 28)
    ),
    plot.caption = element_text(
      colour = "#AAB7CA", size = 10,
      hjust = 0, lineheight = 1.4,
      margin = margin(t = 25)
    ),
    plot.margin = margin(32, 30, 28, 32)
  )

print(age_story)

ggsave(
  "images/digital_skills_age_story.png",
  age_story,
  width = 13, height = 9, dpi = 300,
  bg = "#111D2E"
)