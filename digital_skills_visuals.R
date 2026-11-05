# Run from the DataStory_DigitalSkills project folder.
# Uses the saved data; does not download or overwrite source data.
library(tidyverse)

skills_age <- read_csv("data/digital_skills_age_eu.csv", show_col_types = FALSE)
age_codes <- c("Y16_24", "Y25_34", "Y35_44", "Y45_54", "Y55_64", "Y65_74")
age_labels <- c("16–24", "25–34", "35–44", "45–54", "55–64", "65–74")
stopifnot(!anyDuplicated(skills_age[c("geo", "ind_type", "time")]))
stopifnot(all(is.na(skills_age$values) | between(skills_age$values, 0, 100)))
print(skills_age |> filter(is.na(values)))

colours <- c(Estonia = "#55DCCE", Germany = "#FFD080", Romania = "#EF9A91")
bg <- "#111D2E"
ink <- "#F8F3E9"
muted <- "#C7CFDB"

story_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = bg, colour = NA),
      panel.background = element_rect(fill = bg, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#344155", linewidth = 0.35),
      text = element_text(colour = ink),
      axis.text = element_text(colour = muted, size = 11),
      axis.title = element_text(colour = muted, size = 11),
      plot.title = element_text(family = "serif", size = 25, margin = margin(b = 12)),
      plot.subtitle = element_text(size = 12, lineheight = 1.2, margin = margin(b = 18)),
      plot.caption = element_text(colour = muted, size = 10, hjust = 0,
                                  lineheight = 1.2, margin = margin(t = 18)),
      strip.text = element_text(colour = ink, face = "bold", size = 14),
      panel.spacing = grid::unit(1.2, "lines"),
      legend.position = "top", legend.text = element_text(colour = ink),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = margin(24, 28, 22, 24)
    )
}

selected <- skills_age |>
  filter(geo %in% c("EE", "DE", "RO", "EU27_2020")) |>
  mutate(age = factor(ind_type, levels = age_codes, labels = age_labels),
         country = recode(geo, EE = "Estonia", DE = "Germany", RO = "Romania",
                          EU27_2020 = "EU-27 benchmark"))

# Chart 1: each country gets space; the EU aggregate is repeated as a benchmark.
main <- selected |> filter(time == 2025, geo != "EU27_2020") |>
  mutate(country = factor(country, levels = c("Estonia", "Germany", "Romania")))
benchmark <- selected |> filter(time == 2025, geo == "EU27_2020") |>
  select(age, values) |>
  crossing(country = factor(c("Estonia", "Germany", "Romania"),
                            levels = c("Estonia", "Germany", "Romania")))
stopifnot(nrow(main) == 18, !anyNA(main$values), nrow(benchmark) == 18)

age_story <- ggplot(main, aes(age, values, group = country)) +
  geom_line(data = benchmark, aes(group = country), colour = "#AAB7CA",
            linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(colour = country), linewidth = 1.3) +
  geom_point(aes(colour = country), size = 2.8) +
  geom_text(aes(label = sprintf("%.1f", values), colour = country),
            vjust = -1.1, size = 3.4, show.legend = FALSE) +
  facet_wrap(~country, nrow = 1) +
  scale_colour_manual(values = colours, guide = "none") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .02))) +
  scale_x_discrete(expand = expansion(add = .55)) +
  labs(title = "Digital progress looks different across age groups",
       subtitle = "Basic or above-basic digital skills, 2025\nDashed line: EU-27 aggregate in each age group",
       x = "Age group", y = NULL,
       caption = "Source: Eurostat · isoc_sk_dskl_i21 · I_DSK2_BAB · PC_IND\nReported digital activities, not a direct skills test. Ages 75+ excluded.\nLines connect age groups; they do not follow individuals as they age.") +
  story_theme()

# Chart 2: compare repeated survey estimates, not individual trajectories.
years <- selected |>
  filter(geo %in% c("DE", "EE"), ind_type %in% c("Y25_34", "Y65_74")) |>
  mutate(age_panel = factor(ind_type, levels = c("Y25_34", "Y65_74"),
                            labels = c("Ages 25–34", "Ages 65–74")))
stopifnot(nrow(years) == 12, !anyNA(years$values))

reversal_story <- ggplot(years, aes(time, values, colour = country,
                                    shape = country, group = country)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("%.1f%%", values)),
            vjust = -1.1, size = 3.8, show.legend = FALSE) +
  facet_wrap(~age_panel, nrow = 1) +
  scale_colour_manual(values = colours[c("Estonia", "Germany")]) +
  scale_shape_manual(values = c(Estonia = 16, Germany = 17)) +
  scale_x_continuous(breaks = c(2021, 2023, 2025),
                     expand = expansion(add = .45)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "The younger–older reversal appears in all three years",
       subtitle = "Estonia leads at ages 25–34; Germany leads at ages 65–74.\nShare with basic or above-basic digital skills",
       x = NULL, y = NULL, colour = NULL, shape = NULL,
       caption = "Source: Eurostat · isoc_sk_dskl_i21 · I_DSK2_BAB · PC_IND\nRepeated survey estimates; different people may be sampled each year.\nQuality flags and comparability notes still require review before final publication.") +
  story_theme()

dir.create("images", showWarnings = FALSE)
dir.create("analysis", showWarnings = FALSE)
comparison <- years |>
  select(time, ind_type, geo, values) |>
  pivot_wider(names_from = geo, values_from = values) |>
  mutate(estonia_minus_germany_pp = round(EE - DE, 1)) |>
  arrange(time, ind_type)
write_csv(comparison, "analysis/germany_estonia_comparison.csv")
print(comparison)

# Explicit dimensions prevent the small Viewer pane from determining exports.
ggsave("images/digital_skills_age_panels.png", age_story,
       width = 14, height = 7.5, dpi = 300, bg = bg)
ggsave("images/digital_skills_reversal_years.png", reversal_story,
       width = 12, height = 7.5, dpi = 300, bg = bg)
print(age_story)
print(reversal_story)
message("Saved two charts in images/ and the supporting table in analysis/.")
