library(tidyverse)

# Read our saved age data
skills_age <- read_csv(
  "data/digital_skills_age_eu.csv",
  show_col_types = FALSE
)

age_codes <- c(
  "Y16_24", "Y25_34", "Y35_44",
  "Y45_54", "Y55_64", "Y65_74"
)

# Prepare the 27-country view for 2025
europe_2025 <- skills_age |>
  filter(time == 2025, geo != "EU27_2020") |>
  mutate(
    age = factor(
      ind_type,
      levels = age_codes,
      labels = c(
        "16–24", "25–34", "35–44",
        "45–54", "55–64", "65–74"
      )
    ),
    highlighted = geo %in% c("DE", "EE", "RO")
  )

# Check the data before drawing
print(
  europe_2025 |>
    summarise(
      countries = n_distinct(geo),
      age_groups = n_distinct(age),
      estimates = n(),
      missing = sum(is.na(values))
    )
)

# Country names for the circular visual
country_names <- c(
  AT = "Austria", BE = "Belgium", BG = "Bulgaria",
  HR = "Croatia", CY = "Cyprus", CZ = "Czechia",
  DK = "Denmark", EE = "Estonia", FI = "Finland",
  FR = "France", DE = "Germany", EL = "Greece",
  HU = "Hungary", IE = "Ireland", IT = "Italy",
  LV = "Latvia", LT = "Lithuania", LU = "Luxembourg",
  MT = "Malta", NL = "Netherlands", PL = "Poland",
  PT = "Portugal", RO = "Romania", SK = "Slovakia",
  SI = "Slovenia", ES = "Spain", SE = "Sweden"
)

# Arrange countries by skills among people aged 65–74
country_order <- europe_2025 |>
  filter(ind_type == "Y65_74") |>
  arrange(desc(values)) |>
  pull(geo)

ring_data <- europe_2025 |>
  mutate(
    country = unname(country_names[geo]),
    position = match(geo, country_order),
    band = as.integer(age),
    cell = row_number()
  )

# Draw each country-age cell as a curved segment
ring_cells <- purrr::map_dfr(
  seq_len(nrow(ring_data)),
  function(i) {
    d <- ring_data[i, ]
    angles <- seq(
      (d$position - 1) * 2 * pi / 27 + 0.012,
      d$position * 2 * pi / 27 - 0.012,
      length.out = 25
    )
    inner <- 2.5 + (d$band - 1) * 0.48
    outer <- inner + 0.44

    tibble(
      x = c(inner * sin(angles),
            outer * sin(rev(angles))),
      y = c(inner * cos(angles),
            outer * cos(rev(angles))),
      cell = d$cell,
      values = d$values
    )
  }
)

country_labels <- ring_data |>
  distinct(geo, country, position, highlighted) |>
  mutate(
    theta = (position - 0.5) * 2 * pi / 27,
    x = 5.7 * sin(theta),
    y = 5.7 * cos(theta),
    rotation = 90 - theta * 180 / pi,
    rotation = if_else(
      rotation < -90, rotation + 180, rotation
    )
  )

europe_ring <- ggplot() +
  geom_polygon(
    data = ring_cells,
    aes(x, y, group = cell, fill = values)
  ) +
  geom_text(
    data = country_labels,
    aes(
      x, y, label = country, angle = rotation,
      colour = highlighted, fontface = highlighted
    ),
    size = 3.6,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("FALSE" = "#BAC5D3", "TRUE" = "#FFE0A3")
  ) +
  scale_discrete_manual(
    aesthetics = "fontface",
    values = c("FALSE" = "plain", "TRUE" = "bold")
  ) +
  scale_fill_gradientn(
    colours = c(
      "#673B70", "#B85580", "#EE998A",
      "#F4D5A0", "#8BD6C0", "#24A99F"
    ),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    name = "Basic or above-basic digital skills"
  ) +
  annotate(
    "text", x = 0, y = 0.65,
    label = "EUROPE'S\nDIGITAL DIVIDE",
    family = "serif", fontface = "bold",
    size = 8, lineheight = 1.1, colour = "#FFF1D9"
  ) +
  annotate(
    "text", x = 0, y = -0.65,
    label = "27 countries · 6 age groups\n2025",
    size = 4.2, lineheight = 1.5, colour = "#BAC5D3"
  ) +
  coord_equal(
    xlim = c(-6.6, 6.6), ylim = c(-6.6, 6.6),
    clip = "off"
  ) +
  labs(
    title = "Who is being left behind?",
    subtitle = paste0(
      "One segment per EU country. Six age bands per segment.\n",
      "Read from the inside out: 16–24, 25–34, 35–44, ",
      "45–54, 55–64, 65–74."
    ),
    caption = paste0(
      "Source: Eurostat · isoc_sk_dskl_i21 · I_DSK2_BAB\n",
      "Colour represents a percentage within each age group; ",
      "segment size does not represent population.\n",
      "Countries ordered by the 65–74 estimate. ",
      "Survey-based indicator; ages 75+ excluded."
    )
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(
      fill = "#171D30", colour = NA
    ),
    plot.title = element_text(
      family = "serif", size = 32,
      colour = "#FFF1D9", hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 12, colour = "#BAC5D3",
      hjust = 0.5, lineheight = 1.4,
      margin = margin(t = 12, b = 12)
    ),
    legend.position = "bottom",
    legend.title = element_text(colour = "#FFF1D9", size = 11),
    legend.text = element_text(colour = "#BAC5D3"),
    legend.key.width = grid::unit(2, "cm"),
    plot.caption = element_text(
      colour = "#BAC5D3", size = 9,
      hjust = 0, lineheight = 1.4,
      margin = margin(t = 15)
    ),
    plot.margin = margin(25, 35, 25, 35)
  )

print(europe_ring)

dir.create("images", showWarnings = FALSE)
ggsave(
  "images/europe_digital_skills_circle.png",
  europe_ring,
  width = 13, height = 14, dpi = 300,
  bg = "#171D30"
)

# Lift our three selected countries out of the circle
highlight_positions <- ring_data |>
  distinct(position, highlighted) |>
  mutate(
    theta = (position - 0.5) * 2 * pi / 27,
    offset = if_else(highlighted, 0.42, 0),
    dx = offset * sin(theta),
    dy = offset * cos(theta)
  )

lifted_cells <- ring_cells |>
  left_join(
    ring_data |> select(cell, position),
    by = "cell"
  ) |>
  left_join(
    highlight_positions |> select(position, dx, dy),
    by = "position"
  ) |>
  mutate(x = x + dx, y = y + dy)

lifted_labels <- country_labels |>
  left_join(
    highlight_positions |> select(position, dx, dy),
    by = "position"
  ) |>
  mutate(x = x + dx, y = y + dy)

# Update the existing visual
europe_ring$layers[[1]]$data <- lifted_cells
europe_ring$layers[[2]]$data <- lifted_labels

europe_ring <- europe_ring +
  labs(
    subtitle = paste0(
      "2025 · Age bands from inside to outside:\n",
      "16–24 / 25–34 / 35–44 / 45–54 / 55–64 / 65–74"
    ),
    caption = NULL
  )

print(europe_ring)

ggsave(
  "images/europe_digital_skills_circle.png",
  europe_ring,
  width = 11, height = 12, dpi = 300,
  bg = "#171D30"
)

# Make the three selected segments clearly stand apart
larger_offsets <- ring_data |>
  distinct(position, highlighted) |>
  mutate(
    theta = (position - 0.5) * 2 * pi / 27,
    distance = if_else(highlighted, 0.95, 0),
    dx = distance * sin(theta),
    dy = distance * cos(theta)
  )

lifted_cells <- ring_cells |>
  left_join(
    ring_data |> select(cell, position),
    by = "cell"
  ) |>
  left_join(larger_offsets, by = "position") |>
  mutate(x = x + dx, y = y + dy)

lifted_labels <- country_labels |>
  left_join(
    larger_offsets |> select(position, dx, dy),
    by = "position"
  ) |>
  mutate(x = x + dx, y = y + dy)

# Rebuild both drawing layers explicitly
europe_ring$layers[[1]] <- geom_polygon(
  data = lifted_cells,
  aes(x, y, group = cell, fill = values)
)

europe_ring$layers[[2]] <- geom_text(
  data = lifted_labels,
  aes(
    x, y, label = country,
    angle = rotation, colour = highlighted
  ),
  fontface = "bold",
  size = 3.6,
  show.legend = FALSE
)

print(europe_ring)

ggsave(
  "images/europe_digital_skills_lifted.png",
  europe_ring,
  width = 11, height = 12, dpi = 300,
  bg = "#171D30"
)