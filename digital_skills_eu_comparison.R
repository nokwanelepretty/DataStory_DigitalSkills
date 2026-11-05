# Milestone 3: EU-wide visual draft. Run from the project root.
# Reads the saved extract without changing it.
library(tidyverse)
x <- read_csv("data/digital_skills_age_eu.csv", show_col_types = FALSE)
country_names <- c(AT="Austria", BE="Belgium", BG="Bulgaria", HR="Croatia",
 CY="Cyprus", CZ="Czechia", DK="Denmark", EE="Estonia", FI="Finland",
 FR="France", DE="Germany", EL="Greece", HU="Hungary", IE="Ireland",
 IT="Italy", LV="Latvia", LT="Lithuania", LU="Luxembourg", MT="Malta",
 NL="Netherlands", PL="Poland", PT="Portugal", RO="Romania", SK="Slovakia",
 SI="Slovenia", ES="Spain", SE="Sweden")
d <- x |>
 filter(time == 2025, geo %in% names(country_names),
        ind_type %in% c("Y25_34", "Y65_74")) |>
 select(geo, ind_type, values)
stopifnot(nrow(d)==54, !anyNA(d$values),
          !anyDuplicated(d[c("geo", "ind_type")]))
w <- d |> pivot_wider(names_from=ind_type, values_from=values) |>
 mutate(country=unname(country_names[geo]),
        gap_pp=Y25_34-Y65_74) |> arrange(Y65_74, country) |>
 mutate(country=factor(country, levels=country))
stopifnot(nrow(w)==27, all(w$gap_pp>0))
long <- w |> pivot_longer(c(Y25_34,Y65_74), names_to="age", values_to="value") |>
 mutate(age=factor(age, levels=c("Y25_34","Y65_74"),
                   labels=c("Ages 25–34", "Ages 65–74")))
bg <- "#111D2E"
p <- ggplot(w, aes(y=country)) +
 geom_segment(aes(x=Y65_74, xend=Y25_34, yend=country),
              colour="#536176", linewidth=.8) +
 geom_point(data=long, aes(x=value, colour=age, shape=age), size=3.1) +
 # One-decimal labels sit outside the connecting segment.
 geom_text(aes(x=Y65_74, label=sprintf("%.1f",Y65_74)),
           hjust=1, nudge_x=-1.5, colour="#FFD080", size=3.3) +
 geom_text(aes(x=Y25_34, label=sprintf("%.1f",Y25_34)),
           hjust=0, nudge_x=1.5, colour="#55DCCE", size=3.3) +
 scale_colour_manual(values=c("Ages 25–34"="#55DCCE", "Ages 65–74"="#FFD080")) +
 scale_shape_manual(values=c("Ages 25–34"=16, "Ages 65–74"=17)) +
 scale_x_continuous(breaks=seq(0,100,20),
                    labels=function(z) paste0(z,"%"),
                    limits=c(-3,106), expand=expansion(mult=0)) +
 scale_y_discrete(expand=expansion(add=.7)) +
 labs(title="Older adults trail younger adults\nin every EU country",
      subtitle="Basic or above-basic digital skills · 2025\nCountries ranked by the estimate for ages 65–74; labels show percentages.",
      x="Share of people in each age group", y=NULL, colour=NULL, shape=NULL,
      caption=paste0("Source: Eurostat · isoc_sk_dskl_i21 · I_DSK2_BAB · PC_IND\n",
       "27 countries · 54 estimates. Reported digital activities, not a direct skills test.\n",
       "Comparisons concern different age groups, not individuals ageing. Ages 75+ excluded.")) +
 theme_minimal(base_size=13) +
 theme(plot.background=element_rect(fill=bg,colour=NA),
       panel.background=element_rect(fill=bg,colour=NA),
       text=element_text(colour="#F8F3E9"),
       axis.text=element_text(colour="#D7DFEA"),
       axis.text.y=element_text(size=11),
       axis.title.x=element_text(size=11,margin=margin(t=12)),
       panel.grid.minor=element_blank(),
       panel.grid.major.y=element_blank(),
       panel.grid.major.x=element_line(colour="#2B3749",linewidth=.35),
       legend.position="top",legend.justification="left",
       legend.text=element_text(colour="#F8F3E9",size=12),
       plot.title=element_text(family="serif",size=26,lineheight=1.05,margin=margin(b=12)),
       plot.subtitle=element_text(size=12,lineheight=1.25,margin=margin(b=12)),
       plot.caption=element_text(size=10,colour="#C7CFDB",hjust=0,
                                 lineheight=1.25,margin=margin(t=16)),
       plot.title.position="plot",plot.caption.position="plot",
       plot.margin=margin(24,28,22,24))
dir.create("images",showWarnings=FALSE)
dir.create("analysis",showWarnings=FALSE)
ggsave("images/digital_skills_eu_comparison.png",p,width=11,height=14,dpi=300,bg=bg)
write_csv(w |> mutate(country=as.character(country)),"analysis/eu_age_comparison_2025.csv")
print(p)
message("Saved images/digital_skills_eu_comparison.png and analysis/eu_age_comparison_2025.csv")
# Publication check still required: source quality flags and comparability notes.
