

# Finder den fil med det seneste data i
seneste_rejsekort <- dir("data") |> 
  str_subset("Daglig") |> 
  str_remove("\\D+") |> 
  str_remove("\\D+") |> max()


#Indlæser data til R
data_trafiktal <- readxl::read_xlsx(str_glue("data/Daglig rejseaktivitet frem til {seneste_rejsekort}.xlsx"),
                            sheet = "Korrigeret rejseaktivitet",
                            skip =3, .name_repair = ~str_to_upper(.x) |> 
                              str_replace_all(c(" " = "_", "-" = "_", "Å"="AA", "Æ"="AE", "Ø"="OE")) |> 
                              str_replace_all("_{2,}", "_"))

intr_aar <- data_trafiktal_1$DATO_END |> year() |> max()

#Renser data og laver kolonnerne om til et anvendeligt format
data_trafiktal_1 <- data_trafiktal |> 
  mutate(DATO_END = as.Date(str_c(DATO, "-", AAR), "%d-%b-%Y"),
         DATO_END_2019 = as.Date(str_c(DATO_2019, "-2019"), "%d-%b-%Y")) |> 
  transmute(DATO_END, UGE,
            UGEDAG = str_remove_all(UGEDAG, "[0-9]$"),
            REJSER_I_ALT_HELDAG, REJSER_I_ALT_HELDAG_2019, DATO_END_2019, 
         UGE_2019=UGENUMMER_2019, UGEDAG_2019) |> 
  distinct()


# season_korr_2019 <- data_trafiktal_1 |> 
#   select(ends_with("2019")) |> 
#   rename_with(~str_remove_all(.x, "_2019")) |> 
#   distinct() |> 
#   (\(y) map_dfr(c("Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag",  "Søndag" ), ~y |>
#                   filter(UGEDAG==.x & UGE %in% c(5, 6, 9, 10)) |> 
#                   summarise(UGEDAG=.x,
#                             REJSER_I_ALT_HELDAG = sum(REJSER_I_ALT_HELDAG)/4)))() 
# 
# season_korr <- data_trafiktal_1 |> 
#   select(-ends_with("2019")) |> 
#   distinct() |> 
#   (\(y) map_dfr(c("Mandag", "Tirsdag", "Onsdag", "Torsdag", "Fredag", "Lørdag",  "Søndag" ), ~y |>
#                   filter(UGEDAG==.x & UGE %in% c(5, 6, 9, 10) & year(DATO_END)==2020) |> 
#                   summarise(UGEDAG=.x,
#                             REJSER_I_ALT_HELDAG = sum(REJSER_I_ALT_HELDAG)/4)))() 

# Indlæser season korrektions faktorerne, dette gøres ved ren afskrivning fra Mathiases excel fil
season_korr <- tribble(~UGEDAG, ~HELDAG_SEASON_2019, ~HELDAG_SEASON_2020,
"Mandag", 558073, 593538,
"Tirsdag", 579924, 625059,
"Onsdag", 578736, 622486,
"Torsdag", 607578, 633857,
"Fredag", 635057, 685857,
"Lørdag", 364489, 413229,
"Søndag", 263978, 288867)


# Seasonkorrigerer
data_til_fig <- data_trafiktal_1 |> 
  filter(year(DATO_END)==intr_aar) |> 
  left_join(season_korr |> select(UGEDAG, HELDAG_SEASON_2020)) |> 
  left_join(season_korr |> select(UGEDAG_2019 = UGEDAG, HELDAG_SEASON_2019)) |> 
  mutate(DATO_END, RELATIV_FORSKEL = REJSER_I_ALT_HELDAG/REJSER_I_ALT_HELDAG_2019 -1,
         KORR_FORSKEL = ifelse(DATO_END<=as.Date(str_c(year(DATO_END), "-10-02")), #Da metroen åbnede den 2. oktober skal der kun season korrigeres hertil
                               ((REJSER_I_ALT_HELDAG/(HELDAG_SEASON_2020*REJSER_I_ALT_HELDAG_2019/HELDAG_SEASON_2019)-1)*100), 
                               (REJSER_I_ALT_HELDAG/REJSER_I_ALT_HELDAG_2019-1)*100) |> 
           round(1),
         .keep = "unused" ) |> 
  filter(DATO_END !=c(as.Date("2023-01-01")))
  
#Finder min og maks i til y grafen
y_max <- data_til_fig$KORR_FORSKEL |> max() |>  plyr::round_any(10, ceiling)
y_min <- data_til_fig$KORR_FORSKEL |> min() |>  plyr::round_any(10, floor)

metro <- filter(data_til_fig, 
       DATO_END ==as.Date("2022-10-02")) |> 
  pull(KORR_FORSKEL)

data_rejseaktivitet <- data_til_fig |> 
  ggplot(aes(x = DATO_END, y = KORR_FORSKEL)) +
  geom_line(color =  trm_colors("blå"), size = 1.2) + #Laver linjen
  geom_ma(ma_fun = SMA, n = 7, color = trm_colors("gul"), size = 1.2, linetype = "dotted") + #Laver det glidende gennemsnit
  # geom_segment(aes(x = as.Date(as.Date(str_c(aar, "-02-01"))), xend = as.Date(as.Date(str_c(aar, "-02-01"))), y = y_min, yend = y_max), 
  #              color = trm_colors("grå"), size = 2) + #Laver den grå linje der markerer hvornår samfundet åbnede op igen
  annotate(
    geom = "curve", x = as.Date("2022-10-02")-20, y = metro,
    xend = as.Date("2022-10-02"), yend = metro, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")) #Laver en kommentar pil i figuren
  ) + 
  annotate(geom = "text", x = as.Date("2022-10-02")-35, y = metro, label = "Metro\ncityring", hjust = "left") + #Laver selve teksten
  scale_y_continuous(name = "Pct.", breaks = seq(y_min, y_max, 10), 
                     sec.axis =  dup_axis()) +
  scale_x_date(name = "",
               breaks = "month",          # Date labels for each month.
               minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
               expand = expansion(add = 2),  # Add 15 days to the x-axis on the left and on the right.
               date_labels = "%d. %b. %y",
               limits = c(min(data_til_fig$DATO_END), max(data_til_fig$DATO_END))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  theme_trm(-10,-10,text_size = 12)
