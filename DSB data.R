
library(readr)
library(readxl)
library(flextable)

seneste_dsb <- dir("data") |> 
  str_subset("Rapportering") |> 
  str_remove("\\D+") |> 
  str_remove("\\D+") |> max()

dsb_saml_aar <- dir("data") |> 
  str_subset("^TRM") |> 
  str_remove("\\D+") |> 
  str_remove("\\D+") |> max()

readxl::excel_sheets(str_glue("data/Rapportering TRM S-tog {seneste_dsb}.xlsx"))

data_dsb_2019 <- readxl::read_xlsx(str_glue("data/Rapportering TRM S-tog {seneste_dsb}.xlsx"),
                            sheet = "Ark4",
                            skip =7, .name_repair = ~str_to_upper(.x) |> 
                              str_replace_all(c(" " = "_", "-" = "_", "Å"="AA", "Æ"="AE", "Ø"="OE")) |> 
                              str_replace_all("_{2,}", "_")) |> 
  select(1:4)

rejse_kol <- readxl::read_xlsx(str_glue("data/Rapportering TRM S-tog {seneste_dsb}.xlsx"),
                  sheet = "Ark4",
                  skip =3, .name_repair = ~str_to_upper(.x) |> 
                    str_replace_all(c(" " = "_", "-" = "_", "Å"="AA", "Æ"="AE", "Ø"="OE")) |> 
                    str_replace_all("_{2,}", "_")) |> 
  select(7:10) |> 
  select(starts_with("REJSER")) |> 
  names()

data_dsb_nu <- readxl::read_xlsx(str_glue("data/Rapportering TRM S-tog {seneste_dsb}.xlsx"),
                                   sheet = "Ark4",
                                   skip =3, .name_repair = ~str_to_upper(.x) |> 
                                     str_replace_all(c(" " = "_", "-" = "_", "Å"="AA", "Æ"="AE", "Ø"="OE")) |> 
                                     str_replace_all("_{2,}", "_")) |> 
  select(7:10) |> 
  filter(get(rejse_kol)!=0 & !is.na(get(rejse_kol)))

skip_row <- readxl::read_xlsx(str_glue("data/TRM {dsb_saml_aar}.xlsx"),
                  sheet = "Ark1",
                  skip =0, .name_repair = ~str_to_sentence(.x)) |> 
  mutate(row_number = row_number()) |> 
  filter(if_any(.fns = ~.x==dsb_saml_aar)) |> 
  pull("row_number")

data_dsb_saml <- readxl::read_xlsx(str_glue("data/TRM {dsb_saml_aar}.xlsx"),
                                   sheet = "Ark1",
                                   skip =skip_row+1, .name_repair = ~str_to_sentence(.x)) |> 
  select(where(~all(!is.na(.x))))

saml_dato <- data_trafiktal_1 |> 
  select(DATO_END, DATO_END_2019) |> 
  semi_join(data_dsb_nu, by = c("DATO_END"="DATO"))


data_til_fig_dsb <- data_dsb_nu |> 
  left_join(saml_dato, by = c("DATO"="DATO_END")) |> 
  left_join(data_dsb_2019 |> select(DATO, REJSER_I_2019), 
            by = c("DATO_END_2019"="DATO")) |> 
  mutate(ANDEL = ((get(rejse_kol)/REJSER_I_2019-1)*100) |> 
           (\(y) ifelse(DATO == as.Date("2022-04-04"), as.double(NA), y))()) |> 
  filter(!is.na(REJSER_I_2019))



y_max_dsb <- data_til_fig_dsb$ANDEL |> max(na.rm = T) |>  plyr::round_any(10, ceiling)
y_min_dsb <- data_til_fig_dsb$ANDEL |> min(na.rm = T) |>  plyr::round_any(10, floor)


fig_DSB <- data_til_fig_dsb |> 
  ggplot(aes(x = as.Date(DATO), y = ANDEL)) +
  geom_line(color =  trm_colors("blå"), size = 1.2) +
  geom_ma(ma_fun = SMA, n = 7, color = trm_colors("gul"), size = 1.2, linetype = "dotted") +
  # geom_segment(aes(x = as.Date(str_c(aar, "-02-01")), xend = as.Date(as.Date(str_c(aar, "-02-01"))), 
  #                  y = y_min_dsb, yend = y_max_dsb), 
  #              color = trm_colors("grå"), size = 2) +
  # annotate(
  #   geom = "curve", x = as.Date("2022-10-02")-20, y = metro,
  #   xend = as.Date("2022-10-02"), yend = metro, 
  #   curvature = .3, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = as.Date("2022-10-02")-35, y = metro, label = "Metro\ncityring", hjust = "left") +
  scale_y_continuous(name = "Pct.", breaks = seq(y_min_dsb, y_max_dsb, 10), 
                     sec.axis = sec_axis(~ ., 
                                         breaks = seq(y_min_dsb, y_max_dsb, 10),
                                         name = "Pct.")) +
  scale_x_date(name = "",
               breaks = "month",          # Date labels for each month.
               minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
               expand = expansion(add = 2),  # Add 15 days to the x-axis on the left and on the right.
               date_labels = "%d. %b. %y",
               limits = c(min(data_til_fig$DATO_END), max(data_til_fig$DATO_END))) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  theme_trm(-10,-10,text_size = 12)


data_dsb_saml_end <- data_dsb_saml |>
  select(-any_of("I alt")) |> 
  rowwise() |>
  rename(`Antal i tusinde`=`I tusinde`) |> 
  mutate(`ÅTD` = sum(c_across(where(is.numeric))))


