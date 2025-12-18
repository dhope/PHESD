library(tidyverse)
library(RcppRoll)
theme_set(theme_light(base_size = 14, base_family = "Roboto Condensed"))
df <- read_csv("Wastewater/Ottawa/Data/wastewater_virus.csv", col_types = cols()) |> 
  rowwise() |> 
  mutate(COVID_copies_pavg = mean(c_across(c(covN1_nPMMoV_meanNr,
                      covN2_nPMMoV_meanNr)), na.rm=T )) |> ungroup() |> 
  filter( !qualityFlag)
back_cast_date <- Sys.Date() - lubridate::days(45)

extr_dates <- tibble(sampleDate = seq(min(df$sampleDate), max(df$sampleDate)) )

d <- left_join(extr_dates, df) |> 
    transmute(sampleDate, across(.cols = c(COVID_copies_pavg, InfA_copies_per_pep_copies_avg ,
                     InfB_copies_per_pep_copies_avg ,
                     RSV_copies_per_pep_copies_avg,
                     MPOX_copies_per_pep_copies_avg 
                     ),
                .fns = 
                  ~{roll_meanr(as.numeric(.x), n=7, fill = NA, align = 'right', na.rm = T)})) |> 
  pivot_longer(cols = -c(sampleDate), names_to = "Var", 
               values_to = "pavg") |> 
  # filter(!is.na(pavg)) |> 
  separate(Var, into = c("Virus", "remainder", "rem2"), sep = "_copies_") |> 
  group_by(Virus) |> 
  mutate(rel_viral = pavg/ max(pavg, na.rm=T),
         std_virus = case_when(Virus=="COVID"~pavg/22,
                               Virus=="InfA"~pavg,
                               Virus=="InfB"~NA_real_,
                               Virus=="RSV"~pavg/3,
                               TRUE~pavg),
         year = year(sampleDate),
         doy = yday(sampleDate)) |> 
  group_by(year, Virus) |> mutate(cum_virus = cumsum(rel_viral),
                           cum_days = row_number()) |> ungroup() |> 
  filter(sampleDate %in% df$sampleDate)
    # Based on https://twitter.com/emerc19/status/1600578611710676992 from @emerc19

                     
                     

gatineau <- read_csv(here::here("Wastewater/graph_1-1_gatineau.csv"), col_types = cols()) |> 
  janitor::clean_names() |> 
  mutate(sampleDate = lubridate::ymd(date_du_prelevement),
         pavg = station_depuration_de_gatineau_moy_7_jours/1e7) |> 
  mutate(Virus = "COVID Gatineau") |> 
  mutate(rel_viral = pavg/max(pavg, na.rm=T),
         std_virus = pavg/11)#22)
# m <- read_csv("Wastewater/Ottawa/Data/wwMeasure.csv")


# dates_of_import <- tribble(~date, ~Event,
#                            "2022-03-21", "Masks off",
#                            "2022-04-13", "Masks on",
#                            "2022-05-30", "Masks off",
#                            "2022-08-30", "QC",
#                            "2022-09-06", "ON"
#                            ) |> 
#   mutate(across(date, lubridate::ymd)) |> 
#   mutate(Virus="COVID")
  


since_dec <- 
ggplot(d |> 
         filter(sampleDate> lubridate::ymd("2021-12-01") & grepl("COVID", Virus) ), #|> slice_tail(n=30) ,
       aes(sampleDate, std_virus  )) +
  # geom_point(aes(y = COVID_pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = gatineau |> 
              filter(sampleDate> lubridate::ymd("2021-12-01")),
            # aes(y=pavg),
            linetype = 2) +
  # ggthemes::theme_clean()+
  # geom_vline(data = dates_of_import,
  #            linetype =3,
  #            aes(xintercept = date)) +
  labs(x = "Date", y = "Normalized viral copies") #+
  # geom_hline(yintercept = tail(d$COVID_pavg, n = 1), colour = 'red')  +
  # ggrepel::geom_text_repel(data = dates_of_import,
  #           aes(x = date,
  #               y=0.8e-4,
  #               label = Event)) 

last_year <- d |> 
  filter(doy >= yday((lubridate::today()-60)) & year == (year(today())-1)& Virus == "COVID" &
          doy <= yday(today())) |> 
  mutate(Virus = "Covid - Last year",
         sampleDate = (ymd(glue::glue("{year(Sys.Date())}-01-01"))+doy))


rel_risk <- 
ggplot(d |>
         # bind_rows(gatineau) |> 
         filter(sampleDate >= (lubridate::today()-60)) |> 
         bind_rows(last_year),
       aes(sampleDate, std_virus, colour = Virus)) +
  geom_line(linewidth=1) +
  # ggthemes::theme_clean()+
  rcartocolor::scale_color_carto_d(palette = 'Vivid') +
  theme(axis.ticks.y = element_blank(), axis.text.y =  element_blank()) +
  labs(x = "Date", y = "Viral load (scaled for equivalency)") +
  scale_x_date(#date_breaks = '15 days', 
               date_labels =  "%d\n%b") 

cumulative_virus <- 
  ggplot(d |> filter(Virus == "COVID" & year>2021 & doy<yday(today())) |> 
           mutate(week = week(sampleDate)
                  ) |> 
           summarize(std_virus = mean(std_virus),
                     sampleDate = mean(sampleDate),
                     doy = mean(doy),
                     .by = c(year, week)) |> 
           # slice_sample(n=2, by = c(year, week)) |> 
           arrange(sampleDate) |> 
           mutate(cum_virus=cumsum(std_virus), .by = year), 
         aes(ymd("2023-01-01")-1+doy, cum_virus,
             colour = factor(year))) + 
  geom_line(linewidth =1) +
  labs(x = "", y  = "Cumulative weekly virus detected", colour = "Year") +
  rcartocolor::scale_color_carto_d(palette = "Safe")  +
  theme(legend.position = c(0.2,.8), legend.background = element_blank())

cov_vs_inf <- 
  ggplot(
    filter(d, sampleDate> lubridate::ymd("2021-12-01") &str_detect(Virus, "COVID|InfA") & doy<yday(today())),
    # d |> filter(str_detect(Virus, "COVID|InfA") & year>2021 & doy<yday(today())), 
         aes(sampleDate, std_virus,
             colour = Virus)) + 
  geom_line() +
  labs(x = "", y  = "Cumulative weekly virus detected", colour = "Year") +
  rcartocolor::scale_color_carto_d(palette = "Safe")  +
  theme(legend.position = c(0.2,.8), legend.background = element_blank())
  

last_30 <- 
  ggplot(d |> filter(grepl("COVID", Virus) & sampleDate>=back_cast_date),
         aes(sampleDate, std_virus  )) +
  # geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = gatineau |> 
              filter(sampleDate>=back_cast_date),
            linetype=2) +
  # ggthemes::theme_clean()+
  geom_vline(linetype =2,
             xintercept = lubridate::ymd("2022-03-21")) +
  labs(x = "Date", y = "Normalized viral copies") +
  scale_x_date(#date_breaks = '1 weeks', 
               date_labels =  "%d\n%b") 
library(patchwork)

# fig_out <- since_dec +last_30 + plot_layout(widths = c(0.7, 0.3))

fig_bot <- rel_risk + cov_vs_inf

figure <- since_dec / fig_bot

ggsave("FigOut.png", figure , width = 10, height = 6)

# fig_out





