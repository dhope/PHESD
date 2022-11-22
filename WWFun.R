library(tidyverse)
library(RcppRoll)
theme_set(theme_light(base_size = 14, base_family = "Roboto Condensed"))
df <- read_csv("Wastewater/Ottawa/Data/wastewater_virus.csv") |> 
  rowwise() |> 
  mutate(COVID_copies_pavg = mean(c_across(c(covN1_nPMMoV_meanNr,
                      covN2_nPMMoV_meanNr)), na.rm=T )) |> ungroup() |> 
  filter( !qualityFlag)

d <- df |> 
  transmute(sampleDate, across(.cols = c(COVID_copies_pavg, InfA_copies_per_pep_copies_avg ,
                     InfB_copies_per_pep_copies_avg ,
                     RSV_copies_per_pep_copies_avg,
                     MPOX_copies_per_pep_copies_avg 
                     ),
                .fns = 
                  ~{roll_meanr(as.numeric(.x), n=7, fill = NA, align = 'right')})) |> 
  pivot_longer(cols = -c(sampleDate), names_to = "Var", 
               values_to = "pavg") |> 
  filter(!is.na(pavg)) |> 
  separate(Var, into = c("Virus", "remainder", "rem2"), sep = "_copies_") |> 
  group_by(Virus) |> 
  mutate(rel_viral = pavg/ max(pavg, na.rm=T)) |> ungroup()

                     
                     

gatineau <- read_csv(here::here("Wastewater/graph_1-1_gatineau.csv")) |> 
  janitor::clean_names() |> 
  mutate(sampleDate = lubridate::ymd(date_du_prelevement),
         rollavg = station_depuration_de_gatineau_moy_7_jours/1e7) |> 
  mutate(Virus = "COVID Gatineau") |> 
  mutate(rel_viral = rollavg/max(rollavg, na.rm=T))
# m <- read_csv("Wastewater/Ottawa/Data/wwMeasure.csv")


dates_of_import <- tribble(~date, ~Event,
                           "2022-03-21", "Masks off",
                           "2022-04-13", "Masks on",
                           "2022-05-30", "Masks off",
                           "2022-08-30", "QC",
                           "2022-09-06", "ON"
                           ) |> 
  mutate(across(date, lubridate::ymd)) |> 
  mutate(Virus="COVID")
  


since_dec <- 
ggplot(d |> 
         filter(sampleDate> lubridate::ymd("2021-12-01") & grepl("COVID", Virus) ), #|> slice_tail(n=30) ,
       aes(sampleDate, pavg  )) +
  # geom_point(aes(y = COVID_pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = gatineau |> 
              filter(sampleDate> lubridate::ymd("2021-12-01")),
            aes(y=rollavg),
            linetype = 2) +
  # ggthemes::theme_clean()+
  geom_vline(data = dates_of_import,
             linetype =3,
             aes(xintercept = date)) +
  labs(x = "Date", y = "Normalized viral copies") +
  geom_hline(yintercept = tail(d$COVID_pavg, n = 1), colour = 'red')  +
  ggrepel::geom_text_repel(data = dates_of_import,
            aes(x = date,
                y=0.0028,
                label = Event)) 




rel_risk <- 
ggplot(d |>
         bind_rows(gatineau) |> 
         filter(sampleDate > (lubridate::today()-60)),
       aes(sampleDate, rel_viral, colour = Virus)) +
  geom_line(linewidth=1) +
  # ggthemes::theme_clean()+
  rcartocolor::scale_color_carto_d(palette = 'Vivid') +
  labs(x = "Date", y = "Viral load (proportional to max observed)") +
  scale_x_date(#date_breaks = '15 days', 
               date_labels =  "%d\n%b") 


  

last_30 <- 
  ggplot(d |> filter(grepl("COVID", Virus) )|> slice_tail(n=30) ,
         aes(sampleDate, pavg  )) +
  # geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = slice_tail(gatineau, n=30),aes(y=rollavg), linetype=2) +
  # ggthemes::theme_clean()+
  geom_vline(linetype =2,
             xintercept = lubridate::ymd("2022-03-21")) +
  labs(x = "Date", y = "Normalized viral copies") +
  scale_x_date(#date_breaks = '1 weeks', 
               date_labels =  "%d\n%b") 
library(patchwork)

fig_out <- since_dec +last_30 + plot_layout(widths = c(0.7, 0.3))

figure <- fig_out / rel_risk

ggsave("FigOut.png", figure , width = 10, height = 6)

# fig_out
