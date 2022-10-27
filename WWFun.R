library(tidyverse)
library(RcppRoll)
d <- read_csv("Wastewater/Ottawa/Data/wastewater_virus.csv") |> 
  rowwise() |> 
  mutate(pavg = mean(c_across(c(covN1_nPMMoV_meanNr,
                      covN2_nPMMoV_meanNr)), na.rm=T )) |> ungroup() |> 
  mutate(rollavg = roll_meanr(pavg, n=7, fill = NA, align = 'right') )

gatineau <- read_csv(here::here("Wastewater/graph_1-1_gatineau.csv")) |> 
  janitor::clean_names() |> 
  mutate(sampleDate = lubridate::ymd(date_du_prelevement),
         rollavg = station_depuration_de_gatineau_moy_7_jours/1e7)
# m <- read_csv("Wastewater/Ottawa/Data/wwMeasure.csv")


dates_of_import <- tribble(~date, ~Event,
                           "2022-03-21", "Masks off",
                           "2022-04-13", "Masks on",
                           "2022-05-30", "Masks off",
                           "2022-08-30", "QC",
                           "2022-09-06", "ON"
                           ) |> 
  mutate(across(date, lubridate::ymd))
  


since_dec <- 
ggplot(d |> 
         filter(sampleDate> lubridate::ymd("2021-12-01") & !qualityFlag), #|> slice_tail(n=30) ,
       aes(sampleDate, rollavg  )) +
  geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = gatineau |> 
              filter(sampleDate> lubridate::ymd("2021-12-01")),
            linetype = 2) +
  ggthemes::theme_clean()+
  geom_vline(data = dates_of_import,
             linetype =3,
             aes(xintercept = date)) +
  labs(x = "Date", y = "Normalized viral copies") +
  geom_hline(yintercept = tail(d$rollavg, n = 1), colour = 'red')  +
  ggrepel::geom_text_repel(data = dates_of_import, 
            aes(x = date,
                y=0.0028,
                label = Event))
  

last_30 <- 
  ggplot(d |> slice_tail(n=30) ,
         aes(sampleDate, rollavg  )) +
  geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  geom_line(data = slice_tail(gatineau, n=30), linetype=2) +
  ggthemes::theme_clean()+
  geom_vline(linetype =2,
             xintercept = lubridate::ymd("2022-03-21")) +
  labs(x = "Date", y = "Normalized viral copies")

library(patchwork)

fig_out <- since_dec/ last_30

ggsave("FigOut.png", fig_out , width = 4, height = 9)

# fig_out
