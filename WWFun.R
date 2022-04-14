library(tidyverse)
library(RcppRoll)
d <- read_csv("Wastewater/Ottawa/Data/wastewater_virus.csv") |> 
  rowwise() |> 
  mutate(pavg = mean(c_across(c(covN1_nPMMoV_meanNr,
                      covN2_nPMMoV_meanNr)), na.rm=T )) |> ungroup() |> 
  mutate(rollavg = roll_meanr(pavg, n=7, fill = NA, align = 'right') )
# m <- read_csv("Wastewater/Ottawa/Data/wwMeasure.csv")

since_dec <- 
ggplot(d |> 
         filter(sampleDate> lubridate::ymd("2021-12-01") & !qualityFlag), #|> slice_tail(n=30) ,
       aes(sampleDate, rollavg  )) +
  geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  ggthemes::theme_clean()+
  geom_vline(linetype =2,
  xintercept = lubridate::ymd("2022-03-21")) +
  labs(x = "Date", y = "Normalized viral copies")
    # geom_hline(yintercept = 0.0018745)#0.00229)

last_30 <- 
  ggplot(d |> slice_tail(n=30) ,
         aes(sampleDate, rollavg  )) +
  geom_point(aes(y = pavg),alpha = 0.2)+
  geom_line() +
  ggthemes::theme_clean()+
  geom_vline(linetype =2,
             xintercept = lubridate::ymd("2022-03-21")) +
  labs(x = "Date", y = "Normalized viral copies")

library(patchwork)

since_dec/ last_30
