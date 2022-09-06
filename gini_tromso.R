library(ggplot2)
library(gglorenz)
library(tidyverse)
library(PxWebApiData)
library(dplyr)
library(rjstat)
library(ineq)
library(janitor)
library(httr)




ssb <- "https://data.ssb.no/api/v0/no/table/12558/"




df <- '{ "query": [ { "code": "Region", "selection": { "filter": "vs:Kommune", "values": [ "5401", "1902" ] } }, { "code": "InntektSkatt", "selection": { "filter": "item", "values": [ "00" ] } }, { "code": "ContentsCode", "selection": { "filter": "item", "values": [ "VerdiDesil" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2005", "2020" ] } } ], "response": { "format": "json-stat2" } }'

#ssb data lastes inn slik som er gjort tidligere i makroøkonomi. 
df1 <- POST(ssb, body = df, encode = "json", verbose())

df_tro <- fromJSONstat(content(df1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()

#tar bort Na fra datasetet og filtrerere det slik jeg vil. 


df_tro <- df_tro %>% 
  na.omit() %>% 
  rename("år" = ar,
         inntekt = value) %>% 
  subset(select = -c(inntekt_for_etter_skatt, statistikkvariabel))

#lager Lorenz kurve av tromsø. Ene er 2005 den andre er 2020. 

df_tro %>%
  group_by(region) %>% 
  filter(år %in% c("2005", "2020")) %>%
  ggplot(aes(x = inntekt, colour = år)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  xlab("propesjon av befolkning") +
  ylab("propesjon av inntekt") +
  labs(title= "Innteksfordeling i Tromsø 2005/2020") +
  scale_x_continuous()+
  scale_y_continuous()

#filtrer ut ulike datoene får å finne ut av gini indeksen
tro_2020 <- df_tro %>% 
  filter(region=="Tromsø") %>% 
  select(region, inntekt, år, desil)

tro_2005 <- df_tro %>% 
  filter(region=="Tromsø (-2019)") %>% 
  select(region, inntekt, år, desil)

#GINI indeks i 2020 var 29.3%
ineq(tro_2020$inntekt, type = ("Gini")) * 100

#GINI indeks i 2005 var 29.1%

ineq(tro_2005$inntekt, type = ("Gini")) * 100

#ser en økning i gini fra 2005 til 2020 med 0.2%
