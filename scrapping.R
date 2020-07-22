library(tidyverse)
library(janitor)
library(rvest)
library(lubridate)
library(glue)

reuniones_html <- read_html("https://www.senado.cl/appsenado/index.php?mo=lobby&ac=GetReuniones")

reuniones <- reuniones_html %>% 
  html_nodes("table") %>% 
  html_table()

reuniones <- reuniones[[2]] %>% 
  as_tibble() 

reuniones <- clean_names(reuniones) %>% 
  separate(fecha_duracion_lugar, sep = "  ", extra = "merge",
           into = c("fecha", "duracion", "lugar")) 

reuniones <- reuniones %>% 
  mutate(fecha = as_date(fecha),
         duracion = parse_number(duracion))

hoy <- today()

write_delim(reuniones, delim = "|",
            path = glue("Data/reuniones_senadores_{hoy}.csv"))
