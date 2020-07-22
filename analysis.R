library(tidyverse)
library(patchwork)

reuniones <- read_delim("Data/reuniones_senadores_2020-07-22.csv", delim = "|")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Clean separation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
asistentes_reuniones <- reuniones %>% 
  separate_rows(asistentes, sep = "\\s*;") %>% 
  mutate(asistentes = str_to_title(asistentes),
         asistentes = str_replace(asistentes, "\\s,", ","),
         asistentes = str_trim(asistentes, side = "left")) 

top_10_asistentes <- asistentes_reuniones %>% 
  count(asistentes, sort = TRUE) %>% 
  head(10) %>% 
  mutate(asistentes = fct_reorder(asistentes, n))

top_10_sujetos <- asistentes_reuniones %>% 
  count(sujeto_pasivo, sort = TRUE) %>% 
  drop_na(sujeto_pasivo) %>% 
  top_n(10) %>% 
  mutate(sujeto_pasivo = fct_reorder(sujeto_pasivo, n))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theme_lobby <- function(){
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = .5),
        axis.text.y = element_text(face = "italic"))
}

p1 <- top_10_asistentes %>% 
  ggplot(aes(asistentes, n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "E") +
  labs(x = NULL, y = "N° de reuniones",
       title = "Lobbistas") +
  theme_lobby()  

p2 <- top_10_sujetos %>% 
  ggplot(aes(sujeto_pasivo, n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "E") +
  labs(x = NULL, y = "N° de reuniones",
       title = "Sujetos pasivos") +
  theme_lobby() 

p3 <- p1 + p2 +
  plot_annotation(title = "Top 10 lobbistas y sujetos pasivos",
                  subtitle = "Último período legislativo: 11/03/2018 a 21/07/2020",
                  theme = theme(
                    plot.title = element_text(hjust = .5, face = "bold", size = 22),
                    plot.subtitle = element_text(hjust = .5, face = "italic", size = 14, colour = "gray40")
                  )) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Saving files~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggsave(filename = "Figuras/lobby.png", plot = p3, type = "cairo",
       dpi = 1200, width = 9, height = 5.5)
