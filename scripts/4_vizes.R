## Visualizaciones
## 4/03/2022, Actualizado el 4/03/2022



# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pacman::p_load(tidyverse, tidytext, tm, wordcloud, hrbrthemes)
extrafont::loadfonts(device = "win", quiet = TRUE) 


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



foros <- read_csv("data/foros_ponentes_limpio_20220303.csv",
                  locale = locale(encoding = "LATIN1")) %>% 
  mutate(posicion_ponente = str_replace(posicion_ponente, "En favor", "A favor")) 

foros_anotacion <- read_csv("data/anotaciones_foro.csv", 
                            locale = locale(encoding = "LATIN1"))


# Visualizaciones ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Conteos ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

foros %>% 
  unnest_tokens(palabra, parrafo) %>% 
  filter(!palabra %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces")) %>% 
  filter(!is.na(posicion_ponente)) %>% 
  count(posicion_ponente, palabra, sort =T) %>% 
  ungroup %>% 
  mutate(palabra_order = 
           reorder_within(palabra,n,posicion_ponente)) %>%
  group_by(posicion_ponente) %>% 
  top_n(25) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra_order, n), n,
             fill=n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 5, "discrete")) +
  scale_x_reordered() +
  facet_wrap(~posicion_ponente, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Las 25 palabras más comunes de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf1.jpg", width = 8, height = 6)

 
foros %>% 
  unnest_tokens(bigrama, parrafo, toke ="ngrams", n=2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep=" ") %>% 
  filter(!palabra1 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez")) %>% 
  filter(!palabra2 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez")) %>%
  unite("palabra", c(palabra1, palabra2), sep=" ") %>% 
  filter(!is.na(posicion_ponente)) %>% 
  count(posicion_ponente, palabra, sort =T) %>% 
  ungroup %>% 
  mutate(palabra_order = 
           reorder_within(palabra,n,posicion_ponente)) %>%
  group_by(posicion_ponente) %>% 
  top_n(25) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra_order, n), n,
             fill=n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 5, "discrete")) +
  scale_x_reordered() +
  facet_wrap(~posicion_ponente, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Los 25 bigramas más comunes de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf2.jpg", width = 8, height = 6)

  

## TFIDFS ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

foros %>% 
  unnest_tokens(palabra, parrafo) %>% 
  filter(!palabra %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces")) %>% 
  filter(!is.na(posicion_ponente)) %>%
  filter(!str_detect(palabra, "[0-9]")) %>% 
  count(posicion_ponente, palabra, sort =T) %>% 
  ungroup %>%
  bind_tf_idf(palabra, posicion_ponente,n) %>% 
  arrange(-tf_idf) %>%
  mutate(palabra_order = 
           reorder_within(palabra,tf_idf,posicion_ponente)) %>%
  group_by(posicion_ponente) %>% 
  top_n(30) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra_order, tf_idf), n,
             fill=posicion_ponente)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "navyblue")) +
  scale_x_reordered() +
  facet_wrap(~posicion_ponente, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Las 30 palabras más representativas de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf3.jpg", width = 8, height = 6)


foros %>% 
  unnest_tokens(bigrama, parrafo, toke ="ngrams", n=2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep=" ") %>% 
  filter(!palabra1 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez",
             "sino", "quien", "siguiente", "pregunta", "quién",
             "últimos", "solo")) %>% 
  filter(!palabra2 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez",
             "sino", "quien","siguiente", "pregunta", "quién",
             "últimos", "solo")) %>%
  unite("palabra", c(palabra1, palabra2), sep=" ") %>% 
  filter(!is.na(posicion_ponente)) %>% 
  count(posicion_ponente, palabra, sort =T) %>% 
  ungroup %>% 
  bind_tf_idf(palabra, posicion_ponente,n) %>% 
  arrange(-tf_idf) %>%
  mutate(palabra_order = 
           reorder_within(palabra,tf_idf,posicion_ponente)) %>%
  group_by(posicion_ponente) %>% 
  top_n(30) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra_order, tf_idf), tf_idf,
             fill=posicion_ponente)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "navyblue")) +
  scale_x_reordered() +
  facet_wrap(~posicion_ponente, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Los 30 bigramas más representativos de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf4.jpg", width = 8, height = 6)
  


## TFIDFS por foro ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

foros %>% 
  unnest_tokens(palabra, parrafo) %>% 
  filter(!palabra %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces")) %>% 
  filter(!is.na(posicion_ponente)) %>%
  filter(!str_detect(palabra, "[0-9]")) %>% 
  count(descripcion, posicion_ponente, palabra, sort =T) %>% 
  ungroup %>%
  bind_tf_idf(palabra, posicion_ponente,n) %>% 
  arrange(-tf_idf) %>%
  group_by(descripcion, posicion_ponente) %>% 
  top_n(3) %>% 
  mutate(tf_idf = case_when(
    posicion_ponente == "En contra" ~ -1*tf_idf ,
    TRUE ~ tf_idf
  )) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra, tf_idf), tf_idf,
             fill=posicion_ponente)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "navyblue")) +
  facet_wrap(~descripcion, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Las 30 palabras más representativas de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf5.jpg", width = 8, height = 6)


foros %>% 
  unnest_tokens(bigrama, parrafo, toke ="ngrams", n=2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep=" ") %>% 
  filter(!palabra1 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez",
             "sino", "quien", "siguiente", "pregunta", "quién",
             "últimos", "solo")) %>% 
  filter(!palabra2 %in% 
           c(tm::stopwords("es"), "si", "ser", "va", "hacer",
             "ciento", "pues", "aquí", "hoy", "parte", "cómo",
             "creo", "decir","manera","tener", "puede", "mil",
             "ahí","años", "gracias", "entonces","cada", "vez",
             "sino", "quien","siguiente", "pregunta", "quién",
             "últimos", "solo")) %>%
  unite("palabra", c(palabra1, palabra2), sep=" ") %>% 
  filter(!is.na(posicion_ponente)) %>% 
  count(posicion_ponente, palabra, sort =T) %>% 
  ungroup %>% 
  bind_tf_idf(palabra, posicion_ponente,n) %>% 
  arrange(-tf_idf) %>%
  mutate(palabra_order = 
           reorder_within(palabra,tf_idf,posicion_ponente)) %>%
  group_by(posicion_ponente) %>% 
  top_n(30) %>% 
  ungroup %>% 
  ggplot(aes(reorder(palabra_order, tf_idf), tf_idf,
             fill=posicion_ponente)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "navyblue")) +
  scale_x_reordered() +
  facet_wrap(~posicion_ponente, scales = "free_y") +
  theme_ipsum(grid="X") +
  guides(fill="none") +
  labs(x = "", y="",
       title="Los 30 bigramas más representativos de las personas ponentes",
       subtitle="por posición con respecto a la Reforma Energética",
       caption = "@ludwig_vanB y @jmtoralc | Fuente: Cámara de Diputados")

ggsave("output/grf4.jpg", width = 8, height = 6)




  
  