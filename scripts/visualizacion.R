pacman::p_load(tidyverse, tidytext, tm, wordcloud, udpipe, syuzhet)

foros <- read_csv("data/foros_ponentes_limpio_20220303.csv",
                  locale = locale(encoding = "LATIN1")) %>% 
  mutate(posicion_ponente = str_replace(posicion_ponente, "En favor", "A favor"))

foros_anotacion <- read_csv("data/anotaciones_foro.csv", 
                            locale = locale(encoding = "LATIN1"))

# A favor vs. en contra -----------------------------------------------------------

# Tokens

pal_cuenta <- foros %>% 
  select(parrafo, posicion_ponente) %>% 
  unnest_tokens(palabra, parrafo) %>% 
  filter(!palabra %in% c(stopwords("es"), "ser", "va", "queremos",
                         "parece", "vamos", "vemos", "diputado", "manera",
                         "josé", "van", "cabo", "dado", "cómo", "debemos",
                         "sé", "diría", "tal", "quién", "día", "tan",
                         "vez", "creo", "muchos", "hoy", "sigue",
                         "si", "aquel", "pues", "aquí", "carlos",
                         "jenaro","villamil", "ciento", "sánchez",
                         "entonces", "señor", "muchas", "debe", "pregunta")) %>% 
  filter(!str_detect(palabra, "[0-9]")) %>% 
  mutate(palabra = str_replace(palabra, "transnacionales", "trasnacionales")) %>% 
  count(palabra, posicion_ponente, sort =T)

png("comparacion1.png", width = 1000, height = 1000)
pal_cuenta %>% 
  filter(!is.na(posicion_ponente)) %>% 
  reshape2::acast(palabra ~ posicion_ponente, value.var = "n", fill = 0)%>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 400)
dev.off()




tfidf <- pal_cuenta %>% 
  bind_tf_idf(palabra, posicion_ponente, n) %>% 
  arrange(-tf_idf)

png("comparacion2.png", width = 1000, height = 1000)
tfidf %>% 
  reshape2::acast(palabra ~ posicion_ponente, value.var = "tf_idf", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 400)
dev.off()

# Bigrams


pal_cuenta <- foros %>% 
  select(parrafo, posicion_ponente) %>% 
  unnest_tokens(bigrama, parrafo, token = "ngrams", n = 2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep =" ") %>% 
  filter(!palabra1 %in% c(stopwords("es"), "ser", "va", "queremos",
                         "parece", "vamos", "vemos", "diputado", "manera",
                         "josé", "van", "cabo", "dado", "cómo", "debemos",
                         "sé", "diría", "tal", "quién", "día", "tan",
                         "vez", "creo", "muchos", "hoy", "sigue",
                         "si", "aquel", "pues", "aquí", "carlos",
                         "jenaro","villamil", "ciento", "sánchez",
                         "entonces", "señor", "muchas", "debe", "pregunta")) %>% 
  filter(!str_detect(palabra1, "[0-9]")) %>% 
  filter(!palabra2 %in% c(stopwords("es"), "ser", "va", "queremos",
                          "parece", "vamos", "vemos", "diputado", "manera",
                          "josé", "van", "cabo", "dado", "cómo", "debemos",
                          "sé", "diría", "tal", "quién", "día", "tan",
                          "vez", "creo", "muchos", "hoy", "sigue",
                          "si", "aquel", "pues", "aquí", "carlos",
                          "jenaro","villamil", "ciento", "sánchez",
                          "entonces", "señor", "muchas", "debe", "pregunta")) %>% 
  filter(!str_detect(palabra2, "[0-9]")) %>% 
  unite("palabra", palabra1, palabra2, sep = " ") %>% 
  mutate(palabra = str_replace(palabra, "transnacionales", "trasnacionales")) %>% 
  count(palabra, posicion_ponente, sort =T)

png("comparacion3.png", width = 1000, height = 1000)
pal_cuenta%>% 
  reshape2::acast(palabra ~ posicion_ponente, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 400)
dev.off()

# tf_idf bigram

tfidf <- pal_cuenta %>% 
  bind_tf_idf(palabra, posicion_ponente, n) %>% 
  arrange(-tf_idf)

png("comparacion4.png", width = 1000, height = 1000)
tfidf %>% 
  reshape2::acast(palabra ~ posicion_ponente, value.var = "tf_idf", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 200)
dev.off()




# UDPIPE taggins ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

foros %>% 
  select(doc_id =id, n_foro, afiliacion_ponente, posicion_ponente, partido) -> info_extra
  
  
foros_anotacion %>% 
  select(-X1) %>% 
  left_join(info_extra) %>% 
  filter(upos == "ADJ") %>%
  count(posicion_ponente, lemma, sort =T) %>% 
  filter(!is.na(posicion_ponente))-> adjetivos

png("comparacion5_adjetivos_lemma.png", width = 800, height = 800)
adjetivos %>% 
  reshape2::acast(lemma ~ posicion_ponente, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 500)
dev.off()


foros_anotacion %>% 
  select(-X1) %>% 
  left_join(info_extra) %>% 
  filter(upos == "NOUN") %>%
  count(posicion_ponente, lemma, sort =T) %>% 
  filter(!is.na(posicion_ponente))-> sustantivos

png("comparacion6_sustantivos_lemma.png", width = 800, height = 800)
sustantivos %>% 
  reshape2::acast(lemma ~ posicion_ponente, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 500)
dev.off()


foros_anotacion %>% 
  select(-X1) %>% 
  left_join(info_extra) %>% 
  filter(upos == "VERB") %>%
  count(posicion_ponente, lemma, sort =T) %>% 
  filter(!is.na(posicion_ponente))-> verbo

png("comparacion7_verbo_lemma.png", width = 800, height = 800)
verbo %>% 
  reshape2::acast(lemma ~ posicion_ponente, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("darkred", "navyblue"),
                   max.words = 500)
dev.off()



# sentimientos ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pal_unicas <- foros %>% 
  select(n_foro, id_foro, parrafo, posicion_ponente) %>% 
  unnest_tokens(palabra, parrafo) %>% 
  filter(!palabra %in% tm::stopwords("es")) %>% 
  distinct(n_foro, id_foro, posicion_ponente, palabra)

