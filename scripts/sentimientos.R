## Análisis Foros Reforma Eléctrica: sentimientos
## 4/03/2022, Actualizado el 4/03/2022

pacman::p_load(tidyverse, tidytext, tm, wordcloud, udpipe, syuzhet, dtt)

foros <- read_csv("data/foros_ponentes_limpio_20220303.csv",
                  locale = locale(encoding = "LATIN1")) %>% 
  mutate(posicion_ponente = str_replace(posicion_ponente, "En favor", "A favor")) 

foros_anotacion <- read_csv("data/anotaciones_foro.csv", 
                            locale = locale(encoding = "LATIN1"))

sentimientos <- read_csv("input/ML_SentiCon.csv", 
                         locale = locale(encoding = "LATIN1"))

# Añadir sentimientos
stopwords <- tibble(stopwords=tm::stopwords("es"))

foros_anotacion %>% 
  filter(!lemma %in% stopwords$stopwords) %>% 
  filter(!upos %in% c("PUNCT", "NUM", "SCONJ", "PRON")) %>% 
  left_join(sentimientos %>% 
              rename(lemma = pos_palabras)) %>% 
  filter(!is.na(pos_palabras_pol))->x 
  group_by(doc_id, paragraph_id, sentence_id) %>% 
  summarise(polaridad = mean(pos_palabras_pol)) %>% 
  ungroup %>% 
  rowid_to_column("time")->x

x %>% 
  ggplot(aes(time, polaridad)) +
  geom_line(color = "grey60")+
  geom_smooth() +
  hrbrthemes::theme_ipsum(grid = "")+
  ylim(-1,1) +
  labs(title = "Promedio de polaridad por enunciado en los Foros de la Reforma Energética")


# Siuzhet -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

foros_anotacion %>% 
  filter(!lemma %in% stopwords$stopwords) %>% 
  filter(!upos %in% c("PUNCT", "NUM", "SCONJ", "PRON"))->x



#char_v <- get_sentences(foros$parrafo)
#method <- "nrc"
#lang <- "spanish"
#my_text_values <- get_sentiment(char_v, method=method, language=lang)

#simple_plot(my_text_values)




method <- "nrc"
lang <- "spanish"
my_text_values <- get_sentiment(foros$parrafo, method=method, language=lang)

foros$sentimiento <- my_text_values


foros %>% 
  #group_by(n_foro) %>% 
  #summarise(p = mean(sentimiento)) %>% 
  #ungroup %>%
  mutate(p = zoo::rollmean(sentimiento, 50)) %>% 
  rowid_to_column("id_comp") %>% 
  ggplot(aes(id_comp, p)) +
  geom_col()+
  hrbrthemes::theme_ipsum(grid = "")+
  labs(title = "Promedio de polaridad por enunciado en los Foros de la Reforma Energética")
