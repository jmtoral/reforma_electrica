pacman::p_load(tidyverse, tidyverse, quanteda)
require(quanteda.textstats)

foros <- read_csv("output/foros_reforma_20220223.csv",
                  locale = locale(encoding = "UTF-8")) %>% 
  filter(!str_detect(personaje, "moderador|diputad(o|a)")) %>% 
  mutate(personaje = gsub("(El |La )(diputad(a|o)|ciudadan(a|o)|secretari(a|o)|ponente) ", "",personaje)) %>% 
  mutate(personaje = gsub("\\(vía telemática\\)", "",personaje)) %>%
  mutate(personaje = gsub("Monserrat", "Montserrat",personaje)) %>%
  mutate(personaje = str_squish(personaje)) %>% 
  mutate(personaje = iconv(personaje, from="UTF-8", to ="ASCII//TRANSLIT")) %>% 
  group_by(personaje, n_foro) %>% 
  summarise(texto = paste(parrafo, collapse = " ")) %>% 
  ungroup %>% 
  filter(nchar(personaje)<40) %>% 
  filter(n_foro == "Foro 1") 

corp_per <- corpus(foros, text_field = "texto")
docnames(corp_per) <- foros$personaje

toks_per<- tokens(corp_per, remove_punct = TRUE, remove_symbols = T,
                  remove_numbers = T)

dfmat_per <- dfm(toks_per) %>% 
  dfm_remove(stopwords("es"))  
  dfm_trim(min_termfreq = 0.5, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

tstat_dist <- as.dist(textstat_dist(dfmat_per,
                                    method = "manhattan"))
clust <- hclust(tstat_dist, method="ward.D")
plot(clust, xlab = "Distance", ylab = NULL)

