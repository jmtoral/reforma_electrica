pacman::p_load(tidyverse, tidytext, tm, wordcloud, udpipe)

foros <- read_csv("data/foros_ponentes_limpio_20220303.csv")

# UDPIPE tagging

es <- udpipe_download_model(language = "spanish-ancora")

udmodel_es <- udpipe_load_model(file = es$file_model)

#foros$parrafo <- iconv(foros$parrafo, from = "Latin1", to = "UTF-8")

s <- udpipe_annotate(udmodel_es, x = iconv(foros$parrafo, from = 'latin1', to = 'UTF-8'),
                     doc_id = foros$id)
foros_anotada <- data.frame(s)

write.csv(foros_anotada, "anotaciones_foro.csv")