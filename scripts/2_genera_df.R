pacman::p_load(tidyverse, pdftools, tidytext, rvest)

# Tabla de información

## En esta tabla se concentrará toda la información de cada uno de los foros.

url <- "https://www.diputados.gob.mx/parlamentoreformaelectrica/foros.html"

html <- read_html(url)

n_foro <- html %>% 
  html_nodes('.col-lg-6 h2') %>% 
  html_text()

descripcion <- html %>% 
  html_nodes('.col-12 h4') %>% 
  html_text() %>% 
  str_squish()

fecha <- html %>% 
  html_nodes('.col-lg-6 b') %>% 
  html_text() %>% 
  str_squish() %>% 
  str_remove("Fecha: ")

formato <- html %>% 
  html_nodes('.col-lg-6 p') %>% 
  html_text() %>% 
  str_squish() %>% 
  str_remove("Formato: ")

info_foro <- tibble(n_foro, fecha,
                    descripcion, formato) %>% 
  rowid_to_column("id_foro") %>% 
  mutate(n_foro = str_replace(n_foro, "Foro 22$", "Foro 22 02")) %>% 
  mutate(n_foro = str_replace(n_foro, "Foro 22A", "Foro 22 A"))%>% 
  mutate(n_foro = str_replace(n_foro, "Cierre", "Foro 26"))

# Lista nombrada

## En este proceso, la lista tendrá nombres compatibles con la información en cada elemento.


files <- list.files(path = "input/", pattern = "pdf$",
                    full.names = T)  

est <- lapply(files, pdf_text)

names(est) <- files %>% 
  str_remove("input/") %>% 
  str_remove("\\.pdf") %>% 
  str_replace_all("-", " ") %>% 
  str_remove("[0-9]{6}\\s")



# Contrucción df

# Leo los archivos, limpio, fijo y doy esplendor.

df <- tibble(n_foro = rep(names(est), 
                           sapply(est, length)),
                 texto = unlist(est)) %>% 
  group_by(n_foro) %>% 
  summarise(texto = paste(texto, collapse = " ")) %>% 
  ungroup()


personaje_regex <- "(El |La )(gobernador|gobernadora|alcalde|ingeniera|presentador|dicrect|moderador|moderadora|diput|ciudadan|secretari|ponente|senador|director general)\\s*(.*?)\\s*(?=(: ))"

df_clas <- df %>%
  mutate(texto = str_replace_all(texto, "…|\\.\\.\\. ", " ")) %>% 
  unnest_tokens(parrafo, texto, # Gracias Julia Silge!
                token = "regex", pattern = "\n\n", #"\n\n\n"
                to_lower = F) %>%
  mutate(parrafo = str_squish(parrafo)) %>% 
  mutate(parrafo = str_replace_all(parrafo, " le pregunta al ponente Ramón Jiménez", " ")) %>% 
  mutate(parrafo = str_replace_all(parrafo, "\n", " ")) %>% 
  mutate(parrafo = str_replace_all(parrafo, "Josélyn", "Jocelyn")) %>%
  mutate(parrafo = str_remove_all(parrafo, "El ciudadano : \\(Inaudible\\)")) %>%
  mutate(parrafo = str_remove_all(parrafo, "\\(11:16 horas\\)")) %>%
  mutate(parrafo = str_remove_all(parrafo, "\\(Inaudible\\) ")) %>%
  mutate(personaje = str_extract(parrafo, personaje_regex )) %>% # extraer al hablante
  mutate(parrafo = str_remove(parrafo, personaje_regex)) %>% 
  mutate(parrafo = str_remove(parrafo, "^: ")) %>%
  mutate(parrafo = str_replace(parrafo, ": : ",": ")) %>%
  mutate(parrafo = str_remove_all(parrafo, "–")) %>%
  mutate(borrar = str_detect(parrafo, "^Comisiones Unidas de Puntos Constitucionales|Clausura del foro")) %>% 
  rowid_to_column("id") %>% 
  fill(personaje) %>% 
  filter(borrar == FALSE) %>% 
  filter(parrafo != "---o0o---") %>%
  filter(!str_detect(parrafo, "____")) %>% 
  select(-borrar) %>% 
  left_join(info_foro, by="n_foro")
  

#df_clas %>% filter(duplicados == TRUE) ->z
#errores <- df_clas %>% 
 # filter(str_detect(personaje, "…"))



# Escritura

write.csv(df_clas, "data/foros_completo_reforma_20220223.csv", row.names = F,
          fileEncoding = "UTF-8")

