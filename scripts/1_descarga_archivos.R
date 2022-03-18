pacman::p_load(tidyverse, rvest, pdftools)

url <- "https://www.diputados.gob.mx/parlamentoreformaelectrica/foros.html"

raw_list <- url %>% 
  read_html() %>% 
  html_nodes(".btn-default") %>%  
  html_attr("href") %>% 
  str_subset("\\.pdf") %>% 
  str_subset("Foro-22-02", negate = T) %>%  # EN lo que la arreglan :(
  str_c("https://www.diputados.gob.mx/parlamentoreformaelectrica/", .) %>% 
  walk2(., paste0("input/", basename(.)), download.file, mode = "wb") 

# El archivo estaba mal :(, la página está mal. Todo el maldito sistema está mal.
#pdf_subset('https://www.diputados.gob.mx/parlamentoreformaelectrica/pdf/esteno/Foro-22-02.pdf',
 #          pages = 1:45, output = "input/Foro-22-02.pdf")
