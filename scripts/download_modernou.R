pacman::p_load(tidyverse, rvest)

url <- "https://www.diputados.gob.mx/parlamentoreformaelectrica/foros.html"

raw_list <- url %>% # takes the page above for which we've read the html
  read_html() %>% 
  html_nodes(".btn-default:nth-child(5)") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("\\.pdf") %>% # find those that end in pdf only
  str_c("https://www.diputados.gob.mx/parlamentoreformaelectrica/", .) %>% # prepend the website to the url
 # map(read_html) %>% # take previously generated list of urls and read them
#  map(html_node, "#raw-url") %>% # parse out the 'raw' url - the link for the download button
 # map(html_attr, "href") %>% # return the set of raw urls for the download buttons
#  str_c("https://www.github.com", .) %>% # prepend the website again to get a full url
  walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
