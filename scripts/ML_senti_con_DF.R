# Load the library xml2
library(xml2)
library(XML)
library(tidyverse)

# Read the xml file
df<- read_xml('ML-SentiCon/senticon.es.xml')
# find all nodes that match Driver and extract the attribute value
#layer<-xml_find_all(df, xpath = "//layer")

positivos<-xml_find_all(df, xpath = "//positive")
negativos<-xml_find_all(df, xpath = "//negative")


pos_palabras<- xml_find_all(positivos, xpath = "//lemma") %>% 
  xml_text() %>% str_squish() 
pos_palabras_pos<- xml_find_all(positivos, xpath = "//lemma") %>% xml_attr("pos")
pos_palabras_pol<- xml_find_all(positivos, xpath = "//lemma") %>% xml_attr("pol")
pos_palabras_stf<- xml_find_all(positivos, xpath = "//lemma") %>% xml_attr("std")

positivos_df <- tibble(pos_palabras,
                       pos_palabras_pos,
                       pos_palabras_pol,
                       pos_palabras_stf)



neg_palabras<- xml_find_all(negativos, xpath = "//lemma") %>% 
  xml_text() %>% str_squish() 
neg_palabras_pos<- xml_find_all(negativos, xpath = "//lemma") %>% xml_attr("pos")
neg_palabras_pol<- xml_find_all(negativos, xpath = "//lemma") %>% xml_attr("pol")
neg_palabras_stf<- xml_find_all(negativos, xpath = "//lemma") %>% xml_attr("std")

negativos_df <- tibble(neg_palabras,
                       neg_palabras_pos,
                       neg_palabras_pol,
                       neg_palabras_stf)

write.csv(positivos_df, "input/ML_SentiCon.csv", row.names = F)
