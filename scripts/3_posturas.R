
# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, tidytext, tm)


# Insumos -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Información externa

posiciones <- read_csv("input/lista_ponentes.csv",
                       locale = locale(encoding = "LATIN1")) %>% 
  mutate(n_foro = str_c("Foro ", n_foro)) %>% 
  mutate(n_foro = str_replace(n_foro,"Foro 22A", "Foro 22 A"))

partido <- read_csv("input/partido.csv",
                    locale = locale(encoding = "LATIN1"))

# Limpieza adicional para incorporar información externa

foros <- read_csv("data/foros_completo_reforma_20220223.csv") %>% 
 # mutate(funcionaria = case_when(        # Identificar funcionarios y legisladores
  #  str_detect(personaje, "diputad(a|o)") ~ "Diputada",
  #  str_detect(personaje, "senad(a|o)") ~ "Senadora",
  #  str_detect(personaje, "gobernador(a|o)") ~ "Titular Ejecutivo Local",
  #  str_detect(personaje, "alcalde|alcaldesa") ~ "Presidentea Municipal",
#  )) %>% 
  filter(!str_detect(personaje, "presentador|moderador|secretario (técnico|ejecutivo)")) %>% # Añadir diputad(o|a) para quitar diputados 
  mutate(personaje = gsub("(El |La |)(gobernador|gobernadora|alcalde|ingeniera|director|ejecutivo|técnico|senador|diputad(a|o)|ciudadano|ciudadana|secretari(a|o)|ponente|director general) |:", "",personaje)) %>% 
  mutate(personaje = gsub("\\(vía telemática\\)", "",personaje)) %>%
  mutate(personaje = gsub("Monserrat", "Montserrat",personaje)) %>%
  mutate(personaje = gsub("\\bDel\\b", "del",personaje)) %>%
  mutate(personaje = gsub("(d|D)e (L|l)a", "de la",personaje)) %>%
  mutate(personaje = gsub("Dacsina Peto Vonduben", "Dacsina Peto Von Duben",personaje)) %>%
  mutate(personaje = gsub("Claudia Selene Ávila Flores Alejandro Armenta Mier", "Alejandro Armenta Mier",personaje)) %>%
  mutate(personaje = gsub("Andrés Carlos Morales Mar", "Carlos Andrés Morales Mar",personaje)) %>%
  mutate(personaje = gsub("Ana Lilia Moreno$", "Ana Lilia Moreno González",personaje)) %>%
  mutate(personaje = gsub("Eleazar Nicólas Castro Pérez|Eleazar Castro Pérez", "Eleazar Nicolás Castro Pérez",personaje)) %>%
  mutate(personaje = gsub("Guadalupe Correa-Cabrera", "Guadalupe Correa Cabrera",personaje)) %>%
  mutate(personaje = gsub("Hector", "Héctor",personaje)) %>%
  mutate(personaje = gsub("Oscar|Oscar G.", "Óscar",personaje)) %>%
  mutate(personaje = gsub("Cabello", "Carrillo",personaje)) %>% #WTF
  mutate(personaje = gsub("Irazabal", "Irazábal",personaje)) %>%
  mutate(personaje = gsub("Armézquita", "Amézquita",personaje)) %>%
  mutate(personaje = gsub("Tánori", "Tanori",personaje)) %>%
  mutate(personaje = gsub("Barcenas", "Bárcenas",personaje)) %>%
  mutate(personaje = gsub("Zarate", "Zárate",personaje)) %>%
  mutate(personaje = gsub("Daniel Amézquita$", "Daniel Amézquita Díaz",personaje)) %>%
  mutate(personaje = gsub("Sergio Alcalde Delgado", "Sergio Genaro Alcalde Delgado",personaje)) %>%
  mutate(personaje = gsub("Sergio Gerardo Alcalde Delgado", "Sergio Genaro Alcalde Delgado",personaje)) %>%
  mutate(personaje = gsub("Nancy Josélyn Jiménez Camacho", "Nancy Jocelyn Jiménez Camacho",personaje)) %>%
  mutate(personaje = gsub("Jose|Jose´", "José",personaje)) %>% 
  mutate(personaje = gsub("Ruíz", "Ruiz",personaje)) %>% 
  mutate(personaje = gsub("Maria", "María",personaje)) %>% 
  mutate(personaje = gsub("Hernádez|Hernandez", "Hernández",personaje)) %>% 
  mutate(personaje = gsub("Jorge Chávez Presa", "Jorge Alejandro Chávez Presa",personaje)) %>% 
  mutate(personaje = gsub("^Tonatiuh Martínez Aviña", "Jorge Tonatiuh Martínez Aviña",personaje)) %>% 
  mutate(personaje = gsub("José Romualdo Hernández$", "José Romualdo Hernández Naranjo",personaje)) %>% 
  mutate(personaje = gsub("Cabrera Velazco", "Cabrera Velasco",personaje)) %>%
  mutate(personaje = gsub("Blanca Alcalá Ruiz", "Blanca María del Socorro Alcalá Ruiz",personaje)) %>%
  mutate(personaje = gsub("Johnson", "Johnston",personaje)) %>%
  mutate(personaje = gsub("Matínez", "Martínez",personaje)) %>%
  mutate(personaje = gsub("José Fernando Rodolfo", "José Gerardo Rodolfo",personaje)) %>%
  mutate(personaje = gsub("Gerardo Fernández Noroña", "José Gerardo Rodolfo Fernández Noroña",personaje)) %>%
  mutate(personaje = gsub("Rafael Mateu Lazcano", "Rafael Ismael Mateu Lazcano",personaje)) %>%
  mutate(personaje = gsub("Manuel Buxade Hernández|Manuel Buxadé José Roldán Hernández", 
                          "Manuel Buxadé Hernández",personaje)) %>%
  mutate(personaje = gsub("Ramsés Pech$", "Ramsés Pech Razo",personaje)) %>%
  mutate(personaje = gsub("Maríana", "Mariana",personaje)) %>%
  mutate(personaje = gsub("Luis Espinosa Cházaro", "Luis Ángel Xariel Espinosa Cházaro",personaje)) %>%
  mutate(personaje = gsub("Eduardo Moral Hinojosa", "Eduardo Murat Hinojosa",personaje)) %>%
  mutate(personaje = gsub("Eduardo Méndez Sánchez", "Eduardo Manuel Méndez Sánchez",personaje)) %>%
  mutate(personaje = gsub("Miguel Zárate Martínez", "Miguel Oswaldo Zárate Martínez",personaje)) %>%
  mutate(personaje = gsub("Francisco Barnés de Castro$", "Francisco José Barnés de Castro",personaje)) %>%
  mutate(personaje = gsub("Carlos Rodríguez Sámano$", "Carlos Francisco Rodríguez Sámano",personaje)) %>%
  mutate(personaje = gsub("José Medina Mora$", "José Medina Mora Icaza",personaje)) %>%
  mutate(personaje = gsub("Juan Luis Hernández$", "Juan Luis Hernández Avendaño",personaje)) %>%
  mutate(personaje = gsub("Miguel Reyes Hernández$", "Miguel Santiago Reyes Hernández",personaje)) %>%
  mutate(personaje = gsub("José Rosas Fernández$", "José Bernardo Rosas Fernández",personaje)) %>%
  mutate(personaje = gsub("Luis Manuel Hernández$", "Luis Manuel Hernández González",personaje)) %>%
  mutate(personaje = gsub("Ricardo Mota Palomino$", "Ricardo Octavio Arturo Mota Palomino",personaje)) %>%
  mutate(personaje = gsub("Edna Gisel Diaz Acevedo", "Edna Gisel Díaz Acevedo",personaje)) %>%
  mutate(personaje = gsub("Katia Somohano$|Katya Minerva Somohano Silva", "Katia Somohano Silva",personaje)) %>%
  mutate(personaje = str_squish(personaje)) %>% 
  rename(nombre_ponente = personaje ) %>% 
  mutate(nombre_ponente = str_replace_all(nombre_ponente, "Josélyn", "Jocelyn")) 

  
# Unión


foros_pos <- foros %>% 
  left_join(posiciones) %>% 
  filter(!nombre_ponente %in% c("El ciudadano", "El diputado", "La ciudadana", 
  "Alejandra Frausto Guerrero")) %>% 
  left_join(partido)

#rm(foros, posiciones)

write.csv(foros_pos %>% 
            mutate(posicion_ponente = str_replace(posicion_ponente, "En favor",
                                                  "A favor")), "data/foros_ponentes_limpio_20220303.csv", row.names = F)



# Revisar

foros_pos %>% 
  filter(is.na(posicion_ponente)) %>% 
  distinct(n_foro, nombre_ponente, afiliacion_ponente)->x


x %>% 
  mutate(n_foro = str_remove_all(n_foro, "Foro ")) %>% 
  write.csv("input/extra_ponente.csv", row.names = F)  

