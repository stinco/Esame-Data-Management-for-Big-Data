#####
# Lettura e preparaione dati
# 27/06/2018
#####

# Carico librerie ####
library(tidyverse)
library(maptools)
library(RColorBrewer)
library(colorspace)
library(ggmap)
library(viridis)
library(stringi) #Mi serve per eliminare gli accenti
library(gridExtra) #Mi serve per grid.arrange
library(scales) #Mi serve per trans_new()
# library(rgeos) # Serve per semplificare i poligoni con l'algoritmo di Douglas-Peuker
# library(ggrepel)
library(plotly)
# options(max.print=100)
# gc()

# Scelgo i colori per i partiti ####
# n=20
# pie(rep(1,n),col=rainbow_hcl(n))
# pie(rep(1,n),col=hue_pal()(n))

# col_lega <- rainbow_hcl(20)[9]
# col_m5s <- rainbow_hcl(20)[5]
# col_pd <- rainbow_hcl(20)[2]
# col_fi <- rainbow_hcl(20)[13]

col_lega <- hue_pal()(20)[7]
# col_m5s <- hue_pal()(20)[3]
# col_pd <- hue_pal()(20)[1]
# col_fi <- hue_pal()(20)[14]

#col_lega <- 
col_m5s <- "goldenrod3"
col_pd <- "indianred2"
#col_pd <- "firebrick1"
#col_fi <- "royalblue3"
col_fi <- "royalblue1"
col_leu <- "red4"
# col_svp <- "orangered"
#col_svp <- "gray0"
col_svp <- "violet"
col_altro <- "darkorchid"
col_fdi <- "skyblue2"


# Anagrafica elementi ####
#Stele di Rosetta
comuniViminaleISTAT <- read_csv("comuniViminaleISTAT.csv")
anagraficaProvince <- read_csv("anagraficaProvince.csv")

#Osserva: c'era scritto PROV al posto di PRO
names(comuniViminaleISTAT)[3] <- "COD_PRO"
names(comuniViminaleISTAT)[6] <- "CITTAMETRO"

comuniViminaleISTAT <- comuniViminaleISTAT %>% 
  mutate(COD_REG=as.character(COD_REG),
         COD_PRO=as.character(COD_PRO), 
         COD_CM=as.character(COD_CM),
         PRO_COM=as.character(PRO_COM),
         REGIONE=str_to_upper(stri_trans_general(REGIONE,"Latin-ASCII")),
         PROVINCIA=str_to_upper(stri_trans_general(PROVINCIA,"Latin-ASCII")),
         CITTAMETRO=str_to_upper(stri_trans_general(CITTAMETRO,"Latin-ASCII")),
         COMUNE=str_to_upper(stri_trans_general(COMUNE,"Latin-ASCII")),
         ELIGENDO_C_NOME=str_to_upper(stri_trans_general(ELIGENDO_C_NOME,"Latin-ASCII")))

# provinceViminaleISTAT
provinceViminaleISTAT <- comuniViminaleISTAT %>% 
  dplyr::select(COD_REG, REGIONE, COD_PRO, PROVINCIA, COD_CM, CITTAMETRO) %>% 
  distinct %>% 
  arrange(as.numeric(COD_PRO))

# regioniViminaleISTAT
regioniViminaleISTAT <- provinceViminaleISTAT %>% 
  dplyr::select(COD_REG, REGIONE) %>% 
  distinct %>% 
  arrange(as.numeric(COD_REG))


camera_geopolitico_italia <- read_csv("camera_geopolitico_italia.csv")

camera_geopolitico_italia <- camera_geopolitico_italia %>% 
  mutate(
    nome=str_to_upper(stri_trans_general(nome,"Latin-ASCII"))    
  )

# Affluenza Comuni ####
votantiCIComuni <- read_csv("votantiCIComuni.csv")

votantiCIComuni <- votantiCIComuni %>% 
  mutate(ente=str_to_upper(stri_trans_general(ente,"Latin-ASCII")))

votantiCIComuni <- votantiCIComuni %>% 
  mutate(perc_ore12=perc_ore12/100,
         perc_ore19=perc_ore19/100,
         perc_ore23=perc_ore23/100,
         percprec_ore23=as.numeric(str_replace(percprec_ore23, ",", ""))/100)

# Elimino i valori sbagliati
# Sauze di Cesena (TO)
# votantiCIComuni %>% 
#   filter(perc_ore12>=100 | perc_ore19>=100 | perc_ore23>=100)

votantiCIComuni <- votantiCIComuni %>% 
  mutate(perc_ore12=ifelse(perc_ore12>100 | perc_ore19>100 | perc_ore23>100, NA, perc_ore12),
         perc_ore19=ifelse(perc_ore12>100 | perc_ore19>100 | perc_ore23>100, NA, perc_ore19),
         perc_ore23=ifelse(perc_ore12>100 | perc_ore19>100 | perc_ore23>100, NA, perc_ore23))

votantiCIComuni <- votantiCIComuni %>% 
  dplyr::select(-href, -comuni_perv, -comuni)

# comuniViminaleISTAT %>%
#   dplyr::select(PRO_COM, COMUNE, ELIGENDO_C_NOME) %>% 
#   anti_join(votantiCIComuni, by=c(ELIGENDO_C_NOME="ente"))
# votantiCIComuni %>%
#   dplyr::select(ente, perc_ore23) %>% 
#   anti_join(comuniViminaleISTAT, by=c(ente="ELIGENDO_C_NOME"))

# Correzioni cin comuniViminaleISTAT
correzione_ViminaleISTAT <- comuniViminaleISTAT %>%
  dplyr::select(PRO_COM, COMUNE, ELIGENDO_C_NOME) %>% 
  anti_join(votantiCIComuni, by=c(ELIGENDO_C_NOME="ente")) %>% 
  mutate(ELIGENDO_C_NOME=str_to_upper(stri_trans_general(COMUNE,"Latin-ASCII"))) %>% 
  dplyr::select(PRO_COM, ELIGENDO_C_NOME2=ELIGENDO_C_NOME)

comuniViminaleISTAT <- comuniViminaleISTAT %>% 
  full_join(correzione_ViminaleISTAT, by="PRO_COM") %>% 
  mutate(ELIGENDO_C_NOME=ifelse(!is.na(ELIGENDO_C_NOME2), ELIGENDO_C_NOME2, ELIGENDO_C_NOME)) %>% 
  dplyr::select(-ELIGENDO_C_NOME2)


# Correzioni in votantiCIComuni
# Scrivo i nomi mettendo a posto gli accenti in votantiCIComuni$ente
ente2=c("TORINO",
        "EMARESE",
        "FENIS",
        "VERRES",
        "MILANO",
        "FIE ALLO SCILIAR",
        "FUNES",
        "LUSON",
        "MELTINA",
        "RIO DI PUSTERIA",
        "SANTA CRISTINA VALGARDENA",
        "SELVA DEI MOLINI",
        "SELVA DI VAL GARDENA",
        "TUBRE",
        "VERANO",
        "SEN JAN DI FASSA",
        "GENOVA",
        "BOLOGNA",
        "FIRENZE",
        "ROMA",
        "NAPOLI",
        "BARI",
        "PALERMO")

correzioni_ente <- votantiCIComuni %>%
  dplyr::select(ente) %>% 
  anti_join(comuniViminaleISTAT, by=c(ente="ELIGENDO_C_NOME")) %>% 
  mutate(ente2=ente2)

votantiCIComuni <- votantiCIComuni %>% 
  full_join(correzioni_ente) %>% 
  mutate(ente=ifelse(!is.na(ente2), ente2, ente)) %>% 
  dplyr::select(-ente2)

# # Provo di nuovo l'antijoin
# votantiCIComuni %>%
#   dplyr::select(ente, perc_ore23) %>% 
#   anti_join(comuniViminaleISTAT, by=c(ente="ELIGENDO_C_NOME")) %>% 
#   print(n=30)
# # Restano fuori solo le città metropolitane

# comuniViminaleISTAT %>%
#   dplyr::select(PRO_COM, COMUNE, ELIGENDO_C_NOME) %>% 
#   anti_join(votantiCIComuni, by=c(ELIGENDO_C_NOME="ente")) %>% 
#   print(n=30)
# # Sono rimasti fuori alcuni comuni in cui sono mancanti i dati dell'affluenza
# # Sono tutti comuni della provincia di Prato, eccetto None,
# # che fa parte della città metropolitana di Torino

# Faccio il join votantiCIComuni - comuniViminaleISTAT
# per aggiungere PRO_COM a votantiCIComuni
# Nota: PRO_COM è un codice dell'ISTAT, provincia è un codice del Viminale
votantiCIComuni <- votantiCIComuni %>% 
  right_join(dplyr::select(comuniViminaleISTAT, PRO_COM, ELIGENDO_C_NOME),
             by=c(ente="ELIGENDO_C_NOME")) %>% 
  dplyr::select(PRO_COM, everything())



# Affluenza provincie ####
votantiCIprovince <- read_csv("votantiCIprovince.csv")

votantiCIprovince <- votantiCIprovince %>% 
  mutate(ente=str_to_upper(stri_trans_general(ente,"Latin-ASCII"))) %>% 
  dplyr::select(-href)

# votantiCIprovince %>% 
#   mutate(perc=comuni_perv/comuni) %>% 
#   count(perc)
# #Tutti i comuni sono pervenuti

votantiCIprovince <- votantiCIprovince %>% 
  mutate(perc_ore12=perc_ore12/100,
         perc_ore19=perc_ore19/100,
         perc_ore23=perc_ore23/100,
         percprec_ore23=as.numeric(str_replace(percprec_ore23, ",", ""))/100)

# join tra votantiCIprovincie e provinceViminaleISTAT

votantiCIprovince %>% 
  anti_join(provinceViminaleISTAT, by=c(ente="PROVINCIA"))
# Problemi: MASSA-CARRARA e REGGIO CALABRIA

provinceViminaleISTAT %>% 
  anti_join(votantiCIprovince, by=c(PROVINCIA="ente"))
# Problemi: REGGIO DI CALABRIA, MASSA CARRARA, PRATO

# Faccio la correzione in votantiCIprovince$ente
votantiCIprovince <- votantiCIprovince %>% 
  mutate(ente=ifelse(ente=="MASSA-CARRARA","MASSA CARRARA", ente)) %>% 
  mutate(ente=ifelse(ente=="REGGIO CALABRIA","REGGIO DI CALABRIA", ente))

votantiCIprovince <- votantiCIprovince %>%
  right_join(dplyr::select(provinceViminaleISTAT, COD_PRO, PROVINCIA, COD_REG, REGIONE), by=c(ente="PROVINCIA")) %>% 
  dplyr::select(COD_PRO, COD_REG, REGIONE, everything())
# Ho inserito anche Prato con una riga che è tutta NA


# Aggiungo queste informazioni anche a votantiCIComuni
votantiCIComuni <- votantiCIComuni %>% 
  left_join(dplyr::select(votantiCIprovince, provincia, prov=ente, REGIONE), by=c("provincia"))


























