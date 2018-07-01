#####
# Preparazione dati mappe mute
# 27/06/2018
#####

# Regioni ####
# Reg01012018_g_WGS84.shp
reg2_shp <- readShapePoly("Reg01012018_g_WGS84.shp")
reg2.points <- fortify(reg2_shp)

# head(reg2.points)
# head(reg2_shp@data)
# unique(reg2.points$id)
# # Ci sono 20 regioni
# nrow(reg2.points) 77k righe

# Aggiungo a reg2.points i dettagli da mappare
reg2_shp_data <- reg2_shp@data
reg2_shp_data$id <-  as.character(0:(nrow(reg2_shp_data)-1))
reg2_shp_data <- reg2_shp_data %>%
  mutate(COD_RIP=as.character(COD_RIP),
         COD_REG=as.character(COD_REG),
         DEN_REG=str_to_upper(stri_trans_general(DEN_REG,"Latin-ASCII"))
  )
# head(reg2_shp_data)
# str(reg2_shp_data)

reg2_shp_data <- reg2_shp_data %>% 
  left_join(regioniViminaleISTAT, by="COD_REG")

reg2.points <- 
  left_join(reg2.points,
            dplyr::select(reg2_shp_data,
                          -Shape_Leng, -Shape_Area, -DEN_REG)
  )
# head(reg2.points)


# Provincie e città metropolitane ####
# ProvCM01012018_g_WGS84.shp

prov2_shp <- readShapePoly("ProvCM01012018_g_WGS84.shp")
prov2.points <- fortify(prov2_shp)

# head(prov2.points)
# head(prov2_shp@data)
# unique(prov2.points$id)
# # Ci sono 107 provincie
# nrow(prov2.points) 150k righe

# Aggiungo a prov2.points i dettagli da mappare
prov2_shp_data <- prov2_shp@data
prov2_shp_data$id <-  as.character(0:(nrow(prov2_shp_data)-1))
prov2_shp_data <- prov2_shp_data %>% 
  rename(COD_PRO=COD_PROV,
         DEN_PRO=DEN_PROV) %>% 
  mutate(COD_RIP=as.character(COD_RIP),
         COD_REG=as.character(COD_REG),
         COD_PRO=as.character(COD_PRO),
         COD_CM=as.character(COD_CM),
         COD_PCM=as.character(COD_PCM),
         DEN_PRO=str_to_upper(stri_trans_general(DEN_PRO,"Latin-ASCII")),
         DEN_CM=str_to_upper(stri_trans_general(DEN_CM,"Latin-ASCII")),
         DEN_PCM=str_to_upper(stri_trans_general(DEN_PCM,"Latin-ASCII")),
         SIGLA=str_to_upper(stri_trans_general(SIGLA,"Latin-ASCII"))
  )
# head(prov2_shp_data)
# str(prov2_shp_data)

prov2_shp_data <- prov2_shp_data %>% 
  left_join(regioniViminaleISTAT, by="COD_REG")
prov2_shp_data <- prov2_shp_data %>% 
  left_join(select(provinceViminaleISTAT, COD_PRO, PROVINCIA), by="COD_PRO")

prov2.points <- 
  left_join(prov2.points,
            dplyr::select(prov2_shp_data,
                          -Shape_Leng, -Shape_Area, -COD_CM,
                          -COD_PCM, -DEN_PRO, -DEN_CM, -DEN_PCM)
  ) 
# head(prov2.points)



# Comuni ####
# Com01012018_g_WGS84.shp

cm2_shp <- readShapePoly("Com01012018_g_WGS84.shp")
cm2.points <- fortify(cm2_shp)

# head(cm2.points)
# head(cm2_shp@data)
# unique(cm2.points$id)[length(unique(cm2.points$id))]
# # Ci sono 7959 comuni
# nrow(cm2.points) 854k righe

# Aggiungo a cm2.points i dettagli da mappare
cm2_shp_data <- cm2_shp@data
cm2_shp_data$id <-  as.character(0:(nrow(cm2_shp_data)-1))
cm2_shp_data <- cm2_shp_data %>% 
  rename(COD_PRO=COD_PROV) %>% 
  mutate(COD_RIP=as.character(COD_RIP),
         COD_REG=as.character(COD_REG),
         COD_PRO=as.character(COD_PRO),
         COD_CM=as.character(COD_CM),
         COD_PCM=as.character(COD_PCM),
         PRO_COM=as.character(PRO_COM),
         PRO_COM_T=as.character(PRO_COM_T),
         CC_P_CM=as.logical(CC_P_CM),
         COMUNE=str_to_upper(stri_trans_general(COMUNE,"Latin-ASCII")),
         COMUNE_A=str_to_upper(stri_trans_general(COMUNE_A,"Latin-ASCII"))
  )
# head(cm2_shp_data)
# str(cm2_shp_data)

# cm2_shp_data %>%
#   anti_join(select(comuniViminaleISTAT, PRO_COM, COMUNE), by="PRO_COM")
# select(comuniViminaleISTAT, PRO_COM, COMUNE) %>%
#   anti_join(cm2_shp_data, by="PRO_COM")
# # Mancano solo Fiumicello, Ligosullo, Treppo Carnico, Villa Vicentina

cm2_shp_data <- cm2_shp_data %>% 
  left_join(regioniViminaleISTAT, by="COD_REG")
cm2_shp_data <- cm2_shp_data %>% 
  left_join(select(provinceViminaleISTAT, COD_PRO, PROVINCIA), by="COD_PRO")
cm2_shp_data <- select(cm2_shp_data, -COMUNE) %>% 
  left_join(select(comuniViminaleISTAT, PRO_COM, COMUNE), by="PRO_COM")

cm2.points <- 
  left_join(cm2.points,
            dplyr::select(cm2_shp_data,
                          -Shape_Leng, -Shape_Area,
                          -COD_PCM, -COMUNE_A, CC_P_CM)
  ) 
# head(cm2.points)




# Circoscrizioni ####
# CIRCOSCRIZIONI_CAMERA_2017.shp
circ_shp <- readShapePoly("CIRCOSCRIZIONI_CAMERA_2017.shp")
circ.points <- fortify(circ_shp)

# nrow(circ.points) # 629k righe

# head(circ.points)
# head(circ_shp@data)
# unique(circ.points$id)
# # Ci sono 28 circoscrizioni (inclusa Valle d'Aosta)

circ_shp_data <- circ_shp@data
circ_shp_data$id=as.character(0:(nrow(circ_shp_data)-1))
circ_shp_data <- circ_shp_data %>% 
  mutate(COD_REG=as.character(COD_REG),
         CIRCO17_C=as.character(CIRCO17_C),
         CIRCO17_D=as.character(CIRCO17_D),
         CIRCO17_D=str_to_upper(stri_trans_general(CIRCO17_D,"Latin-ASCII"))
  ) %>% 
  separate(CIRCO17_D, sep="_", into=c("COD_CIRC", "DEN_CIRC"))
# head(circ_shp_data)
# str(circ_shp_data)

circ.points <- 
  left_join(circ.points,
            dplyr::select(circ_shp_data,
                          -Shape_Leng, -Shape_Area,
                          -POP_2011, -SEGGI_TOT, -SEGGI_UNI, -SEGGI_PRO),
            by="id")

# head(circ.points)



# Collegi plurinominali ####
# CAMERA_PLURI_2017.shp

pluri_shp <- readShapePoly("CAMERA_PLURI_2017.shp")
pluri.points <- fortify(pluri_shp)

# nrow(pluri.points) # 813k righe

# head(pluri.points)
# head(pluri_shp@data)
# unique(pluri.points$id)
# # Ci sono 64 collegi plurinominali (inclusa Valle d'Aosta)

pluri_shp_data <- pluri_shp@data
pluri_shp_data$id=as.character(0:(nrow(pluri_shp_data)-1))
pluri_shp_data <- pluri_shp_data %>% 
  mutate(CAM17P_DEN=str_to_upper(stri_trans_general(CAM17P_DEN,"Latin-ASCII")))
# head(pluri_shp_data)
# str(pluri_shp_data)


# Faccio il join tra pluri_shp_data e data_pluri
# Devo fare il matching via nome
# pluri_shp_data %>% 
#   anti_join(data_pluri, by=c("CAM17P_DEN"="collegio"))
# data_pluri[,1:3] %>% 
#   anti_join(pluri_shp_data, by=c("collegio"="CAM17P_DEN"))
# # Il problema è solo TRENTINO ALTO ADIGE

correzioni <- pluri_shp_data %>% 
  anti_join(data_pluri, by=c("CAM17P_DEN"="collegio")) %>% 
  select(CAM17P_DEN) %>% 
  mutate(collegio=ifelse(CAM17P_DEN=="TRENTINO-ALTO ADIGE/SA 1/4DTIROL - 01",
                         "TRENTINO-ALTO ADIGE/SUDTIROL - 01",CAM17P_DEN))

pluri_shp_data <- pluri_shp_data %>% 
  left_join(correzioni, by="CAM17P_DEN") %>% 
  mutate(CAM17P_DEN=ifelse(!is.na(collegio),collegio, CAM17P_DEN)) %>% 
  select(-collegio)

pluri_shp_data <- pluri_shp_data %>% 
  rename(collegio=CAM17P_DEN)

pluri.points <- 
  left_join(pluri.points,
            dplyr::select(pluri_shp_data,
                          -Shape_Leng, -Shape_Area,
                          -POP_2011, -SEGGI_TOT, -SEGGI_UNI, -SEGGI_PRO),
            by="id")

# head(pluri.points)

# head(pluri_shp_data)
# data_pluri[,1:3]
# data_circ[,1:3]



# Collegi uninominali ####
# CAMERA_UNI_2017.shp
uni_shp <- readShapePoly("CAMERA_UNI_2017.shp")
uni.points <- fortify(uni_shp)

# nrow(uni.points) # 1.262k righe

# head(uni.points)
# head(uni_shp@data) #Ho anche il codice della regione e la regione, quindi posso colorare per regione
# unique(uni.points$id)[length(unique(uni.points$id))]
# # Ci sono 232 collegi uninominali (inclusa la Valle d'Aosta)

uni_shp_data <- uni_shp@data
uni_shp_data$id=as.character(0:(nrow(uni_shp_data)-1))
uni_shp_data <- uni_shp_data %>% 
  mutate(CAM17U_DEN=str_to_upper(stri_trans_general(CAM17U_DEN,"Latin-ASCII")),
         CAM17U_NOM=str_to_upper(stri_trans_general(CAM17U_NOM,"Latin-ASCII"))
  )
# head(uni_shp_data)
# str(uni_shp_data)

# Faccio il join tra uni_shp_data e data_uni
# Devo fare il matching via nome
correzioni_uni <- uni_shp_data %>% 
  anti_join(data_uni, by=c("CAM17U_NOM"="collegio")) %>% 
  select(CAM17U_NOM)
# data_uni[,1:3] %>% 
#   anti_join(uni_shp_data, by=c("collegio"="CAM17U_NOM"))

corr <- c("AOSTA",
          "MILANO AREA STATISTICA 117",
          "MILANO - SESTO SAN GIOVANNI",
          "MILANO AREA STATISTICA 74",
          "MILANO AREA STATISTICA 84",
          "MILANO AREA STATISTICA 105",
          "MILANO AREA STATISTICA 144",
          "CANTU",
          "BOLZANO/BOZEN",
          "MERANO/MERAN",
          "BRESSANONE/BRIXEN",
          "SAN DONA' DI PIAVE",
          "GENOVA - UNITA' URBANISTICA SESTRI",
          "GENOVA - UNITA URBANISTICA SAN FRUTTUOSO",
          "FORLI",
          "FIRENZE - QUARTIERE VECCHIO NOVOLI-PERETOLA",
          "BARI - CIRCOSCRIZIONE 8 LIBERTA-MARCONI-SAN GIROLAMO-FESCA",
          "BARI - BITONTO",
          "NARDO",
          "PALERMO - LIBERTA",
          "PATERNO")

correzioni_uni <- correzioni_uni %>% 
  mutate(corr)

uni_shp_data <- uni_shp_data %>% 
  left_join(correzioni_uni, by="CAM17U_NOM") %>% 
  mutate(CAM17U_NOM=ifelse(!is.na(corr), corr, CAM17U_NOM)) %>% 
  select(-corr)

uni_shp_data <- uni_shp_data %>% 
  rename(collegio=CAM17U_NOM)

uni.points <- left_join(uni.points,
                        dplyr::select(uni_shp_data,
                                      -Shape_Leng, -Shape_Area,
                                      -POP_2011),#, -CAM17U_DEN),
                        by="id")

# head(uni.points)
# head(uni_shp_data)
# data_uni[,1:3]
# data_pluri[,1:3]
# data_circ[,1:3]


