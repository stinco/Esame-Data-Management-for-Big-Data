#####
# Preparazione risultati
# 27/06/2018
#####

# Circoscrizioni ####
# Dataset scaricato a mano dal sito del ministero dell'interno
# Pulizia dataset circoscrizioni
circ_minist <- read_csv2("LISTE-Camera-04_03_2018-ITALIA-Circoscrizione.csv") %>% 
  mutate(Ente=str_to_upper(stri_trans_general(Ente,"Latin-ASCII")))

circ_minist <- circ_minist %>% 
  dplyr::select(Ente, 'Liste/Gruppi', 'Voti lista') %>% 
  rename(circoscrizione=Ente, lista='Liste/Gruppi', voti='Voti lista') %>% 
  group_by(circoscrizione) %>% 
  mutate(perc=voti/sum(voti)*100) %>% 
  ungroup

circ_minist <- circ_minist %>% 
  left_join(camera_geopolitico_italia, by=c("circoscrizione"="nome")) %>% 
  dplyr::select(codice=id, everything())

# Problema: il Molise è doppio!
# Molise è anche il nome di una città del Molise in provincia di Campobasso
cod_molise <- "18110190390"
circ_minist <- circ_minist %>% 
  filter(codice!=cod_molise)

# Metto i nomi abbreviati
names_short=c("LEGA","FI","FDI","UDC","M5S","PD","+E", "IEI","CP","SVP","LEU")
names_long=c("LEGA",
             "FORZA ITALIA",
             "FRATELLI D'ITALIA CON GIORGIA MELONI",
             "NOI CON L'ITALIA - UDC",
             "MOVIMENTO 5 STELLE",
             "PARTITO DEMOCRATICO",
             "+EUROPA",
             "ITALIA EUROPA INSIEME",
             "CIVICA POPOLARE LORENZIN",
             "SVP - PATT",
             "LIBERI E UGUALI")

nomi_abbreviati=tibble(names_long, names_short)

circ_minist <- circ_minist %>% 
  left_join(nomi_abbreviati, by=c("lista"="names_long")) %>% 
  mutate(lista = ifelse(!is.na(names_short), names_short, lista)) %>% 
  select(-names_short)


# Aggiungo gli schieramenti
schieramento <- tibble(lista=unique(circ_minist$lista)) %>% 
  mutate(coalizione=lista)

schieramento$coalizione[1:4] <- "CDX"
schieramento$coalizione[c(6:9,nrow(schieramento))] <- "CSX"

circ_minist <- circ_minist %>% 
  left_join(schieramento)


# Voti alle coalizioni
circ_minist_co <- circ_minist %>% 
  group_by(codice, circoscrizione, coalizione) %>% 
  summarize(voti=sum(voti),
            perc=sum(perc)) %>% 
  ungroup

# Preparazione per la cluster analysis
data_circ <- circ_minist %>% 
  dplyr::select(codice, circoscrizione, lista, perc) %>% 
  spread(lista, perc)

data_circ_co <- circ_minist_co %>% 
  dplyr::select(codice, circoscrizione, coalizione, perc) %>% 
  spread(coalizione, perc)

data_circ[,3:ncol(data_circ)] <-
  data_circ[,3:ncol(data_circ)] %>% 
  replace(is.na(.),0)

data_circ_co[,3:ncol(data_circ_co)] <-
  data_circ_co[,3:ncol(data_circ_co)] %>% 
  replace(is.na(.),0)

data_circ <- data_circ %>%
  dplyr::select(codice, circoscrizione,
                `LEGA`,
                `FI`,
                `FDI`,
                `UDC`,
                `M5S`,
                `PD`,
                `+E`,
                `IEI`,
                `CP`,
                `SVP`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

data_circ_co <- data_circ_co %>%
  dplyr::select(codice, circoscrizione,
                `CDX`,
                `M5S`,
                `CSX`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

# Cluster analysis
x <- data_circ[3:ncol(data_circ)]
x=as.matrix(x)
row.names(x) = data_circ$circoscrizione

#Average linkage
d <- dist(x)
ls_av_circ <- hclust(d, method = "average")

#Cerco il numero ottimale di cluster
m <- length(ls_av_circ$height) #Numero di iterazioni (n-1)
s <- c()
for (j in m:2){
  s <- c(s, ls_av_circ$height[j]/ls_av_circ$height[j - 1])
}

# plot(s,type="b",pch=19,xlab="cluster #",ylab="relative separation")
# order(-s)
best_k_circ <- order(-s)+1

# plot(ls_av_circ)
# rect.hclust(ls_av_circ,k=best_k_circ[1], border="red")
# rect.hclust(ls_av_circ,k=best_k_circ[2], border="green", which=c(3,4))

data_circ$cluster1 <- as.factor(cutree(ls_av_circ, k = best_k_circ[1]))
data_circ$cluster2 <- as.factor(cutree(ls_av_circ, k = best_k_circ[2]))

# Aggiungo il vincitore a ogni circoscrizione
vincitore=rep("",nrow(x))
for(i in 1:nrow(x)){
  vincitore[i] <- colnames(x)[which.max(x[i,])]
}
data_circ$vincitore=vincitore

y <- data_circ_co[3:ncol(data_circ_co)]
y=as.matrix(y)
row.names(y) = data_circ_co$circoscrizione

vincitore_co=rep("",nrow(y))
for(i in 1:nrow(y)){
  vincitore_co[i] <- colnames(y)[which.max(y[i,])]
}
data_circ$vincitore_co=vincitore_co

# Inserisco il COD_CIRC per il join
data_circ <- data_circ %>% 
  mutate(COD_CIRC=str_sub(codice,start=1, end=2),
         COD_CIRC=as.character(as.numeric(COD_CIRC))) %>% 
  dplyr::select(COD_CIRC, everything())




# Collegi plurinominali ####
scrutiniCI_p <- read_csv("scrutiniCI_p.csv")

#Rendo numeri i perc
scrutiniCI_p <- scrutiniCI_p %>% 
  mutate(perc=as.numeric(str_replace(perc, ",", ""))/100,
         perc_cifra_el=as.numeric(str_replace(perc_cifra_el, ",", ""))/100)

scrutiniCI_p <- scrutiniCI_p %>% 
  mutate(
    descr_lista=str_to_upper(stri_trans_general(descr_lista,"Latin-ASCII"))    
  )

# scrutiniCI_p %>% 
#   count(tipo_riga)
# # CU: candidati uninominali
# # LI: lista
# # TP: totale proporzionale
# # TS: totale seggi (di ogni seggio CU+LI)
# # TU: totale uninominale
# # XX: separatore

pluri <- scrutiniCI_p %>% 
  filter(tipo_riga=="LI") %>% 
  select(codice, lista=descr_lista, voti, perc)

pluri <- pluri %>% 
  left_join(camera_geopolitico_italia, c(codice="id")) %>% 
  select(codice, collegio=nome, everything())

# Correggo le percentuali
pluri <- pluri %>%
  group_by(codice) %>% 
  mutate(perc=voti/sum(voti)*100) %>% 
  ungroup

# Metto i nomi abbreviati
# nomi_abbreviati
pluri <- pluri  %>% 
  left_join(nomi_abbreviati, by=c("lista"="names_long")) %>% 
  mutate(lista = ifelse(!is.na(names_short), names_short, lista)) %>% 
  select(-names_short)

# Aggiungo lo schieramento
pluri <- pluri %>% 
  left_join(schieramento)

# Voti alle coalizioni
pluri_co <- pluri %>% 
  group_by(codice, collegio, coalizione) %>% 
  summarize(voti=sum(voti),
            perc=sum(perc))

# Preparazione per la cluster analysis
data_pluri <- pluri %>% 
  dplyr::select(codice, collegio, lista, perc) %>% 
  spread(lista, perc)

data_pluri_co <- pluri_co %>% 
  dplyr::select(codice, collegio, coalizione, perc) %>% 
  spread(coalizione, perc)

data_pluri[,3:ncol(data_pluri)] <-
  data_pluri[,3:ncol(data_pluri)] %>% 
  replace(is.na(.),0)

data_pluri_co[,3:ncol(data_pluri_co)] <-
  data_pluri_co[,3:ncol(data_pluri_co)] %>% 
  replace(is.na(.),0)

data_pluri <- data_pluri %>%
  dplyr::select(codice, collegio,
                `LEGA`,
                `FI`,
                `FDI`,
                `UDC`,
                `M5S`,
                `PD`,
                `+E`,
                `IEI`,
                `CP`,
                `SVP`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

data_pluri_co <- data_pluri_co %>%
  dplyr::select(codice, collegio,
                `CDX`,
                `M5S`,
                `CSX`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

# Cluster analysis
x <- data_pluri[3:ncol(data_pluri)]
x=as.matrix(x)
row.names(x) = data_pluri$collegio

#Average linkage
d <- dist(x)
ls_av_pluri <- hclust(d, method = "average")

#Cerco il numero ottimale di cluster
m <- length(ls_av_pluri$height) #Numero di iterazioni (n-1)
s <- c()
for (j in m:2){
  s <- c(s, ls_av_pluri$height[j]/ls_av_pluri$height[j - 1])
}

# plot(s,type="b",pch=19,xlab="cluster #",ylab="relative separation")

# order(-s)
best_k_pluri <- order(-s)+1

# plot(ls_av_pluri)
# rect.hclust(ls_av_pluri,k=best_k_pluri[1], border="red")
# #rect.hclust(ls_av_pluri,k=best_k_pluri[2], border="green", which=c(3,4))
# rect.hclust(ls_av_pluri,k=best_k_pluri[3], border="blue")

data_pluri$cluster1 <- as.factor(cutree(ls_av_pluri, k = best_k_pluri[1]))
data_pluri$cluster2 <- as.factor(cutree(ls_av_pluri, k = best_k_pluri[3]))

# Aggiungo il vincitore a ogni collegio
vincitore=rep("",nrow(x))
for(i in 1:nrow(x)){
  vincitore[i] <- colnames(x)[which.max(x[i,])]
}
data_pluri$vincitore=vincitore

y <- data_pluri_co[3:ncol(data_pluri_co)]
y=as.matrix(y)
row.names(y) = data_pluri_co$collegio

vincitore_co=rep("",nrow(y))
for(i in 1:nrow(y)){
  vincitore_co[i] <- colnames(y)[which.max(y[i,])]
}
data_pluri$vincitore_co=vincitore_co

# Inserisco il COD_P per il join
data_pluri <- data_pluri %>% 
  mutate(COD_P=str_sub(codice,start=1, end=3),
         COD_P=as.character(as.numeric(COD_P))) %>% 
  dplyr::select(COD_P, everything())


# Collegi uninominali ####

scrutiniCI_u <- read_csv("scrutiniCI_u.csv")

#Rendo numeri i perc
scrutiniCI_u <- scrutiniCI_u %>% 
  mutate(perc=as.numeric(str_replace(perc, ",", ""))/100,
         perc_cifra_el=as.numeric(str_replace(perc_cifra_el, ",", ""))/100)

scrutiniCI_u <- scrutiniCI_u %>% 
  mutate(
    descr_lista=str_to_upper(stri_trans_general(descr_lista,"Latin-ASCII"))    
  )

# scrutiniCI_u %>% 
#   count(tipo_riga)
# # CU: candidati uninominali
# # LI: lista
# # TP: totale proporzionale
# # TS: totale seggi (di ogni seggio CU+LI)
# # TU: totale uninominale
# # XX: separatore

uni <- scrutiniCI_u %>% 
  filter(tipo_riga=="LI") %>% 
  select(codice, lista=descr_lista, voti, perc)

uni <- uni %>% 
  left_join(camera_geopolitico_italia, c(codice="id")) %>% 
  select(codice, collegio=nome, everything())

# uni %>%
#   filter(is.na(voti))
# uni %>%
#   filter(is.na(perc))
# # Il problema è la valle d'Aosta (che non conta per il proporzionale)

# Tolgo la Valle d'Aosta
uni <- uni %>% 
  filter(!is.na(voti))

# Correggo le percentuali
uni <- uni %>%
  group_by(codice) %>% 
  mutate(perc=voti/sum(voti)*100) %>% 
  ungroup


# Metto i nomi abbreviati
uni <- uni  %>% 
  left_join(nomi_abbreviati, by=c("lista"="names_long")) %>% 
  mutate(lista = ifelse(!is.na(names_short), names_short, lista)) %>% 
  select(-names_short)

# Aggiungo gli schieramenti
uni <- uni %>% 
  left_join(schieramento)

# Voti alle coalizioni
uni_co <- uni %>% 
  group_by(codice, collegio, coalizione) %>% 
  summarize(voti=sum(voti),
            perc=sum(perc))

# Preparazione per la cluster analysis
data_uni <- uni %>% 
  dplyr::select(codice, collegio, lista, perc) %>% 
  spread(lista, perc)

data_uni_co <- uni_co %>% 
  dplyr::select(codice, collegio, coalizione, perc) %>% 
  spread(coalizione, perc)

data_uni[,3:ncol(data_uni)] <-
  data_uni[,3:ncol(data_uni)] %>% 
  replace(is.na(.),0)

data_uni_co[,3:ncol(data_uni_co)] <-
  data_uni_co[,3:ncol(data_uni_co)] %>% 
  replace(is.na(.),0)

data_uni <- data_uni %>%
  dplyr::select(codice, collegio,
                `LEGA`,
                `FI`,
                `FDI`,
                `UDC`,
                `M5S`,
                `PD`,
                `+E`,
                `IEI`,
                `CP`,
                `SVP`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

data_uni_co <- data_uni_co %>%
  dplyr::select(codice, collegio,
                `CDX`,
                `M5S`,
                `CSX`,
                `LEU`,
                `POTERE AL POPOLO!`,
                `CASAPOUND ITALIA`,
                `IL POPOLO DELLA FAMIGLIA`,
                `ITALIA AGLI ITALIANI`,
                `PARTITO COMUNISTA`,
                `PARTITO VALORE UMANO`,
                `10 VOLTE MEGLIO`,
                `PER UNA SINISTRA RIVOLUZIONARIA`,
                `PARTITO REPUBBLICANO ITALIANO - ALA`,
                `GRANDE NORD`,
                `AUTODETERMINATZIONE`,
                `LISTA DEL POPOLO PER LA COSTITUZIONE`,
                `PATTO PER L'AUTONOMIA`,
                `BLOCCO NAZIONALE PER LE LIBERTA'`,
                `SIAMO`,
                `RINASCIMENTO MIR`,
                `ITALIA NEL CUORE`)

# Hierarchical cluster analysis

x <- data_uni[3:ncol(data_uni)]
x=as.matrix(x)
row.names(x) = data_uni$collegio

#Average linkage
d <- dist(x)
ls_av_uni <- hclust(d, method = "average")

#Cerco il numero ottimale di cluster
m <- length(ls_av_uni$height) #Numero di iterazioni (n-1)
s <- c()
for (j in m:2){
  s <- c(s, ls_av_uni$height[j]/ls_av_uni$height[j - 1])
}

# plot(s,type="b",pch=19,xlab="cluster #",ylab="relative separation")

# order(-s)
best_k_uni <- order(-s)+1

# plot(ls_av_uni, hang = -1, cex = 0.5)
# #rect.hclust(ls_av_uni,k=best_k_uni[1], border="red")
# rect.hclust(ls_av_uni,k=best_k_uni[2], border="green")
# rect.hclust(ls_av_uni,k=best_k_uni[3], border="blue")
# rect.hclust(ls_av_uni,k=best_k_uni[4], border="red")

data_uni$cluster1 <- as.factor(cutree(ls_av_uni, k = best_k_uni[2]))
data_uni$cluster2 <- as.factor(cutree(ls_av_uni, k = best_k_uni[3]))
data_uni$cluster3 <- as.factor(cutree(ls_av_uni, k = best_k_uni[4]))

# Aggiungo il vincitore a ogni collegio
vincitore=rep("",nrow(x))
for(i in 1:nrow(x)){
  vincitore[i] <- colnames(x)[which.max(x[i,])]
}
data_uni$vincitore=vincitore

y <- data_uni_co[3:ncol(data_uni_co)]
y=as.matrix(y)
row.names(y) = data_uni_co$collegio

vincitore_co=rep("",nrow(y))
for(i in 1:nrow(y)){
  vincitore_co[i] <- colnames(y)[which.max(y[i,])]
}
data_uni$vincitore_co=vincitore_co

# Correggo il nome del collegio togliondo il numero
data_uni <- data_uni %>% 
  mutate(collegio=str_replace(collegio, "^[:digit:]{2} (- )?", ""))





