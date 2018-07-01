#####
# Mappe voto
# 29/06/2018
#####

# Vincitori ####

# Circoscrizione
data_circ_vinc <- left_join(circ.points,
                  select(data_circ, COD_CIRC, vincitore, vincitore_co),
                  by="COD_CIRC")

p_circ_vinc <- ggplot() +
  geom_polygon(data=data_circ_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_lega, col_m5s, col_pd, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nelle circoscrizioni",
       fill="Vincitore")

p_circ_vinc_co <- ggplot() +
  geom_polygon(data=data_circ_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nelle circoscrizioni",
       fill="Vincitore")

# Collegi plurinominali
data_pluri_vinc <- left_join(pluri.points,
                             select(data_pluri, collegio, vincitore, vincitore_co),
                             by="collegio")

p_pluri_vinc <- ggplot() +
  geom_polygon(data=data_pluri_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_lega, col_m5s, col_pd, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nei collegi plurinominali",
       fill="Vincitore")

p_pluri_vinc_co <- ggplot() +
  geom_polygon(data=data_pluri_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nei collegi plurinominali",
       fill="Vincitore")



# Collegi uninominali
data_uni_vinc <- left_join(uni.points,
                           select(data_uni, collegio, vincitore, vincitore_co),
                           by="collegio")

p_uni_vinc <- ggplot() +
  geom_polygon(data=data_uni_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.1) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_lega, col_m5s, col_pd, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nei collegi uninominali",
       fill="Vincitore")

p_uni_vinc_co <- ggplot() +
  geom_polygon(data=data_uni_vinc,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.1) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nei collegi uninominali",
       fill="Vincitore")

# Intensità voti ####

# Solo a livello di collegio uninominale
# Lega
data_lega <- data_uni %>%
  dplyr::select(collegio, LEGA) %>% 
  right_join(uni.points, by="collegio")

p_lega <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_lega,
               aes(x=long, y=lat, group=group, fill=LEGA)) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali",
       fill="Voti (%)")

# FI
data_fi <- data_uni %>%
  dplyr::select(collegio, `FI`) %>% 
  right_join(uni.points, by="collegio")

p_fi <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_fi,
               aes(x=long, y=lat, group=group, fill=`FI`)) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali",
       fill="Voti (%)")

# M5S
data_m5s <- data_uni %>%
  dplyr::select(collegio, `M5S`) %>% 
  right_join(uni.points, by="collegio")

p_m5s <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_m5s,
               aes(x=long, y=lat, group=group, fill=`M5S`)) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali",
       fill="Voti (%)")

# PD
data_pd <- data_uni %>%
  dplyr::select(collegio, `PD`) %>% 
  right_join(uni.points, by="collegio")

p_pd <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_pd,
               aes(x=long, y=lat, group=group, fill=`PD`)) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali",
       fill="Voti (%)")



# Cluster ####

# Circoscrizioni
data_circ_cluster <- left_join(circ.points,
                               select(data_circ, COD_CIRC, cluster1, cluster2),
                               by="COD_CIRC")

p_circ_cluster1 <- ggplot() +
  geom_polygon(data=data_circ_cluster,
               aes(x=long, y=lat, group=group, fill=cluster1),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_lega, col_m5s, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Circoscrizioni divise in 3 cluster")

p_circ_cluster2 <- ggplot() +
  geom_polygon(data=data_circ_cluster,
               aes(x=long, y=lat, group=group, fill=cluster2),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_pd, col_lega, col_m5s, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Circoscrizioni divise in 4 cluster")


# Plurinominali
data_pluri_cluster <- left_join(pluri.points,
                                select(data_pluri, collegio, cluster1, cluster2),
                                by="collegio")

p_pluri_cluster1 <- ggplot() +
  geom_polygon(data=data_pluri_cluster,
               aes(x=long, y=lat, group=group, fill=cluster1),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_lega, col_m5s, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Collegi plurinominali divisi in 3 cluster")

p_pluri_cluster2 <- ggplot() +
  geom_polygon(data=data_pluri_cluster,
               aes(x=long, y=lat, group=group, fill=cluster2),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_pd, col_lega, col_leu, col_m5s, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Collegi plurinominali divisi in 5 cluster")



# Uninominali

data_uni_cluster <- left_join(uni.points,
                              select(data_uni, collegio, cluster1, cluster2, cluster3),
                              by="collegio")

p_uni_cluster1 <- ggplot() +
  geom_polygon(data=data_uni_cluster,
               aes(x=long, y=lat, group=group, fill=cluster1),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_lega, col_m5s, col_altro, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Collegi uninominali \ndivisi in 4 cluster")

p_uni_cluster2 <- ggplot() +
  geom_polygon(data=data_uni_cluster,
               aes(x=long, y=lat, group=group, fill=cluster2),
               color="black", size=.5) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_leu, col_lega, col_m5s, col_altro, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Collegi uninominali \ndivisi in 5 cluster")

p_uni_cluster3 <- ggplot() +
  geom_polygon(data=data_uni_cluster,
               aes(x=long, y=lat, group=group, fill=cluster3),
               color="black", size=.1) +
  geom_path(data=reg2.points, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque"),
        legend.position="none") +
  scale_fill_manual(values=c(col_leu, col_pd, col_lega, col_m5s,
                             col_fi, col_altro, col_svp),
                    na.value="#6f6f6f") +
  labs(title="Collegi uninominali \ndivisi in 7 cluster")












