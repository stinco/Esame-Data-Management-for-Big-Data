#####
# Mappe concentrazione regione
# 29/06/2018
#####

# Lombardia ####
data <- data_uni_lomb %>%
  dplyr::select(collegio, LEGA, FI, M5S, PD) %>% 
  right_join(uni.points_lomb, by="collegio")

p_lomb_lega <- ggplot() +
  geom_polygon(data=reg2.points_lomb, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LEGA,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Voti (%)")

p_lomb_fi <- ggplot() +
  geom_polygon(data=reg2.points_lomb, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Voti (%)")

p_lomb_m5s <- ggplot() +
  geom_polygon(data=reg2.points_lomb, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`M5S`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Voti (%)")

p_lomb_pd <- ggplot() +
  geom_polygon(data=reg2.points_lomb, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`PD`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Voti (%)")

# Piemonte ####
data <- data_uni_piem %>%
  dplyr::select(collegio, LEGA, FI, M5S, PD) %>% 
  right_join(uni.points_piem, by="collegio")

p_piem_lega <- ggplot() +
  geom_polygon(data=reg2.points_piem, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LEGA,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Voti (%)")

p_piem_fi <- ggplot() +
  geom_polygon(data=reg2.points_piem, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Voti (%)")

p_piem_m5s <- ggplot() +
  geom_polygon(data=reg2.points_piem, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`M5S`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Voti (%)")

p_piem_pd <- ggplot() +
  geom_polygon(data=reg2.points_piem, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`PD`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Voti (%)")


# Lazio ####

data <- data_uni_lazio %>%
  dplyr::select(collegio, LEGA, FI, FDI, M5S, PD, `+E`) %>% 
  right_join(uni.points_lazio, by="collegio")

p_lazio_lega <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LEGA,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")

# FI

p_lazio_fi <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")

#FDI
p_lazio_fdi <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FDI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fdi) +
  labs(title="Distribuzione dei voti di Fratelli d'Italia",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")


# M5S

p_lazio_m5s <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`M5S`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")

# PD

p_lazio_pd <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`PD`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")


#+E
p_lazio_e <- ggplot() +
  geom_polygon(data=reg2.points_lazio, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`+E`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />FDI: ", round(FDI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%",
                              "<br />+E: ", round(`+E`,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_leu) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti di +Europa",
       subtitle="nei collegi uninominali del Lazio",
       fill="Voti (%)")





# Veneto ####


collegi_uni_ven <- uni_shp_data %>% 
  filter(str_detect(CAM17U_DEN, "VENETO")) %>% 
  select(CAM17U_COD, CAM17U_DEN, collegio)

uni.points_ven <- filter(uni.points, str_detect(CAM17U_DEN, "VENETO"))
reg2.points_ven <- filter(reg2.points, REGIONE=="VENETO")

data_uni_ven <- data_uni %>% 
  right_join(collegi_uni_ven, by="collegio") %>% 
  select(-CAM17U_COD, -CAM17U_DEN)

data <- data_uni_ven %>%
  dplyr::select(collegio, LEGA, FI, M5S, PD) %>% 
  right_join(uni.points_ven, by="collegio")

p_ven_lega <- ggplot() +
  geom_polygon(data=reg2.points_ven, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LEGA,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali del Veneto",
       fill="Voti (%)")

p_ven_fi <- ggplot() +
  geom_polygon(data=reg2.points_ven, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali del Veneto",
       fill="Voti (%)")

p_ven_m5s <- ggplot() +
  geom_polygon(data=reg2.points_ven, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`M5S`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali del Veneto",
       fill="Voti (%)")

p_ven_pd <- ggplot() +
  geom_polygon(data=reg2.points_ven, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`PD`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali del Veneto",
       fill="Voti (%)")

# Campania ####
collegi_uni_camp <- uni_shp_data %>% 
  filter(str_detect(CAM17U_DEN, "CAMPANIA")) %>% 
  select(CAM17U_COD, CAM17U_DEN, collegio)

uni.points_camp <- filter(uni.points, str_detect(CAM17U_DEN, "CAMPANIA"))
reg2.points_camp <- filter(reg2.points, REGIONE=="CAMPANIA")

data_uni_camp <- data_uni %>% 
  right_join(collegi_uni_camp, by="collegio") %>% 
  select(-CAM17U_COD, -CAM17U_DEN)

data <- data_uni_camp %>%
  dplyr::select(collegio, LEGA, FI, M5S, PD) %>% 
  right_join(uni.points_camp, by="collegio")

p_camp_lega <- ggplot() +
  geom_polygon(data=reg2.points_camp, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LEGA,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_lega) +
  labs(title="Distribuzione dei voti della Lega",
       subtitle="nei collegi uninominali della Campania",
       fill="Voti (%)")

p_camp_fi <- ggplot() +
  geom_polygon(data=reg2.points_camp, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`FI`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_fi) +
  labs(title="Distribuzione dei voti di Forza Italia",
       subtitle="nei collegi uninominali della Campania",
       fill="Voti (%)")

p_camp_m5s <- ggplot() +
  geom_polygon(data=reg2.points_camp, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`M5S`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_m5s) +
  labs(title="Distribuzione dei voti del Movimento 5 Stelle",
       subtitle="nei collegi uninominali della Campania",
       fill="Voti (%)")

p_camp_pd <- ggplot() +
  geom_polygon(data=reg2.points_camp, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=`PD`,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Lega: ", round(LEGA,1),"%",
                              "<br />FI: ", round(FI,1),"%",
                              "<br />M5S: ", round(M5S,1),"%",
                              "<br />PD: ", round(PD,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high=col_pd) +
  #guides(fill=FALSE) +
  labs(title="Distribuzione dei voti del Partito Democratico",
       subtitle="nei collegi uninominali della Campania",
       fill="Voti (%)")


