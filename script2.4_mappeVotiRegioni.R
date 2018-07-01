#####
# Mappe voti regioni
# 29/06/2018
#####


# Lombardia ####
collegi_uni_lomb <- uni_shp_data %>% 
  filter(str_detect(CAM17U_DEN, "LOMBARDIA")) %>% 
  select(CAM17U_COD, CAM17U_DEN, collegio)

uni.points_lomb <- filter(uni.points, str_detect(CAM17U_DEN, "LOMBARDIA"))
reg2.points_lomb <- filter(reg2.points, REGIONE=="LOMBARDIA")

data_uni_lomb <- data_uni %>% 
  right_join(collegi_uni_lomb, by="collegio") %>% 
  select(-CAM17U_COD, -CAM17U_DEN)

# Vincitori
data <- left_join(uni.points_lomb,
                  select(data_uni_lomb, collegio, vincitore, vincitore_co),
                  by="collegio")

p_uni_lomb_vinc <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.5) +
  geom_path(data=reg2.points_lomb, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_lega, col_m5s, col_pd),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Vincitore")

p_uni_lomb_vinc_co <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.5) +
  geom_path(data=reg2.points_lomb, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nei collegi uninominali della Lombardia",
       fill="Vincitore")



# Piemonte ####
collegi_uni_piem <- uni_shp_data %>% 
  filter(str_detect(CAM17U_DEN, "PIEMONTE")) %>% 
  select(CAM17U_COD, CAM17U_DEN, collegio)

uni.points_piem <- filter(uni.points, str_detect(CAM17U_DEN, "PIEMONTE"))
reg2.points_piem <- filter(reg2.points, REGIONE=="PIEMONTE")

data_uni_piem <- data_uni %>% 
  right_join(collegi_uni_piem, by="collegio") %>% 
  select(-CAM17U_COD, -CAM17U_DEN)

# Vincitori
data <- left_join(uni.points_piem,
                  select(data_uni_piem, collegio, vincitore, vincitore_co),
                  by="collegio")

p_uni_piem_vinc <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.5) +
  geom_path(data=reg2.points_piem, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_lega, col_m5s, col_pd),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Vincitore")

p_uni_piem_vinc_co <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.5) +
  geom_path(data=reg2.points_piem, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nei collegi uninominali del Piemonte",
       fill="Vincitore")




# Lazio ####
collegi_uni_lazio <- uni_shp_data %>% 
  filter(str_detect(CAM17U_DEN, "LAZIO")) %>% 
  select(CAM17U_COD, CAM17U_DEN, collegio)

uni.points_lazio <- filter(uni.points, str_detect(CAM17U_DEN, "LAZIO"))
reg2.points_lazio <- filter(reg2.points, REGIONE=="LAZIO")

data_uni_lazio <- data_uni %>% 
  right_join(collegi_uni_lazio, by="collegio") %>% 
  select(-CAM17U_COD, -CAM17U_DEN)

# Vincitori
data <- left_join(uni.points_lazio,
                  select(data_uni_lazio, collegio, vincitore, vincitore_co),
                  by="collegio")

p_uni_lazio_vinc <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore),
               color="black", size=.5) +
  geom_path(data=reg2.points_lazio, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_m5s, col_pd),
                    na.value="#6f6f6f") +
  labs(title="Lista vincitrice",
       subtitle="nei collegi uninominali del Lazio",
       fill="Vincitore")

p_uni_lazio_vinc_co <- ggplot() +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=vincitore_co),
               color="black", size=.5) +
  geom_path(data=reg2.points_lazio, aes(x=long, y=lat, group=group),
            color="black", size=1) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_manual(values=c(col_fi, col_pd, col_m5s),
                    na.value="#6f6f6f") +
  labs(title="Coalizione vincitrice",
       subtitle="nei collegi uninominali del Lazio",
       fill="Vincitore")





