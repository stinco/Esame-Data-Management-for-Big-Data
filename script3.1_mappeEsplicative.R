#####
# Mappe delle variabili esplicative
# 29/06/2018
#####

# Mappe
data <- indicatori %>%
  dplyr::select(collegio, LDAB, IV, AAST, PDU, ST, TSO, OSS, EDI, POP_NFR, IVMS) %>% 
  right_join(uni.points, by="collegio")


p_st <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=ST,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Stranieri: ", round(ST,1) ,"‰")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione del tasso di stranieri",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_aast <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=AAST,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Analfabeti: ", AAST ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione del tasso di analfabetismo",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_tso <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=TSO,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Analfabeti: ", TSO ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione del tasso di occupazione",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_edi <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=EDI,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Analfabeti: ", EDI ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione delle residenze antiche",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_nfr <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=POP_NFR,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", POP_NFR ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione dei residenti nati fuori regione",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_pdu <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=PDU,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", PDU ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione dei laureati",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_ivms <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=IVMS,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", IVMS ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione della vulnerabilita' sociale",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_ldab <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=LDAB,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", LDAB ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione densita' abitativa",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_iv <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=IV,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", IV ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione della vecchiaia",
       subtitle="nei collegi uninominali",
       fill="Tasso")

p_oss <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data,
               aes(x=long, y=lat, group=group, fill=OSS,
                   text=paste("<b>", collegio, "</b>",
                              "<br />Nati Fuori Regione: ", OSS ,"%")
               )) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_gradient(low="ghostwhite",high="blue") +
  labs(title="Distribuzione degli occupati nei servizi",
       subtitle="nei collegi uninominali",
       fill="Tasso")
























