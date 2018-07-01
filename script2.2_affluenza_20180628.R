#####
# Affluenza
# 28/06/2018
#####

# Analisi aggregata ####

# Provincie
d_aff2 <- votantiCIprovince %>% 
  dplyr::select(perc_ore12, perc_ore19, perc_ore23, percprec_ore23) %>% 
  gather("hour","perc")

p_aff_boxplot2 <- ggplot(data=d_aff2, aes(x=hour,y=perc)) +
  geom_boxplot() +
  geom_abline(aes(intercept=0, slope=0), linetype=2) +
  geom_abline(aes(intercept=100, slope=0), linetype=2) +
  labs(title="Affluenza provincie") +
  scale_x_discrete(labels=c("12:00","19:00","23:00", "prec")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_cartesian(ylim=c(0,100))

p_aff_boxplot2 <- p_aff_boxplot2 +
  stat_summary(data=filter(d_aff2, hour!="percprec_ore23"), fun.y=mean, geom="line", aes(group=1), color="red") + 
  stat_summary(fun.y=mean, geom="point", color="red", size=2) +
  geom_abline(aes(intercept=mean(votantiCIComuni$percprec_ore23, na.rm=T), slope=0), color="red")


# Comuni
d_aff1 <- votantiCIComuni %>% 
  select(perc_ore12, perc_ore19, perc_ore23, percprec_ore23) %>% 
  gather("hour","perc")

p_aff_boxplot1 <- ggplot(data=d_aff1, aes(x=hour,y=perc)) +
  geom_boxplot() +
  geom_abline(aes(intercept=0, slope=0), linetype=2) +
  geom_abline(aes(intercept=100, slope=0), linetype=2) +
  labs(title="Affluenza comuni") +
  scale_x_discrete(labels=c("12:00","19:00","23:00", "prec")) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  coord_cartesian(ylim=c(0,100))

p_aff_boxplot1 <- p_aff_boxplot1 +
  stat_summary(data=filter(d_aff1, hour!="percprec_ore23"), fun.y=mean, geom="line", aes(group=1), color="red") + 
  stat_summary(fun.y=mean, geom="point", color="red", size=2) +
  geom_abline(aes(intercept=mean(votantiCIComuni$percprec_ore23, na.rm=T), slope=0), color="red")


# Diagrammi a dispersione
p_aff_disp2 <- votantiCIprovince %>% 
  ggplot(aes(x=percprec_ore23, y=perc_ore23))+
  geom_point(aes(text=paste("<b>", ente, "</b>",
                            "<br />", REGIONE,
                            "<br />Perc prec: ", round(`percprec_ore23`,1), "%",
                            "<br />Perc 23:00: ", round(`perc_ore23`,1), "%"),
                 color=REGIONE),
             alpha=.8) +
  geom_smooth(method=lm) +
  #geom_abline(intercept=0, slope=1, linetype=3) +
  labs(title="Affluenza nelle province") +
  theme(legend.position = "none")

p_aff_disp1 <- votantiCIComuni %>% 
  ggplot(aes(x=percprec_ore23, y=perc_ore23))+
  geom_point(aes(text=paste("<b>", ente, "</b>",
                            "<br />", REGIONE,
                            "<br />Perc prec: ", round(`percprec_ore23`,1), "%",
                            "<br />Perc 23:00: ", round(`perc_ore23`,1), "%"),
                 color=REGIONE),
             alpha=.5) +
  geom_smooth(method=lm) +
  #geom_abline(intercept=0, slope=1, linetype=3) +
  labs(title="Affluenza nei comuni") +
  theme(legend.position = "none")



# Mappe geografiche ####

head(prov2.points)
votantiCIprovince

data_aff_prov <- select(votantiCIprovince, COD_PRO, perc_ore23) %>%
  right_join(prov2.points, by="COD_PRO")

p_aff_prov <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_aff_prov, aes(x=long, y=lat, group=group, fill=perc_ore23,
                              text=paste("<b>", PROVINCIA, "</b>",
                                         "<br />Affluenza: ", round(perc_ore23,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_viridis(name = "Affluenza (%)") +
  labs(title="Affluenza a livello provinciale")

# Comuni

# votantiCIComuni %>%
#   anti_join(cm2_shp_data, by="PRO_COM")
# cm2_shp_data %>%
#   anti_join(votantiCIComuni, by="PRO_COM")
# # Gli unici problemi sono i nuovi comuni del FVG

data_aff_cm_1 <- select(votantiCIComuni, PRO_COM, perc_ore23) %>%
  right_join(cm2.points, by="PRO_COM")

p_aff_cm_1 <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_aff_cm_1, aes(x=long, y=lat, group=group, fill=perc_ore23,
                              text=paste("<b>", COMUNE, "</b>",
                                         "<br />Affluenza: ", round(perc_ore23,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_viridis(name = "Affluenza (%)") +
  labs(title="Affluenza a livello comunale")

max <- round(max(votantiCIprovince$perc_ore23, na.rm=T), -1)
min <- round(min(votantiCIprovince$perc_ore23, na.rm=T), -1)

my_breaks <- seq(from=60, to=80, by=5)
my_labs <- c("60-","65","70","75","80+")

data_aff_cm_2 <- data_aff_cm_1 %>% 
  mutate(perc_ore23_schiac = ifelse(perc_ore23>80, 80, perc_ore23),
         perc_ore23_schiac = ifelse(perc_ore23_schiac<60, 60, perc_ore23_schiac))

p_aff_cm_2 <- ggplot() +
  geom_polygon(data=reg2.points, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=data_aff_cm_2, aes(x=long, y=lat, group=group,
                              fill=perc_ore23_schiac,
                              text=paste("<b>", COMUNE, "</b>",
                                         "<br />Affluenza: ", round(perc_ore23,1),"%"))) +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "bisque")) +
  scale_fill_viridis(name = "Affluenza (%)",
                     breaks = my_breaks, labels = my_labs) +
  labs(title="Affluenza a livello comunale")

