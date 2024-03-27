
##################################
#### LOAD DATA
##################################

source("read_data_functions.R")

##################################
# FIGURE 3 #######################
##################################

#### size x distance
stra_d_sz <- plotg.col(stramonita_sz, 4, 7, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Stramonita haemastoma') +
  theme(plot.title = element_text(face = "bold.italic"))

myti_d_sz <- plotg.col(brachi_sz, 13, 2, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Mytilaster solisianus') +
  theme(plot.title = element_text(face = "bold.italic"))

tetra_d_sz <- plotg.col(tetra_sz, 14, 3, "Distance from southernmost site (km)","Size (mm)") + 
  ggtitle ('Tetraclita stalactifera') +
  theme(plot.title = element_text(face = "bold.italic"))

lapa_d_sz <- lapas_sz %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% 
              distinct()) %>%
  filter(tamanho_mm > 0) %>% 
  plotg.col(., 19, 4, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Lottia subrugosa') +
  theme(plot.title = element_text(face = "bold.italic"))

echi_d_sz <- plotg.col(nodi_sz, 17, 2, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Echinolittorina lineolata') +
  theme(plot.title = element_text(face = "bold.italic")) 

legs <- ggplot(nodi_sz, aes(subregion, tamanho_mm, color=subregion)) + 
  geom_point() + 
  scale_color_discrete(name="Subregion") + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        legend.text=element_text(size=14)) +
  scale_color_manual(values=c("purple3", "steelblue2","forestgreen", "goldenrod2","darkorange1", "firebrick4"))


legend1 <- g_legend(legs)

# plot figure 2
ggpubr::ggarrange(stra_d_sz, myti_d_sz, tetra_d_sz, lapa_d_sz, echi_d_sz, legend1, ncol=2, nrow=3)


##################################
# FIGURE 4 #######################
##################################

#### density/cover x distance
str_d_ab <- stramonita_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  ggplot(., aes(x=distance_S, y=abund, color=subregion, group=distance_S)) +
  geom_point(size= 2.2, alpha=0.8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=12, color="black"), 
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16),
        axis.ticks = element_line(colour = "black", size = 0.8),
        strip.text.x = element_text(size = 10), 
        strip.text.y = element_text(size = 10),
        axis.line = element_line(colour = 'black', size = 0.8),
        axis.ticks.length = unit(4, "pt"), 
        legend.position = "",
        plot.title = element_text(size = 12, face="bold")) +
  labs(x="Distance from southernmost site (km)", y="Total abundance (n)") +
  ggtitle ('Stramonita haemastoma') +
  theme(plot.title = element_text(face = "bold.italic")) +
  scale_color_manual(values=c("purple3", "steelblue2","forestgreen", "goldenrod2","darkorange1", "firebrick4"))


myti_d_cv <- plotg.col(brachi_cv, 14, 3, "Distance from southernmost site (km)", "Relative cover") + 
  ggtitle ('Mytilaster solisianus') +
  theme(plot.title = element_text(face = "bold.italic"))

tetra_d_ab <- tetra_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  plotg.col(., 14, 3, "Distance from southernmost site (km)", expression(paste("Density (ind 100 ",cm^-2,")"))) + 
  ggtitle ('Tetraclita stalactifera') +
  theme(plot.title = element_text(face = "bold.italic"))

lapa_d_ab <- lapas_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  plotg.col(., 18, 2, "Distance from southernmost site (km)", expression(paste("Density (ind 100 ",cm^-2,")"))) + 
  ggtitle ('Lottia subrugosa') +
  theme(plot.title = element_text(face = "bold.italic"))

echi_d_ab <- nodi_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  ggplot(., aes(distance_S, abund, color=subregion)) + 
  geom_point(size= 2.2, alpha=0.8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=12, color="black"), 
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16),
        axis.ticks = element_line(colour = "black", size = 0.8),
        strip.text.x = element_text(size = 10), 
        strip.text.y = element_text(size = 10),
        axis.line = element_line(colour = 'black', size = 0.8),
        axis.ticks.length = unit(4, "pt"), 
        legend.position = "",
        plot.title = element_text(size = 12, face="bold")) +
  labs(x="Distance from southernmost site (km)", y="Total abundance (n)") +
  ggtitle ('Echinolittotina lineolata') +
  theme(plot.title = element_text(face = "bold.italic")) +
  scale_color_manual(values=c("purple3", "steelblue2","forestgreen", "goldenrod2","darkorange1", "firebrick4"))


ggpubr::ggarrange(str_d_ab, myti_d_cv, tetra_d_ab, lapa_d_ab, echi_d_ab, legend1, ncol=2, nrow=3)

rm(list=ls())
