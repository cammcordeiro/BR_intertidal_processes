library(openxlsx)
library(tidyverse)
library(glmmTMB)
library(insight)
library(ggeffects)

# setwd("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/Brazil_project")

source("functions/summarySE.r")
source("functions/HighstatLibV6.r")
source("functions/plot_pca_sep.r")
source("functions/func_plot_var.R")

##################################
#### LOAD DATA
##################################

abiot <- read.csv("data/abioticos_2020.csv", header=T)

ordem <- abiot %>% 
  distinct(site, distance_S, lat, long) %>% 
  rownames_to_column("position")

# ECHINOLITTORINA
nodi_sz <- read.csv("data/nodi_sz2020.csv", header=T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

nodi_ab <- read.delim("data/nodi.txt", header=T, encoding="UTF-8") %>% 
  rename(site = X.U.FEFF.site) %>% 
  filter(abund < 1000) %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

# TETRACLITA
tetra_sz <- read.csv("data/tetra_sz2020.csv", header=T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

tetra_ab <- read.csv("data/tetra_ab2020.csv", header=T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

# LOTTIA
lapas_sz <- read.csv("data/lottia_sz2020.csv", header=T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

lapas_ab <- read.delim("data/lottia_ab.txt", header=T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

# STRAMONITA
stramonita_sz <- read.delim("data/stramonita_size.txt", header = T, encoding = "UTF-8") %>% 
  mutate(site = recode(site, 'Ast\xfarias' = 'Astúrias',
                       'Grande (south3)' = 'Grande (Ubatuba)'),
         subregion = recode(subregion, "south1" = "MRBS",
                            "south2" = "SSCh", 
                            "south3" = "Ubatuba",
                            "north1" = "SCRJ",
                            "north2" = "MRRJ",
                            "north3" = "LRRJ")) %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

stramonita_ab <- read.delim("data/stramonita_abundance.txt", header = T) %>% 
  mutate(site = dplyr::recode(site, 
                              '\xc9den' = 'Éden',
                              'Ast\xfarias' = 'Astúrias',
                              'C. Itagu\xe1' = 'Canto do Itaguaí',
                              'Enseada (Guaruj\xe1)' = 'Enseada (Guarujá)',
                              'F\xe9lix' = 'Félix',
                              'P. Coqueiros' = 'Coqueiro',
                              'Gerib\xe1' = 'Geribá',
                              'Guaec\xe1' = 'Guaecá',
                              'Guai\xfaba' = 'Guaiúba',
                              'Indai\xe1' = 'Indaiá',
                              'Itagu\xe1 (Ubatuba)' = 'Itaguá',
                              'P. Forno (B\xfazios)' = 'Forno (Búzios)',
                              'P. Forno (Arr. Cabo)' = 'Forno (Arraial do Cabo)',
                              'R. S. Louren\xe7o' = 'Riviera de São Lourenço',
                              'Pta Cabe\xe7a' = 'Ponta da Cabeça',
                              'S\xe3o Pedro' = 'São Pedro',
                              'Tagua\xedba' = 'Taguaíba',
                              'Pta Cabeça' = 'Ponta da Cabeça',
                              'Prainha (Arr. Cabo)' = 'Prainha (Arraial do Cabo)',
                              'Pta Negra' = 'Maricá',
                              'P. Vila' = 'Saquarema',
                              'T. T. Grande' = 'Toque Toque Grande',
                              'T. T. Pequeno' = 'Toque Toque Pequeno')) %>% 
  mutate(site = gsub("P. ", "", site),
         subregion = recode(subregion, "S1" = "MRBS",
                            "S2" = "SSCh", 
                            "S3" = "Ubatuba",
                            "N1" = "SCRJ",
                            "N2" = "MRRJ",
                            "N3" = "LRRJ")) %>%  
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))) 


# MYTILASTER
brachi_sz <- read.csv("data/mytilaster_sz2020.csv", header=T, encoding = "UTF-8") %>% 
  filter(!is.na(tamanho_mm)) %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

brachi_cv <- read.csv("data/mytilaster_cv2020.csv", header = T, encoding = "UTF-8") %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

# brachi_cv$cover <- brachi_cv$rel_cover


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
  plotg.col(., 19, 4, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Lottia subrugosa') +
  theme(plot.title = element_text(face = "bold.italic"))

echi_d_sz <- plotg.col(nodi_sz, 17, 2, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Echinolittotina lineolata') +
  theme(plot.title = element_text(face = "bold.italic")) 

legs <- nodi_sz %>% 
  mutate(subregion = recode(subregion, 
                            "MRBS" = "Baixada Santista",
                            "SSCh" = "São Sebastião Channel",
                            "SCRJ" = "Costa Verde",
                            "MRRJ" = "Rio de Janeiro",
                            "LRRJ" = "Lagos")) %>% 
  ggplot(aes(subregion, tamanho_mm, color=subregion)) + 
    geom_point() + 
    scale_color_discrete(name="Subregion") + 
    theme(legend.background = element_blank(), 
          legend.key = element_blank(), 
          legend.text=element_text(size=14)) +
    scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


legend1 <- g_legend(legs)

# plot figure 3
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
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


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
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


ggpubr::ggarrange(str_d_ab, myti_d_cv, tetra_d_ab, lapa_d_ab, echi_d_ab, legend1, ncol=2, nrow=3)


##################################
# FIGURE 5 #######################
##################################

##### GET VARIANCES #####

### TETRACLITA
tetra_var <- all_variance("tamanho_mm", "gaussian", tetra_sz %>% 
                            group_by(site, subregion, region) %>% 
                            summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE)), 
                          "vivos_n", "nbinom1", tetra_ab %>% 
                            group_by(site, subregion, region) %>% 
                            summarise(vivos_n = mean(vivos_n, na.rm = TRUE)), "tetra")

### LOTTIA
lottia_var <- all_variance("tamanho_mm", "gaussian", lapas_sz %>% 
                             group_by(site, subregion, region) %>% 
                             summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE)), 
                           "adultos", "nbinom1", lapas_ab %>% 
                             group_by(site, subregion, region) %>% 
                             summarise(adultos = mean(adultos, na.rm = TRUE)), "lottia")

### MYTILASTER
mytilaster_var <- all_variance("tamanho_mm", "gaussian", brachi_sz %>% 
                                 group_by(site, subregion, region) %>% 
                                 summarise(tamanho_mm = mean(tamanho_mm)), 
                               "cover", "binomial", brachi_cv %>% 
                                 group_by(site, subregion, region) %>%
                                 summarise(cover = mean(cover)), "mytilaster")

### STRAMONITA
stramonita_var <- all_variance("tamanho_mm", "gaussian", stramonita_sz %>% 
                                 group_by(site, subregion, region) %>% 
                                 summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE)), 
                               "abund", "nbinom2", stramonita_ab, "stramonita")

### ECHINOLITTORINA
echino_var <- all_variance("tamanho_mm", "gaussian", nodi_sz %>% 
                             group_by(site, subregion, region) %>% 
                             summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE)), 
                           "abund", "nbinom1", nodi_ab, "echinolittorina")


### JOIN OUTPUTS
variancias <- bind_rows(echino_var, stramonita_var, mytilaster_var, lottia_var, tetra_var) %>% 
  mutate(variavel = plyr::mapvalues(variavel, from = c("abund", "tamanho_mm", "vivos_n"),
                                    to = c("density", "size", "density")),
         fator = plyr::mapvalues(fator, from = c("cond.site:subregion:region", "cond.subregion:region", "cond.region", "var.residual"),
                                 to = c("Site", "Subregion", "Region", "Within site")))

### PLOT VARIANCE PARTITIONING
variacias_plot <- variancias %>% 
  mutate(fator = factor(fator, levels = c("Region", "Subregion", "Site", "Within site")),
         variavel = plyr::mapvalues(variavel, from = "adultos", to = "density"),
         variavel = factor(variavel, levels = c("size", "density", "cover")),
         spp = plyr::mapvalues(spp, from = c("stramonita", "lottia", "mytilaster", "echinolittorina", "tetra"),
                               to = c("Str hae", "Lot sub", "Myt sol", "Ech lin", "Tet sta")),
         spp = factor(spp, levels = c("Str hae", "Tet sta", "Myt sol", "Lot sub", "Ech lin"))) %>% 
  filter(fator != "Within site") %>%
  ggplot(aes(x = variavel, y = variance, fill = fator, weight = variance)) + 
    geom_bar(stat="identity", position="fill", color='black') + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.text.x = element_text(size=14, color="black"), 
          axis.text.y = element_text(size=14, color="black"),
          axis.title.x = element_text(color="black", size=14),
          axis.title.y = element_text(color="black", size=16),
          axis.ticks = element_line(colour = "black", size = 0.8),
          strip.text.x = element_text(size = 14), 
          strip.text.y = element_text(size = 14),
          axis.line = element_line(colour = 'black', size = 0.8),
          axis.ticks.length = unit(4, "pt"), 
          legend.position = "top",
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text=element_text(size=14)) +
    labs(y="Proportion of variance", x="") +
    facet_grid(~ spp, scales = 'free') +
    scale_fill_manual(values=c('black','grey30','lightgray','white'))

# plot figure 5
variacias_plot

rm(list=ls())
