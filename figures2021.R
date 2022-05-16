library(openxlsx)
library(glmmTMB)
library(insight)
library(ggeffects)
library(tidyverse)

###
source("functions/summarySE.r")

source("functions/HighstatLibV6.r")

source("functions/plot_pca_sep.r")

#### LOAD DATA

abiot <- read.csv("data/abioticos_2020.csv", header=T)

# ECHINOLITTORINA
nodi_sz <- read.csv("data/nodi_sz2020.csv", header=T, encoding = "UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE))
# nodi_sz$subregion <- factor(nodi_sz$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))
nodi_ab <- read.delim("data/nodi.txt", header=T, encoding="UTF-8") %>% 
  rename(site = X.U.FEFF.site) %>% 
  filter(abund < 1000) %>% 
  group_by(site, subregion, region) %>% 
  summarise(abund = mean(abund, na.rm = TRUE))
# nodi$subregion <- factor(nodi$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))

# TETRACLITA
tetra_sz <- read.csv("data/tetra_sz2020.csv", header=T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE))

tetra_ab <- read.csv("data/tetra_ab2020.csv", header=T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(vivos_n  = mean(vivos_n, na.rm = TRUE))
# tetra_sz$distance_S <- abiot$distance_S [ match(tetra_sz$site, abiot$site)] 
# tetra_ab$distance_S <- abiot$distance_S [ match(tetra_ab$site, abiot$site)] 
# tetra_ab$site <- factor(tetra_ab$site, levels = as.character(ordem$site))
# tetra_sz$site <- factor(tetra_sz$site, levels = as.character(ordem$site))
tetra_ab$subregion <- factor(tetra_ab$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))
tetra_sz$subregion <- factor(tetra_sz$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))

# LOTTIA
lapas_sz <- read.csv("data/lottia_sz2020.csv", header=T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE))

lapas_ab <- read.delim("data/lottia_ab.txt", header=T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(adultos = mean(adultos, na.rm = TRUE))

# lapas_ab$subregion <- factor(lapas_ab$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))
# lapas_sz$subregion <- factor(lapas_sz$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))

# STRAMONITA
stramonita_sz <- read.delim("data/stramonita_size.txt", header = T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE))
# stramonita$subregion <- factor(stramonita$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))

stramonita_ab <- read.delim("data/stramonita_abundance.txt", header = T)

# MYTILASTER
brachi_sz <- read.csv("data/mytilaster_sz2020.csv", header=T, encoding="UTF-8") %>% 
  filter(!is.na(tamanho_mm)) %>% 
  group_by(site, subregion, region) %>% 
  summarise(tamanho_mm = mean(tamanho_mm, na.rm = TRUE)) 

brachi_cv <- read.csv("data/mytilaster_cv2020.csv", header = T, encoding="UTF-8") %>% 
  group_by(site, subregion, region) %>% 
  summarise(cover = mean(cover, na.rm = TRUE))
# brachi_sz$subregion <- factor(brachi_sz$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))
# brachi_cv$subregion <- factor(brachi_cv$subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ"))
# brachi_cv$cover <- brachi_cv$rel_cover

##################################
## PLOT FUNCTIONS #####
##################################

plotg <- function(df, variavelx, variavely, legx, legy) {
  ggplot(df, aes(x=df[,variavelx], y=df[,variavely])) +
    geom_point(alpha = 0.3) +
    #geom_point(alpha = 0.6, aes(color=subregion)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=10),  
          axis.title=element_text(size=10),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank()) +
    stat_summary(fun.y=mean, geom="point", shape=18, size=1, color="black") +
    stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
    #geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + 
    geom_smooth(method="lm", color="black", fill = "grey50", alpha=0.5) +
    labs(x=legx, y=legy)
}

plotg.col <- function(df, variavelx, variavely, legx, legy) {
  ggplot(df, aes(x=df[,variavelx], y=df[,variavely])) +
    geom_point(alpha = 0.6, aes(color=subregion)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.text.x = element_text(size=12, color="black"), 
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(color="black", size=14),
          axis.title.y = element_text(color="black", size=16),
          axis.ticks = element_line(colour = "black", size = 0.8),
          strip.text.x = element_text(size = 10), 
          strip.text.y = element_text(size = 10),
          axis.line = element_line(colour = 'black', size = 0.8),
          axis.ticks.length = unit(4, "pt"), 
          legend.position = "",
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(size = 12, face="bold")) +
    stat_summary(fun.y=mean, geom="point", shape=18, size=1, color="black") +
    stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
    #geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + 
    #geom_smooth(method="lm", color="black", fill = "grey50", alpha=0.5) +
    labs(x=legx, y=legy) + #title(paste(titulo)) +
    scale_color_manual(values=c("red","orange","yellow","green","violet","purple"))
}

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 


# get all sources of variance
all_variance <- function(var1, familia1, dados1, var2, familia2, dados2, spp) {
  
  formila1 <- as.formula(paste(var1, " ~ ", paste(c(1, '(1|region/subregion/site)'), collapse= "+")))
  formila2 <- as.formula(paste(var2, " ~ ", paste(c(1, '(1|region/subregion/site)'), collapse= "+")))
  
  model1 <- glmmTMB(formila1, data = dados1, REML=T, family = familia1, ziformula=~0)
  model2 <- glmmTMB(formila2, data = dados2, REML=T, family = familia2, ziformula=~0)
  
  bind_rows(
    
    # size
    bind_rows(
      model1 %>% 
        VarCorr() %>% 
        unlist() %>%
        data.frame() %>% 
        rownames_to_column(var = 'fator') %>% 
        rename(variance = '.'), 
      
      get_variance_residual(model1) %>% 
        data.frame() %>% 
        rownames_to_column(var = 'fator') %>% 
        rename(variance = '.')  
    ) %>% 
      mutate(variavel = var1,
             spp = spp),
    
    # abundance or cover
    bind_rows(
      model2 %>% 
        VarCorr() %>% 
        unlist() %>%
        data.frame() %>% 
        rownames_to_column(var = 'fator') %>% 
        rename(variance = '.'), 
      
      get_variance_residual(model2) %>% 
        data.frame() %>% 
        rownames_to_column(var = 'fator') %>% 
        rename(variance = '.')  
    ) %>% 
      mutate(variavel = var2,
             spp = spp)
  )
}

##################################
##################################
# VARIANCES

### TETRACLITA
tetra_var <- all_variance("tamanho_mm", "gaussian", tetra_sz, "vivos_n", "nbinom1", tetra_ab, "tetra")
  
# ### TETRACLITA
# tetra_sz_mod <- glmmTMB(tamanho_mm ~ 1 + (1|region/subregion/site), data=tetra_sz, REML=T, family = gaussian, ziformula=~0)
# tetra_ab_mod <- glmmTMB(vivos_n ~ 1 + (1|region/subregion/site), data=tetra_ab, REML=T, family = nbinom1, ziformula=~0)

# tetra_var <- bind_rows(
#   
#   # size
#   bind_rows(
#     tetra_sz_mod %>% 
#       VarCorr() %>% 
#       unlist() %>%
#       data.frame() %>% 
#       rownames_to_column(var = 'fator') %>% 
#       rename(variance = '.'), 
#     
#     get_variance_residual(tetra_sz_mod) %>% 
#       data.frame() %>% 
#       rownames_to_column(var = 'fator') %>% 
#       rename(variance = '.')  
#   ) %>% 
#     mutate(variavel = "size",
#            spp = "Tetraclita"),
#   
#   # abundance
#   bind_rows(
#     tetra_ab_mod %>% 
#       VarCorr() %>% 
#       unlist() %>%
#       data.frame() %>% 
#       rownames_to_column(var = 'fator') %>% 
#       rename(variance = '.'), 
#     
#     get_variance_residual(tetra_ab_mod) %>% 
#       data.frame() %>% 
#       rownames_to_column(var = 'fator') %>% 
#       rename(variance = '.')  
#   ) %>% 
#     mutate(variavel = "abundance",
#            spp = "Tetraclita")
# )



### LOTTIA
lottia_var <- all_variance("tamanho_mm", "gaussian", lapas_sz, "adultos", "nbinom1", lapas_ab, "lottia")

### MYTILASTER
mytilaster_var <- all_variance("tamanho_mm", "gaussian", brachi_sz, "cover", "binomial", brachi_cv, "mytilaster")

### STRAMONITA
stramonita_ab <- stramonita_sz %>% 
  mutate(abund = ifelse(is.na(tamanho_mm), 0, 1)) %>% 
  aggregate(abund ~ site, ., sum) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Ast\xfarias", "Grande (south3)"),
                                to = c("Astúrias", "Grande (Ubatuba)"))) %>% 
  left_join(lapas_sz[,c("site","subregion","region")] %>% unique()) 

stramonita_var <- all_variance("tamanho_mm", "gaussian", stramonita_sz, "abund", "nbinom2", stramonita_ab, "stramonita")

### ECHINOLITTORINA
echino_var <- all_variance("tamanho_mm", "gaussian", nodi_sz, "abund", "nbinom2", nodi_ab, "echinolittorina")


### JOIN OUTPUTS
variancias <- bind_rows(echino_var, stramonita_var, mytilaster_var, lottia_var, tetra_var) %>% 
  mutate(variavel = plyr::mapvalues(variavel, from = c("abund", "tamanho_mm", "vivos_n"),
                                    to = c("density", "size", "density")),
         fator = plyr::mapvalues(fator, from = c("cond.site:subregion:region", "cond.subregion:region", "cond.region", "var.residual"),
                                 to = c("Site", "Subregion", "Region", "Within site")))

### PLOT VARIANCE PARTITIONING
variancias %>% 
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


##################################
##################################
#left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>% 

#### size x distance
stra_d_sz = plotg.col(stramonita_sz, 4, 7, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Stramonita haemastoma') +
  theme(plot.title = element_text(face = "bold.italic"))

myti_d_sz = plotg.col(brachi_sz, 13, 2, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Mytilaster solisianus') +
  theme(plot.title = element_text(face = "bold.italic"))

tetra_d_sz = plotg.col(tetra_sz, 14, 3, "Distance from southernmost site (km)","Size (mm)") + 
  ggtitle ('Tetraclita stalactifera') +
  theme(plot.title = element_text(face = "bold.italic"))

lapa_d_sz = lapas_sz %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% 
              distinct()) %>%
  plotg.col(., 19, 3, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Lottia subrugosa') +
  theme(plot.title = element_text(face = "bold.italic"))

echi_d_sz = plotg.col(nodi_sz, 17, 2, "Distance from southernmost site (km)", "Size (mm)") + 
  ggtitle ('Echinolittotina lineolata') +
  theme(plot.title = element_text(face = "bold.italic"))

legs = ggplot(nodi_sz, aes(subregion, tamanho_mm, color=subregion)) + 
  geom_point() + 
  scale_color_discrete(name="Subregion") + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        legend.text=element_text(size=14))

legend1 <- g_legend(legs)

##### FIGURE 6 #####
ggpubr::ggarrange(stra_d_sz, myti_d_sz, tetra_d_sz, lapa_d_sz, echi_d_sz, legend1, ncol=2, nrow=3)


#### density/cover x distance

str_d_ab = stramonita_ab %>% 
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
  theme(plot.title = element_text(face = "bold.italic"))  

myti_d_cv = plotg.col(brachi_cv, 14, 3, "Distance from southernmost site (km)", "Relative cover") + 
  ggtitle ('Mytilaster solisianus') +
  theme(plot.title = element_text(face = "bold.italic"))

tetra_d_ab = tetra_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  plotg.col(., 14, 3, "Distance from southernmost site (km)", expression(paste("Density (ind 100 ",cm^-2,")"))) + 
  ggtitle ('Tetraclita stalactifera') +
  theme(plot.title = element_text(face = "bold.italic"))

lapa_d_ab = lapas_ab %>% 
  left_join(brachi_cv[,c("site", "distance_S")] %>% distinct()) %>%
  plotg.col(., 18, 2, "Distance from southernmost site (km)", expression(paste("Density (ind 100 ",cm^-2,")"))) + 
  ggtitle ('Lottia subrugosa') +
  theme(plot.title = element_text(face = "bold.italic"))

echi_d_ab = nodi_ab %>% 
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
  theme(plot.title = element_text(face = "bold.italic"))

ggarrange(str_d_ab, myti_d_cv, tetra_d_ab, lapa_d_ab, echi_d_ab, legend1, ncol=2, nrow=3)

##################################################################################################
# END 
##################################################################################################
##################################################################################################
##################################################################################################


plot_dist <- function(df) {
  
  resumo <-	summarySE(df, measurevar="tamanho_mm", groupvars=c("subregion","site","distance_S"), na.rm=T)
  
  ggplot(data=resumo, aes(x=distance_S, y=mean)) +
    geom_smooth(method = "loess") +
    geom_jitter(data=df, alpha = 0.6, width = 0.6, aes(color=subregion, y=tamanho_mm)) +
    geom_errorbar(data=resumo, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
    geom_point(data=resumo, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.text=element_text(size=10),  
          axis.title=element_text(size=10, face="bold"),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          axis.line = element_line(size = 0.5, colour = "black")) +
    labs(x="Distance from southernmost sampling point (km)", y= "Size in mm (mean +- s.e.)") + #, title="Lottia"
    geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
  
}

## points with labels are outliers (mean + 2*sd)
lapas_sz %>% 
  left_join(abiot[, c("site", "distance_S")]) %>% 
  plot_dist()

tetra_sz %>% 
  left_join(abiot[, c("site", "distance_S")]) %>%
  plot_dist()

plot_dist(nodi_sz)

stramonita_sz %>% 
  left_join(abiot[, c("site", "distance_S")]) %>%
  plot_dist()

brachi_sz %>% 
  left_join(abiot[, c("site", "distance_S")]) %>%
  plot_dist()


# #######
# resumoL <-	summarySE(lapas_ab, measurevar="adultos", groupvars=c("subregion","site","distance_S"), na.rm=T)
# 
# ggplot(data=resumoL, aes(x=distance_S, y=mean)) +
#   geom_smooth(method = "loess") +
#   geom_jitter(data=lapas_ab, alpha = 0.6, width = 0.6, aes(color=subregion, y=adultos)) +
#   geom_errorbar(data=resumoL, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   geom_point(data=resumoL, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (mean +- s.e.)")  #, title="Lottia"
# geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# #######
# resumoT <-	summarySE(tetra_ab, measurevar="vivos_n", groupvars=c("subregion","site","distance_S"), na.rm=T)
# 
# ggplot(data=resumoT, aes(x=distance_S, y=mean)) +
#   geom_smooth(method = "loess") +
#   geom_jitter(data=tetra_ab, alpha = 0.6, width = 0.6, aes(color=subregion, y=vivos_n)) +
#   geom_errorbar(data=resumoT, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   geom_point(data=resumoT, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (mean +- s.e.)")  #, title="Lottia"
# geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# ##############
# 
# ##############
# resumoB <-	summarySE(b_cover, measurevar="rel_cover", groupvars=c("subregion","site","distance_S"), na.rm=T)
#   
# ggplot(data=resumoB, aes(x=distance_S, y=mean)) +
#   geom_smooth(method = "loess") +
#   geom_jitter(data=b_cover, alpha = 0.6, width = 0.6, aes(color=subregion, y=rel_cover)) +
#   geom_errorbar(data=resumoB, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   geom_point(data=resumoB, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#     labs(x="Distance from southernmost sampling point (km)", y= "Cover (mean +- s.e.)")  #, title="Lottia"
#     geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# ##############
# ggplot(data=resumoN, aes(x=distance_S, y=mean)) +
#       geom_smooth(method="lm", formula = y ~ splines::bs(x, 3))+
#       geom_jitter(data=nodiab, alpha = 0.6, size=2, aes(color=subregion, y=nodi_ab)) +
#   #geom_errorbar(data=resumoN, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   #geom_point(data=resumoN, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (mean +- s.e.)") #+ #, title="Lottia"
#   #geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# ##############
# ggplot(data=consumers, aes(x=distance_S, y=Str_density)) +
#       geom_smooth(method="lm", formula = y ~ splines::bs(x, 3))+
#       geom_jitter(data=consumers, alpha = 0.6, size=2, aes(color=subregion, y=Str_density)) +
#   #geom_errorbar(data=resumoN, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   #geom_point(data=resumoN, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (mean +- s.e.)") #+ #, title="Lottia"
# #geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# 
# resumoL <-	summarySE(brach, measurevar="tamanho_mm", groupvars=c("subregion","site","distance_S"), na.rm=T)
# 
# ggplot(data=resumoL, aes(x=distance_S, y=mean)) +
#   geom_smooth(method = "loess") +
#   geom_jitter(data=lapas_ab, alpha = 0.6, width = 0.6, aes(color=subregion, y=adultos)) +
#   geom_errorbar(data=resumoL, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   geom_point(data=resumo, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (n)") + #, title="Lottia"
#   geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# ##########
# ggplot(data=consumers, aes(x=distance_S, y=Brach_RW)) +
#   geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) +
#   geom_jitter(data=consumers, alpha = 0.6, size=2, aes(color=subregion, y=Brach_RW)) +
#   #geom_errorbar(data=resumoN, width=1, aes(ymin= mean-se, ymax= mean+se, x=distance_S), colour="black") + 
#   #geom_point(data=resumoN, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         axis.line = element_line(size = 0.5, colour = "black")) +
#   labs(x="Distance from southernmost sampling point (km)", y= "Abundance (mean +- s.e.)") #+ #, title="Lottia"
# #geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)
# 
# 
# ################### original
#   resumo <-	summarySE(lapas_ab, measurevar="adultos", groupvars=c("subregion","site","distance_S"), na.rm=T)
#   
#   ggplot(data=resumo, aes(x=distance_S, y=mean)) +
#     geom_smooth(method = "loess") +
#     geom_jitter(data=lapas_ab, alpha = 0.6, width = 0.6, aes(color=subregion, y=adultos)) +
#     geom_errorbar(data=resumo, width=1, aes(ymin= mean-ci, ymax= mean+ci, x=distance_S), colour="black") + 
#     geom_point(data=resumo, size= 4, bg="black", shape=21, color="lightgray") + #aes(size=cv),
#     theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(), 
#           panel.background = element_blank(), 
#           axis.text=element_text(size=10),  
#           axis.title=element_text(size=10, face="bold"),
#           legend.background = element_blank(),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           axis.line = element_line(size = 0.5, colour = "black")) +
#     labs(x="Distance from southernmost sampling point (km)", y= "Size (mm - mean +- 95% c.i.)", title="Lottia") +
#     geom_text(data=resumo, aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)




# consumers$subregion <- lapas_ab$subregion [ match(consumers$site, lapas_ab$site)]
# consumers$distance_S <- lapas_ab$distance_S [ match(consumers$site, lapas_ab$site)]
# 
# 
# ggplot(consumers, aes(x=distance_S, y=str_densC)) +
#   geom_jitter(alpha = 0.6, width = 0.2, aes(color=subregion)) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank()) +
#   #stat_summary(fun.y=mean, geom="point", shape=18, size=1, color="black") +
#   #stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
#   #geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + #geom_smooth(method="lm") +
#   labs(x="Distance from southernmost site (km)", y="Total abundance (n)")
# 
# ####
# b_cover <- read.xlsx("data/brachi_all.xlsx", sheet=1)
# b_cover$subregion <- lapas_ab$subregion [ match(b_cover$site, lapas_ab$site)]
# b_cover$distance_S <- lapas_ab$distance_S [ match(b_cover$site, lapas_ab$site)] 
# 
# ggplot(b_cover, aes(x=distance_S, y=rel_cover)) +
#   geom_jitter(alpha = 0.6, width = 0.2, aes(color=subregion)) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank()) +
#   stat_summary(fun.y=mean, geom="point", shape=18, size=1, color="black") +
#   stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
#   #geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + #geom_smooth(method="lm") +
#   labs(x="Distance from southernmost site (km)", y="Cover (%)")
# 
# ####
# brach <- read.xlsx("data/brachi_all.xlsx", sheet=2)
# brach$subregion <- lapas_ab$subregion [ match(brach$site, lapas_ab$site)]
# brach$distance_S <- lapas_ab$distance_S [ match(brach$site, lapas_ab$site)] 
# 
# ggplot(brach, aes(x=distance_S, y=tamanho_mm)) +
#   geom_jitter(alpha = 0.6, width = 0.2, aes(color=subregion)) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank()) +
#   stat_summary(fun.y=mean, geom="point", shape=21, size=1, color="white", bg="white") +
#   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1.96), color="black") +
#   #geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + #geom_smooth(method="lm") +
#   labs(x="Distance from southernmost site (km)", y="Size (mm)")
# 
# #
# ggplot(consumers, aes(x=distance_S, y=Brach_RW)) +
#   geom_jitter(alpha = 0.6, width = 0.6, aes(color=subregion)) +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text=element_text(size=10),  
#         axis.title=element_text(size=10, face="bold"),
#         legend.background = element_blank(),
#         legend.key = element_blank(),
#         legend.title = element_blank()) +
#   #stat_summary(fun.y=mean, geom="point", shape=18, size=1, color="black") +
#   #stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
#   geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) + #geom_smooth(method="lm") +
#   labs(x="Distance from southernmost site (km)", y="Total abundance (n)")
# 
# 
# 
# 
