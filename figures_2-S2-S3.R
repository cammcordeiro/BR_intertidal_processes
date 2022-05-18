# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SUPPLEMENTARY MATERIAL #
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# clear memory

rm(list=ls())

# load packages
library(tidyverse)
library(openxlsx)
library(lubridate)
library(factoextra)
library(vegan)
library(patchwork)


# load functions
source("functions/summarySE.r")
source("functions/plot_pca_sep.r")
source("functions/HighstatLibV6.R")

# set wd
# setwd("/Users/cesarcordeiro/Google Drive/Post-doc_UNIFESP/Dados/NDVI/out/")
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

##### LER ESSE ARQUIVO
ndvi <- read.csv("data/ndvi_ok_31May18.csv", header=TRUE, sep=",") # versão atualizada a partir dos comandos das linhas 34 - 75
# abiot2020 <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/abioticos_2020.csv", header=T)
abiot <- read.csv("data/tetra_ab2020.csv", header=T) %>% 
  select(site, subregion, chl_mean:inclinacao, distance_S) %>% 
  distinct()


ndvi <- ndvi %>% 
  select(site, diff) %>% 
  mutate(site = plyr::mapvalues(site, from = c("Prainha_AC", "Pta_Cabeca", "Canto_Itaguai", "Indaia", "Riviera", "Forno_BZ",
                                               "Geriba", "Asturias", "Eden", "Enseada_GUA", "Guaiuba", "Sao_Pedro", "Taguaiba",
                                               "Marica", "Prainha_PTY", "Guaeca", "Pontal_Cruz", "Toque_Grande", "Toque_Pequeno",
                                               "Enseada_UBA", "Felix", "Itagua", "Grande", "Forno_AC"),
                                to = c("Prainha (Arraial do Cabo)", "Ponta da Cabeça", "Canto do Itaguaí", "Indaiá", "Riviera de São Lourenço", "Forno (Búzios)",
                                       "Geribá", "Astúrias", "Éden", "Enseada (Guarujá)", "Guaiúba", "São Pedro", "Taguaíba",
                                       "Maricá", "Prainha (Paraty)", "Guaecá", "Pontal da Cruz", "Toque Toque Grande", "Toque Toque Pequeno",
                                       "Enseada (Ubatuba)", "Félix", "Itaguá", "Grande (Ubatuba)", "Forno (Arraial do Cabo)"))) %>% 
  left_join(abiot %>% 
              select(site, subregion, distance_S)) %>% 
  filter(diff > 0) %>% 
  mutate(subregion = factor(subregion, levels = c("LRRJ", "MRRJ", "SCRJ", "Ubatuba", "SSCh", "MRBS")))


#
ordem <- ndvi %>% 
  select(site, distance_S) %>% 
  distinct() %>% 
  arrange(distance_S) %>% 
  pull(site)


##########################
## FIGURE S2 (SVG 750x400)
ndvi %>% 
  mutate(site = factor(site, levels = ordem)) %>% 
  ggplot(aes(x=distance_S, y=diff, color = subregion)) +
  geom_point(alpha = 0.6) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.text.x = element_text(size=12, color="black", angle = 0, vjust = 1, hjust = 0.5), 
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16),
        axis.ticks = element_line(colour = "black", size = 0.8),
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        axis.line = element_line(colour = 'black', size = 0.8),
        axis.ticks.length = unit(4, "pt"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 12, face="bold")) +
  stat_summary(fun=mean, geom="point", shape=18, size=1, color="black") +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
  labs(x="Distance from southernmost site (km)", y="NDVI") +
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


##########################
## FIGURE S3 (SVG 750x400?)
# abiot <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/old/abiotics.csv", header=T)
# abiotic_all <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/old/abioticos_jul2019.csv", header=T) 

vars <- c("chl_mean", "sst_mean", "fwd_mean", "wf_log", "rugosity",
         "inclinacao", "NDVI", "subregion", "site")

abiotic <- ndvi %>% 
  group_by(site) %>% 
  summarise(NDVI = mean(diff)) %>% 
  left_join(abiot) %>% 
  select(vars) %>% 
  column_to_rownames("site")

which(is.na(abiotic), arr.ind = TRUE)
#abiotic[31, 5] <- 1.14

#
corvif(abiotic %>% select(-subregion))

res.pca <- prcomp(abiotic[-8], center = TRUE, scale. = TRUE)

((res.pca$sdev)^2 / sum((res.pca$sdev)^2)) %>% cumsum()

### PLOTS

pca_1x2 <- fviz_pca_biplot(res.pca, 
                       axes = c(1, 2),
                       label = "var",
                       title = "",
                       repel = TRUE,
                       habillage = abiotic$subregion,
                       # addEllipses = TRUE,
                       labelsize = 5,
                       pointsize = 2,
                       # ellipse.level = 0.95,
                       # alpha.var="contrib",
                       col.var = "black",
                       ggtheme = theme_minimal()) +
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"null"))# + lims(y = c(-3.5, 2))


hull_1x2 <- res.pca$x %>%
  data.frame() %>%
  bind_cols(subregion = abiotic$subregion) %>% 
  group_by(subregion) %>% 
  dplyr::slice(chull(PC1, PC2)) 

Gpca_1 <- pca_1x2 +
  geom_polygon(data = hull_1x2, aes(x = PC1, y = PC2, fill = subregion, color = subregion), alpha = 0.2, linetype = 2) +
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))


#
pca_1x3 <- fviz_pca_biplot(res.pca, 
                           axes = c(1, 3),
                           label = "var",
                           title = "",
                           repel = TRUE,
                           habillage = abiotic$subregion,
                           labelsize = 5,
                           pointsize = 2,
                           col.var = "black",
                           ggtheme = theme_minimal()) +
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0,"null"))# + lims(y = c(-3.5, 2))

hull_1x3 <- res.pca$x %>%
  data.frame() %>%
  bind_cols(subregion = abiotic$subregion) %>% 
  group_by(subregion) %>% 
  dplyr::slice(chull(PC1, PC3)) 


Gpca_2 <- pca_1x3 +
  geom_polygon(data = hull_1x3, aes(x = PC1, y = PC3, fill = subregion, color = subregion), alpha = 0.2, linetype = 2) +
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))

# FIGURE
Gpca_1 + Gpca_2


##########################
### FIGURE 7
# RDA

consumers <- read.csv("data/consumers.csv", header =T)
fauna <- consumers[,c(1, 4:6,8:10,12:15)] 

fauna.range <- consumers[,c(4:6,8:10,12:15)] %>%
  decostand("range", na.rm = T) %>%
  cbind(site = fauna$site, .) %>%
  column_to_rownames('site')

fauna.hel <- decostand(fauna[-1],"hellinger", na.rm = T) %>% 
  cbind(site = fauna$site, .) %>% 
  column_to_rownames('site')

allvar <- left_join(fauna.hel %>% rownames_to_column("site"), abiotic %>% rownames_to_column("site")) %>% 
  select(subregion, everything()) %>% 
  column_to_rownames('site')

allvar1 <- allvar %>% 
  na.omit() # removed = Tarituba, Ferradurinha, Ranchos, Itagua
  
# MODELS
envi.sdz <- decostand(allvar1 %>% select(chl_mean:NDVI), "standardize")
dens.hel <- allvar1 %>% select(Tetr_density, Brach_cover, lapa_dens, str_densC, nodi_ab)
size.hel <- allvar1 %>% select(Tetr_size, Brach_size, lapa_size, str_sizeC, nodi_size)

# rda.fauna <- rda ( cbind(dens.hel, size.hel) ~., envi.sdz)
rda.fauna <- rda ( fauna.range %>% na.omit() ~., envi.sdz)
# rda.fauna <- rda ( cbind(dens.hel, size.hel) %>% decostand("range") ~., envi.sdz)

vif.cca(rda.fauna) # colinearity
summary(rda.fauna) 
(R2.adj <- RsquareAdj(rda.fauna)$adj.r.squared)

anova.cca(rda.fauna, step=10000) # step codifica o n?mero de permutacoes
anova.cca(rda.fauna, step=10000, by="axis") # aqui testamos a significancia de cada eixo

plot(rda.fauna, type = "text", scaling = 0) 

### reduced model
ordistep(rda.fauna, direction="both")
rda.fauna.red <- rda(cbind(dens.hel, size.hel) ~., envi.sdz[,c('wf_log','sst_mean','fwd_mean')]) 
vif.cca(rda.fauna.red) # colinearity
(R2.adj<-RsquareAdj(rda.fauna.red)$adj.r.squared)

anova.cca(rda.fauna.red, step=10000)
anova.cca(rda.fauna.red, step=10000, by="axis")
ordistep(rda.fauna.red, direction="both")
plot(rda.fauna.red)

## RDA Table
# check VIF
data.frame(VIF = vif.cca(rda.fauna.red)) %>% 
  knitr::kable()

# Variables correlation with axes
scores(rda.fauna.red, display = 'reg') %>% 
  as.data.frame() %>% 
  knitr::kable() 


# https://programmer.ink/think/r-redundancy-analysis-rda-ggplot2.html
uu = rda.fauna
ii = summary(uu)  #View analysis results
sp = as.data.frame(ii$species[,1:2])*2#Depending on the drawing result, the drawing data can be enlarged or reduced to a certain extent, as follows
st = as.data.frame(ii$sites[,1:2]) %>% 
  rownames_to_column("site") %>% 
  left_join(abiot %>% select(site, subregion)) %>% 
  mutate(subregion = factor(subregion, levels = c("MRBS","SSCh","Ubatuba","SCRJ","MRRJ","LRRJ")))

yz = as.data.frame(ii$biplot[,1:2])

c("LRRJ","MRRJ","SCRJ","Ubatuba","SSCh","MRBS")


# polygon
hullSR <- st %>%
  group_by(subregion) %>% 
  dplyr::slice(chull(RDA1, RDA2))

# plot
ggplot() +
  geom_point(data = st, aes(RDA1, RDA2, color = subregion), size = 1) + #, shape = subregion
  scale_shape_manual(values = c(1, 21:25)) +
  geom_point(data = sp, aes(RDA1, RDA2), shape = 3, size = 2) +
  geom_text(data = sp, aes(RDA1, RDA2, label = row.names(sp)), vjust = -1) +
  geom_segment(data = yz %>% slice(2:4), 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle = 22.5, length = unit(0.35,"cm"), type = "closed"), 
               linetype = 1, size = 0.6, colour = "blue") +
  geom_segment(data = yz %>% slice(1, 5:7), 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(angle = 22.5, length = unit(0.3,"cm"), type = "closed"), 
               linetype = 2, size = 0.6, colour = "black") +
  ggrepel::geom_text_repel(data = yz, aes(RDA1, RDA2, label = row.names(yz)), label.padding = 0.25) +
  labs(x = paste("RDA 1 (", format(100 *ii$concont[[1]][2,1], digits=4), "%)", sep=""),
       y = paste("RDA 2 (", format(100 *ii$concont[[1]][2,2], digits=4), "%)", sep=""))+
  guides(shape=guide_legend(title=NULL,color="black"),
         fill=guide_legend(title=NULL))+
  theme_bw() + 
  geom_polygon(data = hullSR, aes(x = RDA1, y = RDA2, fill = subregion, color = subregion), alpha = 0.3, linetype = 2) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')) +
  scale_fill_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))

ggsave("figure2.svg", width = 8, height = 4.5, units = "in")


## VARIANCE PARTITIONING
# https://stackoverflow.com/questions/50177409/how-to-calculate-species-contribution-percentages-for-vegan-rda-cca-objects
# RDA1 influence
sort(round(100*scores(rda.fauna.red, display = "bp", scaling = 0)[,1]^2, 2), decreasing = TRUE)
sort(round(100*scores(rda.fauna.red, display = "sp", scaling = 0)[,1]^2, 2), decreasing = TRUE)

# RDA2 influence
sort(round(100*scores(rda.fauna.red, display = "bp", scaling = 0)[,2]^2, 2), decreasing = TRUE)
sort(round(100*scores(rda.fauna.red, display = "sp", scaling = 0)[,2]^2, 2), decreasing = TRUE)

sum(100*scores(rda.fauna.red, display = "sp", scaling = 0)[,1]^2)

# particao variancias
tetra_ab2019 <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/old/tetra_ab.csv", header=T)

pcnm.matrix <- tetra_ab2019 %>% 
  select(site, easting, northing) %>% 
  group_by(site) %>% 
  summarise(easting = mean(easting),
            northing = mean(northing)) %>% 
  filter(!site %in% c("Tarituba", "Ferradurinha", "Ranchos", "Itaguá")) %>% 
  column_to_rownames('site') %>% 
  dist() %>% 
  pcnm()

pcnm.vectors <- as.data.frame(pcnm.matrix$vectors) 

varpart(fauna.range %>% na.omit(), envi.sdz, pcnm.vectors) %>% plot()


#############################################
###### TESTING CORRELATIONS ######
#############################################
cor.test(fauna$nodi_size, fauna$nodi_ab, method = 'spearman') 
cor.test(fauna$Tetr_size, fauna$Tetr_density, method = 'spearman')
cor.test(fauna$Brach_cover, fauna$Brach_size, method = 'spearman')
cor.test(fauna$lapa_size, fauna$lapa_dens, method = 'spearman') #
cor.test(fauna$str_sizeC, fauna$str_densC, method = 'spearman') #

fauna %>% 
  dplyr::select(-site) %>%
  PerformanceAnalytics::chart.Correlation(histogram=TRUE, pch=19)

# lapa_size x Tetr_density (n = 60)
lapa_tetr <- cor.test(fauna$lapa_size, fauna$Tetr_density, method = 'spearman') #
plot(fauna$lapa_size, fauna$Tetr_density, las=1)

# lapa_size x Brach_size (n = 56)
lapa_brach <- cor.test(fauna$lapa_size, fauna$Brach_size, method = 'spearman') #
plot(fauna$lapa_size, fauna$Brach_size, las=1)

# lapa_size x nodi_size (n = 60)
lapa_nodi <- cor.test(fauna$lapa_size, fauna$nodi_size, method = 'spearman') #
plot(fauna$lapa_size, fauna$nodi_size, las=1)



fauna %>% 
  ggplot(aes(x = lapa_size, y = Tetr_density)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  geom_label(aes(x = 2, y = 100), 
             label = paste("r =", lapa_tetr$estimate[[1]] %>% round(2), "p < 0.001"))

fauna %>% 
  ggplot(aes(x = lapa_size, y = Brach_size)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  geom_label(aes(x = 2, y = 13), 
             label = paste("r =", lapa_brach$estimate[[1]] %>% round(2), "p < 0.001"))

fauna %>% 
  ggplot(aes(x = lapa_size, y = nodi_size)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  geom_label(aes(x = 2, y = 6), 
             label = paste("r =", lapa_nodi$estimate[[1]] %>% round(2), "p < 0.001"))

#############################################
#############################################


bind_cols(
  allvar1 %>% 
    select(subregion, Tetr_density, Brach_cover, lapa_dens, str_densC, nodi_ab) %>% 
    pivot_longer(cols = -subregion, values_to = "density", names_to = "abund"), 
  allvar1 %>% 
    select(subregion, Tetr_size, Brach_size, lapa_size, str_sizeC, nodi_size) %>% 
    pivot_longer(cols = -subregion, values_to = "size", names_to = "tamanho") %>% 
    select(-subregion)
  ) %>% 
  ggplot(aes(x = density, y = size)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_grid(. ~ abund, scales = 'free') +
    theme_classic()




