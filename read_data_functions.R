#####
# READ DATA and FUNCTIONS
#####

library(openxlsx)
library(tidyverse)
library(glmmTMB)
library(insight)
library(ggeffects)
library(patchwork)
library(lme4)

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
nodi_sz <- read.csv("data/nodi_sz2020.csv", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 

nodi_ab <- read.delim("data/nodi.txt", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


# TETRACLITA
tetra_sz <- read.csv("data/tetra_sz2020.csv", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


tetra_ab <- read.csv("data/tetra_ab2020.csv", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


# LOTTIA
lapas_sz <- read.csv("data/lottia_sz2020.csv", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


lapas_ab <- read.delim("data/lottia_ab.txt", header=T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


# STRAMONITA
stramonita_sz <- read.delim("data/stramonita_size.txt", header = T) %>% 
  mutate(site = recode(site, 'Ast\xfarias' = 'Astúrias',
                       'Grande (south3)' = 'Grande (Ubatuba)'),
         subregion = recode(subregion, "south1" = "MRBS",
                            "south2" = "SSCh", 
                            "south3" = "Ubatuba",
                            "north1" = "SCRJ",
                            "north2" = "MRRJ",
                            "north3" = "LRRJ")) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


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
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 



# MYTILASTER
brachi_sz <- read.csv("data/mytilaster_sz2020.csv", header=T) %>% 
  filter(!is.na(tamanho_mm)) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 


brachi_cv <- read.csv("data/mytilaster_cv2020.csv", header = T) %>% 
  mutate(subregion = plyr::mapvalues(subregion, from = c("LRRJ", "MRRJ", "SCRJ", "MRBS"),
                                     to = c("Lakes", "Rio de Janeiro", "Green Coast", "Baixada Santista"))) %>% 
  mutate(subregion = factor(subregion, levels = c("Lakes","Rio de Janeiro","Green Coast","Ubatuba","SSCh","Baixada Santista"))) 

rm(ordem, abiot)
