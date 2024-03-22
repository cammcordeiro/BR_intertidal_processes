library(openxlsx)
library(tidyverse)
library(glmmTMB)
library(insight)
library(ggeffects)
library(patchwork)
library(ggpubr)

# setwd("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/Brazil_project")

source("read_data_functions.R")

##################################

theme_var <- theme(panel.grid.major = element_blank(), 
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
                   legend.background = element_blank(),
                   legend.key = element_blank(),
                   legend.title = element_blank(),
                   legend.text=element_text(size=14),
                   plot.title = element_text(face = "bold", size = 12)) 

##################################
# STRAMONITA
##################################

str_random1 <- bind_rows(
  glmmTMB(abund ~ (1|region/subregion), data=stramonita_ab, REML=T, family = nbinom2) %>% 
  get_variance() %>% 
  unlist() %>% 
  data.frame() %>% 
  rownames_to_column(var = "effect") %>% 
  rename(variance = ".") %>% 
  mutate(model = "Stramonita",
         variable = "abundance",
         effect = gsub("var.intercept.", "", effect) %>% 
           gsub("var.", "", .)),
  glmmTMB(tamanho_mm ~ (1|region/subregion/site), data=stramonita_sz, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Stramonita",
           variable = "size",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .))
) %>% 
  filter(effect %in% c("site:subregion:region", "subregion:region", "region", "residual")) %>%
  mutate(effect = plyr::mapvalues(effect, from = c("site:subregion:region", "subregion:region", "region", "residual"),
                                  to = c("site", "subregion", "region", "within site")) %>% 
           factor(., levels = c("region", "subregion", "site", "within site"))) %>% 
  ggplot(aes(x = variable, y = variance, fill = effect, weight = variance)) + 
  geom_bar(stat="identity", position="fill", color='black') + 
  theme_var +
  labs(y="Proportion of variance", x="") +
  scale_fill_manual(values=c('black','grey30','lightgray','white')) +
  ggtitle ('(D)')

# checar q para abundance within site é between site
str_random <- str_random1 +
  theme(legend.position = "")

legenda <- get_legend(str_random1)

##################################
# Mytilaster
##################################

myt_random <- bind_rows(
  glmmTMB(cover ~ (1|region/subregion/site), data=brachi_cv, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Mytilaster",
           variable = "cover",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)),
  glmmTMB(tamanho_mm ~ (1|region/subregion/site), data=brachi_sz, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Mytilaster",
           variable = "size",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)) 
) %>% 
  filter(effect %in% c("site:subregion:region", "subregion:region", "region", "residual")) %>%
  mutate(effect = plyr::mapvalues(effect, from = c("site:subregion:region", "subregion:region", "region", "residual"),
                                  to = c("site", "subregion", "region", "within site")) %>% 
           factor(., levels = c("region", "subregion", "site", "within site"))) %>% 
  ggplot(aes(x = variable, y = variance, fill = effect, weight = variance)) + 
  geom_bar(stat="identity", position="fill", color='black') + 
  theme_var +
  labs(y="Proportion of variance", x="") +
  scale_fill_manual(values=c('black','grey30','lightgray','white')) +
  theme(legend.position = "")


##################################
# Tetraclita
##################################

tetra_random <- bind_rows(
  glmmTMB(vivos_n ~ (1|region/subregion/site), data=tetra_ab, REML=T, family = nbinom1) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Tetraclita",
           variable = "abundance",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)),
  glmmTMB(tamanho_mm ~ (1|region/subregion/site), data=tetra_sz, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Tetraclita",
           variable = "size",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .))
) %>% 
  filter(effect %in% c("site:subregion:region", "subregion:region", "region", "residual")) %>%
  mutate(effect = plyr::mapvalues(effect, from = c("site:subregion:region", "subregion:region", "region", "residual"),
                                  to = c("site", "subregion", "region", "within site")) %>% 
           factor(., levels = c("region", "subregion", "site", "within site"))) %>% 
  ggplot(aes(x = variable, y = variance, fill = effect, weight = variance)) + 
  geom_bar(stat="identity", position="fill", color='black') + 
  theme_var +
  labs(y="Proportion of variance", x="") +
  scale_fill_manual(values=c('black','grey30','lightgray','white')) +
  theme(legend.position = "")



##################################
# Lottia
##################################

lot_random <- bind_rows(
  glmmTMB(adultos ~ (1|region/subregion/site), data=lapas_ab, REML=T, family = nbinom1) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Lottia",
           variable = "abundance",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)),
  glmmTMB(tamanho_mm ~ (1|region/subregion/site), data=lapas_sz, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Lottia",
           variable = "size",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)) 
) %>% 
  filter(effect %in% c("site:subregion:region", "subregion:region", "region", "residual")) %>%
  mutate(effect = plyr::mapvalues(effect, from = c("site:subregion:region", "subregion:region", "region", "residual"),
                                  to = c("site", "subregion", "region", "within site")) %>% 
           factor(., levels = c("region", "subregion", "site", "within site"))) %>% 
  ggplot(aes(x = variable, y = variance, fill = effect, weight = variance)) + 
  geom_bar(stat="identity", position="fill", color='black') + 
  theme_var +
  labs(y="Proportion of variance", x="") +
  scale_fill_manual(values=c('black','grey30','lightgray','white')) +
  theme(legend.position = c(.02, .90),
        legend.justification = c("left"),
        legend.box.just = "left",
        legend.margin = margin(0, 0, 0, 0)) +
  theme(legend.position =  "none") +
  theme(legend.position = "")



##################################
# Echinolittorina 
##################################

nodi_random <- bind_rows(
  glmmTMB(abund ~ (1|region/subregion), data=nodi_ab, REML=T, family = nbinom2) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Echinolittorina",
           variable = "abundance",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)),
  glmmTMB(tamanho_mm ~ (1|region/subregion/site), data=nodi_sz, REML=T) %>% 
    get_variance() %>% 
    unlist() %>% 
    data.frame() %>% 
    rownames_to_column(var = "effect") %>% 
    rename(variance = ".") %>% 
    mutate(model = "Echinolittorina",
           variable = "size",
           effect = gsub("var.intercept.", "", effect) %>% 
             gsub("var.", "", .)) 
  ) %>% 
  filter(effect %in% c("site:subregion:region", "subregion:region", "region", "residual")) %>%
  mutate(effect = plyr::mapvalues(effect, from = c("site:subregion:region", "subregion:region", "region", "residual"),
                                  to = c("site", "subregion", "region", "within site")) %>% 
           factor(., levels = c("region", "subregion", "site", "within site"))) %>% 
  ggplot(aes(x = variable, y = variance, fill = effect, weight = variance)) + 
  geom_bar(stat="identity", position="fill", color='black') + 
  theme_var +
  labs(y="Proportion of variance", x="") +
  scale_fill_manual(values=c('black','grey30','lightgray','white')) +
  theme(legend.position = "")

# checar q para abundance within site é between site