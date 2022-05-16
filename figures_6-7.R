##################################
######## FIGURES 6 AND 7 #########
##################################

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(glmmTMB)

##################################
## STRAMONITA #####
##################################
# stramonita_pred = ggarrange(m1, m2, ncol = 2, nrow = 1, align = "hv")
# stramonita_pred
# ggsave("stramonita_pred.tiff", dpi = 600, compression = 'lzw')

theme_pred <- theme_classic() + 
  theme(axis.text.x = element_text(size=12, color="black"), 
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(color="black", size=14),
        axis.title.y = element_text(color="black", size=16),
        axis.ticks = element_line(colour = "black", size = 0.8),
        axis.line = element_line(colour = 'black', size = 0.8),
        axis.ticks.length = unit(4, "pt"),
        strip.text.x = element_text(size = 10), strip.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold.italic", size = 10))

#### abund ~ wf_km + Pp_ok (perna perna presence)

## Predicted values for effect of wave fetch
str_wf <- read.csv("data/stramonita_abundance.txt", header=T, sep="\t") %>% 
  glmmTMB(abund ~ wf_log + Pp_ok, data = ., REML=T, family = nbinom2) %>% 
  ggpredict(., c("wf_log")) %>% 
  data.frame() %>% 
  rename(wf_log = x) %>% 
  ggplot(aes(wf_log, predicted)) + 
  geom_point(data = read.csv("data/stramonita_abundance.txt", header=T, sep="\t"),
             aes(wf_log, abund), size= 2.2, alpha=0.3) + 
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  theme_pred +
  labs(x = "log[wave fetch (km)]", y = "Abundance (n)") +
  scale_y_continuous(breaks = seq(0, 95, 15)) + 
  ggtitle ('Stramonita haemastoma')

## prediction for presence of mussel Perna perna
teste <- read.csv("data/stramonita_abundance.txt", header=T, sep="\t") %>% 
  glmmTMB(abund ~ wf_log + Pp_ok, data = ., REML=T, family = nbinom2) %>%
  ggpredict(., c("Pp_ok")) %>% 
  data.frame() %>% 
  mutate(x = plyr::mapvalues(x, from = c(0, 1), to = c("absent", "present"))) %>% 
  dplyr::rename(Pp_ok = x,
         abund = predicted)

str_pp <- read.csv("data/stramonita_abundance.txt", header=T, sep="\t") %>% 
  mutate(Pp_ok = plyr::mapvalues(Pp_ok, from = c(0, 1), to = c("absent", "present"))) %>% 
  ggplot(aes(Pp_ok, abund)) + 
  geom_jitter(position=position_jitter(0.1), alpha=0.5, size=2.5, color="darkgray")+
  geom_smooth() +
  theme_pred +
  labs(x = expression(paste(italic("Perna perna"), " occurrence")), 
       y = "") +  #"Abundance (n)"
       # y = expression(paste("Density (whelks 1.25",m^-2,")"))) +
  scale_y_continuous(breaks = seq(0, 95, 15)) +
  geom_errorbar(data=teste, width=0.1, aes(ymin = abund-conf.low, ymax = abund+conf.low, x = Pp_ok), colour="black") + 
  geom_point(data=teste, size= 4, bg="black", shape=21, color="lightgray") +
  ggtitle ('Stramonita haemastoma')


#### tamanho_mm ~ Brach_cover
## Predicted values for effect of wave fetch
str_brac <- read.csv("data/stramonita_size.txt", header=T, sep="\t") %>% 
  ggplot(aes(Brach_cover, tamanho_mm)) + 
  geom_smooth(method = 'lm', color = 'black') +
  geom_smooth(data = read.csv("data/stramonita_size.txt", header=T, sep="\t") %>% 
                glmmTMB(tamanho_mm ~ Brach_cover + (1|site), data= ., REML = TRUE) %>% 
                ggpredict(., c("Brach_cover")) %>%
                data.frame() %>% 
                dplyr::rename(Brach_cover = x,
                       tamanho_mm = predicted),
              aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(size= 2.2, alpha=0.3) + 
  theme_pred +
  labs(x = expression(paste(italic("Mytilaster solisianus"), " cover (%)")), 
       y = "Size (mm)") +
  scale_y_continuous(breaks = seq(0, 95, 15)) +
  ggtitle ('Stramonita haemastoma')



##################################
## LOTTIA #####
##################################

lapas_sz1 <- read.csv("data/lottia_sz2020.csv", header=T)
lapas_sz <- lapas_sz1 %>% 
  filter(estagio_recruta_adulto == 'A') %>% 
  group_by(site, quadrado) %>% 
  summarise(mean_size = mean(tamanho_mm)) %>% 
  dplyr::select(site, mean_size) %>% 
  inner_join(., unique(lapas_sz1[,c(1,6:10,12:18)]), by='site') %>% 
  data.frame()

lapas1 <- left_join(lapas_sz %>% aggregate(mean_size ~ site+subregion+easting+northing, ., mean),
                    lapas_sz %>% select(site, sst_mean, fwd_mean, rugosity) %>% distinct())

gam_lp = mgcv::gam(mean_size ~ s(sst_mean) + s(rugosity), data=lapas1)

## mean_size ~ fwd_mean
lot_sz_sst <- mgcv::gam(mean_size ~ s(sst_mean) + s(rugosity), data=lapas1) %>%
  ggpredict(., c("sst_mean[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(sst_mean = x) %>%
  ggplot(aes(sst_mean, predicted)) +
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = lapas1, aes(y = mean_size, x = sst_mean), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)")),
       y = "Size (mm)") +
  ggtitle ('Lottia subrugosa') +
  scale_y_continuous(breaks = seq(0, 15, 5))

## mean_size ~ rugosity
lot_sz_rug <- mgcv::gam(mean_size ~ s(sst_mean) + s(rugosity), data=lapas1) %>%
  ggpredict(., c("rugosity[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(rugosity = x) %>%
  ggplot(aes(rugosity, predicted)) +
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = lapas1, aes(y = mean_size, x = rugosity), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = "Roughness", y = "") + #"Size (mm)"
  ggtitle ('Lottia subrugosa') +
  scale_y_continuous(breaks = seq(0, 15, 5))

##########
lapas <- read.table("data/lottia_ab.txt", header=T, sep="\t") ## Dataset without Leblon, C. Itaguá and Éden
lapas <- droplevels(subset(lapas, !is.na(adultos))) ##removing NA's according to 'adultos' column#

## adultos ~ Chl-a
lot_ab_chl <- lapas %>%
  group_by(site) %>% 
  summarise(adultos = mean(adultos),
            chl_mean = mean(chl_mean)) %>% 
  ggplot(aes(y = adultos, x = chl_mean)) +
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Chlorophyll", italic(" a "), " (mg.", l^-1,")")), 
       y = expression(paste("Density (ind.100",cm^-2,")"))) +
  ggtitle ('Lottia subrugosa')


## adultos ~ wf_log
lot_ab_wf <- lapas %>%
  group_by(site) %>% 
  summarise(adultos = mean(adultos),
            wf_log = mean(wf_log)) %>% 
  ggplot(aes(y = adultos, x = wf_log)) +
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = "log[wave fetch (km)]", 
       y = "") + #expression(paste("Density (ind.100",cm^-2,")"))
  ggtitle ('Lottia subrugosa')


##################################
## ECHINOLITTORINA #####
##################################

# tamanho_mm ~ lat
# nod_sz_lat <- read.csv("data/nodi_sz2020.csv", header=T) %>% 
#   group_by(site) %>% 
#   summarise(tamanho_mm = mean(tamanho_mm)) %>% 
#   left_join(abiot <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/abioticos_2020.csv", header = T) %>% 
#               select(site, lat)) %>% 
#   ggplot(., aes(lat, tamanho_mm)) + 
#   geom_point(alpha=0.4, size= 2.2) +
#   theme_pred +
#   geom_smooth(method = "lm", color = "black") +
#   labs(x = "Latitude", y = "Average size (mm)") +
#   ggtitle ('Echinolittorina lineolata')

nod_sz_lat <- read.csv("data/nodi_sz2020.csv", header=T) %>% 
  group_by(site) %>%
  summarise(tamanho_mm = mean(tamanho_mm)) %>%
  left_join(abiot <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/abioticos_2020.csv", header = T)) %>%
  ggplot(aes(x = lat, y = tamanho_mm)) + 
    geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
    geom_point(size= 2.2, alpha=0.4) +
    theme_classic() +
    theme_pred +
    scale_x_reverse() +
    labs(x = "Latitude (degrees)", y = "Size (cm)") +
    ggtitle ('Echinolittorina lineolata')

# read.csv("data/nodi_sz2020.csv", header=T) %>% 
#   group_by(site) %>%
#   summarise(tamanho_mm = mean(tamanho_mm)) %>%
#   left_join(abiot <- read.csv("~/Google Drive/PUBLICACOES/CONSUMERS_SE-BR/2021/data/abioticos_2020.csv", header = T) %>%
#               select(site, lat, region)) %>% 
#   lm(tamanho_mm ~ region, .) %>% anova()

##################################
## MYTILASTER #####
##################################
# cover ~ s(fwd_mean) + wf_log

myt_cv <- read.csv("data/mytilaster_cv2020.csv", header = T) # Mytilaster strata
myt_cv$str_ab[is.na(myt_cv$str_ab)] <- 0.01
myt_cv[29:30, 3] <- 0.84
myt_cv[389:390, 3] <- 0.73

myt_agg <- left_join(myt_cv %>% select(-str_ab, -subregion) %>% aggregate(cover ~ site+region, ., mean),
                     myt_cv %>% select(site, subregion, chl_mean:inclinacao, str_ab, easting, northing) %>% distinct())

# model
scam_cv <- scam::scam(cover ~ wf_log + s(fwd_mean, bs="mpi"), data=myt_agg)

# Wave fetch
myt_cv_wf <- predict(scam_cv, se=T) %>% # check if this is the right way to predict values
  data.frame() %>% 
  mutate(wf_log = myt_agg$wf_log) %>% 
  ggplot(aes(y = fit, x = wf_log)) + 
  geom_smooth(method = 'lm', color = "black") +
  geom_point(data = myt_agg, aes(y = cover, x = wf_log), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = "log[wave fetch (km)]", y = "Cover (%)") +
  ggtitle ('Mytilaster solisianus') + 
  ylim(0,1)


# Freshwater
myt_cv_fwd <- predict(scam_cv, se=T) %>%
  data.frame() %>% 
  mutate(fwd_mean = myt_agg$fwd_mean) %>% 
  ggplot(aes(y = fit, x = fwd_mean)) + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cr"), color = "black") +
  geom_point(data = myt_agg, aes(y = cover, x = fwd_mean), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = "Freshwater index", y = "") + #"Cover (%)"
  ggtitle ('Mytilaster solisianus') +
  ylim(0,1)

#### tamanho_mm ~ sst_mean
# Predicted values for effect of SST
myt_size1 <- read.csv("data/mytilaster_sz2020.csv", header=T)
myt_size1$str_ab[is.na(myt_size1$str_ab)] <- 0.01
myt_size <- myt_size1 %>% na.omit() %>% droplevels()

newdata <- aggregate(.~site+region+subregion, FUN=mean, data=myt_size)
m2 <- scam::scam(tamanho_mm~s(sst_mean, bs="mpd"), data = newdata)

myt_sz_sst <- predict(m2, se=T) %>% # check if this is the right way to predict values
  data.frame() %>% 
  mutate(sst_mean = newdata$sst_mean) %>% 
  ggplot(aes(sst_mean, fit)) + 
  geom_ribbon(aes(y = NULL, ymin = fit-se.fit, ymax = fit+se.fit), fill = "grey50", alpha=0.5)+
  geom_smooth(stat = "identity", color = "black") + 
  geom_point(data = newdata, aes(y = tamanho_mm, x = sst_mean), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)"), " cover (%)"), 
       y = "Size (mm)") +
  ggtitle ('Mytilaster solisianus')


##################################
## TETRACLITA #####
##################################

## mean_size ~ fwd_mean
tetra_sz1 <- read.csv("data/tetra_sz2020.csv", header=T) 

tetra_sz <- read.csv("data/tetra_sz2020.csv", header=T) %>% 
  dplyr::group_by(site, quadrado) %>% 
  dplyr::summarise(mean_size = mean(tamanho_mm)) %>% 
  dplyr::select(site, mean_size) %>% 
  inner_join(., unique(tetra_sz1[,c(1,4:17)]), by='site') %>% 
  data.frame() %>% 
  filter(mean_size < mean(mean_size) + sd(mean_size) & 
           mean_size > mean(mean_size) - sd(mean_size)) 

tetra1 <- left_join(tetra_sz %>% select(-str_ab, -subregion) %>% aggregate(mean_size ~ site+region+easting+northing, ., mean),
                    tetra_sz %>% select(site, fwd_mean) %>% distinct())

tet_sz_fwd <- lm(mean_size ~ fwd_mean, data=tetra1) %>% 
  ggpredict(c("fwd_mean[all]"), type = c("fe")) %>% 
  data.frame() %>% 
  rename(fwd_mean = x) %>% 
  ggplot(., aes(fwd_mean, predicted)) +
  geom_ribbon(aes(y = NULL, ymin = conf.low, ymax = conf.high), fill = "grey50", alpha=0.5) +
  geom_line(mapping = aes(x = fwd_mean, y = predicted), size = 0.5, color="black") +
  geom_point(data = tetra1, aes(y = mean_size, x = fwd_mean), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = "Freshwater index", 
       y = "Size (cm)") +
  ggtitle ('Tetraclita stalactifera')


## PA ~ sst_mean
tetra_ab <- read.csv("data/tetra_ab2020.csv", header=T)
tetra_ab$str_ab[is.na(tetra_ab$str_ab)] <- 0.01
# tetra_ab <- tetra_ab %>% 
#   mutate(PA = ifelse(vivos_n == 0, 0, 1))
# tetra_p <- tetra_ab %>% 
#   filter(vivos_n > 0)
# 
# tetra2 <- left_join(tetra_ab %>% select(-str_ab, -replicate, -subregion) %>% aggregate(vivos_n~site+region, ., mean),
#                     tetra_ab %>% select(-str_ab, -replicate, -subregion) %>% aggregate(cbind(sst_mean, easting, northing, fwd_mean)~site+region, ., mean))
# 
# tet_pa_sst <- glmmTMB(PA ~ sst_mean + (1|site), data=tetra_ab, REML=T, family = binomial) %>% 
#   ggpredict(., c("sst_mean[all]"), type = c("fe")) %>% 
#   data.frame() %>% 
#   dplyr::rename(sst_mean = x) %>% 
#   ggplot(., aes(sst_mean, predicted)) + 
#     geom_jitter(data = tetra_ab, aes(y = PA, x = sst_mean), 
#                 alpha = 0.4, height = 0.07, width = 0.01) +
#     geom_ribbon(aes(y = NULL, ymin = conf.low, ymax = conf.high), fill = "grey50", alpha=0.5) +
#     geom_line(mapping = aes(x = sst_mean, y = predicted), size = 0.5, color="black") +
#     theme_pred +
#     labs(x = expression(paste("Sea surface temperature (",degree,"C)")), 
#          y = expression(paste(italic("Tetraclita stalactifera")," potential presence"))) +
#     ggtitle ('Tetraclita stalactifera') +
#     scale_y_continuous(breaks = seq(0, 1, 1)) 
#   

## vivos_n ~ s(sst_mean)

tet_ab_sst <- tetra_ab %>%
  group_by(site) %>% 
  summarise(abun = mean(vivos_n),
            sst_mean = mean(sst_mean)) %>% 
  ggplot(aes(y = abun, x = sst_mean)) + 
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)")), 
       y = expression(paste("Density (ind.100",cm^-2,")"))) +
  ggtitle ('Tetraclita stalactifera')

# model
# scam_ts = scam::scam(vivos_n ~ s(sst_mean, bs="mpd"), data=tetra2)

# tet_ab_sst <- predict(scam_ts, se=T) %>%
#   data.frame() %>% 
#   mutate(sst_mean = tetra2$sst_mean) %>% 
#   ggplot(aes(y = fit, x = sst_mean)) + 
#   geom_ribbon(aes(y = NULL, ymin = fit-se.fit, ymax = fit+se.fit), fill = "grey50", alpha=0.5)+
#   #geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cr", k=7), color = "black") +
#   geom_line(mapping = aes(x = sst_mean, y = fit), size = 0.3, color="black") +
#   geom_point(data = tetra2, aes(y = vivos_n, x = sst_mean), alpha = 0.4) +
#   theme_pred +
#   labs(x = expression(paste("Sea surface temperature (",degree,"C)")), 
#        y = expression(paste("Density (barnacles.100",cm^-2,")"))) +
#   ggtitle ('Tetraclita stalactifera')


########################################

rm(list = setdiff(ls(), c("theme_pred", "str_wf", "str_pp", "str_brac", 
                          "myt_cv_wf", "myt_cv_fwd", "myt_sz_sst",
                          "tet_sz_fwd", "tet_pa_sst", "tet_ab_sst",
                          "lot_sz_sst", "lot_sz_rug", "nod_sz_lat")))

library(patchwork)

#### figure 6 ####
(str_wf + str_pp) / (str_brac + nod_sz_lat) / (lot_sz_sst + lot_sz_rug) / (lot_ab_chl + lot_ab_wf)

#### figure 7 ####
(myt_cv_wf + myt_cv_fwd) / (myt_sz_sst + tet_sz_fwd) / (tet_ab_sst + plot_spacer()) / (plot_spacer() + plot_spacer())

#rm(list=ls())
