##################################
######## FIGURES 6 AND 7 #########
##################################

source("fig random_effects.R")

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
        plot.title = element_text(face = "bold", size = 12))

#### abund ~ wf_km + Pp_ok (perna perna presence)

## Predicted values for effect of wave fetch
str_wf <- stramonita_ab %>% 
  glmmTMB(abund ~ wf_log + Pp_ok, data = ., REML=T, family = nbinom2) %>% 
  ggpredict(., c("wf_log")) %>% 
  data.frame() %>% 
  rename(wf_log = x) %>% 
  ggplot(aes(wf_log, predicted)) + 
  geom_point(data = read.csv("data/stramonita_abundance.txt", header=T, sep="\t"),
             aes(wf_log, abund), size= 2.2, alpha=0.3) + 
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  theme_pred +
  labs(x = bquote('Wave fetch'~'['~log[10]~'(no. of cells) ]'), 
       y = "Abundance (n)") +
  scale_y_continuous(breaks = seq(0, 95, 15)) + 
  ggtitle ('(A)')

## prediction for presence of mussel Perna perna
teste <- stramonita_ab %>% 
  glmmTMB(abund ~ wf_log + Pp_ok, data = ., REML=T, family = nbinom2) %>%
  ggpredict(., c("Pp_ok")) %>% 
  data.frame() %>% 
  mutate(x = plyr::mapvalues(x, from = c(0, 1), to = c("absent", "present"))) %>% 
  dplyr::rename(Pp_ok = x,
         abund = predicted)

str_pp <- stramonita_ab %>% 
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
  ggtitle ('(B)')


#### tamanho_mm ~ Brach_cover
## Predicted values for effect of wave fetch
str_brac <- stramonita_sz %>% 
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
  ggtitle ('(C)')

##################################
## LOTTIA #####
##################################

gam_lp <- glmmTMB(tamanho_mm ~ sst_mean + fwd_mean + rugosity + (1|subregion/site), data=lapas_sz, REML=T) 

# size x sst
lot_sz_sst <- gam_lp %>%
  ggpredict(., c("sst_mean[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(sst_mean = x) %>%
  ggplot(aes(sst_mean, predicted)) +
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = lapas_sz, aes(y = tamanho_mm, x = sst_mean), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)")),
       y = "Size (mm)") +
  ggtitle ('(A)') +
  scale_y_continuous(breaks = seq(0, 15, 5))

# size x roughness
lot_sz_rug <- gam_lp %>%
  ggpredict(., c("rugosity[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(rugosity = x) %>%
  ggplot(aes(rugosity, predicted)) +
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = lapas_sz, aes(y = tamanho_mm, x = rugosity), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = "Roughness", y = "") + #"Size (mm)"
  ggtitle ('(B)') +
  scale_y_continuous(breaks = seq(0, 15, 5))

# sst x fwd
lot_sz_fwd <- gam_lp %>%
  ggpredict(., c("fwd_mean[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(fwd_mean = x) %>%
  ggplot(aes(fwd_mean, predicted)) +
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = lapas_sz, aes(y = tamanho_mm, x = fwd_mean), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = "FwD",
       y = "Size (mm)") +
  ggtitle ('(C)') +
  scale_y_continuous(breaks = seq(0, 15, 5))

##########

## adultos ~ Chl-a
lot_ab_chl <- lapas_ab %>%
  group_by(site) %>% 
  summarise(adultos = mean(adultos),
            chl_mean = mean(chl_mean)) %>% 
  ggplot(aes(y = adultos, x = chl_mean)) +
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Chlorophyll", italic(" a "), " (mg.", l^-1,")")), 
       y = expression(paste("Density (ind.100",cm^-2,")"))) +
  ggtitle ('(E)')


## adultos ~ wf_log
lot_ab_wf <- lapas_ab %>%
  group_by(site) %>% 
  summarise(adultos = mean(adultos),
            wf_log = mean(wf_log)) %>% 
  ggplot(aes(y = adultos, x = wf_log)) +
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = bquote('Wave fetch'~'['~log[10]~'(no. of cells) ]'), 
       y = "") + #expression(paste("Density (ind.100",cm^-2,")"))
  ggtitle ('(F)')


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

nod_sz_lat <- nodi_sz %>% 
  group_by(site) %>%
  summarise(tamanho_mm = mean(tamanho_mm)) %>%
  left_join(abiot <- read.csv("data/abioticos_2020.csv", header = T)) %>%
  ggplot(aes(x = lat, y = tamanho_mm)) + 
    geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
    geom_point(size= 2.2, alpha=0.4) +
    theme_classic() +
    theme_pred +
    scale_x_reverse() +
    labs(x = "Latitude (degrees)", y = "Size (cm)") +
    ggtitle ('(A)')

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

myt_agg <- left_join(brachi_cv %>% select(-str_ab, -subregion) %>% aggregate(cover ~ site+region, ., mean),
                     brachi_cv %>% select(site, subregion, chl_mean:inclinacao, str_ab, easting, northing) %>% distinct())
scam_cv <- scam::scam(cover ~ wf_log + s(fwd_mean, bs="mpi"), data=myt_agg)

# cover x wave fetch
myt_cv_wf <- predict(scam_cv, se=T) %>% # check if this is the right way to predict values
  data.frame() %>% 
  mutate(wf_log = myt_agg$wf_log) %>% 
  ggplot(aes(y = fit, x = wf_log)) + 
  geom_smooth(method = 'lm', color = "black") +
  geom_point(data = myt_agg, aes(y = cover, x = wf_log), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = bquote('Wave fetch'~'['~log[10]~'(no. of cells) ]'), y = "Cover (%)") +
  ggtitle ('(A)') + 
  ylim(0,1)


# cover x FwD
myt_cv_fwd <- predict(scam_cv, se=T) %>%
  data.frame() %>% 
  mutate(fwd_mean = myt_agg$fwd_mean) %>% 
  ggplot(aes(y = fit, x = fwd_mean)) + 
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cr"), color = "black") +
  geom_point(data = myt_agg, aes(y = cover, x = fwd_mean), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = "FwD", y = "") + #"Cover (%)"
  ggtitle ('(B)') +
  ylim(0,1)

# size x SST
myt_sz_sst <- lmer(tamanho_mm ~ sst_mean + (1|site), data = brachi_sz, REML= TRUE) %>% 
  ggpredict(., c("sst_mean[all]"), type = c("fe")) %>%
  data.frame() %>%
  rename(SST = x) %>% 
  ggplot(aes(SST, predicted)) + 
  geom_smooth(aes(ymin = conf.low, ymax = conf.high), stat = "identity", color = "black") +
  geom_point(data = brachi_sz, aes(y = tamanho_mm, x = sst_mean), size= 2.2, alpha=0.4) + 
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)"), " cover (%)"), 
       y = "Size (mm)") +
  ggtitle ('(C)')


##################################
## TETRACLITA #####
##################################

# size ~ fwd_mean
tet_sz_fwd <- tetra_sz %>% 
  glmmTMB(tamanho_mm ~ fwd_mean + (1|site), ., REML=T) %>% 
  ggpredict(c("fwd_mean[all]"), type = c("fe")) %>% 
  data.frame() %>% 
  rename(fwd_mean = x) %>% 
  ggplot(., aes(fwd_mean, predicted)) +
  geom_ribbon(aes(y = NULL, ymin = conf.low, ymax = conf.high), fill = "grey50", alpha=0.5, stat = "identity") +
  geom_line(mapping = aes(x = fwd_mean, y = predicted), linewidth = 0.5, color="black") +
  geom_point(data =  tetra_sz %>% 
               group_by(site, quadrado) %>%
               summarise(tamanho_mm = mean(tamanho_mm),
                         fwd_mean = mean(fwd_mean)), aes(y = tamanho_mm, x = fwd_mean), size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = "FwD", 
       y = "Size (cm)") +
  ggtitle ('(A)')


# abundance x SST
tet_ab_sst <- tetra_ab %>%
  group_by(site) %>%
  summarise(abun = mean(vivos_n),
            sst_mean = mean(sst_mean)) %>%
  ggplot(aes(y = abun, x = sst_mean)) + 
  geom_smooth(method = 'lm', formula = y ~ x, fill = "grey50", color = "black") +
  geom_point(size= 2.2, alpha=0.4) +
  theme_pred +
  labs(x = expression(paste("Sea surface temperature (",degree,"C)"), " cover (%)"), 
       y = expression(paste("Density (ind.100",cm^-2,")"))) +
  ggtitle ('(B)')


########################################


# Figura 5 = Stramonita
((str_wf + str_pp) / ((str_brac + (str_random + legenda))))
 
# Figure 6 = Lottia
((lot_sz_sst + lot_sz_rug) / ((lot_sz_fwd + ((lot_random + ggtitle ('(D)')) + legenda))) / 
    (lot_ab_chl + lot_ab_wf)) 

# Figura 7 = Mytilaster
((myt_cv_wf + myt_cv_fwd)) / ((myt_sz_sst + ((myt_random + ggtitle ('(D)')) + legenda)))

# Figure 8 = Tetraclita
(tet_sz_fwd / tet_ab_sst / (tetra_random + ggtitle ('(C)') + legenda))

# Figure 9 = Echinolittorina
(nod_sz_lat + ((nodi_random + ggtitle ('(B)')) + legenda))


#rm(list=ls())

