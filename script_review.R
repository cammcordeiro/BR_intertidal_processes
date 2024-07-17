
lapas_sz %>% 
  group_by(subregion) %>% 
  summarise(media = mean(mean_size),
            sd = sd(mean_size))

# # A tibble: 6 × 3
# subregion media    sd
# <fct>     <dbl> <dbl>
# 1 MRBS       6.37  2.35
# 2 SSCh       6.38  2.65
# 3 Ubatuba    6.58  3.03
# 4 SCRJ       6.45  4.15
# 5 MRRJ      10.6   3.86
# 6 LRRJ       8.98  1.92

lm(mean_size ~ subregion, lapas_sz) %>% 
  emmeans::emmeans(pairwise~subregion, type = "response") %>% plot

# Analysis of Variance Table
# 
# Response: mean_size
# Df Sum Sq Mean Sq F value    Pr(>F)    
# subregion   5 1690.9  338.19  42.348 < 2.2e-16 ***
# Residuals 671 5358.5    7.99                


# $contrasts
# contrast       estimate    SE  df t.ratio p.value
# MRBS - SSCh    -0.00964 0.399 671  -0.024  1.0000
# MRBS - Ubatuba -0.21277 0.301 671  -0.708  0.9810
# MRBS - SCRJ    -0.08006 0.560 671  -0.143  1.0000
# MRBS - MRRJ    -4.19695 0.347 671 -12.088  <.0001
# MRBS - LRRJ    -2.60645 0.321 671  -8.116  <.0001
# SSCh - Ubatuba -0.20313 0.416 671  -0.488  0.9966
# SSCh - SCRJ    -0.07042 0.630 671  -0.112  1.0000
# SSCh - MRRJ    -4.18731 0.451 671  -9.286  <.0001
# SSCh - LRRJ    -2.59681 0.431 671  -6.022  <.0001
# Ubatuba - SCRJ  0.13271 0.572 671   0.232  0.9999
# Ubatuba - MRRJ -3.98418 0.367 671 -10.863  <.0001
# Ubatuba - LRRJ -2.39368 0.342 671  -6.994  <.0001
# SCRJ - MRRJ    -4.11689 0.598 671  -6.883  <.0001
# SCRJ - LRRJ    -2.52639 0.583 671  -4.331  0.0002
# MRRJ - LRRJ     1.59050 0.384 671   4.145  0.0005

lapas_sz %>% 
  ggplot(aes(x = subregion, y = mean_size, fill = subregion)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.3) +
    theme_classic() +
    labs(y = "size (cm)", x = "") +
    theme(legend.position = "") +
    ggtitle ('Lottia subrugosa') +
    theme(plot.title = element_text(face = "bold.italic"))
    scale_fill_manual(values=c("purple3", "steelblue2","forestgreen", "goldenrod2","darkorange1", "firebrick4"))



#####################################################
#####################################################
tetra_sz %>% 
  group_by(subregion) %>% 
  summarise(media = mean(mean_size),
            sd = sd(mean_size))

# # A tibble: 6 × 3
# subregion media    sd
# <fct>     <dbl> <dbl>
# 1 MRBS       4.24 0.693
# 2 SSCh       4.43 0.595
# 3 Ubatuba    4.34 0.637
# 4 SCRJ       4.46 0.650
# 5 MRRJ       4.36 0.595
# 6 LRRJ       4.63 0.580

lm(mean_size ~ subregion, tetra_sz) %>% 
  emmeans::emmeans(pairwise~subregion, type = "response") %>% plot

# Analysis of Variance Table
# 
# Response: mean_size
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# subregion   5  11.092 2.21843  5.5796 4.908e-05 ***
#   Residuals 598 237.761 0.39759                      
  
# $contrasts
# contrast       estimate     SE  df t.ratio p.value
# MRBS - SSCh     -0.1854 0.0976 598  -1.900  0.4031
# MRBS - Ubatuba  -0.0993 0.0750 598  -1.324  0.7716
# MRBS - SCRJ     -0.2118 0.1044 598  -2.029  0.3271
# MRBS - MRRJ     -0.1179 0.0867 598  -1.360  0.7509
# MRBS - LRRJ     -0.3862 0.0766 598  -5.044  <.0001
# SSCh - Ubatuba   0.0861 0.0979 598   0.880  0.9513
# SSCh - SCRJ     -0.0264 0.1219 598  -0.217  0.9999
# SSCh - MRRJ      0.0675 0.1071 598   0.630  0.9888
# SSCh - LRRJ     -0.2008 0.0991 598  -2.027  0.3284
# Ubatuba - SCRJ  -0.1125 0.1047 598  -1.075  0.8913
# Ubatuba - MRRJ  -0.0186 0.0870 598  -0.214  0.9999
# Ubatuba - LRRJ  -0.2869 0.0770 598  -3.729  0.0029
# SCRJ - MRRJ      0.0939 0.1133 598   0.828  0.9622
# SCRJ - LRRJ     -0.1744 0.1058 598  -1.649  0.5665
# MRRJ - LRRJ     -0.2683 0.0884 598  -3.035  0.0300
  
  #####################################################
  #####################################################
  
  abiot <- read.csv2("~/Meu Drive (cammcordeiro@gmail.com)/PUBLICACOES/INTERTIDAL_CONSUMERS/2024/Brazil_project/data/abioticos.csv")
  
abiot %>% 
  mutate(ext_m_brachi = ext_m_brachi-ext_m_tetra,
         ext_m_chth = ext_m_chth-(ext_m_tetra+ext_m_brachi)) %>% 
  group_by(site) %>% 
  summarise(ext_m_tetra = mean(ext_m_tetra, na.rm=TRUE),
            ext_m_brachi = mean(ext_m_brachi, na.rm=TRUE),
            ext_m_chth = mean(ext_m_chth, na.rm=TRUE),
            incl_graus_tetra = mean(incl_graus_tetra, na.rm=TRUE),
            incl_graus_brachi = mean(incl_graus_brachi, na.rm=TRUE),
            incl_graus_chth = mean(incl_graus_chth, na.rm=TRUE)) %>% 
  filter(!is.na(ext_m_tetra),
         !is.na(ext_m_brachi),
         !is.na(ext_m_chth)) %>% 
  mutate(alt_tetra = sin(incl_graus_tetra*0.01745329)*ext_m_tetra,
         alt_brach = sin(incl_graus_brachi*0.01745329)*ext_m_brachi,
         alt_chth = sin(incl_graus_chth*0.01745329)*ext_m_chth) %>% 
  select(alt_tetra:alt_chth) %>% 
  summarise(quantile(alt_tetra),
            quantile(alt_brach),
            quantile(alt_chth))

abiot %>% 
  mutate(ext_m_brachi = ext_m_brachi-ext_m_tetra,
         ext_m_chth = ext_m_chth-(ext_m_tetra+ext_m_brachi)) %>% 
  group_by(site) %>% 
  summarise(extensao = mean(extensao, na.rm=TRUE),
            inclinacao = mean(inclinacao, na.rm=TRUE)) %>% 
  filter(!is.na(extensao),
         !is.na(inclinacao)) %>% 
  mutate(altura = sin(inclinacao*0.01745329)*extensao) %>% 
  summarise(quantile(altura))


#####################################################
#####################################################

readxl::read_excel("/Users/cesarcordeiro/Downloads/2015/air_temp.xlsx") %>% 
  group_by(subregion) %>% 
  summarise(air_mean = mean(temp),
            air_sd = sd(temp))

# local           subregion        temp_mean temp_sd
# <chr>           <chr>                <dbl>   <dbl>
# 1 Arraial do Cabo Lakes                 23.3    2.17
# 2 Copacabana      Rio de Janeiro        23.8    3.18
# 3 Iguape          Baixada Santista      21.7    4.28
# 4 Parati          Green Coast           23.1    4.32

read.csv2("~/Meu Drive (cammcordeiro@gmail.com)/PUBLICACOES/INTERTIDAL_CONSUMERS/2024/Brazil_project/data/abioticos_2020.csv", sep = ",") %>% 
  mutate(sst_mean = as.numeric(sst_mean)) %>% 
  group_by(subregion) %>% 
  summarise(sst_mean = mean(sst_mean, na.rm = T),
            sst_sd = sd(sst_mean, na.rm = T))

# # A tibble: 6 × 3
# subregion temp_mean temp_sd
# <chr>         <dbl>   <dbl>
# 1 LRRJ           23.1  0.0715
# 2 MRBS           25.4  0.212 
# 3 MRRJ           24.2  0.246 
# 4 SCRJ           25.8  0.398 
# 5 SSCh           25.5  0.378 
# 6 Ubatuba        25.4  0.426 
