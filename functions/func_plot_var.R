
##################################
####### PLOTTING FUNCTIONS #######
##################################

plotg <- function(df, variavelx, variavely, legx, legy) {
  ggplot(df, aes(x=df[,variavelx], y=df[,variavely])) +
    geom_point(alpha = 0.3) +
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
    stat_summary(fun=mean, geom="point", shape=18, size=1, color="black") +
    stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), color="black") +
    labs(x=legx, y=legy) + #title(paste(titulo)) +
    scale_color_manual(values=c("purple3", "steelblue2","forestgreen", "goldenrod2","darkorange1", "firebrick4"))
}

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 


##################################
####### VARIANCE FUNCTION ########
##################################


# get all sources of variance
# all_variance <- function(var1, familia1, dados1, var2, familia2, dados2, spp) {
#   
#   formila1 <- as.formula(paste(var1, " ~ ", paste(c(1, '(1|region/subregion/site)'), collapse= "+")))
#   formila2 <- as.formula(paste(var2, " ~ ", paste(c(1, '(1|region/subregion/site)'), collapse= "+")))
#   
#   model1 <- glmmTMB(formila1, data = dados1, REML=T, family = familia1, ziformula=~0)
#   model2 <- glmmTMB(formila2, data = dados2, REML=T, family = familia2, ziformula=~0)
#   
#   bind_rows(
#     
#     # size
#     bind_rows(
#       model1 %>% 
#         VarCorr() %>% 
#         unlist() %>%
#         data.frame() %>% 
#         rownames_to_column(var = 'fator') %>% 
#         rename(variance = '.'), 
#       
#       get_variance_residual(model1) %>% 
#         data.frame() %>% 
#         rownames_to_column(var = 'fator') %>% 
#         rename(variance = '.')  
#     ) %>% 
#       mutate(variavel = var1,
#              spp = spp),
#     
#     # abundance or cover
#     bind_rows(
#       model2 %>% 
#         VarCorr() %>% 
#         unlist() %>%
#         data.frame() %>% 
#         rownames_to_column(var = 'fator') %>% 
#         rename(variance = '.'), 
#       
#       get_variance_residual(model2) %>% 
#         data.frame() %>% 
#         rownames_to_column(var = 'fator') %>% 
#         rename(variance = '.')  
#     ) %>% 
#       mutate(variavel = var2,
#              spp = spp)
#   )
# }


all_variance <- function(var1, familia1, dados1, var2, familia2, dados2, spp, variable1, variable2) {
  
  formila1 <- as.formula(paste(var1, " ~ ", paste('(1|region/subregion/site)', collapse= "+")))
  formila2 <- as.formula(paste(var2, " ~ ", paste('(1|region/subregion/site)', collapse= "+")))
  
  model1 <- glmmTMB(formila1, data = dados1, REML=T, family = familia1)
  model2 <- glmmTMB(formila2, data = dados2, REML=T, family = familia2)
  
  bind_rows(
    
    model1 %>% 
      get_variance() %>% 
      unlist() %>% 
      data.frame() %>% 
      rownames_to_column(var = "effect") %>% 
      rename(variance = ".") %>% 
      mutate(model = spp,
             variable = variable1,
             effect = gsub("var.intercept.", "", effect) %>% 
               gsub("var.", "", .)),
    
    model2 %>% 
      get_variance() %>% 
      unlist() %>% 
      data.frame() %>% 
      rownames_to_column(var = "effect") %>% 
      rename(variance = ".") %>% 
      mutate(model = spp,
             variable = variable2,
             effect = gsub("var.intercept.", "", effect) %>% 
               gsub("var.", "", .)
      )
  )
}