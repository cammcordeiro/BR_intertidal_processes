plot_pca_sep <- function(dados, dados1, X, Y, widthB=0.4, heightB=0.4) {

  library(factoextra)
  library(ggplot2)
# plot invisible = ind, then var

a <- fviz_pca_biplot(prcomp(dados1, center = TRUE, scale. = TRUE), repel = TRUE,
                col.var = "black", # Variables color
                col.ind=dados$region, #col.ind = "cos2",  # Individuals color
                labelsize = 3, pointsize = 1, #alpha.var = 0.5,
                invisible = "var"
                 ) + 
                scale_color_manual(values=c(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))) + #"red", "orange", "#E5E500", "darkgreen", "darkblue"
                theme(panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing = unit(0,"null"))
                               

b <- fviz_pca_biplot(prcomp(dados1, center = TRUE, scale. = TRUE), repel = TRUE,
                col.var = "black", # "contrib"
                labelsize = 3.5, #alpha.var = 0.6,
                invisible = "ind"
                 ) + 
                 theme(legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text = element_blank(),
                               axis.title = element_blank(),
                               axis.line = element_blank()
                      )

A <- viewport(width=1, height=1, x=0.5, y=0.5)                 
B <- viewport(width= widthB, height= heightB, x=X, y=Y)

print(a, vp=A)
print(b, vp=B)

}


# # # # # # # # # # # # # # # # # # # 

plot_dist <- function(df, var, title = "", ...) {
	
	resumo <-	summarySE(df, measurevar=var, groupvars=c("subregion","site","distance_S"), na.rm=T)


ggplot(resumo, aes(x=distance_S, y=mean)) +
	geom_smooth(method = "loess") +
    # geom_smooth(method="lm", formula = y ~ splines::bs(x, 3)) +
    geom_errorbar(width=1, aes(ymin= mean-ci, ymax= mean+ci), colour="black") + 
    geom_point(aes(size=cv, bg=subregion), shape=22, color="lightgray") +
    scale_fill_manual(values=c(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))) +
    theme(panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               axis.line = element_line(size = 0.5, colour = "black")) +
	labs(x="Distance from southernmost sampling point (km)", y= paste(var, "mean +- 95% c.i.", sep=" "), title=paste(title)) +
	geom_text(aes(label=ifelse(mean > mean(mean)+2*sd(mean), as.character(site), '')), hjust=1, vjust=2, size=3)

}