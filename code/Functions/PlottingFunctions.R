# Function: PlottingFunctions.R
# By: Sabine Loos
# Latest Update: 07.25.2019

# DESCRIPTION: These functions change the elements of a theme for plots in ggplot

# FUNCTIONS: There are multiple functions in this file including:
## plotTheme - general plotting theme
## plotThemeCoeff - plotting theme for a coefficient plot
## plotThemeMap - plotting theme for maps
## my_loess - function to create a loess plot with ggplot
## my_fn - function to to create loess plot for correlation matrix using ggpairs
## my_fn_diag - funtion to create histograms for correlation matrix using ggpairs
## plot_coeff - plotting a coefficient plot
## plot_vario - making a nice looking variogram
## plot_SA_ridgeline - plotting a ridgeline (stacked densities) using output of sensitivity analysis
## plot_raster_nepal - make maps of 11 districts outside Kathmandu using a raster dataset
## plot_points_nepal - makes maps of 11 districts outside Kathmandu using a points dataset
## saveplot - wrapper function for ggsave, with pre-specified inputs

# DEPENDENCIES:
## mapping functions call sp2gg


# loading required packages, functions and data ---------------------------
require(ggplot2)
if(!exists("sp2gg")){
  source("code/Functions/sp2gg.R") #contains functions to convert spatial to ggplot dataframe
}
# see if district data exists
if(!exists("dist11_gg")) {
  # load boundary polygon for the 11 districts outside of Nepal
  dist11_shp <- readRDS("data/Nepal_11dist_boundaries.rds")
  dist11_gg <- sp2gg(dist11_shp) #self-written functions
}
# fonts (change if on windows)
suppressMessages(require(extrafont))
suppressMessages(font_import(pattern = "OpenSans-Regular", prompt = F))
suppressMessages(loadfonts(device = "win"))

# Plotting themes ---------------------------------------------------------
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text(size=(base_size-2), color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=(base_size-2)),
    axis.title = element_text(size=(base_size-2)),
    axis.text = element_text(size=(base_size-2)),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    plot.margin=grid::unit(c(1,1,1,1), "mm"),
    legend.margin = margin(c(3,1,1,1)),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(size=(base_size-2),colour = "black", face = "italic"))
}

plotThemeCoeff <- function(base_size = 12) {
  theme(
    text = element_text(size =(base_size-2), color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=(base_size-2)),
    axis.title = element_text(size=(base_size)),
    axis.text = element_text(size=(base_size-2)),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

plotThemeMap <- function(base_size = 12) {
  theme(
    text = element_text( color = "black", family = "Open Sans"),
    plot.title = element_text(size = (base_size+4),colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "transparent", color = "white"),
    strip.text = element_text(size=base_size),
    axis.title = element_text(size=(base_size-2)),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.8,0.9),
    legend.direction = "horizontal",
    legend.text = element_text(size = (base_size-2),colour = "black", face = "italic",
                               margin = margin(r = 1, unit = "pt")),
    legend.title = element_text(size=(base_size-2), vjust = 1),
    legend.key = element_blank(),
    legend.key.width = unit(12, "pt"),
    legend.key.height = unit(8, "pt"),
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    legend.margin = margin(c(1,1,1,1)))
}

# Specific types of plots -------------------------------------------------
# quick way to plot a loess plot given data and mapping
my_loess <- function(data, mapping, method="loess", sel_shape = ".", color_smooth = "dodgerblue3", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(shape = sel_shape, cex = 2, alpha = 1) + 
    geom_smooth(method=method, color = color_smooth, ...)
  p
}
# functions for loess plots in the off-diagonals of a correlation matrix
my_fn <- function(data = pred_grid_df, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) +
    scale_color_manual(values = cols)
}
# functions for histograms in the diagonals of a correlation matrix
my_fn_diag <- function(data, mapping, dens=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_histogram, dens)+ 
    scale_fill_manual(values = cols)
}
# coefficient plots
# note all vectors should have matching order
plot_coeff <- function(var_vec, # vector of variables
                       coeff_vec, # vector of coefficients for those variables
                       se_vec, # vector of standard errors of coefficients 
                       names_vec = var_vec, # names that you want to use for plotting (if different from var_vec) 
                       var_color, # color for coefficients and standard errors
                       plot_true = F, # plot true coefficients?
                       truecoeff_vec = NA, # vector of true coefficients (in same order as coeff_vec)
                       var_title = expression("Damage variable,"~ X[k]), 
                       coeff_title = expression("Trend coefficient,"~beta[k]),
                       base_size =10){
  beta_coeff <- data.frame(int = var_vec,
                           beta = as.numeric(coeff_vec),
                           se = se_vec)
  beta_coeff$int <- as.factor(setNames(names_vec, beta_coeff$int))
  beta_coeff$int <- factor(beta_coeff$int,levels = beta_coeff$int[order(abs(beta_coeff$beta), decreasing = F)])
  newx = stringr::str_wrap(levels(beta_coeff$int), width = 10)
  p.coeff <-
    ggplot(beta_coeff,aes(y = beta, x = int)) +
    geom_hline(aes(yintercept = 0), lty = "dotted", color = "black")+
    geom_point(size =0.75, color = GDIF_col)+
    geom_linerange(aes(ymin = beta - se/2, ymax = beta + se/2), color = var_color)+
    labs(x = var_title, y = coeff_title)+
    scale_x_discrete(labels = newx)+
    coord_flip() + plotThemeCoeff(base_size = base_size)
  if(plot_true){
    beta_coeff$beta_true = as.numeric(truecoeff_vec)
    p.coeff <- p.coeff + geom_point(data = beta_coeff, size = 0.75, aes(y = beta_true, x = int))
  }
  return(p.coeff)
}
# variogram plot
plot_vario <- function(variomod_trend,
                       variomod_detrend,
                       plot_true = F,
                       variotrue = NA,
                       names_vario,
                       dist_title = "Distance, h (km)",
                       semivar_title = expression("Semivariance,"~gamma),
                       base_size = 10,
                       resid_col = GDIF_col,
                       trend_col = OG_col){
  # sill and range parameters
  c0_trend = variomod_trend$var_model$psill[1]
  c0_detrend = variomod_detrend$var_model$psill[1]
  r_detrend = variomod_detrend$var_model$range[2]
  # create dataframe for plotting
  vario.theor <- variogramLine(variomod_detrend$var_model, maxdist = max(variomod_detrend$exp_var$dist), dist_vector = variomod_detrend$exp_var$dist)
  vario.theor$gamma_exp <- variomod_detrend$exp_var$gamma
  vario.theor$np <- variomod_detrend$exp_var$np
  
  vario.theor$gamma_trend <- variogramLine(variomod_trend$var_model, maxdist = max(variomod_detrend$exp_var$dist), dist_vector = variomod_detrend$exp_var$dist)$gamma
  vario.theor$gamma_trend_exp <- variomod_trend$exp_var$gamma
  vario.theor$np_trend <- variomod_trend$exp_var$np
  vario.theor <- rbind(c(0, c0_detrend,0,  0,c0_trend,0,0), vario.theor)
  
  vario.theor_melt <- reshape2::melt(vario.theor, id.vars = "dist", measure.vars = c("gamma_trend", "gamma"))
  levels(vario.theor_melt$variable) <- names_vario
  # plot variogram
  p.vario <- ggplot(vario.theor_melt) + 
    geom_segment(aes(x = r_detrend/1000, xend = r_detrend/1000, y = 0, yend = variogramLine(resid.vgm$var_model,dist_vector =r_detrend)$gamma), color = resid_col, lty = "dotted")+
    geom_point(data = vario.theor[-1,],aes(x = dist/1000, y = gamma_trend_exp), alpha = 0.5, color = trend_col)+
    geom_point(data = vario.theor[-1,],aes(x = dist/1000, y = gamma_exp), alpha = 0.5, color = resid_col)+
    geom_line(aes(x = dist/1000, y = value, group = variable, color = variable))+
    scale_x_continuous(expand = c(0, 0), limits = c(0,79)) + scale_y_continuous(expand = c(0, 0), limits = c(0,1.62))+
    scale_color_manual("", values = c(trend_col, resid_col))+
    # geom_text(data = vario.theor[-1,],aes(x = dist, y = gamma_exp, label = np), 
    # nudge_x = -1, nudge_y = -0.075, check_overlap = T, size = 2)+
    labs(x = dist_title, y = semivar_title, size = "Number of field surveyed grids")+
    plotTheme(base_size = base_size) + theme(legend.position = c(0.8,0.2), 
                                      legend.direction = "vertical") + guides(size = F)
  if(plot_true){
    c0true <- variotrue$var_model$psill[1]
    vario.theor$dist_true <- append(0, resid.vgm_true$exp_var$dist)
    vario.theor$gamma_true <- append(c0true, variogramLine(variotrue$var_model, maxdist = max(variotrue$exp_var$dist), dist_vector = variotrue$exp_var$dist)$gamma)
    vario.theor$gamma_true_exp <- append(0,variotrue$exp_var$gamma)
    vario.theor_melt <- reshape2::melt(vario.theor, id.vars = "dist", measure.vars = c("gamma_trend", "gamma", "gamma_true"))
    levels(vario.theor_melt$variable) <- append(names_vario, "True residual variogram")
    p.vario <- p.vario + geom_point(data = vario.theor[-1,],aes(x = dist_true/1000, y = gamma_true_exp), alpha = 0.5, color = "black") + 
      geom_line(data = vario.theor,aes(x = dist/1000, y = gamma_true), alpha = 0.5, color = "black")
  }
  return(p.vario)
}

# ridgeline plots for sensitivity analysis
plot_SA_ridgeline <- function(SA_data, 
                              xvar_char, 
                              yvar_char,
                              colvar_char,
                              error_eng,
                              cols,
                              cols_labs,
                              x_title, y_title, base_size = 8,
                              x_lims){
  require(ggridges)
  x_ind <- which(names(SA_data) %in% xvar_char)
  y_ind <- which(names(SA_data) %in% yvar_char)
  if(is.factor(SA_data[,y_ind]) == F) {SA_data[,y_ind] <- factor(SA_data[,y_ind])}
  
  p.SA <- ggplot(data = SA_data, 
         aes_string(x = xvar_char, y = yvar_char)) +
    geom_density_ridges(rel_min_height = 0.0001,
                        size = 0.5,
                        vline_size = 0.3, vline_color = cols_gdifeng[1],
                        scale = 1.5, rel_min_height = 0.01, alpha = 0.5,
                        stat = "binline", binwidth = 0.01,
                        quantile_lines = TRUE, quantiles = 2,
                        aes_string(fill = colvar_char, colour = colvar_char, height = "..count..")) +
    geom_vline(aes(xintercept = error_eng, colour = "ENG", fill = "ENG"),size = .5)+
    # scale_discrete_manual("point_color",name="Damage Data",values=cols, labels = cols_labs, guide = "none") +
    scale_colour_manual(name="Damage Data",values=cols, labels = cols_labs,guide = "none") +
    scale_fill_manual(name="Damage data",values=cols, labels = cols_labs) +
    scale_y_discrete(expand = c(0,0.05))+
    scale_x_continuous(expand = c(0,0),limits = x_lims)+
    labs(x = x_title, y = y_title)+
    plotTheme(base_size = base_size)+
    theme(legend.position = c(0.8,0.8), legend.direction = "vertical")
  return(p.SA)
}

# Mapping Nepal ----------------------------------------------
plot_raster_nepal <- function(raster, draw_kathmandu = T, 
                              draw_districts = T,
                              draw_field = F,
                              draw_scalebar = T,
                              dist_gg = dist11_gg,
                              field_df = field_df,
                              legend_title = "",
                              scale_legend = F,
                              legend_lims = round(range(as.vector(raster), na.rm = T),2),
                              base_size = 10,
                              line_size = 0.25,
                              col_scale =  "viridis",
                              direction = 1,
                              ...) {
  require(rasterVis); require(viridis); 
  # latitude for kathmandu
  kat_pt <- data.frame(long = 85.3240, lat = 27.7172, name = "Kathmandu")
  
  # base plot (just raster)
  p <- gplot(raster) + geom_tile(aes(fill = value))
  
  # adding scale bar
  if(draw_scalebar){
    require(ggsn)
    p <- p + ggsn::scalebar(dist11_gg,location = "bottomleft",dist = 25, 
                            st.size=2, height=0.01, dd2km = TRUE, model = 'WGS84')
  }
    
  
  # adding districts
  if(draw_districts){
    # plot
    p <- p + geom_polygon(data = dist11_gg, aes(long, lat, group=group), 
                          colour = "seashell4", fill = "transparent", size = line_size)
  }
  
  # adding point for kathmandu
  if (draw_kathmandu) {
    p <- p + geom_point(data = kat_pt, aes(long, y = lat), shape = 18, color = "tomato3")+
      geom_text(data = kat_pt, aes(long, y = lat, label = name), hjust = 0, nudge_x = 0.04, size = 2,family = "Open Sans")
  }
  
  # adding points for field
  if(draw_field){
    p <- p+ geom_point(data = field_df, aes(coords.x1, coords.x2),shape = 19, size = 0.75)
  }
  # final touches
  if(scale_legend){
    p <- p +
      scale_fill_viridis(legend_title,na.value = NA, discrete = F, direction = direction, limits = legend_lims, breaks = legend_lims, guide=guide_colourbar(title.position="top"), option = col_scale)+
      plotThemeMap(base_size = base_size) + coord_equal()
  }else{
    p <- p +
      scale_fill_viridis(legend_title,na.value = NA, discrete = F, direction = direction, guide=guide_colourbar(title.position="top"), option = col_scale)+
      plotThemeMap(base_size = base_size) + coord_equal()
  }
  
  return(p)
}

plot_points_nepal <- function(field_df, draw_kathmandu = T, 
                              draw_districts = T,
                              draw_field = F,
                              dist_gg = dist11_gg,
                              legend_title = "",
                              scale_legend = F,
                              base_size = 10,
                              line_size = 0.5,
                              legend_lims = round(range(as.vector(raster), na.rm = T),2),
                              ...) {
  require(viridis); require(ggsn)
  # latitude for kathmandu
  kat_pt <- data.frame(long = 85.3240, lat = 27.7172, name = "Kathmandu")
  
  # base plot (just points)
  p <- ggplot(field_df) + geom_point(data = field_df, aes(coords.x1, coords.x2,...))+
    ggsn::scalebar(dist11_gg,location = "bottomleft",dist = 25, 
             st.size=2, height=0.01, dd2km = TRUE, model = 'WGS84')
  
  # adding districts
  if(draw_districts){
    p <- p + geom_polygon(data = dist11_gg, aes(long, lat, group=group), colour = "seashell4", fill = "transparent", size = line_size)
  }
  
  # adding point for kathmandu
  if (draw_kathmandu) {
    p <- p + geom_point(data = kat_pt, aes(long, y = lat), shape = 18, color = "tomato3")+
      geom_text(data = kat_pt, aes(long, y = lat, label = name), hjust = 0, nudge_x = 0.04, size = 3,family = "Open Sans")
  }
  
  p <- p +  plotThemeMap(base_size = base_size) + coord_equal()

  return(p)
}
# Saving plots ------------------------------------------------------------
saveplot <- function(plot = last_plot(), width = 6, height = 4, 
                     units = "in", dpi = 600, 
                     file_locn = "figs/DPM_bound_data/", 
                     file_name = "plot",...){
  ggsave(paste0(file_locn, file_name, ".png"), plot = plot, 
         width = width, height = height, units = units, dpi = dpi, ...=...)
}

