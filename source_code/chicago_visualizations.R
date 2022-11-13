# devtools::install_github("dmwelgus/MapChi")
library(MapChi)

# modification of function from dmwelgus/MapChi package
heat_map_continuous <- function(regions, summary_df, regions_var, fill_var, 
                                legend_name, palette = NULL, low_color = "#fff5eb", 
                                high_color = "#7f2704", na_replace = NA, lines = "black", 
                                title = NULL, title_size = 15,
                                inputted_text = NULL, neighborhood_column,
                                statistic_of_interest,
                                statistic_label, diversity_variable) {
  if (!is.null(palette)) {
    
    colors <- c("green", "blue", "red", "orange", "purple")
    low    <- c("#e5f5f9", "#deebf7", "#fee0d2", "#fee6ce", "#efedf5")
    high   <- c("#00441b", "#08306b", "#67000d", "#7f2704", "#3f007d")
    
    min_color <- low[colors  == palette]
    max_color <- high[colors == palette]
    
  } else {
    
    min_color <- low_color
    max_color <- high_color
  }
  
  step_1 <- get(regions)
  step_1@data$id <- rownames(step_1@data)
  
  step_2 <- fortify(step_1)
  
  chi.df <- merge(step_2, step_1@data, by = "id")
  
  merge_vars <- c("AREA_NUMBE", "TRACTCE10", "DIST_NUM", "ZIP")
  types      <- c("CAs", "tracts", "districts", "zips")
  
  merge_var.x <- merge_vars[types == regions]
  
  df <- merge(chi.df, summary_df, by.x = merge_var.x, by.y = regions_var, all.x = TRUE)
  
  df$fill_it <- df[, fill_var]
  df$fill_it[is.na(df$fill_it)] <- na_replace
  
  df <- df[order(df$order), ]
  
  if (length(statistic_of_interest) != length(statistic_label)) {
    stop("Must have the same number of statistics and labels")
  }
  
  map_output <- ggplot(df) + 
    geom_polygon(aes(long, lat, group = group, fill = fill_it)) +
    geom_path(aes(long, lat, group = group,
                  text = paste0("Neighborhood: ",
                                eval(parse(text = neighborhood_column)),
                                "\n", statistic_label, ": ",
                                eval(parse(text = statistic_of_interest)),
                                "\nCommunity Group: ",
                                eval(parse(text = diversity_variable)))),
              color = lines) +
    theme(axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.title = element_blank(),
          plot.title = element_text(size = title_size),
          panel.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white")) +
    coord_equal() +
    scale_fill_continuous(low = min_color, high = max_color,
                          guide = guide_colorbar(title = legend_name, 
                                                 title.theme = element_text(size = 13, angle = 0),
                                                 label.theme = element_text(size = 11, angle = 0))) +
    ggtitle(title)
  
  map_output
}

heat_map_discrete <- function(regions, summary_df, regions_var,
                              fill_var, legend_name, palette, na_replace = NA,
                              lines = "black", title = NULL, title_size = 15, region_labels = FALSE,
                              neighborhood_column,  
                              diversity_variable) {
  
  if (region_labels == TRUE & !regions %in% c("CAs", "districts")) {
    stop("region_labels only available for Community Areas and Police Districts")
  }
  
  step_1 <- get(regions)
  step_1@data$id <- rownames(step_1@data)
  
  step_2 <- fortify(step_1)
  
  chi.df <- merge(step_2, step_1@data, by = "id")
  
  merge_vars <- c("AREA_NUMBE", "TRACTCE10", "DIST_NUM", "ZIP")
  types      <- c("CAs", "tracts", "districts", "zips")
  
  merge_var.x <- merge_vars[types == regions]
  
  
  df <- merge(chi.df, summary_df, by.x = merge_var.x, by.y = regions_var, all.x = TRUE)
  
  df$fill_it <- df[, fill_var]
  # add in na_replace 
  df$fill_it[is.na(df$fill_it)] <- na_replace
  
  df <- df[order(df$order), ]
    
    map_output <- ggplot(df) + 
      geom_polygon(aes(long, lat, group = group, fill = fill_it)) +
      geom_path(aes(long, lat, group = group,
                                      text = paste0("Neighborhood: ",
                                                    eval(parse(text = neighborhood_column)),
                                                    "\nCommunity Group: ",
                                                    eval(parse(text = diversity_variable)))), 
                         color = lines) +
      theme(axis.ticks = element_blank(), 
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(), 
                     axis.title = element_blank(),
                     plot.title = element_text(size = title_size),
                     panel.background = element_rect(fill = "white"), 
                     panel.grid.major = element_line(colour = "white"),
                     panel.grid.minor = element_line(colour = "white")) +
      coord_equal() +
      scale_fill_manual(values = palette, 
                                 guide = guide_legend(title = legend_name, 
                                                               title.theme = element_text(size = 13, angle = 0),
                                                               label.theme = element_text(size = 11, angle = 0))) +
      ggtitle(title)

    map_output
}