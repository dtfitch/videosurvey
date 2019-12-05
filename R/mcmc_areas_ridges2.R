# Jane Carlen
# This is an extrension of bayesplot::mcmc_areas_ridges that allows me to recode names with the varname_dict I supply and to change the thickness of the density outline.


library(purrr)
varname_dict =  readRDS("../data/variable_name_dictionary.RDS")

mcmc_areas_ridges2 <- function (x, pars = character(), regex_pars = character(), transformations = list(), 
                                ..., prob_outer = 1, prob = 1, bw = NULL, adjust = NULL, 
                                kernel = NULL, n_dens = NULL, 
                                # my added args = jc
                                size = .1, names = varname_dict) 
{
  bayesplot:::check_ignored_arguments(...)
  data <- mcmc_areas_ridges_data(x, pars = pars, regex_pars = regex_pars, 
                                 transformations = transformations, prob = prob, prob_outer = prob_outer, 
                                 bw = bw, adjust = adjust, kernel = kernel, n_dens = n_dens)
  #recode names -jc
  data$parameter = as.character(data$parameter)
  tmp = sapply(paste0('(?<!\\.)', names(varname_dict),"$"), grep, data$parameter, perl = TRUE)
  names(tmp) = varname_dict

  for (x in 1:length(tmp)) {
    if (length(tmp[[x]]) > 0) {data$parameter[tmp[[x]]] = names(tmp)[x]}
  }
  data$parameter = as.factor(data$parameter)
  datas <- data %>% split(data$interval)
  x_lim <- range(datas$outer$x)
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", size = 0.5)
  }
  else {
    geom_ignore()
  }
  args_outer <- list(mapping = aes_(height = ~density), color = "black",#bayesplot:::get_color("dark"), 
                     fill = NA, stat = "identity", size = .1)
  layer_outer <- do.call(ggridges::geom_density_ridges, args_outer)
  test_plot <- ggplot(datas$outer) + aes_(x = ~x, y = ~parameter) + 
    layer_outer
  soft_build <- ggplot_build(test_plot)
  scaler1 <- unique(soft_build$data[[1]][["scale"]])
  scaler2 <- unique(soft_build$data[[1]][["iscale"]])
  scale <- scaler1 * scaler2
  layer_list_inner <- list()
  par_draw_order <- levels(unique(data$parameter))
  bg <- bayesplot_theme_get()[["panel.background"]][["fill"]] %||% 
    "white"
  for (par_num in seq_along(unique(data$parameter))) {
    this_par <- par_draw_order[par_num]
    next_pars <- par_draw_order[par_num < seq_along(par_draw_order)]
    this_par_data <- datas$inner %>% dplyr::filter(.data$parameter == 
                                                     this_par) %>% mutate(color = "black",#bayesplot:::get_color("dark"),
                                                                          fill = "gray80")#bayesplot:::get_color("light"))
    next_par_data <- datas$outer %>% dplyr::filter(.data$parameter %in% 
                                                     next_pars) %>% mutate(color = "black",#bayesplot:::get_color("dark"), 
                                                                           fill = bg)
    args_inner <- list(mapping = aes_(height = ~density,
                                      color = ~color, fill = ~fill),
                       size = size,
                       data = dplyr::bind_rows(this_par_data,  next_par_data), 
                       scale = scale,
                       stat = "identity")
    layer_list_inner[[par_num]] <- do.call(ggridges::geom_ridgeline, 
                                           args_inner)
  }
  ggplot(datas$outer) + aes_(x = ~x, y = ~parameter) + layer_outer + 
    scale_y_discrete(limits = unique(rev(data$parameter)), 
                     expand = c(0.05, 0.6)) + layer_list_inner +
    layer_vertical_line + 
    scale_fill_identity() + scale_color_identity() + xlim(x_lim) + 
    yaxis_title(FALSE) + xaxis_title(FALSE) + bayesplot_theme_get() + 
    bayesplot:::grid_lines_y(color = "gray90") + theme(axis.text.y = element_text(hjust = 1, 
                                                                      vjust = 0))
}

