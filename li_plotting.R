# for is_empty
library(purrr)

p_val_df <- function(df, grouper, y, x) {
  p_df <- df %>%
    rstatix::group_by(!! sym(grouper))  %>%
    # finally! pass strings as formulas https://stackoverflow.com/questions/66515390/t-test-with-column-number-instead-of-column-name
    rstatix::t_test(reformulate(x, y)) %>%
    rstatix::add_significance(p.col = "p") %>% 
    rstatix::add_xy_position(x = grouper, dodge = 0.8) # important for positioning!
  
  return(p_df)
}

# iteration help https://stackoverflow.com/questions/4856849/looping-over-variables-in-ggplot
plot_small_scatter_helper <- function(summary, time_palette, shapes, ys, add_p = F, is_log=F, identifier='', add_lm=F) {
  for (i in 1:length(ys)) {
    plot_small_scatter(summary, time_palette, shapes, 'time', ys[i], add_p, is_log=is_log, identifier = identifier, add_lm = add_lm)
  }
}

plot_small_scatter <- function(summary, time_palette, shapes, x, y, add_p = F, add_lm=F, identifier='', is_log=F) {
  # must group for proper dodge
  plt <- summary %>% group_by(cond) %>% 
    ggplot(aes_string(x = x, y = y))
  
  plt <- plt + 
    geom_point(position = position_jitterdodge(), size = 5, color = outline_color, stroke = 1,
               aes_string(shape = 'time', alpha = 'time', fill = 'cond')) +
    # choose shapes https://blog.albertkuo.me/post/point-shape-options-in-ggplot/
    scale_shape_manual(values = shapes) +
    scale_y_continuous(labels = scientific) +
    # draw_key_point(color = 'black') +
    # make time scales same https://stackoverflow.com/questions/38238960/how-do-you-control-color-using-facet-wrap-and-ggplot
    # custom palette
    scale_fill_manual(values = time_palette) +
    scale_alpha_manual(values = c(1, 0.5)) 
  if (add_p) {
    # calculate p 
    df_p <- p_val_df(summary, x, y, 'cond')
    plt <- plt +
      add_pvalue(df_p,
                 color = outline_color,
                 xmin = 'xmin',
                 xmax = 'xmax',
                 label = '{p.signif}',
                 tip.length = 0) 
  }
  if (is_log) {
    plt <- plt + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) +
      annotation_logticks(sides = 'l') 
  }
  if (add_lm) {
    # http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
    plt <- plt + geom_smooth(color = outline_color, method = lm)
  }
  
  save_light_graph(paste0(y, identifier), plt)
}


