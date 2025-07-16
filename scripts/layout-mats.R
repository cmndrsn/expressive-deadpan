# va-modeling.R

matrix_scatter <- rbind(c(1, 2, 3, 6, 6),
                        c(1, 2, 3, 6, 6),
                        c(1, 2, 3, 6, 6),
                        c(4, 5, NA, 6, 6),
                        c(4, 5, NA, 6, 6),
                        c(4, 5, NA, 6, 6),
                        c(7, 7, 7, 7, 7))

# custom legend for expressive vs. deadpan conditions

legend_edp <- ggpubr::get_legend(
  ggplot2::ggplot(
    data.frame(
      x = 1:2, 
      y = 1:2, 
      Condition = c('Expressive', 'Deadpan')
    ), 
    ggplot2::aes(x = x, y = y, shape = Condition, colour = Condition)
  ) +
    ggplot2::geom_point()+
    ggplot2::scale_colour_manual(values = c('purple4', 'darkgreen'))+
    ggplot2::scale_shape_manual(values = c(21, 19))+
    ggplot2::theme(legend.position = 'bottom')
)


# va-comparison.qmd ---------------------------------------------------------

circLayout <- rbind(
  rep(1, each = 8),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6)
)

corrSigLayout <-  rbind(
  rep(c(2,3,5,4), times = c(6,4,4,6)),
  rep(c(2,3,5,4), times = c(6,4,4,6)),              
  rep(c(2,3,5,4), times = c(6,4,4,6)),
  rep(c(2,3,5,4), times = c(6,4,4,6)),
  rep(c(2,3,5,4), times = c(6,4,4,6)),
  rep(c(2,3,5,4), times = c(6,4,4,6)),
  rep(1, times = 20)
)


pctCorLay <- rbind(
  c(2,3,4,5),
  c(2,3,4,5),
  c(2,3,4,5),
  c(6,7,8,9),
  c(6,7,8,9),
  c(6,7,8,9),
  c(1,1,1,1)
)

# Get legend for correlation by condition figure --------------------------

dummyDat <- data.frame(
  x = c(
    1,
    2
  ),
  y = c(
    1,
    3
  ), 
  z = c(
    'Major',
    'minor'
  ), 
  w = c(
    'Bach',
    'Chopin'
  ), 
  v = c(
    'Regression', 
    'Unity'
  )
)

correlationLegendManual <- ggplot2::ggplot(
  data = dummyDat, 
  ggplot2::aes(
    x = x,
    y = y,
    color = z, 
    shape = w, 
    linetype = v)
) +
  ggplot2::geom_point()+
  ggplot2::theme(
    legend.position = 'top'
  ) +
  ggplot2::geom_abline(
    color = 'black'
  ) +
  ggplot2::geom_smooth(
    color = 'grey50'
  ) +
  ggplot2::scale_linetype_manual(
    name = 'Line',
    breaks= c(
      'Unity', 
      'Regression'
    ),
    values = c(
      9,
      1
    )
  ) +
  ggplot2::scale_color_manual(
    name = 'Mode',
    breaks = c(
      'Major', 
      'minor'
    ),
    values = c(
      'Major' = 'firebrick1', 
      'minor' = 'dodgerblue2'
    )
  ) +
  ggplot2::scale_shape_manual(
    name = 'Composer',
    breaks = c(
      'Bach', 
      'Chopin'
    ),
    values = c(
      16,
      17
    )
  )

correlationLegend <- cowplot::get_plot_component(
  correlationLegendManual, 
  'guide-box-top', 
  return_all = TRUE
)

# Get legend for percent correlation figure -------------------------------

percentLegendManual <- ggplot2::ggplot(
  data = dummyDat, 
  ggplot2::aes(
    x = x,
    y = y,
    color = z, 
    shape = w)
) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = 1,
      y = 1
    )
  ) +
  ggplot2::theme(
    legend.position = 'top'
  ) +
  ggplot2::scale_color_manual(
    name='Mode',
    breaks=c(
      'Major', 
      'minor'
    ),
    values=c(
      'Major'='firebrick1', 
      'minor'='dodgerblue2'
    )
  )+
  ggplot2::scale_shape_manual(
    name = 'Composer',
    breaks = c(
      'Bach', 
      'Chopin'
    ),
    values = c(
      16,
      17)
  ) 


percentLegend <- cowplot::get_plot_component(
  percentLegendManual, 
  'guide-box-top', 
  return_all = TRUE
)
