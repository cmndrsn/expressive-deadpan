source("lib/functions.R")
source("scripts/joins.R")
source("scripts/layout-mats.R")

# Select necessary columns and prepare dataframes ----------------------------------------

predictors <- c(
  'mode', 'arPerf', 'pitchHeight', 
  'rms', 'rms_std', 'brightness_mean', 'ambitus_range'
)

df_valence <- df_full |> dplyr::select(
  valence,
  dplyr::any_of(predictors), 
  condition
)
df_arousal <- df_full |> dplyr::select(
  arousal,
  dplyr::any_of(predictors), 
  condition
)

# create piece-level dataframe for comparing differences between conditions

df_full_summary <- df_full |> dplyr::group_by(
  composer, albumID, pieceID
) |>
  dplyr::summarise(
    dplyr::across(
      c("valence", "arousal", predictors[predictors != 'mode']),
      mean
    ),
    mode = unique(mode)
  )

# calculate piecewise differences from deadpan vs. interpretive conditions

df_deadpan <- subset(
  df_full_summary, 
  stringr::str_detect(
    df_full_summary$albumID, "eadpan"
  )
)
df_interpretive <- subset(
  df_full_summary, 
  !stringr::str_detect(
    df_full_summary$albumID, "eadpan"
  )
)

# verify pieces are in same order

all(
  paste0(df_deadpan$composer, df_deadpan$pieceID) == 
    paste0(df_interpretive$composer, df_interpretive$pieceID)
)

# Models ----------------------------------------------------------

# set seed for reproducibility
set.seed(123)

# tune lambda at alpha = 0.5
# (balancing ridge regression and lasso penalty parameters)

# fit valence model (all ratings)

en_valence_dp <- elastic_selection(
  valence ~ . - condition, 
  subset(df_valence, condition == "deadpan"), 
  alpha = 0.5
)

en_valence_ex <- elastic_selection(
  valence ~ . - condition, 
  subset(df_valence, condition == "expressive"), 
  alpha = 0.5
)

# fit arousal model (all ratings)

en_arousal_dp <- elastic_selection(
  arousal ~ . - condition, 
  subset(df_arousal, condition == "deadpan"), 
  alpha = 0.5
)

en_arousal_ex <- elastic_selection(
  arousal ~ . - condition, 
  subset(df_arousal, condition == "expressive"), 
  alpha = 0.5
)
var_imp_val_dp <- get_var_imp(en_valence_dp)
var_imp_val_ex <- get_var_imp(en_valence_ex)
var_imp_val_dp$dim <- var_imp_val_ex$dim <- "Valence - Ratings"
var_imp_val_dp$condition <- "deadpan"
var_imp_val_ex$condition <- "expressive"

var_imp_aro_dp <- get_var_imp(en_arousal_dp)
var_imp_aro_ex <- get_var_imp(en_arousal_ex)
var_imp_aro_dp$dim <- var_imp_aro_ex$dim <- "Arousal - Ratings"
var_imp_aro_dp$condition <- "deadpan"
var_imp_aro_ex$condition <- "expressive"


var_imp_conditions <- do.call(
  rbind,
  list(
    var_imp_aro_dp,
    var_imp_aro_ex,
    var_imp_val_dp,
    var_imp_val_ex
  )
)


# add column controlling transparency of dots for coefficients of 0

var_imp_conditions$alpha <- 0
var_imp_conditions$alpha[var_imp_conditions$val != 0] <- 1


# add pretty names

var_imp_conditions$var <- dplyr::case_match(
  var_imp_conditions$var,
  "mode" ~ "Mode",
  "ambitus_range" ~ "Pitch Range",
  "pitchHeight" ~ "Pitch Height",
  "arPerf" ~ "Attack Rate",
  "brightness_mean" ~ "High Freq. Energy",
  "rms" ~ "Intensity",
  "rms_std" ~ "Intensity Variability"
)


var_imp_conditions$var <- factor(
  var_imp_conditions$var,
  levels = 
    rev(c(
      "Mode",
      "Attack Rate",
      "Pitch Height",
      "Pitch Range",
      "Intensity",
      "Intensity Variability",
      "High Freq. Energy"
    ))
)

plot_imp_valence_conditions <- plot_imp_ratings_grouped(
  subset(
    var_imp_conditions, 
    dim == "Valence - Ratings"
  )
)
plot_imp_arousal_conditions <- plot_imp_ratings_grouped(
  subset(
    var_imp_conditions, 
    dim == "Arousal - Ratings"
  )
)

# Scatterplots -----------------------------------------------------

# add composer columns to arousal df, valence df

df_arousal$composer <- df_full$composer
df_arousal$pieceID <- df_full$pieceID
df_valence$composer <- df_full$composer
df_valence$pieceID <- df_full$pieceID

df_arousal_plot <- df_arousal |> 
  dplyr::group_by(
    condition,
    composer, 
    pieceID
  ) |>
  dplyr::summarize(
    dplyr::across(
      c(
        arousal,
        predictors[predictors != "mode"]
      ),
      mean
    ),
    mode = unique(mode)
  )


df_valence_plot <- df_valence |> 
  dplyr::group_by(
    condition,
    composer, 
    pieceID
  ) |>
  dplyr::summarize(
    dplyr::across(
      c(
        valence,
        predictors[predictors != "mode"]
      ),
      mean
    ),
    mode = unique(mode)
  )


plots_arousal <- gridExtra::grid.arrange(
  plot_scatter(
    df_arousal_plot, 
    y = df_arousal_plot$arousal,
    x = df_arousal_plot$arPerf,
    legend.position = 'none'
  )+
    ggplot2::labs(
      x = "Attack Rate", 
      y = "Arousal"
    )+
    ggplot2::ylim(
      1,
      100
    ),
  plot_scatter(
    df_arousal_plot,
    y = df_arousal_plot$arousal,
    x = df_arousal_plot$pitchHeight,
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank()
  )+
    ggplot2::labs(
      x = "Pitch Height"
    )+
    ggplot2::ylim(1,100),
  # plot_scatter(
  #   df_arousal_plot, 
  #   y = df_arousal_plot$arousal,
  #   x = df_arousal_plot$ambitus_range,
  #   legend.position = 'none',
  #   axis.text.y = ggplot2::element_blank()
  # )+
  #   ggplot2::labs(
  #     x = "Pitch Range"
  #   )+
  #   ggplot2::ylim(1,100),
  plot_scatter(
    df_arousal_plot, 
    y = df_arousal_plot$arousal,
    x = df_arousal_plot$rms,
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank()
  ) +
    ggplot2::labs(
      x="Intensity"
    ) +
    ggplot2::ylim(
      1,
      100
    ),
  plot_scatter(
    df_arousal_plot, 
    y = df_arousal_plot$arousal,
    x = df_arousal_plot$rms_std,
    legend.position = 'none'
  )+
    ggplot2::labs(
      x = "Intensity\nVariability",
      y = "Arousal"
    )+
    ggplot2::ylim(1,100),
 plot_scatter(
   df_arousal_plot, 
   y = df_arousal_plot$arousal,
   x = df_arousal_plot$brightness_mean,
   legend.position = 'none',
   axis.text.y = ggplot2::element_blank()
 )+
   ggplot2::labs(
     x = "High Freq.\nEnergy"
   )+
   ggplot2::ylim(1,100),
  plot_imp_arousal_conditions,
 legend_edp,
  layout_matrix = matrix_scatter
)


plots_valence <- gridExtra::grid.arrange(
  ggplot2::ggplot(
    df_valence_plot, 
    ggplot2::aes(
      x = mode, 
      y = valence,
      group = condition,
      shape = condition,
      colour = condition
    )
  )+
    ggdist::stat_halfeye(
      position = ggplot2::position_dodge(width = 1.25)
    )+
    ggplot2::scale_shape_manual(values = c(expressive = 19, deadpan = 21))+
    ggplot2::scale_colour_manual(values = c(deadpan = 'purple4', expressive = 'darkgreen'))+
    ggplot2::theme_classic()+
    ggplot2::theme(
      legend.position = 'none'
    )+
    ggplot2::labs(
      x = "Mode", 
      y = "Valence"
    )+
    ggplot2::ylim(
      1,
      7
    )+
    ggplot2::scale_y_continuous(limits = c(1,7), breaks = 1:7),
  plot_scatter(
    df_valence_plot, 
    y = df_valence_plot$valence,
    x = df_valence_plot$arPerf,
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank()
  )+
    ggplot2::labs(
      x = "Attack Rate"
    )+
    ggplot2::ylim(
      1,
      7
    )+
    ggplot2::scale_y_continuous(limits = c(1,7), breaks = 1:7),
  plot_scatter(
    df_valence_plot, 
    y = df_valence_plot$valence,
    x = df_valence_plot$pitchHeight,
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank()
  ) +
    ggplot2::labs(
      x = "Pitch Height"
    )+
    ggplot2::ylim(
      1,
      7
    )+
    ggplot2::scale_y_continuous(limits = c(1,7), breaks = 1:7),
  # plot_scatter(
  #   df_valence_plot, 
  #   y = df_valence_plot$valence,
  #   x = df_valence_plot$ambitus_range,
  #   legend.position = 'none',
  #   axis.text.y = ggplot2::element_blank()
  # ) +
  #   ggplot2::labs(
  #     x = "Pitch Range"
  #   )+
  #   ggplot2::ylim(
  #     1,
  #     7
  #   ),
  plot_scatter(
    df_valence_plot, 
    y = df_valence_plot$valence,
    x = df_valence_plot$rms_std,
    legend.position = 'none',
    #axis.text.y = ggplot2::element_blank()
  ) +
    ggplot2::labs(
      x = "Intensity\nVariability",
      y = "Valence"
    )+
    ggplot2::ylim(
      1,
      7
    )+
    ggplot2::scale_y_continuous(limits = c(1,7), breaks = 1:7),
  plot_scatter(
    df_valence_plot, 
    y = df_valence_plot$valence,
    x = df_valence_plot$brightness_mean,
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank()
  ) +
    ggplot2::labs(
      x = "High Freq.\nEnergy"
    )+
    ggplot2::ylim(
      1,
      7
    )+
  ggplot2::scale_y_continuous(limits = c(1,7), breaks = 1:7),
  plot_imp_valence_conditions,
  legend_edp,
 layout_matrix = matrix_scatter
)

fig3 <- ggpubr::ggarrange(
  plots_valence
)

fig3 <- ggpubr::annotate_figure(
  fig3,
  top = ggpubr::text_grob(
    "Valence", 
    face = "bold", 
    size = 14
  )
)

fig4 <- ggpubr::ggarrange(
  plots_arousal
)

fig4 <- ggpubr::annotate_figure(
  fig4,
  top = ggpubr::text_grob(
    "Arousal", 
    face = "bold", 
    size = 14
  )
)

# fig3ab <- ggpubr::ggarrange(
#   fig3, 
#   fig4,
#   nrow = 2,
#   labels = "auto"
# )

ggplot2::ggsave(
  fig3,
  filename = paste0(
    here::here(), 
    '/img/fig3.tiff'
  ), 
  width = 7.5, 
  height = 4.5,
  bg = "white"
)
ggplot2::ggsave(
  fig4,
  filename = paste0(
    here::here(), 
    '/img/fig4.tiff'
  ), 
  width = 7.5, 
  height = 4.5,
  bg = "white"
)

# ggplot2::ggsave(
#   fig3ab,
#   filename = paste0(
#     here::here(), 
#     '/img/fig3ab.png'
#   ), 
#   width = 15, 
#   height = 9,
#   bg = "white"
# )
# 


