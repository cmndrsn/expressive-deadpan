
df_cues <- df_full |>
  dplyr::group_by(
    albumID,
    pieceID
  ) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        predictors[-1]
      ),
      mean
    ),
    mode = unique(mode)
)

df_cues$condition <- ifelse(
  stringr::str_detect(df_cues$albumID, "eadpan"),
  "deadpan",
  "expressive"
)

df_cues <- df_cues |> dplyr::select(
  albumID,
  pieceID,
  condition,
  mode,
  arPerf,
  rms,
  pitchHeight,
  ambitus_range,
  brightness_mean,
  rms_std
)


df_cues |>
  tidyr::pivot_longer(
    cols = 5:10
) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = value,
      colour = mode
    )
  ) +
  ggplot2::geom_freqpoly() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(name), 
    cols = ggplot2::vars(condition),
    scales = "free_x"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = "top"
  )
