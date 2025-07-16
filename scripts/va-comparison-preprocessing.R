# Pre-processing ----------------------------------------------------------

# Note: this file is sourced in va-comparison.qmd. To reproduce
# analyses, it is better to open that file. 

# Set seed for simulations

set.seed(123)

# For writing pretty key signatures ---------------------------------------

flatSym <- '\\u266d'
sharpSym <- '\\u266f'

# Measure differences between conditions ----------------------------------

load(paste0(here::here(), '/data/bootstrap-2024-10-27.RData'))

bootData |>
  dplyr::group_by(pieceID) |>
  dplyr::summarize(valenceDiffMean = mean(valenceDiff),
            valenceDiffLCI = quantile(valenceDiff, 0.025),
            valenceDiffUCI = quantile(valenceDiff, 0.975),
            arousalDiffMean = mean(arousalDiff),
            arousalDiffLCI = quantile(arousalDiff, 0.025),
            arousalDiffUCI = quantile(arousalDiff, 0.975)
  ) -> bootPlotDF


bootPlotDF$valenceDiffSig <- sign(bootPlotDF$valenceDiffUCI) == sign(bootPlotDF$valenceDiffLCI)
bootPlotDF$arousalDiffSig <- sign(bootPlotDF$arousalDiffUCI) == sign(bootPlotDF$arousalDiffLCI)
bootPlotDF$mode <- substr(bootPlotDF$pieceID, 1, 1)
bootPlotDF$composer <- substr(
  bootPlotDF$pieceID, 
  nchar(bootPlotDF$pieceID), 
  nchar(bootPlotDF$pieceID)
)
bootPlotDF$mode <- as.factor(bootPlotDF$mode)
levels(bootPlotDF$mode) <- c('Minor', "Major")
bootPlotDF$composer <- as.factor(bootPlotDF$composer)
levels(bootPlotDF$composer) <- c('Bach', "Chopin")



# Data assembly -----------------------------------------------------------
# Take expressive and deadpan experiments from full data set:

emoData <- read.csv(paste0(here::here(), '/data/annotations.csv'))
encoded_features <- read.csv(paste0(here::here(), '/data/equated-features.csv'))
# load dependencies & functions, analyses
emoData <- dplyr::left_join(emoData, encoded_features)

bach <- subset(emoData, expID %in% c(141, 142))
chop <- subset(emoData, expID %in% c(155, 137))

bach$key <- prettyKeyCol(bach$key)
chop$key <- prettyKeyCol(chop$key)

# remove this if errors produced (calls C# piece Db for consistency with score)

## Chopin:

chopE <- subset(chop, expID == '155')

chopD <- subset(chop, expID == '137')

## Bach:

bachE <- subset(bach, expID == '141')

bachD <- subset(bach, expID == '142')

# collect deadpan ratings
deadpan <- rbind(chopD, bachD)
# collect expressive ratings
expressive <- rbind(chopE, bachE)
# collect chopin ratings
chopConditions <- rbind(chopD, chopE)
# collect bach ratings
bachConds <- rbind(bachD, bachE)
# collect all ratings
fullDat <- rbind(deadpan, expressive)
# replace participant IDs with unique values
pptUnique <- paste0(
  fullDat$participant, 
  substr(
    fullDat$composer,
    1,
    1
   ), 
  substr(
    fullDat$condition,
    1,
    1
  )
)

fullDat$participant <- pptUnique



# Bootstrap Significance Plots --------------------------------------------

# make duplicate for plot
panel2Plot <- data.frame(bootPlotDF)
# name key signature
panel2Plot$key <- nameKeysSimple(
  substr(
    panel2Plot$pieceID, 
    1, 
    nchar(panel2Plot$pieceID)-1
  )
)

# replace c# with c#/db for panel2plot

panel2Plot$key <- as.factor(
  prettyKeyCol(panel2Plot$key)
)

# recode C# key to Db in pretty key names

levels(
  panel2Plot$key
  )[
  which(
    levels(
      panel2Plot$key
      ) == eval(
      parse(
        text = paste0(
          "'", 
          'C', 
          sharpSym, 
          "'"
        )
      )
    )
  )
] <- 
  eval(
    parse(
      text=paste0(
        "'", 
        'C', 
        sharpSym, 
        '/D',  
        flatSym, 
        "'"
      )
    )
  )

# initialize column to colour mode
panel2Plot$modeColVal <- 'n'
# now distinguish significant colours for valence
panel2Plot$modeColVal[panel2Plot$valenceDiffSig == T] <- 
  panel2Plot[panel2Plot$valenceDiffSig == T,]$mode
panel2Plot$modeColAro <- 'n'
# and arousal
panel2Plot$modeColAro[panel2Plot$arousalDiffSig == T] <- 
  panel2Plot[panel2Plot$arousalDiffSig == T,]$mode

bs1 <- subset(panel2Plot) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = valenceDiffMean,
      xmin = valenceDiffLCI, 
      xmax = valenceDiffUCI,
      y = key, 
      label = key,
      alpha = valenceDiffSig,
      color = modeColVal
      )
  ) +
  ggplot2::geom_vline(
    xintercept = 0, 
    colour = 'grey', 
    linetype= 2
  ) +
  ggplot2::geom_errorbarh(
    position = ggplot2::position_dodge(
      width = 0.5)
  )+
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.5
    ), 
    ggplot2::aes(
      shape = composer
    )
  )+
  ggplot2::theme_classic() +
  ggplot2::labs(
    x = 'Difference', 
    y = 'Piece'
  )+
  ggplot2::scale_colour_manual(
    values = c(
      'dodgerblue', 
      'firebrick2', 
      'grey'
    ),
    name = 'Mode', 
    labels = c(
      'Major', 
      'Minor', 
      "")
    ) +
  ggplot2::scale_alpha_manual(
    values = c(
      0.6, 
      1
    ), 
    name = 'Difference', 
    labels = c(
      'Nonsignificant', 
      'Significant')
    ) +
  ggplot2::scale_shape_manual(
    values = c(
      16,
      17
    ), 
    name = 'Composer'
  ) +
  ggplot2::theme(
    legend.position = 'none'
  )+
  ggplot2::facet_grid(
    cols = ggplot2::vars(composer)
)

bs2 <- subset(panel2Plot) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = arousalDiffMean,
      y = key, 
      label = key,
      xmin = arousalDiffLCI, 
      xmax = arousalDiffUCI,
      alpha = arousalDiffSig,
      color = modeColAro
      )
  )+
  ggplot2::geom_vline(
    xintercept = 0, 
    colour = 'grey', 
    linetype= 2
  )+
  ggplot2::geom_errorbarh(
    position = ggplot2::position_dodge(
      width = 0.5
    )
  )+
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.5
    ), 
    ggplot2::aes(
      shape = composer
    )
  )+
  ggplot2::theme_classic()+
  ggplot2::labs(
    x = 'Difference', 
    y = 'Piece'
  )+
  ggplot2::scale_colour_manual(
    values = c(
      'dodgerblue', 
      'firebrick2', 
      'grey'
    ),
    name = 'Mode', 
    labels = c(
      'Major', 
      'Minor', 
      ""
    )
  )+
  ggplot2::scale_alpha_manual(
    values = c(
      0.6, 
      1
    ), 
    name = 'Difference', 
    labels = c(
      'Nonsignificant', 
      'Significant'
    )
  )+
  ggplot2::scale_shape_manual(
    values = c(
      16,
      17
    ), 
    name = 'Composer'
  )+
  ggplot2::theme(
    legend.position = 'none'
  )+
  ggplot2::scale_y_discrete(
    position = 'right'
  )+
  ggplot2::facet_grid(
    cols = ggplot2::vars(
      composer
    )
)





