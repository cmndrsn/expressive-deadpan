
# Functions ---------------------------------------------------------------

# for calculating percent differences and changes....
.getCoefficientofVariation <- function(X) sd(X)/mean(X)

prettyKeyCol <- function(dat)
{ 
  # isolate letter name:
  keyChroma <- substr(
    dat,
    1,
    1
  )
  # isolate sharp and flat signs
  keyAccidental <- substr(
    dat,
    2,
    2
  )
  # replace "b" with proper flat symbol
  keyAccidental[keyAccidental == "b"] <- eval(
    parse(
      text=paste0(
        "'", 
        flatSym, 
        "'"
      )
    )
  )
  # replace "#" with proper sharp symbol
  keyAccidental[keyAccidental == "#"] <- eval(
    parse(
      text=paste0(
        "'", 
        sharpSym, 
        "'"
      )
    )
  )
  # create fancy key signature
  fancyKey <- paste0(
    keyChroma, 
    keyAccidental
  )
  # replace original key column with fancy key:
  return(fancyKey)
}

#internal function for assigning key signature based on piece ID

nameKeysSimple <- function(
    pieceID, 
    sortMode = TRUE
){
  pieceID <- as.character(pieceID)
  key <- as.factor(pieceID)
  key <- plyr::revalue(key, 
    c(
      M0 = "C", m0 = "c", M1 = "C#", m1 = "c#", 
      M2 = "D", m2 = "d",M3 = "Eb", m3 = "eb", 
      M4 = "E", m4 = "e", M5 = "F", m5 = "f",
      M6 = "F#", m6 = "f#", M7 = "G", m7 = "g", 
      M8 = "Ab", m8 = "g#", M9 = "A", m9 = "a", 
      M10 = "Bb", m10 = "bb", M11 = "B", m11 = "b"
      )
    )
  if(sortMode)
  {
    major <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 
              'F#', 'G', 'Ab', 'A', 'Bb', 'B')
    minor <- c('c', 'c#', 'd', 'eb', 'e', 'f', 
              'f#', 'g', 'g#', 'a', 'bb', 'b')
    key <- factor(key, levels = c(major, minor))
  }
  
  return(key)
}

# Paired Circumplex (Fig 2) -----------------------------------------------

pairedCircumplex <- function(
    dat, 
    groupCol = expID, 
    chosenLvls, 
    style = 'mp' 
){
  # I am making a copy of the groupCol argument because subsetting is difficult
  # with bracket notation
  dat$temp <- dat[,groupCol]
  names(dat)[ncol(dat)] = "groupCol"
  
  dat |> 
    dplyr::group_by(groupCol, mode, key) |> 
    dplyr::summarize(valence = mean(valence),
              arousal = mean(arousal)) -> dat

    dat$valence <- dat$valence/7
    dat$arousal <- dat$arousal/100
  
  message("Using point for ", chosenLvls[2], "\n and text for ", chosenLvls[1])
  
  circumPlot <- dat |> 
    ggplot2::ggplot(ggplot2::aes(valence,arousal, color=mode, label = key))+ 
    ggplot2::geom_hline(yintercept = (50.5/100))+
    ggplot2::geom_vline(xintercept = (4/7))+
    ggplot2::theme_classic()+
    ggplot2::labs(x="Valence",y="Arousal")+
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()) + 
      ggplot2::geom_line(
        ggplot2::aes(
          group = key, 
          color=mode
        ), 
        size = 1, 
        alpha = 0.4, 
        linetype = 1
      ) +
      ggplot2::geom_point(
        data = subset(dat, groupCol == chosenLvls[1]), shape = 16)+
      ggplot2::geom_point(
        data = subset(dat, groupCol == chosenLvls[2]), shape = 21, fill = 'white') +
      ggplot2::scale_colour_manual(values = c("firebrick", "dodgerblue1"))
}

# Bootstrap Differences ---------------------------------------------------

# Step 1: For each condition, randomly sample 30 ratings (with replacement) for 
## a given piece. Calculate mean absolute difference for valence and arousal, 
## along with Euclidean distance 
# Step 2: After calculating summary statistics on individual pieces, ungroup
## data and calculate coefficient of variation across all sampled pieces.
## Steps 1 and 2 give us the results of 1 virtual experiment.
# Step 3: Repeat this process for 10,000 virtual experiments
# Step 4: Calculate bootstrap confidence intervals for all 10,000 replications.

.randomDifferenceRating <- function(
    pieceToSample, 
    DF1, 
    DF2, 
    sampleSize = 30
){
  # we need fixed index numbers to sample the rows corresponding to pieceID of interest
  DF1$index = 1:nrow(DF1)
  DF2$index = 1:nrow(DF2)
  
  # first, randomly sample one row number in first dataframe where pieceID is pieceID of interest
  thisRowDF1 = sample(as.numeric(DF1$index[DF1$pieceID == pieceToSample]), size = sampleSize, replace = T)
  # now do same for second dataframe
  thisRowDF2 = sample(as.numeric(DF2$index[DF2$pieceID == pieceToSample]), size = sampleSize, replace = T)
  # show this row in dataframe 1
  rowDF1 = DF1[thisRowDF1,]
  #print(rowDF1)
  rowDF2 = DF2[thisRowDF2,]
  #print(rowDF2)
  # now take squared difference of valence and arousal between df1 and df2
  
  valenceA <- mean(as.numeric(rowDF1$valence))
  valenceB <- mean(as.numeric(rowDF2$valence))
  arousalA <- mean(as.numeric(rowDF1$arousal))
  arousalB <- mean(as.numeric(rowDF2$arousal))
  valenceDiff <- (mean(as.numeric(rowDF1$valence)) - 
                    mean(as.numeric(rowDF2$valence)))
  arousalDiff <- (mean(as.numeric(rowDF1$arousal)) - 
                    mean(as.numeric(rowDF2$arousal)))

  # return values as dataframe
  
  returnValues <- data.frame(
    valenceDiff, 
    arousalDiff, 
    pieceID = pieceToSample,
    valenceA, 
    valenceB, 
    arousalA, 
    arousalB
  )
  
  return(returnValues)
}

.collapseSampleDifferenceArray <- function(dat)
{
  dat <- dat |> 
    dplyr::group_by(pieceID) |> 
    dplyr::summarize(
      valenceDiff = mean(valenceDiff),
      arousalDiff = mean(arousalDiff),
      valenceA = valenceA,
      valenceB = valenceB,
      arousalA = arousalA,
      arousalB = arousalB
    )
  dat <- dat |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      valenceA_cv = .getCoefficientofVariation(valenceA),
      valenceB_cv = .getCoefficientofVariation(valenceB),
      arousalA_cv = .getCoefficientofVariation(arousalA),
      arousalB_cv = .getCoefficientofVariation(arousalB)
    )
  return(dat)
}

# applies .randomDifferenceRating across a vector of randomly sampled (with replacement) pieceIDs 
.sampleDifferenceArray <- function(
    DF1, 
    DF2, 
    collapseRatings = T, 
    sampleSize = 30
  ){
  pieceSample <- unique(as.character(DF1$pieceID))
  
  # apply function across a vector of randomly sampled piece IDs called "pieceSample"
  
  differenceSample <- sapply(
    pieceSample, 
    function(x){#message("sampling ",x); 
      .randomDifferenceRating(
        pieceToSample = as.character(x), 
        DF1 = DF1, 
        DF2 = DF2, 
        sampleSize = sampleSize)},
    simplify = F)
  
  differenceSample <- purrr::map_df(
    differenceSample, 
    ~as.data.frame(.x), 
    .id="pieceID"
  )
  # transpose it for dataframe formatting
  
  if(collapseRatings) return(
    .collapseSampleDifferenceArray(differenceSample)
  )
  # return formatted dataframe
  else return(differenceSample)
}

bootstrapCircumplexDifference <- function(
  DF1, 
  DF2, 
  replications = 1000,
  returnSummary = FALSE,
  style = 'mp', 
  scaleVars = TRUE,
  sampleSize = 30,
  seed = 1
) {
  # for reproducibility:
  set.seed(seed)
  # first scale and center data
  if(scaleVars)
  {
    # this is to scale data across groups: add column so we can subset them later
    DF1$tempID <- 'a'
    DF2$tempID <- 'b'
    boundDat <- rbind(DF1, DF2)
    boundDat$valence <- as.numeric(
      scale(
        boundDat$valence
        )
    )
    boundDat$arousal <- as.numeric(
      scale(
        boundDat$arousal
        )
    )
    DF1 <- subset(
      boundDat, 
      tempID == 'a'
    ) 
    DF1$tempID <- NULL
    DF2 <- subset(
      boundDat, 
      tempID == 'b'
    ) 
    DF2$tempID <- NULL
    
    message("Scaled valence and arousal values")
  }
  message(
    "Performing bootstrap distance calculations with ", 
    replications, 
    " replications"
  )
  # apply .sampleDifferenceArray function to pieceSample and repeat this process 1000 times
  bootFrame <- pbapply::pbreplicate(
    n = replications, 
    expr = .sampleDifferenceArray(
      DF1 = DF1, 
      DF2 = DF2, 
      collapseRatings = TRUE,
      sampleSize = sampleSize
      ), 
    simplify = F
    )
  #format as dataframe
  bootFrame <- do.call('rbind', bootFrame)
  
  
  # summarize data:
  bootSumm <- bootFrame |> 
    dplyr::group_by(pieceID) |> 
    dplyr::summarize(
      valenceDist = mean(valenceDiff),
      arousalDist = mean(arousalDiff),
      valenceLCI = quantile(valenceDiff, 0.025),
      valenceUCI = quantile(valenceDiff, 0.975),
      arousalLCI = quantile(arousalDiff, 0.025),
      arousalUCI = quantile(arousalDiff, 0.975),
      valenceSig = sign(valenceLCI) == sign(valenceUCI),
      arousalSig = sign(arousalLCI) == sign(arousalUCI),
      valenceA = mean(valenceA),
      valenceB = mean(valenceB),
      arousalA = mean(arousalA),
      arousalB = mean(arousalB)
      ) 
  # create variable for mode
  bootSumm$mode <- substr(
    bootSumm$pieceID, 
    1, 
    1
  )
  # rename mode to "Major" and "minor"
  bootSumm$mode <- forcats::fct_recode(
    bootSumm$mode, 
    Major = 'M', 
    minor = 'm'
  )
  
  bootFrame <- bootFrame |> 
    dplyr::mutate(
      valenceDiffAbs = abs(valenceDiff),
      arousalDiffAbs = abs(arousalDiff)
    )
  if(returnSummary) {
    return(bootSumm)
  } else {
    return(bootFrame)
  }
}

summarizeBootCircumplexDiffs <- function(bootData) {
  bootSumm <- bootData |> 
    dplyr::group_by(pieceID) |> 
    dplyr::summarize(
      valenceDist = mean(valenceDiff),
      arousalDist = mean(arousalDiff),
      valenceLCI = quantile(valenceDiff, 0.025),
      valenceUCI = quantile(valenceDiff, 0.975),
      arousalLCI = quantile(arousalDiff, 0.025),
      arousalUCI = quantile(arousalDiff, 0.975),
      valenceSig = sign(valenceLCI) == sign(valenceUCI),
      arousalSig = sign(arousalLCI) == sign(arousalUCI)
    ) 
  return(bootSumm)
}

# Elastic Net Functions -------------------------------------------

elastic_selection <- function(
    formula,
    data, # dataframe
    folds=10, # use 10-fold cross-validation
    alpha=0.5, # lasso regression
    ...
){
  cv_glm <- glmnetUtils::cv.glmnet(
    formula = formula,
    data = data,
    type.measure = 'deviance',
    nfolds = folds,
    alpha = alpha,
    ...
  )
  model_coefs <- as.matrix(coef(cv_glm))
  included_vars <- which(model_coefs != 0)
  selected_variables <- row.names(model_coefs)[included_vars]
  selected_variables <- selected_variables[
    !(
      selected_variables %in% '(Intercept)'
    )
  ]
  excluded_vars <- model_coefs[which(model_coefs == 0)]
  dropped_variables <- which(model_coefs == 0)
  dropped_variables <- dropped_variables[
    !(
      dropped_variables %in% '(Intercept)'
    )
  ]
  R2 <- cv_glm$glmnet.fit$dev.ratio[
    which(cv_glm$glmnet.fit$lambda == cv_glm$lambda.1se)
  ] 
  message("Vars selected: ", selected_variables)
  message("\nVars dropped:" , dropped_variables)
  message("R^2: ", R2)
  return(cv_glm);
}

# functions to calculate and plot variable importance

get_var_imp <- function(
    x, 
    convert_mode = TRUE
) {
  coefs <- coef(x) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column('var')
  names(coefs) <- c("var", "val")
  # remove intercept term
  coefs <- coefs[coefs$var != "(Intercept)",]
  coefs$val <- abs(coefs$val)
  if(convert_mode) {
    numeric_vars <- coefs[!stringr::str_detect(coefs$var, 'mode'),]
    # convert major and minor to one value
    mode_val <- mean(coefs[stringr::str_detect(
      coefs$var, 
      'mode'
    ),]$val)
    coefs <- rbind(
      numeric_vars, 
      data.frame(
        var='mode', 
        val=mode_val
      )
    )
    coefs$val <- as.numeric(coefs$val)
  }
  # perform min-max transform
  coefs$val <- (coefs$val - min(coefs$val))/
    (max(coefs$val) - min(coefs$val)) * 100
  coefs$var <- factor(
    coefs$var, 
    levels = rev(
      sort(
        as.character(
          coefs$var
        )
      )
    )
  )
  return(coefs)
}

plot_imp_ratings <- function(var_imp) {
  ggplot2::ggplot(
    data = var_imp,
    ggplot2::aes(
      x = abs(val),
      y = var,
      xmin = 0,
      xmax = abs(val),
      alpha = alpha,
      colour = condition
    ) 
  ) +
  ggplot2::geom_vline(xintercept = 0)+
  ggplot2::geom_linerange(position = ggplot2::position_dodge(width = 0.2))+
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.2)) +
  ggplot2::theme_classic()+
  ggplot2::scale_colour_manual(c(deadpan = 'purple4', expressive = 'darkgreen'))+
  ggplot2::facet_wrap(
    facets = ggplot2::vars(
      dim
    )
  ) +
  ggplot2::theme(legend.position = 'none')+
  ggplot2::labs(x = "Importance", y = "Variable")
}

plot_imp_ratings_grouped <- function(var_imp) {
  var_imp$label <- "Cue"
  ggplot2::ggplot(
    data = var_imp,
    ggplot2::aes(
      x = abs(val),
      y = var,
      xmin = 0,
      xmax = abs(val),
      alpha = alpha,
      shape = condition,
      colour = condition,
      fill = condition
    ) 
  ) +
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::geom_linerange(position = ggplot2::position_dodge(width = 0.2))+
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.2)) +
    ggplot2::scale_shape_manual(values = c(21,19))+
    ggplot2::scale_fill_manual(values = c("white", "white"))+
    ggplot2::scale_colour_manual(values = c(deadpan = 'purple4', expressive = 'darkgreen'))+
    ggplot2::theme_classic()+
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        label
      )
    ) +
    ggplot2::theme(legend.position = 'none')+
    ggplot2::labs(x = "Importance", y = "Variable")
}



# cue importance scatter plots ------------------------------------

plot_scatter <- function(
    dat, 
    x, 
    y, 
    xLab = ' ', 
    yLab = '', 
    ...
) {
  ggplot2::ggplot(
    dat, 
    ggplot2::aes(
      x = x,
      y = y,
      colour = condition
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = condition,
        fill = condition,
        colour = condition
      )
    )+
    ggplot2::geom_smooth(
      method = 'lm',
      se = F
    ) +
    ggplot2::theme_classic()+
    ggplot2::labs(
      x = xLab, 
      y = ''
    ) +
    ggplot2::scale_shape_manual(values = c(deadpan = 21, expressive = 19))+
    ggplot2::scale_fill_manual(values = c(deadpan = "white", expressive = NA))+
    ggplot2::scale_colour_manual(values = c(deadpan = 'purple4', expressive = 'darkgreen'))+
    ggplot2::theme(...) -> p
  
  return(p)
}
