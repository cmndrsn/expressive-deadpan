
# Data read-in ----------------------------------------------------

# read in analyzed features

df_extracted_features <- read.csv(
  "data/extracted-features.csv"
)

# read in annotations

df_annotations <- read.csv(
  "data/annotations.csv"
)

# read equated

df_equated_features <- read.csv(
  'data/equated-features.csv'
)



# add identify pieces based on file names

df_extracted_features$pieceID <- paste0(
  substr(df_extracted_features$mode, 1, 1), 
  df_extracted_features$chroma
)

# simplify naming 

df_extracted_features$summary <- paste0(
  df_extracted_features$variable, "_", 
  df_extracted_features$stat_name
)

# remove unnecessary columns

df_extracted_features <- dplyr::select(
  df_extracted_features,
  -c("X", "chroma", "mode", "stat_name", "variable", "performer")
)

# convert to wide format

df_features <- tidyr::pivot_wider(
  data = df_extracted_features,
  names_from = summary,
  values_from = value
)

# scale brightness and rms SD columns

df_features$brightness_mean <- scale(df_features$brightness_mean)
df_features$rms_std <- scale(df_features$rms_std)

remove(df_extracted_features)

# Merge in brightness difference between conditions ---------------

# subset deadpan ratings based on albumID

cues_dp <- subset(
  df_features, 
  stringr::str_detect(
    df_features$albumID, 
    "eadpan"
  )
)

# subset interpretive
cues_ep <- subset(
  df_features, 
  !stringr::str_detect(
    df_features$albumID, 
    "eadpan"
  )
)

# match ordering of dataframes
cues_dp <- cues_dp[order(cues_dp$composer, cues_dp$pieceID),]
cues_ep <- cues_ep[order(cues_ep$composer, cues_ep$pieceID),]
# verify they match
all(
  paste0(cues_dp$composer, cues_dp$pieceID) == 
    paste0(cues_ep$composer, cues_ep$pieceID)
)
# calculate absolute difference in brightness
cues_dp$brightness_diff <- abs(cues_dp$brightness_mean - cues_ep$brightness_mean)
# calculate absolute difference in rms sd
cues_dp$rms_std_diff <- abs(cues_dp$rms_std - cues_ep$rms_std)

## select information needed for merge

df_cue_diffs <- cues_dp |> 
  dplyr::select(
    composer,
    pieceID,
    brightness_diff,
    rms_std_diff
)

# join brightness and rms differences with other features

df_features <- dplyr::left_join(
  df_features,
  df_cue_diffs,
  by = dplyr::join_by(
    composer,
    pieceID
  )
)

remove(
  df_cue_diffs,
  cues_dp,
  cues_ep
)

df_features <- dplyr::left_join(
  df_features,
  df_equated_features,
  by = dplyr::join_by(
    composer,
    set,
    albumID,
    pieceID
  )
)

remove(
  df_equated_features
)

# more detail than needed for merge

df_full <- dplyr::left_join(
  df_annotations,
  df_features,
  by = dplyr::join_by(
    condition,
    expID,
    composer,
    set,
    albumID,
    pieceID,
    key,
    chroma
  )
)

remove(
  df_annotations,
  df_features
)




