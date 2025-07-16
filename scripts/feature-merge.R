# Load in perceptual data -----------------------------------------

ratings <- readr::read_csv("data/equated-features.csv")

# Load librosa analyses -----------------------------------------

librosa <- readr::read_csv("qjep/data/librosa/audio_features.csv")
librosa[,"...1"] <- NULL

# Load and format MIRToolbox analyses --------------------------------------------

mirtoolbox <- lapply(
  list.files(
    "qjep/data/mirtoolbox", 
    pattern = "*.csv"),
  function(x) {
    read.csv(
      paste0(
        "qjep/data/mirtoolbox/",
        x
      )
    )  
  }
)

mirtoolbox <- do.call(rbind, mirtoolbox)

# make names consistent with librosa file

mirtoolbox$stat_name <- "mean"
mirtoolbox$variable <- "brightness"
mirtoolbox <- dplyr::rename(
  mirtoolbox,
  value = "brightness"
)

# Prepare audio features ------------------------------------------

# bind together analyses

audio_features <- dplyr::bind_rows(
  librosa,
  mirtoolbox
)

# remove filepath characters

audio_features$file <- stringr::str_remove_all(
  audio_features$file,
  "audio/|chopin-1/|chopin-1-dp/|bach-1/|bach-1-dp/|.wav"
)

# generate columns from filename

audio_features <- tidyr::separate_wider_delim(
  audio_features, 
  cols = "file",
  names = c("composer", "set", "performer", "chroma", "mode", "albumID"),
  delim = "_"
)

audio_features$performer <- dplyr::first(audio_features$performer)
  
# Format Music21 analyses -----------------------------------------

# load midi analyses

midi_features <- readr::read_csv("qjep/data/music21/midi_analysis.csv")
midi_features[,"...1"] <- NULL

# make names consistent with librosa file

midi_features$stat_name <- "range"
midi_features$variable <- "ambitus"
midi_features <- dplyr::rename(
  midi_features,
  value = "ambitus"
)

# remove filepath characters

midi_features$file <- stringr::str_remove_all(
  midi_features$file,
  "mid/|chopin-1/|chopin-1-dp/|bach-1/|bach-1-dp/|.mid"
)

# generate columns from filename

midi_features <- tidyr::separate_wider_delim(
  midi_features, 
  cols = "file",
  names = c("composer", "set", "performer", "chroma", "mode", "albumID"),
  delim = "_"
)

# fill in missing information from filenames

midi_features$performer <- "ashkenazy"
midi_features$composer <- tolower(midi_features$composer)
midi_features[midi_features$composer == "bach",]$albumID <- "bachAshkenazy2006"
midi_features[midi_features$composer == "chopin",]$albumID <- "chopinAshkenazy1992"

# duplicate ambitus for both composers (same for interpretive & deadpan files)

midi_features_2 <- midi_features
midi_features_2$albumID <- paste0(
  midi_features_2$albumID,
  "Deadpan"
)

midi_features <- rbind(
  midi_features,
  midi_features_2
)
remove(midi_features_2)


# Bind audio and analysis features --------------------------------

analyzed_features <- dplyr::bind_rows(
  audio_features,
  midi_features
)

analyzed_features$composer <- tolower(analyzed_features$composer)
analyzed_features$performer <- tolower(analyzed_features$performer)


# Output resulting CSV --------------------------------------------

write.csv(
  analyzed_features,
  file = "Data/feature_analysis.csv"
)


