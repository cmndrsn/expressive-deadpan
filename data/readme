## Annotations

`annotations.csv` contains the valence and arousal annotations of participants from each of the four experiments (two expressive, two deadpan).

`bootstrap...RData`: RData file containing results from bootstrap simulation conducted for examining significant differences in valence and arousal across analyzed excerpts.

## Features
`equated-features.csv` is a dataset of features calculated manually from encodings of musical scores or audio files. 
Attack rate (`arPerf`) is calculated by counting the number of note onsets in the first eight measures of a prelude performance (simultaneous onsets counting as one attack), and dividing this by the duration of the performed excerpt (excluding the two-second fade). 
Pitch height (`pitchHeight`) summarizes the average pitch level of 8-measure excerpts, encoded manually using the keyboard number of each pitch.
Mode (`mode`) is simply the declared major or minor mode of an excerpt and is encoded based on the title/key signature of the prelude.
RMS (`rms`) (calculated with Audacity) measures the average intensity in decibels. Deadpan excerpts are matched in average RMS with the original recordings using the RMS Normalize plug-in for Audacity.

`extracted-features.csv` is a dataset of features extracted from the prepared audio files, extracted using librosa and music21 Python libraries, and the mirtoolbox Matlab library. This set of MIR features was calculated for broader use in the Emotional Piano Project. These features are estimated directly from the audio, using the duration of audio excerpts excluding the final two seconds (to account for fadeout). The functions are calculated using default parameters (see the Librosa documentation for additional details). Mean, median, and standard deviation of the following Librosa features are summarized (where applicable): spectral centroid, zero-crossing rate, high-frequency energy (estimated manually from STFT coefficients),  RMS, tempo, and onset strength.
From MIRToolbox, we calculated high-frequency energy (`brightness_mean`) using a 3000Hz cut-off for consistency with Quinto & Thompson's (2013) study. 
We extracted the ambitus (i.e., pitch range) from MIDI files using music21.


From this larger set of features, the QJEP manuscript considers attack rate (`arPerf`), pitch height (`pitchHeight`), mode (`mode`), and intensity (`rms`) from `equated-features.csv`, and intensity variability (`rms_std`; Librosa feature), pitch range (`ambitus_range`; music21 feature), and high-frequency energy  (`brightness_mean`; mirtoolbox feature) from `extracted-features.csv`.


