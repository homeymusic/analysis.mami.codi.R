source('code/setup.R')

plot_consonance_dissonance_major_minor(core_pitches_low_tonic_harmonics(),'Intervals with Harmonics and Low Tonic')
plot_consonance_dissonance_major_minor(core_pitches_high_tonic_harmonics(),'Intervals with Harmonics and High Tonic')
plot_consonance_dissonance_major_minor(
  dplyr::bind_rows(core_pitches_low_tonic_harmonics(),core_pitches_high_tonic_harmonics())
  ,'Intervals with Harmonics and High and Low Tonic')

