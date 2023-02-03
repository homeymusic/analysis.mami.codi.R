source('code/setup.R')

# auditory model
plot_consonance_dissonance_major_minor(core_pitches_low_tonic(),'Intervals with Low Tonic')
plot_consonance_dissonance_major_minor(core_pitches_high_tonic(),'Intervals with High Tonic')
plot_consonance_dissonance_major_minor(
  dplyr::bind_rows(core_pitches_low_tonic(),core_pitches_high_tonic())
  ,'Intervals with High and Low Tonic')

