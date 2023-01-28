library(mami.codi.R)
library(magrittr)
library(dplyr)
library(purrr)
source('data/chords.R')

plot_consonance_dissonance_major_minor <- function(chords,chords_name,include_path=FALSE) {
  title = chords_name
  p=auditory_plot(chords,c('major_minor','consonance_dissonance'),
                  title=title,
                  include_path=include_path,
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance")
  save_auditory_plots(p,'results/plots')
}
plot_consonance_dissonance_major_minor(core_pitches_low_tonic(),'Intervals with Low Tonic')
plot_consonance_dissonance_major_minor(core_pitches_high_tonic(),'Intervals with High Tonic')
plot_consonance_dissonance_major_minor(
  dplyr::bind_rows(core_pitches_low_tonic(),core_pitches_high_tonic())
  ,'Intervals with High and Low Tonic')
plot_consonance_dissonance_major_minor(all_major_triads(),'All Major Triads')
plot_consonance_dissonance_major_minor(major_minor_triads(),'Major and Minor Triads')
plot_consonance_dissonance_major_minor(major_dual_minor_triads(),'Major and Dual Minor Triads')
plot_consonance_dissonance_major_minor(minor_dual_major_triads(),'Minor and Dual Major Triads')
plot_consonance_dissonance_major_minor(major_minor_dual_minor_triads(),'Major, Minor and Dual Minor Triads')
chords = dplyr::bind_rows(diatonic_scales())
plot_consonance_dissonance_major_minor(chords,'Diatonic Scales')
chords = dplyr::bind_rows(diatonic_triads())
plot_consonance_dissonance_major_minor(chords,'Diatonic Triads')
chords = dplyr::bind_rows(major_triad_progression(),minor_triad_progression())
plot_consonance_dissonance_major_minor(chords,'Progression: Major & Minor Triads')
chords = dplyr::bind_rows(major_triad_progression(),phrygian_triad_progression())
plot_consonance_dissonance_major_minor(chords,'Progression: Major & Phrygian Triads')
chords = dplyr::bind_rows(ionian_tonic_chords())
plot_consonance_dissonance_major_minor(chords,'Progression: Major Scale Chords')
