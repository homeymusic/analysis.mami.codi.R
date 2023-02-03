source('code/setup.R')

plot_consonance_dissonance_major_minor(core_pitches_low_tonic_harmonics(),'Intervals with Harmonics and Low Tonic')
plot_consonance_dissonance_major_minor(core_pitches_high_tonic_harmonics(),'Intervals with Harmonics and High Tonic')
plot_consonance_dissonance_major_minor(
  dplyr::bind_rows(core_pitches_low_tonic_harmonics(),core_pitches_high_tonic_harmonics())
  ,'Intervals with Harmonics and High and Low Tonic')

# code to create the data
#
# two_octaves.11harmonics.hertz = -1200:1200 %>% lapply(function(cents) {
#   auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 11L)
# }) %>% bind_rows
# saveRDS(two_octaves.hertz,file='data/two_octaves.11harmonics.hertz.RDS')

two_octaves.11harmonics.hertz <- readRDS('data/two_octaves.11harmonics.hertz.RDS')

p = auditory_plot(two_octaves.11harmonics.hertz,c('major_minor','consonance_dissonance'),
                  title='Frequency Two Octaves',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance",
                  include_text=FALSE)
save_auditory_plots(p,'results/plots')
p

plot(two_octaves.11harmonics.hertz$pitch.hertz,
     two_octaves.11harmonics.hertz$consonance_dissonance,
     log='x', pch=20)

two_octaves.11harmonics.semitones = -12:12 %>% lapply(function(semitone) {
  a(semitone, num_harmonics = 11L)
}) %>% bind_rows

points(two_octaves.11harmonics.semitones$pitch.hertz,
       two_octaves.11harmonics.semitones$consonance_dissonance,
       pch=5, col='darkgrey')

text(two_octaves.11harmonics.semitones$pitch.hertz,
     two_octaves.11harmonics.semitones$consonance_dissonance,
     two_octaves.11harmonics.semitones$name.semitones,
     pos=3, col='darkgrey', family='Arial Unicode MS')

plot(two_octaves.11harmonics.semitones$pitch.hertz,
       two_octaves.11harmonics.semitones$consonance_dissonance,
       pch=20, log='x')

text(two_octaves.11harmonics.semitones$pitch.hertz,
     two_octaves.11harmonics.semitones$consonance_dissonance,
     two_octaves.11harmonics.semitones$name.semitones,
     pos=3, family='Arial Unicode MS')


# stretched harmonics
two_octaves.11harmonics.stretch.semitones = -12:12 %>% lapply(function(semitone) {
  a(semitone, num_harmonics = 11L, harmonics_coefficient = 1.1)
}) %>% bind_rows

plot(two_octaves.11harmonics.stretch.semitones$pitch.hertz,
     two_octaves.11harmonics.stretch.semitones$consonance.low,
     pch=20, log='x')

text(two_octaves.11harmonics.stretch.semitones$pitch.hertz,
     two_octaves.11harmonics.stretch.semitones$consonance.low,
     two_octaves.11harmonics.stretch.semitones$name.semitones,
     pos=3, family='Arial Unicode MS')

plot(two_octaves.11harmonics.stretch.semitones$pitch.hertz,
     two_octaves.11harmonics.stretch.semitones$consonance_dissonance,
     pch=20, log='x')

text(two_octaves.11harmonics.stretch.semitones$pitch.hertz,
     two_octaves.11harmonics.stretch.semitones$consonance_dissonance,
     two_octaves.11harmonics.stretch.semitones$name.semitones,
     pos=3, family='Arial Unicode MS')
