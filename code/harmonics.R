source('code/setup.R')

for (h in 0:12) {
  plot_consonance_dissonance_major_minor(
    core_pitches(num_harmonics=h), paste('Intervals Low Tonic',h,'Harmonics'))
}

fifteen_semitones.0harmonics.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200))
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.0harmonics.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 0 Harmonics',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

# 5 harmonics

fifteen_semitones.5harmonics.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 5)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.5harmonics.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 5 Harmonics',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

fifteen_semitones.5stretched.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 5,
                 stretching = 2.1)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.5stretched.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 5 Stretched',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

fifteen_semitones.5compressed.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 5,
                 stretching = 1.9)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.5compressed.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 5 Compressed',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

# 10 harmonics

fifteen_semitones.10harmonics.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 10)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.10harmonics.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 10 Harmonics',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

fifteen_semitones.10stretched.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 10,
                 stretching = 2.1)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.10stretched.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 10 Stretched',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

fifteen_semitones.10compressed.hertz = 0:1500 %>% lapply(function(cents) {
  auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200), num_harmonics = 10,
                 stretching = 1.9)
}) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

p=auditory_plot(
  fifteen_semitones.10compressed.hertz,c('pitch.hertz','consonance_dissonance'),
  title='Frequency 10 Compressed',
  xlab='Frequency (Hz)',
  ylab="Dissonance and Consonance", include_text = FALSE)
save_auditory_plots(p,'results/plots')
p

p=auditory_plot(core_pitches(num_harmonics = 10),
                c('pitch.hertz','consonance_dissonance'),
                title='10 Harmonics',
                xlab='Frequency (Hz)',
                ylab="Dissonance and Consonance",
                x_symmetrical=FALSE,x_log2=TRUE)
save_auditory_plots(p,'results/plots')
p

p=auditory_plot(core_pitches(num_harmonics = 10, stretching = 2.1),
                c('pitch.hertz','consonance_dissonance'),
                title='10 Stretched',
                xlab='Frequency (Hz)',
                ylab="Dissonance and Consonance",
                x_symmetrical=FALSE,x_log2=TRUE)
save_auditory_plots(p,'results/plots')
p


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
