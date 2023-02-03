source('code/setup.R')

# code to create the data
#
# two_octaves.hertz = -1200:1200 %>% lapply(function(cents) {
#   auditory.hertz(C4.HERTZ * 2 ^ (cents / 1200))
# }) %>% bind_rows
# saveRDS(two_octaves.hertz,file='data/two_octaves.hertz.RDS')

two_octaves.hertz <- readRDS('data/two_octaves.hertz.RDS')

p = auditory_plot(two_octaves.hertz,c('major_minor','consonance_dissonance'),
                  title='Frequency Two Octaves as mami.codi',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance",
                  include_text=FALSE)
save_auditory_plots(p,'results/plots')
p

p=auditory_plot(two_octaves.hertz,c('pitch.hertz','consonance_dissonance'),
                title='Intervals Frequency versus Consonance',
                xlab='Average Frequency (Hz)',
                ylab="Dissonance and Consonance",
                x_symmetrical=FALSE,x_log2=TRUE, include_text=FALSE)
save_auditory_plots(p,'results/plots')
p

plot(two_octaves.hertz$pitch.hertz,two_octaves.hertz$consonance_dissonance,
     log='x', pch=20)

two_octaves.semitones = -12:12 %>% lapply(function(semitone) {
  a(semitone)
}) %>% bind_rows

points(two_octaves.semitones$pitch.hertz,two_octaves.semitones$consonance_dissonance,
       pch=5, col='darkgrey')

text(two_octaves.semitones$pitch.hertz,two_octaves.semitones$consonance_dissonance,
     two_octaves.semitones$name.semitones,
     pos=3, col='darkgrey', family='Arial Unicode MS')
