source('code/setup.R')

create_data = TRUE
create_plots = TRUE
major_only = FALSE
minor_only = FALSE

for (h in 0:12) {
  for (s in c(1.9,2.0,2.1)) {
    if (h == 0 && s != 2.0) {
      # skip it
    } else {
      filename = paste0('data/harmonics/',
                        if (major_only) {'major.only.'}
                        else if (minor_only)  {'minor.only.'}
                        ,'h.',h,'.s.',s,'.RDS')
      print(filename)

      if (create_data) {
        sweep = 0:1700 %>% lapply(function(cents) {
          auditory.hertz(B3.HERTZ * 2 ^ (cents / 1200),
                         tonic.hertz = C4.HERTZ,
                         num_harmonics = h,
                         stretching = s)
        }) %>% bind_rows %>% tidyr::drop_na(major_minor, consonance_dissonance)

        if (major_only) {
          sweep = sweep %>% dplyr::filter(major_minor > 0)
        } else if (minor_only) {
          sweep = sweep %>% dplyr::filter(major_minor < 0)
        }

        saveRDS(sweep,file=filename)
      } else {
        sweep = readRDS(filename)
      }

      for (w in seq(from=1,to=101,by=2)) {
        plot_title = paste0(if (major_only) {'major.only.'}
                            else if (minor_only)  {'minor.only.'},
                            'h.',h,'.s.',s,'.w.',w)
        print(plot_title)
        if (create_plots) {
          p=auditory_plot(
            sweep,c('pitch.hertz','consonance_dissonance'),
            title=plot_title,
            xlab='Semitones',
            ylab="Dissonance and Consonance", include_text = FALSE,
            moving_average=w)

          save_auditory_plots(p,'results/plots/harmonics',filetypes='pdf')
        }
      }
    }
  }
}

# TODO: figure out moving average
# function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
# OR use zoo https://www.storybench.org/how-to-calculate-a-rolling-average-in-r/

for (h in 0:12) {
  plot_consonance_dissonance_major_minor(
    core_pitches(num_harmonics=h), paste('Intervals Low Tonic',h,'Harmonics'))
}

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
