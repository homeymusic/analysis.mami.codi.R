library(mami.codi.R)
library(magrittr)
library(dplyr)
library(purrr)

diatonic_pitches = list(
  Ionian     = c(0,2,4,5,7,9,11,12),
  Phrygian   = c(0,1,3,5,7,8,10,12),
  Mixolydian = c(0,2,4,5,7,9,10,12),
  Aeolian    = c(0,2,3,5,7,8,10,12),
  Dorian     = c(0,2,3,5,7,9,10,12),
  Lydian     = c(0,2,4,6,7,9,11,12),
  Locrian    = c(0,1,3,5,6,8,10,12),
  Chromatic  = 0:12
)

scales = (imap(diatonic_pitches, function(x,y) {
  list(a(x,tonic = 0,  name = paste(y, 'Low')),
       a(x,tonic = 12, name = paste(y, 'High')))
})) %>% bind_rows

p = auditory_plot(scales,c('major_minor','consonance_dissonance'),
                  title='Diatonic Scales',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance")


save_auditory_plots(p,'results/plots')
p
