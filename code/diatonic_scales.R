library(mami.codi.R)
library(magrittr)
library(dplyr)
library(purrr)

diatonic_pitches = list(
  ionian     = c(0,2,4,5,7,9,11,12),
  phrygian   = c(0,1,3,5,7,8,10,12),
  mixolydian = c(0,2,4,5,7,9,10,12),
  aeolian    = c(0,2,3,5,7,8,10,12),
  dorian     = c(0,2,3,5,7,9,10,12),
  lydian     = c(0,2,4,6,7,9,11,12),
  locrian    = c(0,1,3,5,6,8,10,12),
  chromatic  = 0:12
)

scales = (imap(diatonic_pitches, function(x,y) {
  list(a(x,tonic = 0,  name = paste(y, 'Up')),
       a(x,tonic = 12, name = paste(y, 'Down')))
})) %>% bind_rows

p = auditory_plot(scales,c('major_minor','consonance_dissonance'),
                  title='Diatonic Scales',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance")


save_auditory_plots(p,'results/plots')
p
