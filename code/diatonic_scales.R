library(mami.codi.R)
library(magrittr)
library(dplyr)
library(purrr)
source('data/chords.R')

scales = (imap(diatonic_pitches(), function(x,y) {
  list(a(x,tonic = 0,  name = paste(y, 'Low')),
       a(x,tonic = 12, name = paste(y, 'High')))
})) %>% bind_rows

p = auditory_plot(scales,c('major_minor','consonance_dissonance'),
                  title='Diatonic Scales',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance")
save_auditory_plots(p,'results/plots')
