library(mami.codi.R)
library(magrittr)
library(dplyr)
library(purrr)
source('data/chords.R')

plot_consonance_dissonance_major_minor <- function(chords,chords_name,include_path=FALSE,variable_point_size=FALSE) {
  title = chords_name
  p=auditory_plot(chords,c('major_minor','consonance_dissonance'),
                  title=title,
                  include_path=include_path,
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance",
                  variable_point_size=variable_point_size)
  save_auditory_plots(p,'results/plots')
}