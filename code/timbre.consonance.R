source('code/setup.R')

dir = '/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/output/batches/'
stretched_dyads = list()
behavior = list()
s_options = c('Stretched','Harmonic','Compressed')

for (s in s_options) {
  filename = paste0(dir, s, ' dyads (3 dB roll-off)/models/mami.codi.rds')
  stretched_dyads[[s]] = readRDS(filename)

  filename = paste0(dir, s, ' dyads (3 dB roll-off)/behaviour/profile.rds')
  behavior[[s]] =readRDS(filename)
}

filename = paste0(dir, 'Bonang dyads/models/mami.codi.rds')
bonang = readRDS(filename)

mami.codi.stretched = stretched_dyads[['Stretched']]$full$raw_profile$interval %>% lapply(function(semitone) {
    pitch.hertz = C4.HERTZ * 2 ^ (semitone / 12)
    auditory.hertz(c(C4.HERTZ,pitch.hertz),C4.HERTZ,
                   num_harmonics=10,stretching=2.1,
                   sweeper.hertz = pitch.hertz)
  }) %>% bind_rows

p=auditory_plot(mami.codi.stretched,c('sweeper.hertz','consonance_dissonance'),
                title='Stretched Dyads',
                xlab='Frequency (Hz)',
                ylab="Dissonance and Consonance",
                x_symmetrical=FALSE, include_text=FALSE)

smooth = stretched_dyads[['Stretched']]$full$profile %>% mutate(pitch.hertz=C4.HERTZ*2^(interval / 12))
p = p + ggplot2::geom_line(data=smooth,color='white', linewidth=1,
                           ggplot2::aes(x=pitch.hertz,
                                        y=output))

save_auditory_plots(p,'results/plots')

for (name in names(stretched_dyads)) {
  p = stretched_dyads[[name]]$full$plot() + ggplot2::scale_x_continuous(breaks = 0:15) +
    ggplot2::ggtitle(name)
  print(p)
  save_auditory_plots(p,'results/plots/timbre',filetypes = 'pdf')
}

p = bonang$full$plot() + ggplot2::scale_x_continuous(breaks = 0:15) +
  ggplot2::ggtitle('bonang')
print(p)
save_auditory_plots(p,'results/plots/timbre',filetypes = 'pdf')

for (s in s_options) {
  stretched_dyads        = stretched_dyads[[s]]$full$raw_profile
  smooth_stretched_dyads = stretched_dyads[[s]]$full$profile

  p = plot(stretched_dyads$interval,stretched_dyads$output,pch=2,col='#ABDAF3', main=paste(s))
  print(p)
  print(lines(smooth_stretched_dyads$interval,smooth_stretched_dyads$output,col='#FF5500'))
  print(axis(1, at=0:15))
  print(abline(v = 0:15,lty = 2, col = "gray"))
}

bonang        = bonang[[s]]$full$raw_profile
smooth_bonang = bonang[[s]]$full$profile

p = plot(bonang$interval,bonang$output,pch=2,col='#ABDAF3', main=paste(s))
print(p)
print(lines(smooth_bonang$interval,smooth_bonang$output,col='#FF5500'))
print(axis(1, at=0:15))
print(abline(v = 0:15,lty = 2, col = "gray"))
