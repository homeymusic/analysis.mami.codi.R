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
