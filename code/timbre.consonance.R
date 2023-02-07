source('code/setup.R')

dir = '/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/output/batches/'
results = list()
behavior = list()
s_options = c('Stretched','Harmonic','Compressed')
t_options = c('lowest','highest','all','max','major','minor','major.d.0.01')

# TODO: grab the Bonang results

for (s in s_options) {
  for (t in t_options) {
    filename = paste0(dir, s, ' dyads (3 dB roll-off)/models/mami.codi.R.',t,'.as.tonic.rds')
    results[[paste0(s,'.',t,'.tonic')]] = readRDS(filename)

    filename = paste0(dir, s, ' dyads (3 dB roll-off)/behaviour/profile.rds')
    behavior[[s]] =readRDS(filename)
  }
}

for (name in names(results)) {
  print(results[[name]]$full$plot() + ggplot2::scale_x_continuous(breaks = 0:15) +
          ggplot2::ggtitle(name))
}

for (s in s_options) {
  high         = results[[paste0(s,'.highest.tonic')]]$full$raw_profile
  smooth_major = results[[paste0(s,'.major.tonic')]]$full$profile
  low          = results[[paste0(s,'.lowest.tonic')]]$full$raw_profile

  print(plot(high$interval,high$output,pch=2,col='#ABDAF3', main=paste(s)))
  print(points(low$interval,low$output,pch=6,col='#F3A904'))
  print(lines(smooth_major$interval,smooth_major$output,col='#FF5500'))
  print(axis(1, at=0:15))
  print(abline(v = 0:15,lty = 2, col = "gray"))
}
