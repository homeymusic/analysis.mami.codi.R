# TODO: create the big 4 plots in Homey Style with
# major-minor and consonance-dissonance

source('code/setup.R')


mami.codi.results.rds = '/Users/landlessness/Documents/git/homeymusic/analysis.mami.codi.R/data/mami.codi.results.rds'
mami.codi.results = NULL

# TRUE generates new data
# FALSE loads data from file
if (FALSE) {

  mami.codi.results = tibble::tibble(bonang.dtw = numeric(),
                                     compressed.dtw = numeric(),
                                     harmonic.dtw = numeric(),
                                     stretched.dtw = numeric(),
                                     bonang.pearson = numeric(),
                                     compressed.pearson = numeric(),
                                     harmonic.pearson = numeric(),
                                     stretched.pearson = numeric(),
                                     label = character(),
                                     metric = numeric(),
                                     tonic_selector = numeric(),
                                     high_register = numeric(),
                                     low_register = numeric(),
                                     resolution = numeric())

  experiments = c('harmonic','stretched','compressed','bonang')
  canonical.experiment = experiments[1]
  # TODO: add harmonic once it's done: experiments = c('bonang','comp ressed','harmonic','stretched')
  data.dir = '/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/explorations/data/'

  # make one experiment the canonical experiment
  canonical.models.dir = paste0(data.dir, canonical.experiment, '/models/')
  canonical.files = list.files(canonical.models.dir,pattern='.rds')

  print(paste('analyzing canonical experiment:',canonical.experiment))
  progress.bar = txtProgressBar(min = 1, max = length(canonical.files), initial = 1)
  p = 1

  canonical.behavior.filename = paste0(data.dir, canonical.experiment, '/behaviour/profile.rds')
  canonical.behavior = readRDS(canonical.behavior.filename)

  for (canonical.file in canonical.files) {

    canonical.model.filename = paste0(canonical.models.dir,canonical.file)
    canonical.mami.codi = readRDS(canonical.model.filename)

    label = canonical.mami.codi$full$model$label
    checkmate::assert_true(paste0(label,'.rds') == canonical.file)

    results <- tibble::tibble(
      model = canonical.mami.codi$full$profile$output,
      behavior = canonical.behavior$profile$rating
    )

    results.normalized <- (preProcess(results, method=c('range')) %>%
                             predict(results))

    dtw = dtw(results.normalized$model,
              results.normalized$behavior,
              distance_only=TRUE)$normalizedDistance

    pearson = cor(results.normalized$model,
                  results.normalized$behavior,
                  method='pearson')

    mami.codi.results <- mami.codi.results %>% tibble::add_row (
      "{canonical.experiment}.dtw"     := dtw,
      "{canonical.experiment}.pearson" := pearson,
      label = label,
      metric = canonical.mami.codi$full$model$metric,
      tonic_selector = canonical.mami.codi$full$model$tonic_selector,
      high_register  = canonical.mami.codi$full$model$high_register,
      low_register   = canonical.mami.codi$full$model$low_register,
      resolution = canonical.mami.codi$full$model$resolution
    )
    p = p+1
    setTxtProgressBar(progress.bar,p)
  }

  checkmate::assert_true(canonical.files %>% unique %>% length
                         == mami.codi.results$label %>% unique %>% length)

  for (experiment in experiments[-1]) {
    print(paste('analyzing experiment:',experiment))
    progress.bar = txtProgressBar(min = 1,
                                  max = nrow(mami.codi.results),
                                  initial = 1)

    p=1

    experiment.dir =  paste0(data.dir, experiment)
    experiment.behavior.filename = paste0(experiment.dir, '/behaviour/profile.rds')
    experiment.behavior = readRDS(experiment.behavior.filename)

    experiment.models.dir = paste0(experiment.dir, '/models/')

    for (i in 1:nrow(mami.codi.results)) {
      label = mami.codi.results$label[i]
      filename = paste0(experiment.models.dir,label,'.rds')
      experiment.model.mami.codi = readRDS(filename)

      dtw = dtw(experiment.model.mami.codi$full$profile$output,
                experiment.behavior$profile$rating,
                distance_only=TRUE)$normalizedDistance

      pearson = cor(experiment.model.mami.codi$full$profile$output,
                    experiment.behavior$profile$rating,
                    method='pearson')

      mami.codi.results <- dplyr::rows_update(
        mami.codi.results,
        tibble::tibble(
          label=label,
          "{experiment}.dtw" := dtw,
          "{experiment}.pearson" := pearson
        ),
        by = 'label')

      p = p+1
      setTxtProgressBar(progress.bar,p)
    }
  }

  mami.codi.results <- mami.codi.results %>% dplyr::mutate(
    composite.dtw = (bonang.dtw + compressed.dtw + harmonic.dtw + stretched.dtw),
    composite.pearson = (bonang.pearson + compressed.pearson + harmonic.pearson + stretched.pearson))

  max.dtw = mami.codi.results$composite.dtw %>% max

  mami.codi.results <- mami.codi.results %>% dplyr::mutate(
    composite = (max.dtw - composite.dtw) * composite.pearson
  )

  saveRDS(mami.codi.results, mami.codi.results.rds)
} else {
  mami.codi.results = readRDS(mami.codi.results.rds)
}

tuning = mami.codi.results %>% dplyr::filter(grepl('m.1.t.1.h.2.l.-1.r', label)) %>%
  dplyr::arrange(dplyr::desc(composite))
print(plot(tuning$resolution,tuning$composite,log='x',main='t.1.h.2.l.-1'))
print(abline(v=tuning$resolution[1]))
print(abline(h=tuning$composite[1]))
text(min(tuning$resolution), tuning$composite[1],
     as.character(tuning$composite[1]%>% round(2)), pos = 3)
label = paste0(tuning$resolution[1],' (d=',(100*1/tuning$resolution[1]) %>% round(2),'%)')
text(tuning$resolution[1]-2, min(tuning$composite), label, pos = 3, srt=90, offset = 3)
print(tuning,n=10)

print(plot(mami.codi.results$resolution,mami.codi.results$composite,log='x',main='all data'))

homey.brown       = '#664433'
homey.cream       = '#F3DDAB'
homey.dark.cream  = '#7F745A'
homey.blue        = '#ABDAF3'
homey.red         = '#FF5500'
homey.maize       = '#F3A904'
homey.green       = '#73DE73'

# read one model
# mami.codi = readRDS('/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/explorations/data/harmonic/models/mami.codi.m.1.t.1.h.2.l.-1.r.100.rds')
# plot(mami.codi$full$profile$interval, mami.codi$full$profile$output,
#      col=homey.brown, pch=1,main=paste(file))
# print(abline(v = 0:15,lty = 2, col = "gray"))
# print(axis(1, at=0:15))


model.experiment.filename <- function(label, experiment) {
  paste0(data.dir, experiment, '/models/',label,'.rds')
}

plot_mami.codi <- function(result, experiment) {
  f = model.experiment.filename(result$label,experiment)
  mami.codi = readRDS(f)

  print(plot(mami.codi$full$raw_profile$interval,
             mami.codi$full$raw_profile$output,
             col=homey.dark.cream,
             main=paste(experiment,'resolution:',result$resolution,'composite:',result$composite)))
  print(lines(mami.codi$full$profile$interval, mami.codi$full$profile$output,
              col=homey.red, lwd = 3))
  print(abline(v = 0:15,lty = 2, col = "gray"))
  print(axis(1, at=0:15))
}

# winner so far: m1 t1 h2 l-1 r100

results = mami.codi.results %>% dplyr::arrange(dplyr::desc(composite.dtw))
for (rank in 10:1) {
  for (experiment in experiments) {
    plot_mami.codi(results %>% dplyr::slice(rank),experiment)
  }
}

print(results, n=100)

# results = mami.codi.results %>% dplyr::arrange(desc(pearson)) %>%
#   dplyr::filter(grepl('h.2.l.-2', label))
#
# for (rank in 25:1) {
#   # best
#   plot_mami.codi(results %>% dplyr::slice(rank))
# }
#
# print(results,n=Inf)
#
#
# plot_mami.codi(ref_4.1 %>% dplyr::slice(2))

# correlation plot
# print(plot(behavior$profile$rating, mami.codi$full$profile$output,
#      pch=10,col=homey.red, main=results$label[rank]))
# print(abline(v = 0:15,lty = 2, col = "gray"))
# print(axis(1, at=0:15))

# major.minor = readRDS('/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/explorations/data/no.timbre/models/mami.codi.m.1.t.1.h.2.l.-1.r.73.54.rds')
#
# plot(no.timbre$interval,
#            no.timbre$consonance_dissonance,
#            col=no.timbre$color,
#            main=paste('maj:gold min:blue neu:red low.ref:-1.octave high.ref:2.octaves d:1.36%'))
# abline(v = 0:15,lty = 2, col = "gray")
# axis(1, at=0:15)

#
# intervals = seq(60,75,0.015)
# data = intervals %>% lapply(function(interval) {
#   chord = hrep::freq(hrep::sparse_fr_spectrum(c(60,interval)))
#   mami.codi(chord,
#             FUN = periodicity,
#             high_register  =  2,
#             low_register   = -1,
#             resolution     = 73.54,
#             tonic_selector = tonic_selectors()[1])
# }) %>% bind_rows
#
# data$interval <- intervals
#
# data$color="blue" # minor
# data$color[data$major_minor>0]="gold" # major
# data$color[data$major_minor==0]="red" # neutral
#
# saveRDS(data, 'explorations/data/no.timbre/models/mami.codi.m.1.t.1.h.2.l.-1.r.73.54.rds')
