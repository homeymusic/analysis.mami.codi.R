source('code/setup.R')

mami.codi.results = tibble::tibble(bonang = numeric(),
                                   compressed = numeric(),
                                   harmonic = numeric(),
                                   stretched = numeric(),
                                   composite = numeric(),
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

for (canonical.file in canonical.files) {

  canonical.behavior.filename = paste0(data.dir, canonical.experiment, '/behaviour/profile.rds')
  canonical.behavior.filename = paste0(data.dir, canonical.experiment, '/behaviour/profile.rds')
  canonical.behavior = readRDS(canonical.behavior.filename)

  canonical.model.filename = paste0(canonical.models.dir,canonical.file)
  canonical.mami.codi = readRDS(canonical.model.filename)

  label = canonical.mami.codi$full$model$label
  checkmate::assert_true(paste0(label,'.rds') == canonical.file)

  pearson = cor(canonical.behavior$profile$rating,
                canonical.mami.codi$full$raw_profile$output,
                method='pearson')

  mami.codi.results <- mami.codi.results %>% tibble::add_row (
    "{canonical.experiment}" := pearson,
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

    pearson = cor(experiment.behavior$profile$rating,
                  experiment.model.mami.codi$full$raw_profile$output,
                  method='pearson')

    mami.codi.results <- dplyr::rows_update(
      mami.codi.results,
      tibble::tibble(label=label,"{experiment}" := pearson),
      by = 'label')

    p = p+1
    setTxtProgressBar(progress.bar,p)
  }
}

range = list()
for (experiment in experiments) {
  results = mami.codi.results[experiment]
  range[experiment] = max(results) - min(results)
}

mami.codi.results <- mami.codi.results %>% dplyr::mutate(
  composite = (bonang + compressed + harmonic + stretched))

homey.brown       = '#664433'
homey.cream       = '#F3DDAB'
homey.dark.cream  = '#7F745A'
homey.blue        = '#ABDAF3'
homey.red         = '#FF5500'
homey.maize       = '#F3A904'
homey.green       = '#73DE73'

# print(plot(behavior$profile$interval, behavior$profile$rating,
#            pch=4,col=homey.green))
# print(abline(v = 0:15,lty = 2, col = "gray"))
# print(axis(1, at=0:15))

#
# file = 'mami.codi.m.2.t.1.h.6.l.-1.r.100.rds'
# filename = paste0(mami.codi.dir,file)
# mami.codi = readRDS(filename)
# plot(mami.codi$full$profile$interval, mami.codi$full$profile$output,
#            col=homey.brown, pch=1,main=paste(file))
# print(abline(v = 0:15,lty = 2, col = "gray"))
# print(axis(1, at=0:15))
#

model.experiment.filename <- function(label, experiment) {
  paste0(data.dir, experiment, '/models/',label,'.rds')
}

plot_mami.codi <- function(result, experiment) {
  f = model.experiment.filename(result$label,experiment)
  mami.codi = readRDS(f)

  print(plot(mami.codi$full$raw_profile$interval,
             mami.codi$full$raw_profile$output,
             col=homey.dark.cream,
             main=paste(experiment,result$label,'cor:',
                        result$composite %>% round(2))))
  print(lines(mami.codi$full$profile$interval, mami.codi$full$profile$output,
             col=homey.red, lwd = 3))
  print(abline(v = 0:15,lty = 2, col = "gray"))
  print(axis(1, at=0:15))
}

# winner so far: m1 t1 h2 l-1 r100

results = mami.codi.results %>% dplyr::arrange(dplyr::desc(composite))
for (rank in 25:1) {
  for (experiment in experiments) {
    plot_mami.codi(results %>% dplyr::slice(rank),experiment)
  }
}

print(results, n=30)

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
