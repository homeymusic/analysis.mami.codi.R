source('code/setup.R')

dir = '/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/explorations/data/'

behavior_filename = paste0(dir, 'stretched/behaviour/profile.rds')
behavior = readRDS(behavior_filename)

mami.codi.dir = paste0(dir,'stretched/models/')
files = list.files(mami.codi.dir,pattern='.rds')

mami.codi.results = tibble::tibble(pearson = numeric(),
                           label = character(),
                           tonic_selector = numeric(),
                           reference.high.octaves = numeric(),
                           reference.low.octaves = numeric(),
                           frequency.resolution = numeric(),
                           experiment = character(),
                           filename = character())

homey.brown       = '#664433'
homey.cream       = '#F3DDAB'
homey.dark.cream  = '#7F745A'
homey.blue        = '#ABDAF3'
homey.red         = '#FF5500'
homey.maize       = '#F3A904'
homey.green       = '#73DE73'

for (file in files) {
  filename = paste0(mami.codi.dir,file)
  mami.codi = readRDS(filename)

  pearson = cor(behavior$profile$rating, mami.codi$full$profile$output,
                method='pearson')

  mami.codi.results <- mami.codi.results %>% tibble::add_row (
    pearson = pearson,
    label = mami.codi$full$model$label,
    tonic_selector = mami.codi$full$model$tonic_selector,
    reference.high.octaves = mami.codi$full$model$reference.high.octaves,
    reference.low.octaves = mami.codi$full$model$reference.low.octaves,
    frequency.resolution = mami.codi$full$model$frequency.resolution,
    experiment = 'dyads.stretched.usa',
    filename = filename
  )
}

print(plot(behavior$profile$interval, behavior$profile$rating,
           pch=4,col=homey.green))
print(abline(v = 0:15,lty = 2, col = "gray"))
print(axis(1, at=0:15))

plot_mami.codi <- function(result) {
  mami.codi = readRDS(result$filename)

  print(plot(mami.codi$full$profile$interval, mami.codi$full$profile$output,
             col=homey.brown, pch=1,
             main=paste(result$label,'cor:',result$pearson %>% round(2))))
  print(abline(v = 0:15,lty = 2, col = "gray"))
  print(axis(1, at=0:15))
}

results = mami.codi.results %>% dplyr::arrange(desc(pearson))
for (rank in 10:1) {
  # best
  plot_mami.codi(results %>% dplyr::slice(rank))
}

print(results, n=50)
#
# results = mami.codi.results %>% dplyr::arrange(desc(pearson)) %>%
#   dplyr::filter(grepl('mami.codi.m.1.t.1.h.10.l.-1', label))
#
# for (rank in 24:1) {
#   # best
#   plot_mami.codi(results %>% dplyr::slice(rank))
# }
#
# print(results,n=Inf)

#
# plot_mami.codi(ref_4.1 %>% dplyr::slice(2))

# correlation plot
# print(plot(behavior$profile$rating, mami.codi$full$profile$output,
#      pch=10,col=homey.red, main=results$label[rank]))
# print(abline(v = 0:15,lty = 2, col = "gray"))
# print(axis(1, at=0:15))
