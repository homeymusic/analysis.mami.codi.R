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

filename = (mami.codi.results %>% dplyr::arrange(desc(pearson)))$filename[1]

mami.codi = readRDS(filename)

homey.brown       = '#664433'
homey.cream       = '#F3DDAB'
homey.dark.cream  = '#7F745A'
homey.blue        = '#ABDAF3'
homey.red         = '#FF5500'
homey.maize       = '#F3A904'
homey.green       = '#73DE73'

plot(mami.codi$full$profile$interval, mami.codi$full$profile$output,
     col=homey.brown, pch=1)
abline(v = 0:15,lty = 2, col = "gray")
axis(1, at=0:15)

plot(behavior$profile$interval, behavior$profile$rating,
       pch=4,col=homey.green)
abline(v = 0:15,lty = 2, col = "gray")
axis(1, at=0:15)

plot(behavior$profile$rating, mami.codi$full$profile$output,
     pch=10,col=homey.red)
abline(v = 0:15,lty = 2, col = "gray")
axis(1, at=0:15)
