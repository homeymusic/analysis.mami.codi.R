source('code/setup.R')
dir = './data'

model.experiments.rds = paste0(dir,'/model.experiments.rds')
model.experiments = NULL

if (TRUE) {

  model.experiments = tibble::tibble(
    model = character(),
    bonang.dtw = numeric(),
    compressed.dtw = numeric(),
    harmonic.dtw = numeric(),
    stretched.dtw = numeric(),
    bonang.pearson = numeric(),
    compressed.pearson = numeric(),
    harmonic.pearson = numeric(),
    stretched.pearson = numeric())

  models = c('interference.simplex', 'harmonicity.simplex',
             'periodicity.duplex','periodicity.simplex')

  experiments = c('harmonic','stretched','compressed','bonang')

  for (model in models) {
    for (experiment in experiments) {
      filename = paste0(dir,'/timbre.models/behavior','.',experiment,'.rds')
      behavior.experiment = readRDS(filename)

      filename = paste0(dir,'/timbre.models/',model,'.',experiment,'.rds')
      model.experiment = readRDS(filename)

      dynamic.time.warping = dtw(model.experiment$full$profile$output,
                                 behavior.experiment$profile$rating,
                                 distance_only=TRUE)$normalizedDistance

      pearson = cor(model.experiment$full$profile$output,
                    behavior.experiment$profile$rating,
                    method='pearson')

      model.experiments <- model.experiments %>%
        dplyr::rows_upsert (
          tibble::tibble(
            model                   = model,
            "{experiment}.dtw"     := dynamic.time.warping,
            "{experiment}.pearson" := pearson
          ),
          by = 'model'
        )
    }
  }

  model.experiments <- model.experiments %>% dplyr::mutate(
    composite.dtw = (bonang.dtw + compressed.dtw + harmonic.dtw + stretched.dtw),
    composite.pearson = (bonang.pearson + compressed.pearson + harmonic.pearson + stretched.pearson))

  max.dtw = model.experiments$composite.dtw %>% max

  model.experiments <- model.experiments %>% dplyr::mutate(
    composite = (max.dtw - composite.dtw) * composite.pearson
  )

  saveRDS(model.experiments, model.experiments.rds)
} else {
  model.experiments = readRDS(model.experiments.rds)
}
