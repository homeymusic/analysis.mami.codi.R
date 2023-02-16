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

  for (experiment in experiments) {

    filename = paste0(dir,'/timbre.models/behavior','.',experiment,'.rds')
    behavior.experiment = readRDS(filename)

    results <- tibble::tibble(
      behavior = behavior.experiment$profile$rating
    )

    for (model in models) {
      filename = paste0(dir,'/timbre.models/',model,'.',experiment,'.rds')
      model.experiment = readRDS(filename)
      results <- results %>% dplyr::mutate(
        "{model}" := model.experiment$full$profile$output
      )
    }
    results.normalized <- (preProcess(results, method=c('range')) %>%
                             predict(results))

    for (model in models) {

      dynamic.time.warping = dtw(results.normalized[[model]],
                                 results.normalized$behavior,
                                 distance_only=TRUE)$normalizedDistance

      pearson = cor(results.normalized[[model]],
                    results.normalized$behavior,
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
  ) %>% dplyr::arrange(dplyr::desc(composite))

  saveRDS(model.experiments, model.experiments.rds)
} else {
  model.experiments = readRDS(model.experiments.rds)
}
