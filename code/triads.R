source('code/setup.R')
chord.combinations <- readRDS('data/chord.combinations.RDS')

stats = chord.combinations %>% seq_along %>% lapply(function(i) {
  tibble::tibble_row(
    count = nrow(chord.combinations[[i]]),
    major_minor_max = chord.combinations[[i]]$major_minor %>% max,
    major_minor_min = chord.combinations[[i]]$major_minor %>% min,
    major_minor_range = .data$major_minor_max - .data$major_minor_min,
    consonance_dissonance_max = chord.combinations[[i]]$consonance_dissonance %>% max,
    consonance_dissonance_min = chord.combinations[[i]]$consonance_dissonance %>% min,
    consonance_dissonance_range = .data$consonance_dissonance_max - .data$consonance_dissonance_min,
    ratio = consonance_dissonance_range / major_minor_range
  )
}) %>% bind_rows

major_minor_range = (stats$major_minor_max %>% max) -
  (stats$major_minor_min %>% min)
consonance_dissonance_range = (stats$consonance_dissonance_max %>% max) -
  (stats$consonance_dissonance_min %>% min)
paste('ma.mi / co.di:',major_minor_range / consonance_dissonance_range)
paste('co.di / ma.mi:',consonance_dissonance_range / major_minor_range)

# code used to generate the chord combinations stored in RDS file
#
# chord.combinations = lapply(1:12,function(chord_length) {
#   print(chord_length)
#   chords = lapply(1:chord_length,function(tonic_index) {
#     combn(0:12,chord_length,function(pitches) {
#       auditory(pitches,pitches[tonic_index])
#     }, simplify=FALSE)
#   }) %>% bind_rows
# })
#
# low  = combn(0:12,1,function(pitches){auditory(pitches,tonic=0)},simplify = FALSE) %>% bind_rows
# high = combn(0:12,1,function(pitches){auditory(pitches,tonic=12)},simplify = FALSE) %>% bind_rows
# chord.combinations[[1]] <- bind_rows(low, high)
#
# saveRDS(chord.combinations,file='data/chord.combinations.RDS')
