source('code/setup.R')

chord.combinations.by.length <- readRDS('data/chord.combinations.RDS')
chord.combinations <- chord.combinations.by.length %>% bind_rows
distinct.chord.combinations <- chord.combinations %>%
  distinct(major_minor, consonance_dissonance, .keep_all = TRUE)

###
# code used to generate the chord combinations stored in RDS file
#
# a list of chord combos grouped by number of pitches in each chord
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
# saveRDS(chord.combinations,file='data/chord.combinations.RDS')
#

stats = chord.combinations.by.length %>% seq_along %>% lapply(function(i) {
  tibble::tibble_row(
    count = nrow(chord.combinations.by.length[[i]]),
    major_minor_max = chord.combinations.by.length[[i]]$major_minor %>% max,
    major_minor_min = chord.combinations.by.length[[i]]$major_minor %>% min,
    major_minor_range = .data$major_minor_max - .data$major_minor_min,
    consonance_dissonance_max = chord.combinations.by.length[[i]]$consonance_dissonance %>% max,
    consonance_dissonance_min = chord.combinations.by.length[[i]]$consonance_dissonance %>% min,
    consonance_dissonance_range = .data$consonance_dissonance_max - .data$consonance_dissonance_min,
    ratio = consonance_dissonance_range / major_minor_range
  )
}) %>% bind_rows
stats <- tibble::rowid_to_column(stats,"chord_length")
major_minor_range = (stats$major_minor_max %>% max) -
  (stats$major_minor_min %>% min)
consonance_dissonance_range = (stats$consonance_dissonance_max %>% max) -
  (stats$consonance_dissonance_min %>% min)
paste('ma.mi / co.di:',major_minor_range / consonance_dissonance_range)
paste('co.di / ma.mi:',consonance_dissonance_range / major_minor_range)
plot(stats$major_minor_range, stats$consonance_dissonance_range)
text(stats$major_minor_range, stats$consonance_dissonance_range, stats$chord_length,-1)
abline(a=0,b=1)

# most major chords
chord.combinations %>% arrange(desc(major_minor),desc(consonance_dissonance))
# most minor chords
chord.combinations %>% arrange(major_minor,desc(consonance_dissonance))

# most consonant
# example of most major chord
chord.combinations %>% arrange(desc(consonance_dissonance),desc(major_minor))
# most dissonant
chord.combinations %>% arrange(consonance_dissonance,desc(major_minor))

# overall stats
chord.combinations %>% summary

mami.max = chord.combinations$major_minor %>% max
mami.min = chord.combinations$major_minor %>% min
codi.max = chord.combinations$consonance_dissonance %>% max
codi.min = chord.combinations$consonance_dissonance %>% min

(codi.max - codi.min) / (mami.max - mami.min)

# all chord combinations versus unique major-minor and consonance-dissonance
num.chord.combinations <- chord.combinations %>% nrow
num.distinct.chord.combinations <- distinct.chord.combinations %>% nrow

# percent unique
num.distinct.chord.combinations / num.chord.combinations

plot(distinct.chord.combinations$major_minor,
     distinct.chord.combinations$consonance_dissonance)
text(distinct.chord.combinations$major_minor,
     distinct.chord.combinations$consonance_dissonance,
     distinct.chord.combinations$integer_name, pos=3)

p = auditory_plot(distinct.chord.combinations,c('major_minor','consonance_dissonance'),
                  title='Distinct Chord Combinations',
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance",
                  include_text=FALSE)
save_auditory_plots(p,'results/plots')
p

cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='pearson')
cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='kendall')
cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='spearman')

cor(distinct.chord.combinations$consonance.low, distinct.chord.combinations$consonance.high,
    method='pearson')
cor(distinct.chord.combinations$consonance.low, distinct.chord.combinations$consonance.high,
    method='kendall')
cor(distinct.chord.combinations$consonance.low, distinct.chord.combinations$consonance.high,
    method='spearman')

combo.consonance.data = chord.combinations %>% group_by(consonance_dissonance) %>%
  summarise(n=n(),name=first(integer_name))

combo.consonance.data %>% summary

plot(combo.consonance.data$consonance_dissonance,combo.consonance.data$n)
plot((combo.consonance.data %>% arrange(desc(n)))$n)
plot(combo.consonance.data$consonance_dissonance,combo.consonance.data$n,log='y')
text(combo.consonance.data$consonance_dissonance,combo.consonance.data$n,combo.consonance.data$name,pos=3,log='y')


heatmap.data = chord.combinations %>% group_by(major_minor,consonance_dissonance) %>%
  summarise(n=n(),name=last(integer_name))
plot(heatmap.data$major_minor,heatmap.data$consonance_dissonance)
text(heatmap.data$major_minor,heatmap.data$consonance_dissonance,
     heatmap.data$name,pos=3)
p = ggplot(heatmap.data,aes(x=major_minor,y=consonance_dissonance,color=n)) +
  geom_point(size=2) + theme_bw() + ggtitle('Heat Map Chord Frequency') +
  scale_colour_continuous(direction=-1, trans='log10', type='viridis')

save_auditory_plots(p,'results/plots')


octave = a(c(0,12))
minor_second = a(c(0,1))
augmented_triad = a(c(0,4,8))
chromatic = a(0:12)
octave$consonance_dissonance
minor_second$consonance_dissonance
augmented_triad$consonance_dissonance
chromatic$consonance_dissonance
(octave$consonance_dissonance - heatmap.data$consonance_dissonance %>% min) / 2

