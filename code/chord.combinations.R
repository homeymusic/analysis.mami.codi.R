source('code/setup.R')

###
# a list of chord combos grouped by number of pitches in each chord
#
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
# saveRDS(chord.combinations,file='data/chord.combinations.RDS')
#
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

###
# all chords combined into one tibble
#
chord.combinations <- chord.combinations %>% bind_rows
# example of most major chord is {0,3,6,10} with 0 tonic ma.mi: 2.29 co.di: 2.29
chord.combinations %>% arrange(desc(major_minor),desc(consonance_dissonance))
chord.combinations %>% filter(major_minor > 2.2)

# example of most minor chord is {0,4,7,10} with 10 as tonic ma.mi: -2.29 co.di: 2.29
chord.combinations %>% filter(major_minor < -2.2)

# most consonant is {0,12} ma.mi: 0 co.di: 6.91
a(c(0,12))

# most dissonant is {0:12} ma.mi: 0 co.di: 0
a(c(0:12))

# overall stats
chord.combinations %>% summary

# 53,235 chord combinations
# only 124 are unique in major-minor and consonance-dissonance space
num.chord.combinations <- chord.combinations %>% nrow
distinct.chord.combinations <- chord.combinations %>%
  distinct(major_minor, consonance_dissonance, .keep_all = TRUE)
num.distinct.chord.combinations <- distinct.chord.combinations %>% nrow

# 0.002329295 or 0.23% are distinct
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

# pearson: 0.4326733
cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='pearson')
# kendall: 0.3299057
cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='kendall')
# spearman: 0.3838189
cor(chord.combinations$consonance.low, chord.combinations$consonance.high,
    method='spearman')

# pearson 0.3490965
cor(distinct.chord.combinations$consonance.low, distinct.chord.combinations$consonance.high,
    method='pearson')
# kendall: 0.232105
cor(distinct.chord.combinations$consonance.low, distinct.chord.combinations$consonance.high,
    method='kendall')
# spearman: 0.3199943
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
  summarise(n=n())
p = ggplot(heatmap.data,aes(x=major_minor,y=consonance_dissonance,color=n)) +
  geom_point(size=4) + theme_bw() + ggtitle('Heat Map Chord Frequency')
  scale_colour_continuous(direction=-1, trans='log10', type='viridis')

save_auditory_plots(p,'results/plots')
