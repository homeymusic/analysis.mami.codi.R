source('code/setup.R')

# polarized auditory model
intervals = interval_components()
up = dplyr::bind_rows(purrr::map2(intervals$integer_position,intervals$name,
                               ~pa(.x,name=.y,tonic=0,polarity='up')))
plot(x=up$average_midi_note,y=up$consonance.low)
text(x=up$average_midi_note,y=up$consonance.low,up$label)
plot_consonance_dissonance_major_minor(up,'Polarity Up Intervals')
down = dplyr::bind_rows(purrr::map2(intervals$integer_position,intervals$name,
                                  ~pa(.x,name=.y,tonic=12,polarity='down')))
plot(x=down$average_midi_note,y=down$consonance.high)
text(x=down$average_midi_note,y=down$consonance.high,down$label)
plot_consonance_dissonance_major_minor(down,'Polarity Down Intervals')

# auditory model
plot_consonance_dissonance_major_minor(core_pitches_low_tonic(),'Intervals with Low Tonic')
plot_consonance_dissonance_major_minor(core_pitches_high_tonic(),'Intervals with High Tonic')
plot_consonance_dissonance_major_minor(
  dplyr::bind_rows(core_pitches_low_tonic(),core_pitches_high_tonic())
  ,'Intervals with High and Low Tonic')
