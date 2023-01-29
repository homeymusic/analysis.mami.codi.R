source('code/setup.R')

plot_consonance_dissonance_major_minor(all_major_triads(),'All Major Triads')
plot_consonance_dissonance_major_minor(major_minor_triads(),'Major and Minor Triads')
plot_consonance_dissonance_major_minor(major_dual_minor_triads(),'Major and Dual Minor Triads')
plot_consonance_dissonance_major_minor(minor_dual_major_triads(),'Minor and Dual Major Triads')
plot_consonance_dissonance_major_minor(major_minor_dual_minor_triads(),'Major, Minor and Dual Minor Triads')
chords = dplyr::bind_rows(diatonic_triads())
plot_consonance_dissonance_major_minor(chords,'Diatonic Triads')
chords = dplyr::bind_rows(major_triad_progression())
plot_consonance_dissonance_major_minor(chords,'Progression: Major Triads',
                                       include_path=TRUE,variable_point_size=TRUE)
chords = dplyr::bind_rows(minor_triad_progression())
plot_consonance_dissonance_major_minor(chords,'Progression: Minor Triads',
                                       include_path=TRUE,variable_point_size=TRUE)
chords = dplyr::bind_rows(phrygian_triad_progression())
plot_consonance_dissonance_major_minor(chords,'Progression: Phrygian Triads',
                                       include_path=TRUE,variable_point_size=TRUE)
chords = dplyr::bind_rows(ionian_tonic_chords())
plot_consonance_dissonance_major_minor(chords,'Progression: Major Scale Chords',
                                       include_path=TRUE,variable_point_size=TRUE)
