plot_consonance_dissonance_major_minor <- function(chords,chords_name,include_path=FALSE) {
  title = chords_name
  p=auditory_plot(chords,c('major_minor','consonance_dissonance'),
                  title=title,
                  include_path=include_path,
                  xlab='Minor and Major',
                  ylab="Dissonance and Consonance")
  save_auditory_plots(p)
  expect_true(!is.null(p))
}
test_that('plot of consonance_dissonance major_minor of core pitches with low tonic makes sense',{
  plot_consonance_dissonance_major_minor(core_pitches_low_tonic(),'Intervals with Low Tonic')
})
test_that('plot of consonance_dissonance major_minor of core pitches with high tonic makes sense',{
  plot_consonance_dissonance_major_minor(core_pitches_high_tonic(),'Intervals with High Tonic')
})
test_that('plot of consonance_dissonance major_minor of core pitches with high and low tonic makes sense',{
  plot_consonance_dissonance_major_minor(
    dplyr::bind_rows(core_pitches_low_tonic(),core_pitches_high_tonic())
    ,'Intervals with High and Low Tonic')
})
test_that("plot all major triads", {
  plot_consonance_dissonance_major_minor(all_major_triads(),'All Major Triads')
})
test_that("plot major and minor triads", {
  plot_consonance_dissonance_major_minor(major_minor_triads(),'Major and Minor Triads')
})
test_that("plot major and dual minor triads", {
  plot_consonance_dissonance_major_minor(major_dual_minor_triads(),'Major and Dual Minor Triads')
})
test_that("plot minor and dual major triads", {
  plot_consonance_dissonance_major_minor(minor_dual_major_triads(),'Minor and Dual Major Triads')
})
test_that("plot major, minor and dual minor triads", {
  plot_consonance_dissonance_major_minor(major_minor_dual_minor_triads(),'Major, Minor and Dual Minor Triads')
})
test_that('diatonic modes look good',{
  chords = dplyr::bind_rows(diatonic_scales())
  plot_consonance_dissonance_major_minor(chords,'Diatonic Scales')
})
test_that('diatonic triads look good',{
  chords = dplyr::bind_rows(diatonic_triads())
  plot_consonance_dissonance_major_minor(chords,'Diatonic Triads')
})
test_that('major & minor triads progression looks interesting',{
  chords = dplyr::bind_rows(major_triad_progression(),minor_triad_progression())
  plot_consonance_dissonance_major_minor(chords,'Progression: Major & Minor Triads')
})
test_that('major & phrygian triads progression looks interesting',{
  chords = dplyr::bind_rows(major_triad_progression(),phrygian_triad_progression())
  plot_consonance_dissonance_major_minor(chords,'Progression: Major & Phrygian Triads')
})
test_that('ionian chord progression looks interesting',{
  chords = dplyr::bind_rows(ionian_tonic_chords())
  plot_consonance_dissonance_major_minor(chords,'Progression: Major Scale Chords')
})
