interval_components <- function() {
  tibble::tibble(
    integer_position = 0:12,
    name = c("Tonic","Minor 2nd","Major 2nd","Minor 3rd","Major 3rd",
             "Perfect 4th","Tritone","Perfect 5th","Minor 6th",
             "Major 6th","Minor 7th","Major 7th","Octave")
  )
}
core_pitches_low_tonic <- function() {
  intervals = interval_components()
  dplyr::bind_rows(purrr::map2(intervals$integer_position,intervals$name,
                               ~a(.x,name=.y,tonic=0)))
}
core_pitches_high_tonic <- function() {
  intervals = interval_components()
  dplyr::bind_rows(purrr::map2(intervals$integer_position,intervals$name,
                               ~a(.x,name=.y,tonic=12)))
}
major_triads <- function() {
  list(
    "root position"=a(c(0,4,7)%>%sort,
                      tonic=0,
                      name="Major Triad\nRoot Position"),
    "1st inversion"=a(c(0+12,4,7)%>%sort,
                      tonic=0+12,
                      name="Major Triad\n1st Inversion"),
    "2nd inversion"=a(c(0+12,4+12,7)%>%sort,
                      tonic=0+12,
                      name="Major Triad\n2nd Inversion")
  )
}
minor_triads <- function() {
  list(
    "root position"=a(c(0,3,7)%>%sort,
                      tonic=0,
                      name="Minor Triad\nRoot Position"),
    "1st inversion"=a(c(0+12,3,7)%>%sort,
                      tonic=0+12,
                      name="Minor Triad\n1st Inversion"),
    "2nd inversion"=a(c(0+12,3+12,7)%>%sort,
                      tonic=12,
                      name="Minor Triad\n2nd Inversion")
  )
}
# dual major / mixolydian
dual_major_triads <- function() {
  list(
    "root position"=a((-c(0,3,7)+12)%>%sort,
                      tonic=12,
                      name="Dual Major Triad\nRoot Position"),
    "1st inversion"=a((-c(0+12,3,7)+12)%>%sort,
                      tonic=0,
                      name="Dual Major Triad\n1st Inversion"),
    "2nd inversion"=a((-c(0+12,3+12,7)+12)%>%sort,
                      tonic=0,
                      name="Dual Major Triad\n2nd Inversion")
  )
}
# dual minor / phyrgian
dual_minor_triads <- function() {
  list(
    "root position"=a((-c(0,4,7)+12)%>%sort,
                      tonic=12,
                      name="Dual Minor Triad\nRoot Position"),
    "1st inversion"=a((-c(0+12,4,7)+12)%>%sort,
                      tonic=0,
                      name="Dual Minor Triad\n1st Inversion"),
    "2nd inversion"=a((-c(0+12,4+12,7)+12)%>%sort,
                      tonic=0,
                      name="Dual Minor Triad\n2nd Inversion")
  )
}
major_open_triads <- function() {
  list(
    "6/3"=a(c(0,3,8)%>%sort,
            tonic=0,
            name="Major 6/3"),
    "6/4"=a(c(0,5,9)%>%sort,
            tonic=0,
            name="Major 6/4")
  )
}
all_major_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(major_open_triads()),
    dplyr::bind_rows(dual_major_triads())
  )
}
major_minor_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(minor_triads())
  )
}
major_dual_minor_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(dual_minor_triads())
  )
}
minor_dual_major_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(minor_triads()),
    dplyr::bind_rows(dual_major_triads()),
  )
}
major_minor_dual_minor_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(minor_triads()),
    dplyr::bind_rows(dual_minor_triads())
  )
}
minor_6_chords <- function() {
  list(
    "minor 6/3"=a(c(0+12,3,7)%>%sort,
                  tonic=3,
                  midi_reference = 60 + 9,
                  name="Minor 6/3"),
    "minor 6/4"=a(c(0+12,3+12,7)%>%sort,
                  tonic=7,
                  midi_reference = 60 + 9,
                  name="Minor 6/4")
  )
}
augmented_triads <- function() {
  list(
    'augmented_triad_up' = a(c(0,4,8),
                             tonic=0,
                             midi_reference = 60 + 6,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = a(c(0,4,8),
                               tonic=8,
                               midi_reference = 60 + 6,
                               name='Augmented Triad Down')

  )
}
augmented_triad <- function() {
  a(c(0,4,8),
    name='Augmented Triad')
}
max_dissonance_triad <- function() {
  a(c(0,1,11),name='Max Dissonance Triad')
}
symmetrical_augmented_triads <- function() {
  list(
    'augmented_triad_up' = a(c(0,4,8),
                             tonic=0,
                             midi_reference = 60 + 6,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = a((-c(0,4,8))%>%sort,
                               tonic=0,
                               midi_reference = 60 + 6,
                               name='Augmented Triad Down')

  )
}
phrygian_6_chords <- function() {
  list(
    "6/3"=a((-c(0+12,4,7))%>%sort,
            tonic=-4,
            name="Dual Minor 6/3",
            midi_reference=60+4),
    "6/4"=a((-c(0+12,4+12,7))%>%sort,
            tonic=-7,
            name="Dual Minor 6/4",
            midi_reference=60+4)
  )
}
seventh_chords <- function() {
  list(
    "Major"=a(c(0,4,7,11),
              tonic=0,
              name="Major"),
    "Dominant Flat Five"=a(c(0,4,6,10),
                           tonic=0,
                           name="Dominant Flat Five"),
    "Dominant"=a(c(0,4,7,10),
                 tonic=0,
                 name="Dominant"),
    "Augmented"=a(c(0,4,8,10),
                  tonic=0,
                  name="Augmented"),
    "Augmented Major"=a(c(0,4,8,11),
                        tonic=0,
                        name="Augmented Major"),
    "minor"=a(c(0,3,7,10),
              tonic=0,
              name="Minor"),
    "minor major"=a(c(0,3,7,11),
                    tonic=0,
                    name="Minor-Major"),
    "half-diminished"=a(c(0,3,6,10),
                        tonic=0,
                        name="Half-Diminished"),
    "diminished major"=a(c(0,3,6,11),
                         tonic=0,
                         name="Diminished Major"),
    "diminished"=a(c(0,3,6,9),
                   tonic=0,
                   name="Diminished")
  )
}
diatonic_pitches <- function() {
  list(
    Ionian     = c(0,2,4,5,7,9,11,12),
    Phrygian   = c(0,1,3,5,7,8,10,12),
    Mixolydian = c(0,2,4,5,7,9,10,12),
    Aeolian    = c(0,2,3,5,7,8,10,12),
    Dorian     = c(0,2,3,5,7,9,10,12),
    Lydian     = c(0,2,4,6,7,9,11,12),
    Locrian    = c(0,1,3,5,6,8,10,12),
    Chromatic  = 0:12
  )
}
diatonic_scales <- function() {
  list(
    'ionian'         = a(c(0,2,4,5,7,9,11,12), tonic=0,  name = 'C Ionian', midi_reference = 60 + 0),
    'phrygian'       = a(c(0,1,3,5,7,8,10,12), tonic=12, name = 'E Phrygian', midi_reference = 60 + 4),
    'aeolian'        = a(c(0,2,3,5,7,8,10,12), tonic=0,  name = 'A Aeolian', midi_reference = 60 + 9),
    'mixolydian'     = a(c(0,2,4,5,7,9,10,12), tonic=12, name = 'G Mixolydian', midi_reference = 60 + 7),
    'dorian up'      = a(c(0,2,3,5,7,9,10,12), tonic=0,  name = 'D Dorian\nTonic Below', midi_reference = 60 + 2),
    'dorian down'    = a(c(0,2,3,5,7,9,10,12), tonic=12, name = 'D Dorian\nTonic Above', midi_reference = 60 + 2),
    'locrian'        = a(c(0,1,3,5,6,8,10,12), tonic=0,  name = 'B Locrian', midi_reference = 60 + 11),
    'lydian'         = a(c(0,2,4,6,7,9,11,12), tonic=12, name = 'F Lydian', midi_reference = 60 + 5),
    'chromatic up'   = a(0:12, tonic=0,  name='Chromatic\nTonic Below', midi_reference = 60),
    'chromatic down' = a(0:12, tonic=12, name='Chromatic\nTonic Above', midi_reference = 60)
  )
}
diatonic_triads <- function() {
  list(
    'ionian'     =a(c(0,4,7),
                    name = 'C Ionian P5/M3 I:IV:V', midi_reference = 60 + 0),
    'phrygian'   =a((-c(0,4,7) %>% sort), tonic = 0,
                    name = 'E Phrygian -P5/-M3 -i:-iv:-v', midi_reference = 60 + 4),
    'aeolian'    =a(c(0,3,7),
                    name = 'A Aeolian P5/m3 i:iv:v', midi_reference = 60 + 9),
    'mixolydian' =a((-c(0,3,7) %>% sort), tonic = 0,
                    name = 'G Mixolydian -P5/-m3 -I:-IV:-V', midi_reference = 60 + 7),
    'dorian up'  =a(c(0,5,9),
                    name = 'D Dorian Up M6/P4 I:IV:VII', midi_reference = 60 + 2),
    'dorian down'=a((-c(0,5,9) %>% sort), tonic = 0,
                    name = 'D Dorian Down M6/P4 -i:-iv:-vii', midi_reference = 60 + 2),
    'locrian'    =a(c(0,3,8),
                    name = 'B Locrian m6/m3 I:IV:VII', midi_reference = 60 + 11),
    'lydian'     =a((-c(0,3,8) %>% sort), tonic = 0,
                    name = 'F Lydian -m6/-m3 -i:-iv:-vii', midi_reference = 60 + 5)
  )
}
major_triad_progression <- function(){
  major_triad = c(0,4,7)
  list(
    a(major_triad  , tonic=0, name='I'),
    a(major_triad+5, tonic=0, name='IV'),
    a(major_triad+7, tonic=0, name='V'),
    a(major_triad  , tonic=0, name='I', include_label=FALSE)
  )
}
minor_triad_progression <- function(){
  minor_triad = c(0,3,7)
  list(
    a(minor_triad  , tonic=0, name='i'),
    a(minor_triad+5, tonic=0, name='iv'),
    a(minor_triad+7, tonic=0, name='v'),
    a(minor_triad  , tonic=0, name='i', include_label=FALSE)
  )
}
phrygian_triad_progression <- function(){
  phrygian_triad = -c(0,4,7)
  list(
    a(phrygian_triad  , tonic=0, name='-i'),
    a(phrygian_triad-5, tonic=0, name='-iv'),
    a(phrygian_triad-7, tonic=0, name='-v'),
    a(phrygian_triad  , tonic=0, name='-i', include_label=FALSE)
  )
}
voice_leading_progression <- function(){
  major_triad = c(0,4,7)
  list(
    a(major_triad  ),
    a(major_triad+1),
    a(major_triad+2),
    a(major_triad+3),
    a(major_triad+4),
    a(major_triad+5),
    a(major_triad+6),
    a(major_triad+7),
    a(major_triad+8),
    a(major_triad+9),
    a(major_triad+10),
    a(major_triad+11),
    a(major_triad+12),
    a(major_triad+13),
    a(major_triad  )
  )
}

#################
# tonic chords
#
lydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "II"=c(2,6,9),
       "iii"=c(4,7,11),
       "iv\u00B0"=c(6,9,12),
       "V"=c(7,11,14),
       "vi"=c(9,12,16),
       "vii"=c(11,14,18)
  )
}
ionian_tonic_chords <- function() {
  list("I"         =a(c( 0, 4, 7)   ,tonic=0, name='I'),
       "ii"        =a(c( 2, 5, 9)   ,tonic=0, name='ii'),
       "iii"       =a(c( 4, 7,11)   ,tonic=0, name='iii'),
       "IV"        =a(c( 5, 9,12)   ,tonic=0, name='IV'),
       "V"         =a(c( 7,11,14)   ,tonic=0, name='V'),
       "vi"        =a(c( 9,12,16)   ,tonic=0, name='vi'),
       "vii\u00B0" =a(c(11,14,17)   ,tonic=0, name='vii\u00B0'),
       "VIII"      =a(c( 0, 4, 7)+12,tonic=0, name='VIII')
  )
}
mixolydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii\u00B0"=c(4,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi"=c(9,12,16),
       "VII"=c(10,14,17)
  )
}
dorian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii"=c(2,5,9),
       "III"=c(3,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi\u00B0"=c(9,12,15),
       "VII"=c(10,14,17)
  )
}
aeolian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii\u00B0"=c(2,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v"=c(7,10,14),
       "VI"=c(8,12,15),
       "VII"=c(10,14,17)
  )
}
phrygian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "II"=c(1,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v\u00B0"=c(7,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
locrian_tonic_chords <- function() {
  list("i\u00B0"=c(0,3,6),
       "II"=c(1,5,8),
       "iii"=c(3,6,10),
       "iv"=c(5,8,12),
       "V"=c(6,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
salzer_schachter_1.1.a <- function() {
  list(a(0),
       a(2),
       a(0),
       a(-1),
       a(0),
       a(2),
       a(0))
}
salzer_schachter_1.1.b <- function() {
  list(a(0),
       a(2),
       a(5),
       a(4),
       a(2),
       a(5),
       a(4),
       a(5),
       a(4),
       a(2),
       a(0))
}
salzer_schachter_1.1.c <- function() {
  list(a(0),
       a(2),
       a(5),
       a(4),
       a(9),
       a(7),
       a(5),
       a(2),
       a(4),
       a(2),
       a(0)
  )
}
salzer_schachter_1.2 <- function() {
  list(a(0),
       a(9),
       a(4),
       a(11),
       a(7),
       a(16),
       a(12),
       a(9),
       a(11),
       a(7),
       a(2),
       a(0))
}
salzer_schachter_1.3 <- function() {
  list(a(0),
       a(2),
       a(3),
       a(5),
       a(7),
       a(8),
       a(7),
       a(5),
       a(3),
       a(2),
       a(0))
}

major_triad_tonic = major_triads()[["root position"]]
major_triad_first_inversion = major_triads()[["1st inversion"]]
major_triad_second_inversion = major_triads()[["2nd inversion"]]

minor_triad_tonic = minor_triads()[["root position"]]
minor_triad_first_inversion = minor_triads()[["1st inversion"]]
minor_triad_second_inversion = minor_triads()[["2nd inversion"]]

locrian = diatonic_scales()[['locrian']]
