setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('magrittr')

haven::read_spss("학부생_적성.sav") %>% 
  full_join(haven::read_spss("학부생_정서.sav"), by = 'Name') %>% 
  googlesheets4::write_sheet(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=520256541',
  sheet = 'aptitude_emotion_converted'
  )