setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('tidyverse')

googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=520256541',
  sheet = 'aptitude_emotion'
  ) %>% 
  select(Name, 공간지각, 공통성, 동형찾기, 모양, 산수, 상식, 숫자, 어휘, 짝짓기) %>%
  filter(!is.na(공간지각)) %>% 
  haven::write_sav("./processed/학부생_적성.sav")

haven::read_sav("./processed/학부생_적성.sav")

googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=520256541',
  sheet = 'aptitude_emotion'
  ) %>% 
  select(Name, 정서조절, 정서지각) %>% 
  filter(!is.na(정서조절)) %>% 
  haven::write_sav("./processed/학부생_정서.sav")

haven::read_sav("./processed/학부생_정서.sav")