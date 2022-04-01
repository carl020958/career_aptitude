# --------
# setting
# --------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('tidyverse')


# ---------------------------------
# check data validation and upload
# ---------------------------------


dir_file <- fs::dir_ls('./data_raw')

par_list <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=329286851',
  sheet = '등록'
  ) %>%
  transmute(Name, 명단 = '명단')


# 적응기제_성격의자화상(적응기제)
# names(readxl::read_xlsx(dir_file[1]))[ncol(readxl::read_xlsx(dir_file[1]))]
dir_file[1]

# Name(이름, 학번 끝 4자리) 확인
readxl::read_xlsx(dir_file[1]) %>% 
  filter(!is.na(Q570)) %>%
  distinct(Q74, .keep_all = T) %>% 
  slice(-1) %>% 
  mutate(Name = paste0(Q71, str_sub(Q74, -4, -1))) %>% 
  left_join(par_list, by = 'Name') %>% 
  filter(is.na(명단)) %>% 
  select(Name, Q71, Q74)

# Write Googlesheet
readxl::read_xlsx(dir_file[1]) %>% 
  filter(!is.na(Q570)) %>%
  distinct(Q74, .keep_all = T) %>% 
  mutate(Name = paste0(Q71, str_sub(Q74, -4, -1))) %>% 
  mutate(Q71 = case_when(Name == '함은잔0579' ~ '함은진',
                          T ~ Q71)
         # ,
         # Q74 = case_when(Name == '' ~ '',
         #                 T ~ Q74)
         ) %>% 
  # slice(-1) %>%
  mutate(Name = paste0(Q71, str_sub(Q74, -4, -1))) %>% 
  relocate(Name, .before = StartDate) %>% 
  janitor::row_to_names(1) %>% 
  rename(Name = '1. 이름456)') %>% 
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0',
    sheet = '적응기제_성격의자화상'
  )

# 적성검사(적성검사)
dir_file[2]
# 적성은 마지막 소검사 첫번째 문항 기준

# Name(이름, 학번 끝 4자리) 확인
readxl::read_xlsx(dir_file[2]) %>% 
  filter(!is.na(Q456)) %>%
  distinct(Q629, .keep_all = T) %>% 
  slice(-1) %>% 
  mutate(Name = paste0(Q626, str_sub(Q629, -4, -1))) %>% 
  left_join(par_list, by = 'Name') %>% 
  filter(is.na(명단)) %>% 
  select(Name, Q626, Q629)

# Write Googlesheet
readxl::read_xlsx(dir_file[2]) %>% 
  filter(!is.na(Q456)) %>% 
  distinct(Q629, .keep_all = T) %>% 
  mutate(Name = paste0(Q608, str_sub(Q629, -4, -1))) %>% 
  # mutate(Q608 = case_when(Name == '' ~ '',
  #                        T ~ Q608)
         # ,
         # Q629 = case_when(Name == '' ~ '',
         #                 T ~ Q629)
  # ) %>% 
  mutate(Name = paste0(Q626, str_sub(Q629, -4, -1))) %>% 
  relocate(Name, .before = StartDate) %>% 
  mutate(Name = case_when(Name == '1. 이름456)' ~ 'Name',
                          T  ~ Name)) %>% 
  janitor::row_to_names(1) %>%
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0',
    sheet = '적성검사'
  )


# 정서교양검사(정서교양)
dir_file[3]
# 정서는 정서조절 첫번째 문항 기준

# Name(이름, 학번 끝 4자리) 확인
readxl::read_xlsx(dir_file[3]) %>% 
  filter(!is.na(Q86)) %>%
  distinct(Q99, .keep_all = T) %>% 
  slice(-1) %>% 
  mutate(Name = paste0(Q106, str_sub(Q99, -4, -1))) %>% 
  left_join(par_list, by = 'Name') %>% 
  filter(is.na(명단)) %>% 
  select(Name, Q106, Q99)


# Write Googlesheet
readxl::read_xlsx(dir_file[3]) %>% 
  filter(!is.na(Q86)) %>% 
  distinct(Q99, .keep_all = T) %>% 
  mutate(Name = paste0(Q106, str_sub(Q99, -4, -1))) %>% 
  mutate(Q106 = case_when(Name == '정지여0524' ~ '정지연',
                         T ~ Q106)
  # ,
  # Q629 = case_when(Name == '' ~ '',
  #                 T ~ Q629)
  ) %>%
  mutate(Name = paste0(Q106, str_sub(Q99, -4, -1))) %>% 
  relocate(Name, .before = StartDate) %>% 
  mutate(Name = case_when(Name == '1. 이름456)' ~ 'Name',
                          T  ~ Name)) %>% 
  janitor::row_to_names(1) %>%
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0',
    sheet = '정서'
  )

