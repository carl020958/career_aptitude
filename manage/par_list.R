setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('tidyverse')

# -------------
# 신청자 정보
# -------------
par_list <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=329286851', 
  sheet = '설문지 응답 시트1')

names(par_list)[5] = '개인정보_동의'

par_list <- par_list %>% 
  filter(개인정보_동의 == '동의함')

# --------------------------
# 중복 신청한 참여자들 제거
# --------------------------
# 가장 마지막으로 신청한 것을 남기기 
last_element_list <- par_list %>% 
  rename(std_id = "5. 학번") %>% 
  select(std_id) %>% 
  mutate(n = 1:n()) %>% 
  group_by(std_id) %>% 
  filter(n() > 1) %>%
  slice(which.max(n)) %>% #가장 마지막 n만 남김
  ungroup() %>% 
  pull(n)

# 중복자 명단
duplicated_delete_list <- par_list %>% 
  rename(std_id = "5. 학번") %>% 
  select(std_id) %>% 
  mutate(n = 1:n()) %>% 
  group_by(std_id) %>% 
  filter(n() > 1) %>% 
  filter(!n %in% last_element_list) %>% 
  pull(n)

# 전체 명단에서 중복자 명단 지우기
par_list <- par_list %>% 
  mutate(n = 1:n()) %>% 
  filter(!n %in% duplicated_delete_list) %>% 
  select(-n) %>%
  rename(성명 = '1. 성명',
         학번 = '5. 학번') %>% 
  mutate(Name = paste0(성명, str_sub(학번, -4, -1))) %>% 
  relocate(Name, .before = 타임스탬프)

#최종 확인(MUST BE TRUE)
dim(par_list %>% select(학번) %>% unique())[1] == dim(par_list)[1]

# ------
# write
# ------
par_list %>% 
  googlesheets4::sheet_write(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=329286851',
  sheet = '등록')

