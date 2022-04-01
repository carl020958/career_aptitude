setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('tidyverse')


# --------------------
# import data & index
# --------------------

index <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1RjLoxineZI6VHRADgqScuem_u7vg6PZxJGTc6puNnG0/edit#gid=0', 
    sheet = 'index')

par_list <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500', 
  sheet = '등록') %>% 
  select(Name)

d_1 <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0', 
  sheet = '적응기제_성격의자화상')

d_2 <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0', 
  sheet = '적성검사')

d_3 <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0', 
    sheet = '정서')


# ===========================================
# 데이터 1차 전처리(raw의 복수, 무응답 제거) 
# ===========================================

# -----------
# self-report
# -----------
# names(d_1)[ncol(d_1)]
raw_processed_1 <-
  d_1 %>% 
  filter(!is.na('125. 나는 주변 사람들로부터 충분히 인정받고 있다.')) %>% 
  distinct(Name, .keep_all = T)

raw_1 <- bind_cols(raw_processed_1[, 1], 
                   raw_processed_1[, 27:ncol(d_1)])

# par_list와 정신건강 간 학번 상이 여부 확인
check_1 <- 
  raw_1 %>% 
  left_join(par_list %>% mutate(확인 = '확인'), by = 'Name') %>% 
  select(Name, 확인) %>% 
  filter(is.na(확인)) %>% 
  pull(Name)

check_1

# raw_processed_1 %>%
#   filter(Name %in% check_1) %>%
#   View()


# ---------
# 적성검사 
# ---------
raw_processed_2 <- 
  d_2 %>% 
  # filter(!is.na('Timing - 클릭 수...529')) %>% 
  distinct(Name, .keep_all = T)

# 변수명 변경
raw_processed_2_questions <- raw_processed_2[, 42:529]

names(raw_processed_2_questions) <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1RjLoxineZI6VHRADgqScuem_u7vg6PZxJGTc6puNnG0/edit#gid=0',
  sheet = 'quest'
  ) %>% 
  filter(assessment == 'aptitude_ver3') %>% 
  pull(questions)

raw_2 <- bind_cols(raw_processed_2[, 1],
                   raw_processed_2_questions)

# 0 이여야함
dim(raw_2 %>% filter(is.na(동형찾기1)))[1]

# par_list와 정신건강 간 학번 상이 여부 확인
check_2 <- 
  raw_2 %>% 
  left_join(par_list %>% mutate(확인 = '확인'), by = 'Name') %>% 
  select(Name, 확인) %>% 
  filter(is.na(확인)) %>% 
  pull(Name)

check_2

# raw_processed_2 %>%
#   filter(Name %in% check_2) %>%
#   View()

# ---------
# 정서교양 
# ---------
raw_processed_3 <- 
  d_3 %>% 
  # filter(!is.na('Timing - 클릭 수...77')) %>% 
  distinct(Name, .keep_all = T)

raw_3 <- bind_cols(raw_processed_3[, 1] ,
                   raw_processed_3[, 30:57], raw_processed_3[, 62:73])

# par_list와 정신건강 간 학번 상이 여부 확인
check_3 <- 
  raw_3 %>% 
  left_join(par_list %>% mutate(확인 = '확인'), by = 'Name') %>% 
  select(Name, 확인) %>% 
  filter(is.na(확인)) %>% 
  pull(Name)

check_3

# raw_processed_3 %>%
#   filter(Name %in% check_3) %>%
#   View()



# ===========
# merge data
# ===========

# --------------------------
# merge data 1(자기보고식) 
# --------------------------
raw_processed_num <-
  par_list %>%
  left_join(raw_1, by = 'Name')

data_processed_num <-
  raw_processed_num %>%
  pivot_longer(-Name, names_to = 'item', values_to = 'value') %>%
  filter(!is.na(value)) %>%
  mutate(value = as.numeric(substr(value, 1, 1))) %>%
  inner_join(index, by = 'item') %>%
  mutate(value = case_when(r == 'r' ~ likert_max + likert_min - value,
                           TRUE ~ value)) %>%
  group_by(Name, scale) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'scale', values_from = 'value')


# ---------------------------
# merge data 2(문제풀이 형)
# ---------------------------
raw_processed_solve <-
  raw_2 %>%
  full_join(raw_3, by = 'Name') %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-Name, names_to = 'questions', values_to = 'value') %>%
  mutate(value = substr(value, 1, 1),
         value = case_when(value %in% c('①', '➀') ~ '1',
                           value %in% c('②', '➁') ~ '2',
                           value %in% c('③', '➂') ~ '3',
                           value %in% c('④', '➃') ~ '4',
                           value %in% c('⑤', '➄') ~ '5',
                           T ~ value)) %>%
  left_join(
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1RjLoxineZI6VHRADgqScuem_u7vg6PZxJGTc6puNnG0/edit#gid=0',
      sheet = 'quest'
    ) %>% 
      select(-assessment), by = 'questions') %>%
  filter(!is.na(answer)) %>%
  filter(!is.na(value)) %>%
  filter(value %in% c(1,2,3,4,5)) %>% 
  mutate(correct = case_when(value == answer ~ 1,
                            T ~ 0)) %>%
  group_by(Name, scale) %>%
  summarise(value = sum(correct)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'scale', values_from = 'value') %>%
  select(Name, 공간지각, 공통성, 동형찾기, 모양, 산수, 상식, 숫자, 어휘, 짝짓기, 정서조절, 정서지각)

raw_processed_solve %>%
  googlesheets4::sheet_write(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=0',
    sheet = 'aptitude_emotion'
  )


# ======
# write 
# ======

data_processed_num %>% 
  googlesheets4::sheet_write(
  'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=520256541',
  sheet = 'final_num'
)

