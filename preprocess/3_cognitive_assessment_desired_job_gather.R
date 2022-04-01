setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('tidyverse')

needed_column <- 
  c('Name', '프로그램', '타임스탬프', '소속 캠퍼스', '성명', '2. 국적', '3. 성별', '4. (만) 나이', '학번', '6. 핸드폰 번호', 
  '이메일 주소', '7. 소속 학부', '8. 전공(제 1전공)', '9. 학부 학년 과정', '10. 희망 직무', '직업군',
  '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)',
  '(2순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)', 
  '(3순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)')

googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit',
  '등록'
) %>% 
  mutate(프로그램 = '2021년도 여름방학 학생상담센터 온라인 진로 적성검사',
         직업군 = '') %>% 
  select(needed_column) %>% 
  left_join(
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=379313781',
      sheet = 'aptitude_emotion_converted'),
    by = 'Name'
  ) %>% 
  select(needed_column, 
         공간지각, 짝짓기, 모양, 산수, 상식, 어휘, 공통성, 숫자, 동형찾기,
         공간지각_환산점수, 짝짓기_환산점수, 모양_환산점수,	산수_환산점수, 상식_환산점수,	
         어휘_환산점수, 공통성_환산점수, 숫자_환산점수,	동형찾기_환산점수, 
         언어이해_환산점수, 지각추론_환산점수, 작업기억_환산점수, 처리속도_환산점수, 전체IQ) %>% 
  filter(!is.na(전체IQ)) %>% 
  writexl::write_xlsx('data.xlsx')

