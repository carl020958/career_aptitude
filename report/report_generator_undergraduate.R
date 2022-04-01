pacman::p_load('rmarkdown', 'tidyverse')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

job_list <- c('대학교수', '교사(초, 중등)', '관리, 사무직', '기업경영(예. CEO, 대표이사)', 
              '법조계(변호사, 판사, 검사)', '건축가', '의사', '간호사', '공무원(행정고시 5급 공채)', 
              '공무원(기술고시 5급 공채)', '공무원(외무고시 5급 공채)','공무원(7급, 9급 공채)', 
              '연구원', '전문자격직(예. 회계사, 세무사, 관세사, 변리사)', 
              '금융권(예. 은행, 증권사, 딜러)', 'IT 관련직(예. 개발자, 빅데이터 엔지니어)', 
              '크리에이터, 유튜버', '서비스업', '언론인', '엔터테인먼트 산업')

# ------------
# import data
# ------------
{
  # participant data
  d <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
      sheet = '등록') %>% 
    select(Name, '8. 전공(제 1전공)', '10. 희망 직무', '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)')
  
  names(d) <- c('Name', '소속학과', '희망_직무', '희망_직군')
  
  ### job_group big data
  
  # data1
  job_group <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/112NQ56xz97FTCiRUX38_AWb6XTPJgyWDtzo90UwEBcQ/edit#gid=0',
      sheet = '시트1'
    )
  names(job_group) <- c('id', '취업정보_직군', '취업정보1_회사', '직군_분류', '비고', '검토요망')
  
  # 희망_직군에 주관식 입력한 사람, 최대한 유사한 것으로 바꾸기
  ## 희망직군 업데이트 및 수정 필요할듯, 최종적으로는 희망_직무 & 희망_직군으로 결과지 주는 것이 좋을 듯 ##
  d <- 
    d %>% 
    left_join(
      googlesheets4::read_sheet(
        'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=1408260580',
        sheet = '시트4'
      ),
      by = c('희망_직군' = '입력')
    ) %>% 
    mutate(희망_직군_최종 = ifelse(is.na(변환), 희망_직군, 변환)) %>% 
    transmute(Name, 소속학과, 희망_직무, 희망_직군 = 희망_직군_최종)
  
  # TRUE여야 함!! 아닌 경우, 응답한 희망직종이 객관식 문항에 없는 것이여서 수정 필요
  dim(d %>%
        filter(!is.na(희망_직군)) %>%
        filter(!희망_직군 %in% job_list))[1] == 0
  
  # data2
  job_data <-   
    readxl::read_excel('/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/적성/pre_process/job_group/(통합) 졸업생 취업현황.xlsx',
                       sheet = 'Sheet1') # ZSU15
  
    # readxl::read_excel('/Users/jisu/Dropbox_Carl/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/적성/pre_process/job_group/(통합) 졸업생 취업현황.xlsx',
    #                    sheet = 'Sheet1') # ZSU17
  
  ### cognitive assessment data
  
  # cognitive assessment big data
  cog_job <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1EoGE01oEdd-zpxXLZWAmcPhFMYV4M7YeQPFEZ3NX93U/edit#gid=0',
      sheet = '시트1'
    )
  
  cog_job_df <-
    cog_job %>%
    select('8. 소속 대학원 및 학부',
           '9. 전공(제 1전공)',
           '다음의 직업군 중 자신이 가장 희망하는 직업군을 선택해 주십시오.',
           '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)',
           '환산_공간지각',	'환산_짝짓기', '환산_모양',	'환산_산수',
           '환산_상식',	'환산_어휘', '환산_공통성', '환산_숫자', '환산_동형찾기',
           '환산_언어이해',	'환산_지각추론',	'환산_작업기억',	'환산_처리속도',	'전체_IQ') %>%
    rename(소속 = '8. 소속 대학원 및 학부',
             전공 = '9. 전공(제 1전공)',
             희망_직업군 = '다음의 직업군 중 자신이 가장 희망하는 직업군을 선택해 주십시오.',
             희망_직군_1순위 = '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)'
    )
  
  # current participant cognitive assessment & emotion data
  cog_df <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=379313781',
      sheet = 'aptitude_emotion_converted')
  
  cog_df[is.na(cog_df)] <- 0
  
  
  # self report data
  num_d <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=379313781',
      sheet = 'final_num')
  
}

# TRUE여야 함!! 아닌 경우, 응답한 희망직종이 객관식 문항에 없는 것이여서 수정 필요
dim(d %>% 
      filter(!is.na(희망_직군)) %>% 
      filter(!희망_직군 %in% job_list))[1] == 0

# 적성검사 다한 사람 기준으로 결과지 제작
par <- 
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=470386533',
      sheet = 'aptitude_emotion_converted') %>% 
    filter(!is.na(공간지각))


for(i in 1:length(par$Name)) {
  
  parID <- par$Name[i]
    render(
      'reportPaper_undergrad_career.Rmd',
      # file 2
      output_file =  paste(parID, ".html", sep = ''),
      output_dir = './html'
    )  
}





