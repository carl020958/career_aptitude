###21_1_undergrad_career

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('flexdashboard', 'kableExtra', 'plotly', 'tidyverse', 'highcharter', 'reactable', 'htmltools')

# ------------
# 수검자 세팅
# ------------
# parID <- '홍길동0041'
parName <- substr(parID, 1, nchar(parID)-4)

# ------------
# 디자인 세팅
# ------------
colSet <- c('#ff7c80','#ffcd2d','#f8cbad','#99ccff','#dae9f6','#99cc00')

# -----------
# 함수 세팅
# -----------
# .my_color_bar <- 
#   function(color, width){
#     paste0('<span style="display: inline-block; direction: ltr; 
#          unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; 
#          background-color:', color,'; width: ', width, '%">', width,'</span>')
#   }

.bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# ------------
# 문항 꾸러미
# ------------
# 적성 검사
cog_test <- c("환산_공간지각", "환산_짝짓기", "환산_모양", "환산_산수", 
              "환산_상식", "환산_어휘", "환산_공통성", "환산_숫자", "환산_동형찾기")
cog_index <- c("환산_언어이해", "환산_지각추론", "환산_작업기억", "환산_처리속도")

# 적성 big data
job_list <- c('대학교수', '교사(초, 중등)', '관리, 사무직', '기업경영(예. CEO, 대표이사)', 
              '법조계(변호사, 판사, 검사)', '건축가', '의사', '간호사', '공무원(행정고시 5급 공채)', 
              '공무원(기술고시 5급 공채)', '공무원(외무고시 5급 공채)','공무원(7급, 9급 공채)', 
              '연구원', '전문자격직(예. 회계사, 세무사, 관세사, 변리사)', 
              '금융권(예. 은행, 증권사, 딜러)', 'IT 관련직(예. 개발자, 빅데이터 엔지니어)', 
              '크리에이터, 유튜버', '서비스업', '언론인', '엔터테인먼트 산업')


# 정서교양 검사
emo <- c('정서지각_환산점수', '정서조절_환산점수')

# 성격의 자화상
personality_portrit <- c("안전형_Bright",	"안전형_Dark", 
                         "수도형_Bright",	"수도형_Dark", 
                         "개성형_Bright",	"개성형_Dark", 
                         "모험형_Bright",	"모험형_Dark", 
                         "열정형_Bright",	"열정형_Dark", 
                         "사교형_Bright",	"사교형_Dark", 
                         "스타형_Bright",	"스타형_Dark", 
                         "모범형_Bright",	"모범형_Dark", 
                         "가족형_Bright",	"가족형_Dark", 
                         "지지형_Bright",	"지지형_Dark")

def_mechan<- c("적응기제_예상", "적응기제_억제", "적응기제_유머", 
               "적응기제_이타주의", "적응기제_승화", 
               "적응기제_억압", "적응기제_반동형성", "적응기제_이지화", "적응기제_전위", 
               "적응기제_신체화", "적응기제_공상", "적응기제_투사", 
               "적응기제_수동공격", "적응기제_해리", "적응기제_행동화")


# # ------------
# # import data 
# # ------------
# {
#   # participant data
#   d <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
#       sheet = '등록') %>% 
#     select(Name, '8. 전공(제 1전공)', '10. 희망 직무', '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)')
#    
#   names(d) <- c('Name', '소속학과', '희망_직무', '희망_직군')
# 
#   ### job_group big data
# 
#   # data1
#   job_group <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/112NQ56xz97FTCiRUX38_AWb6XTPJgyWDtzo90UwEBcQ/edit#gid=0',
#       sheet = '시트1'
#     )
#   names(job_group) <- c('id', '취업정보_직군', '취업정보1_회사', '직군_분류', '비고', '검토요망')
#   
#   # 희망_직군에 주관식 입력한 사람, 최대한 유사한 것으로 바꾸기
#   ## 희망직군 업데이트 및 수정 필요할듯, 최종적으로는 희망_직무 & 희망_직군으로 결과지 주는 것이 좋을 듯 ##
#   d <- 
#     d %>% 
#     left_join(
#       googlesheets4::read_sheet(
#         'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=1408260580',
#         sheet = '시트4'
#       ),
#       by = c('희망_직군' = '입력')
#     ) %>% 
#     mutate(희망_직군_최종 = ifelse(is.na(변환), 희망_직군, 변환)) %>% 
#     transmute(Name, 소속학과, 희망_직무, 희망_직군 = 희망_직군_최종)
# 
#   # TRUE여야 함!! 아닌 경우, 응답한 희망직종이 객관식 문항에 없는 것이여서 수정 필요
#   dim(d %>%
#     filter(!is.na(희망_직군)) %>%
#     filter(!희망_직군 %in% job_list))[1] == 0
# 
#   # data2
#   job_data <-   
#   readxl::read_excel('/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/적성/pre_process/job_group/(통합) 졸업생 취업현황.xlsx',
#                      sheet = 'Sheet1') # ZSU15
# 
#   ### cognitive assessment data
# 
#   # cognitive assessment big data
#   cog_job <-
#     googlesheets4::read_sheet(
#     'https://docs.google.com/spreadsheets/d/1EoGE01oEdd-zpxXLZWAmcPhFMYV4M7YeQPFEZ3NX93U/edit#gid=0',
#     sheet = '시트1'
#     )
# 
#   cog_job_df <-
#     cog_job %>%
#     select('8. 소속 대학원 및 학부',
#            '9. 전공(제 1전공)',
#            '다음의 직업군 중 자신이 가장 희망하는 직업군을 선택해 주십시오.',
#            '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)',
#            '환산_공간지각',	'환산_짝짓기', '환산_모양',	'환산_산수',
#            '환산_상식',	'환산_어휘', '환산_공통성', '환산_숫자', '환산_동형찾기',
#            '환산_언어이해',	'환산_지각추론',	'환산_작업기억',	'환산_처리속도',	'전체_IQ') %>%
#     rename(소속 = '8. 소속 대학원 및 학부',
#              전공 = '9. 전공(제 1전공)',
#              희망_직업군 = '다음의 직업군 중 자신이 가장 희망하는 직업군을 선택해 주십시오.',
#              희망_직군_1순위 = '(1순위) 다음의 직업군 중 자신이 희망하는 직군을 고르시오. (택1)'
#     )
# 
#   # current participant cognitive assessment & emotion data
#   cog_df <-
#   googlesheets4::read_sheet(
#     'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=379313781',
#     sheet = 'aptitude_emotion_converted')
# 
#   cog_df[is.na(cog_df)] <- 0
# 
# 
#   # self report data
#   num_d <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=379313781',
#       sheet = 'final_num')
# 
# }


# --------
# data set
# --------
{
  job_df <- 
    bind_cols(job_data, 
              job_group %>% select(직군_분류)) %>% 
    filter(!is.na(취업정보1_회사)) 
  
  long <- 
    num_d %>%
    pivot_longer(가족형_Bright:지지형_Dark, names_to = 'cate', values_to = 'value') %>%
    mutate(value = as.numeric(value)) %>%
    group_by(cate) %>%
    mutate(percentile = round(percent_rank(value)*100,1),
           meanByCate = mean(value, na.rm=T),
           medianByCate = median(value, na.rm=T)) %>%
    ungroup()
}

# =====
# plots
# =====

# ---------
# 적성 검사
# ---------

# 적성 검사 데이터 준비
{
  par_job_group <- 
    d %>% 
    filter(Name == parID) %>% 
    pull(희망_직군)
  
  par_interest_job <- 
    cog_job_df %>% 
    group_by(희망_직군_1순위) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = T))) %>% 
    filter(희망_직군_1순위 == par_job_group) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
    rename_all(~ c('Name', cog_test, cog_index, '전체_IQ'))
  
  par_cog <- 
    cog_df %>% 
    filter(Name == parID) %>% 
    select(Name, contains('환산'), 전체IQ) %>% 
    select(-c(정서지각_환산점수, 정서조절_환산점수)) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
    rename_all(~ c('Name', cog_test, cog_index, '전체_IQ'))
    
}

# 적성 검사(소검사)
{
  par_interest_job_subtest <- 
    par_interest_job %>% 
    select(Name, cog_test) %>%
    pivot_longer(환산_공간지각:환산_동형찾기, names_to = "cate", values_to = "value") %>%
    separate(cate, sep = "_", , into = c("환산", "cate")) %>% 
    select(-환산)
  
  par_cog_subtest <- 
    par_cog %>% 
    select(Name, cog_test) %>%
    pivot_longer(환산_공간지각:환산_동형찾기, names_to = "cate", values_to = "value") %>%
    separate(cate, sep = "_", , into = c("환산", "cate")) %>% 
    select(-환산)
    
  cogPlot <- 
  par_cog_subtest %>% 
    plot_ly(
      x = ~ cate,
      y = ~ value,
      name = '본인',
      text = '본인',
      type = 'scatter',
      mode = 'lines + markers ',
      line = list(dash = 'line',
                  color = '#990000',
                  width = 2,
                  simplfy = F),
      marker = list(color = '#990000'),
      hovertemplate = paste(
        "<b>본인</b><br>",
        "소검사: <b>%{x}</b><br>",
        "점수: <b>%{y: .0f}점</b><extra></extra>"
      )
    ) %>% 
    
    add_trace(
      data = par_interest_job_subtest,
      x = ~ cate,
      y = ~ value,
      name = ~ Name,
      text = ~ Name,
      type = 'scatter',
      mode = 'lines + markers ',
      line = list(dash = "dot",
                  width = 2,
                  color = '#ADA28F',
                  simplyfy = F),
      marker = list(color = '#ADA28F'),
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "소검사: <b>%{x}</b><br>",
        "점수: <b>%{y: .0f}점</b><extra></extra>"
      )
    ) %>% 
    
    layout(
      title = "",
      bargap = 0.6,
      xaxis = list(title = "",
                   ticktext = list("공간지각", "짝짓기", "모양", "산수", "상식", "어휘", "공통성", "숫자", "동형찾기"),
                   tickvals = list(0,1,2,3,4,5,6,7,8),
                   showline = T),
      yaxis = list(side = 'left',
                   title = "",
                   overlaying = "y",
                   showgrid = T,
                   zeroline = F,
                   range = c(0,150)),
      legend = list(x = 100, y = 100),
      showlegend = T
    ) %>%
    
    add_text(
      data = par_cog_subtest,
      x = ~ cate,
      y = ~ value,
      text = ~ value,
      textfont = list(
        # family = "sans serif",
        size = 12,
        color = "#990000"),
      textposition = "top",
      showlegend = F
      ) %>%
    
    config(displayModeBar = F)
}


# 적성 검사 결과
{
  par_interest_job_index <- 
    par_interest_job %>% 
    select(Name, cog_index) %>%
    pivot_longer(환산_언어이해:환산_처리속도, names_to = "cate", values_to = "value") %>%
    separate(cate, sep = "_", , into = c("환산", "cate")) %>% 
    select(-환산)
  
  par_cog_index <- 
    par_cog %>% 
    select(Name, cog_index) %>%
    pivot_longer(환산_언어이해:환산_처리속도, names_to = "cate", values_to = "value") %>%
    separate(cate, sep = "_", , into = c("환산", "cate")) %>% 1
    select(-환산)
  
  cog_indexPlot <- 
    par_cog_index %>% 
    plot_ly(
      x = ~ cate,
      y = ~ value,
      name = '본인',
      text = '본인',
      type = 'scatter',
      mode = 'lines + markers ',
      line = list(dash = 'line',
                  color = '#990000',
                  width = 2,
                  simplfy = F),
      marker = list(color = '#990000'),
      hovertemplate = paste(
        "<b>본인</b><br>",
        "소검사: <b>%{x}</b><br>",
        "점수: <b>%{y: .0f}점</b><extra></extra>"
      )
    ) %>% 
    
    add_trace(
      data = par_interest_job_index,
      x = ~ cate,
      y = ~ value,
      name = ~ Name,
      text = ~ Name,
      type = 'scatter',
      mode = 'lines + markers ',
      line = list(dash = "dot",
                  width = 2,
                  color = '#ADA28F',
                  simplyfy = F),
      marker = list(color = '#ADA28F'),
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "소검사: <b>%{x}</b><br>",
        "점수: <b>%{y: .0f}점</b><extra></extra>"
      )
    ) %>% 
    
    layout(
      title = "",
      bargap = 0.6,
      xaxis = list(title = "",
                   ticktext = list("언어이해", "지각추론", "작업기억", "처리속도"),
                   tickvals = list(0,1,2,3),
                   showline = T),
      yaxis = list(side = 'left',
                   title = "",
                   overlaying = "y",
                   showgrid = T,
                   zeroline = F,
                   range = c(0,150)),
      legend = list(x = 100, y = 100),
      showlegend = F
    ) %>%
    
    add_text(
      data = par_cog_index,
      x = ~ cate,
      y = ~ value,
      text = ~ value,
      textfont = list(
        # family = "sans serif",
        size = 12,
        color = "#990000"),
      textposition = "top",
      showlegend = F
      ) %>%
    
    config(displayModeBar = F)
  
  
}

# 적성 검사 결과
{
  
  par_cog_result <- 
    par_cog %>% 
    pivot_longer(환산_공간지각:전체_IQ, names_to = "cate", values_to = "value") %>%
    separate(cate, sep = "_", , into = c("환산", "cate")) %>%
    select(-환산) %>%
    mutate(구분 = case_when(value > 0 & value < 70 ~ "매우 낮음",
                          value >= 70 & value < 80 ~ "낮음",
                          value >= 80 & value < 90 ~ "평균하",
                          value >= 90 & value < 110 ~ "평균",
                          value >= 110 & value < 120 ~ "평균상",
                          value >= 120 & value < 130 ~ "높음",
                          value >= 130 ~ "매우 높음",
                          T ~ "응답없음")) %>%
    select(-c(Name)) %>%
    mutate(분류 = c("소척도", "소척도", "소척도", "소척도",
                  "소척도", "소척도", "소척도", "소척도", "소척도",
                  "지수", "지수", "지수", "지수",
                  "전체")) %>%
    transmute(분류, 척도 = cate, 점수 = value, 구분)
    
  cog_kbl <-
    par_cog_result %>%
    knitr::kable(align = c('c', 'c', 'c', 'c')) %>%
    kable_styling(bootstrap_options = c("condensed", "responsive", "hover", "striped")) %>%
    collapse_rows(columns = 1) %>%
    row_spec(0, background = "#990000" , color = "white") %>% 
    column_spec(1, popover = paste0('척도: ', par_cog_result$척도, '<br>',
                                    '점수: ', par_cog_result$점수, '점 <br>',
                                    '구분: ', par_cog_result$구분))
}

# 적성 검사 프로파일 유사도
{
  
    binded <- 
      bind_cols(
        cog_job_df %>% 
          rename(직군 = 희망_직군_1순위) %>% 
          group_by(직군) %>% 
          summarise(across(where(is.numeric), ~ mean(.x, na.rm = T))) %>% 
          ungroup() %>% 
          pivot_longer(-직군, names_to = 'index', values_to = 'value') %>% 
          filter(index %in% c('환산_공간지각', '환산_짝짓기', '환산_모양', '환산_산수', 
                              '환산_상식', '환산_어휘', '환산_공통성', '환산_숫자', '환산_동형찾기')) %>%
          pivot_wider(names_from = '직군', values_from = 'value') %>% 
          select(-index),
                
        par_cog %>% 
          mutate(Name = '본인') %>% 
          pivot_longer(-Name, names_to = 'index', values_to = 'value') %>% 
          filter(index %in% c('환산_공간지각', '환산_짝짓기', '환산_모양', '환산_산수', 
                              '환산_상식', '환산_어휘', '환산_공통성', '환산_숫자', '환산_동형찾기')) %>%
          pivot_wider(names_from = 'Name', values_from = 'value') %>% 
          select(-index)
      )

    par_cos_df <- 
      bind_cols(
      tibble(직무 = names(lsa::cosine(as.matrix(binded))[, 23])),
      tibble(cosine = lsa::cosine(as.matrix(binded))[, 23])
      ) %>% 
      filter(cosine != 1) %>% 
      arrange(desc(cosine)) %>% 
      mutate(n = 1:n())
    
    cos_kbl <- 
      par_cos_df %>% 
      select(-cosine) %>% 
      slice(1:10) %>% 
      transmute(직무, 순위 = paste0(n, '위')) %>% 
      knitr::kable(align = c('c', 'c')) %>%
      kable_styling(bootstrap_options = c("condensed", "responsive", "hover", "striped")) %>%
      row_spec(0, background = "#990000" , color = "white") %>% 
      column_spec(1, popover = paste0('직무: ', par_cos_df$직무, '<br>',
                                      '순위: ', par_cos_df$n, '위 <br>'))

}

# -------------------
# 졸업생의 취업 현황
# -------------------

# 졸업생의 직무 및 취업 현황
{
  
  par_dept <-
    d %>%
    filter(Name == parID) %>%
    pull(소속학과)
  
  par_dept_job <- 
    job_df %>%
    filter(학과명 == par_dept) %>%
    select(취업정보1_회사, 직군_분류)
  
  # par_job_group_df  <- 
  #   par_dept_job %>% 
  #   filter(!is.na(직군_분류)) %>% 
  #   count(직군_분류) %>% 
  #   mutate(r = n / sum(n),
  #          r = r * 100,
  #          r = round(r, 2)) %>% 
  #   arrange(desc(r))
  
  par_dept_job_df <- 
    par_dept_job %>% 
    group_by(취업정보1_회사, 직군_분류) %>% 
    count() %>% 
    ungroup() %>% 
    filter(!is.na(직군_분류)) %>% 
    mutate(r = n / sum(n),
           r = r * 100,
           r = round(r, 3)) %>% 
    transmute(직무 = 직군_분류, 비율 = r, 직장 = 취업정보1_회사)
    
  # reactable(ver3)
  par_dept_job_RT <-
    reactable::reactable(
      par_dept_job_df,
      defaultSorted = '비율',
      groupBy = '직무',
      defaultColDef = colDef(
        align = 'center'
        # headerStyle = list(background = "#D4C9B5")
      ),
      columns = list(
        비율 = colDef(name = '비율 (%)',
                      aggregate = 'sum',
                      defaultSortOrder = 'desc',
                      align = 'left',
                      cell = function(value){
                        width = paste0(value / max(par_dept_job_df$비율) * 100, "%")
                        .bar_chart(value, width = width)
                    }
        )
      ),
      bordered = T,
      searchable = T,
      striped = T,
      highlight = T,
      showSortable = TRUE,
      language = reactableLang(
        searchPlaceholder = "검색",
        noData = "검색 결과 없음",
        pageInfo = "총 {rows}개의 결과 중 {rowStart}에서 {rowEnd} 사이의 결과",
        pagePrevious = "\u276e",
        pageNext = "\u276f",
        # pagePreviousLabel = "이전 페이지",
        # pageNextLabel = "다음 페이지"
      )
    )
}

# -------------
# 정서교양 검사
# -------------

# 정서 교양 검사
{
  par_emo <- 
    cog_df %>% 
    filter(Name == parID) %>% 
    select(정서지각_환산점수, 정서조절_환산점수) %>% 
    pivot_longer(정서지각_환산점수:정서조절_환산점수, names_to = "cate", values_to = "value") %>% 
    mutate(cate = str_replace(cate, "환산_", "")) %>% 
    arrange(match(cate, emo))
  
  emoPlot <- 
    highchart() %>%
    hc_chart(type = "column") %>%
    hc_colors(color = c(
      colSet[4],colSet[5])
    ) %>% 
    hc_plotOptions(column = list(
      pointPadding = 0.2,
      borderWidth = 0,
      colorByPoint =T)
    )  %>%
    hc_xAxis(
      categories = c("정서지각", "정서조절"),
      tickmarkPlacement = 'on',
      lineWidth = 0,
      labels = list(
        style = list(fontSize= '15px'))
    ) %>%
    hc_yAxis(
      min = 0,
      max = 150,
      labels = list(
        format = "{value}"
      )
    ) %>%
    hc_add_series(
      name = "본인",
      data = par_emo,
      hcaes(cate, value),
      marker = list(radius = 12),
      type = "column",
      
      dataLabels = list(
        format = "{point.y:.0f}",
        enabled = T,
        align= "center",
        verticalAlign= "below",
        color="white",
        style = list(
          fontFamily= 'noto',
          fontSize = '25px') # 막대
      )
    ) %>%
    hc_tooltip(crosshairs = T,
               enabled= T,
               headerFormat = "<b>{point.key}</b><br>",
               pointFormat = "본인점수: <b>{point.value}점</b><br>")
  
}

# --------------
# 성격의 자화상
# --------------

# 성격의 자화상
{
    personality_portrit_df <- long %>%
      filter(Name == parID & cate %in% personality_portrit) %>% 
      mutate(cate = str_replace(cate, '_', ''))
    
    pp_strength_df <- personality_portrit_df %>% 
      filter(grepl('Bright', cate)) %>% 
      mutate(cate = str_replace(cate, 'Bright', '')) 
    
    pp_weakness_df <-   personality_portrit_df %>% 
      filter(grepl('Dark', cate)) %>% 
      mutate(cate = str_replace(cate, 'Dark', '')) %>% 
      mutate(percentile = percentile*-1)
    
    ppPlot <- highchart() %>%
      hc_chart(type= 'bar') %>%
      hc_title(text= '') %>%
      hc_subtitle(text= '') %>%
      
      hc_xAxis(
        
        list(categories=pp_strength_df$cate,
             reversed=FALSE,
             labels=list(step= 1)),
        
        list(categories= pp_weakness_df$cate,
             opposite= TRUE,
             reversed= FALSE,
             linkedTo= 0,
             labels=list(step= 1))
      ) %>%
      
      hc_tooltip(
        shared = FALSE,
        formatter = JS("function () {
                     return this.point.category + '<br/>' +
                     '<b>' + this.series.name + '</b> ' +
                     '<b>' + ' 백분위:' + '</b> ' +
                     Highcharts.numberFormat(Math.abs(this.point.y), 1) + '%';}")
      ) %>%
      
      hc_yAxis(title= list(text= ''),
               crosshair = T,
               endonTick = T,
               tickInterval = 10,
               min = -100,
               max = 100,
               labels=list(formatter=JS("function () {
                 return Math.abs(this.value) + '%';
               }"))
      ) %>%
      
      
      hc_colors(color = c(colSet[1], colSet[4])
      ) %>%
      
      
      hc_plotOptions(column = list(
        colorByPoint = T)
      ) %>%
      
      hc_plotOptions(series=list(stacking= 'normal')
      ) %>%
      
      
      hc_series(
        list(name= 'Dark Side',
             data= pp_weakness_df$percentile),
        
        list(name= 'Bright Side',
             data= pp_strength_df$percentile
        ))
}

# --------
# 적응기제
# --------

# 적응기제
{
    def_mechan_d <- long %>%
      filter(Name == parID & cate %in% def_mechan) %>% 
      arrange(match(cate, def_mechan)) %>% 
      mutate(cate = stringr::str_replace(cate, "적응기제_", ""))
      
    
    def_mechanismPlot <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_colors(color = c(
        colSet[4], colSet[4], colSet[4], colSet[4], colSet[4],
        colSet[2], colSet[2], colSet[2], colSet[2],
        colSet[1], colSet[1], colSet[1], colSet[1], colSet[1], colSet[1]
      )
      ) %>% 
      hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0,
        colorByPoint =T)
      )  %>%
      hc_xAxis(
        crosshair = F,
        categories = def_mechan_d$cate,
        # title = list(text="적응 기제"),
        tickmarkPlacement = 'on',
        lineWidth = 1,
        labels = list(
          # enabled = F,
          style = list(fontSize= '12px'))
      ) %>%
      hc_yAxis(
        crosshair = T,
        fontsize = '8px',
        min = 0,
        max = 100,
        labels = list(
          format = "{value}%"
        )
      ) %>%
      hc_add_series(
        name = "본인",
        data = def_mechan_d,
        hcaes(cate, percentile),
        marker = list(radius = 12),
        type = "column",
        
        dataLabels = list(
          format = "{point.y:.0f}%",
          enabled = T,
          align= "center",
          verticalAlign= "middle",
          color="white",
          style = list(
            fontFamily= 'noto',
            fontSize='10px')
        )
      ) %>% 
      hc_tooltip(crosshairs = T,
                 enabled= T,
                 headerFormat = "<b>적응기제</b><br>",
                 pointFormat = "본인점수: <b>{point.value}점</b><br>
                   타인평균: {point.meanByCate:.1f}점")
}

