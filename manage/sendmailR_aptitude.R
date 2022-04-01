# --------
# setting
# --------
# Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home') #ZSU_17
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home') #ZSU_15

#JAVA 8로 환경 변수 설정되어 있는지 확인
Sys.getenv('JAVA_HOME')

pacman::p_load('rJava', 'mailR', 'devtools', 'tidyverse', 'googlesheets4')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ----------
# email 내용
# ----------
email_inhalt <- 
'
안녕하세요, 고려대학교 학생상담센터입니다. <br><br>

진로 적성검사 프로그램에 참여하신 학우님을 환영합니다. :D <br>

또한, 검사 준비로 인해 검사 안내가 늦어져 죄송합니다. <br><br>

본 프로그램 참여자 분은 총 4가지 검사

<span style = "text-decoration: underline; font-weight: bold;">
<다요인 적성검사>, <정서교양 검사>, <성격의 자화상 검사>, 그리고 <적응기제 검사>
</span>
를 실시하시게 됩니다. <br>

<span style = "font-weight: bold;">
이 중 다요인 적성검사와 정서교양 검사는 ‘역량’ 측정검사로 이 두 검사는 일정한 제한 시간내에 응답을 하도록 구성되어 있습니다. 
</span>
<br>
<span style = "font-weight: bold; color: red;">
검사가 시작되면 중간에 멈추거나 다시 돌아갈 수 없으니, 반드시 충분한 여유시간이 있을 때 독립된 장소에서 PC를 사용해서 최선을 다해 응답
</span>
해주시길 바랍니다. <br><br>

<span style = "font-weight: bold;">
1) <다요인 적성검사>
</span>
는 인지 역량을 측정하는 9개의 소검사로 진행됩니다. <br>
객관식 296문항으로 구성, 
<span style = "text-decoration: underline">
약 60 분
</span>
(제한)이 소요 됩니다. <br>
검사 시작 후에는 이전으로 돌아가거나 건너 뛸 수 없으므로 충분한 시간을 갖고 검사에 임하여 주시기 바랍니다.
<br>
* 링크: <a href="https://kupsychology.qualtrics.com/jfe/form/SV_4Nv5cwhFqINqkgm">다요인 적성 검사 링크</a>
<br><br>

<span style = "font-weight: bold;">
2) <정서교양 검사>
</span>
는 정서지각과 정서조절 두 가지 영역을 평가하는 검사로, 객관식 40문항으로 구성, 
<span style = "text-decoration: underline">
약 15분(제한)
</span>
이 소요됩니다. <br>
각 검사는 시트마다 할당된 시간이 지나야 다음 진행이 가능하므로, 독립된 PC 환경에서 진행하여 주시기 바랍니다.
<br>

* 링크: <a href="https://kupsychology.qualtrics.com/jfe/form/SV_a93H5hDazFMuYCO">정서교양 검사 링크</a>
<br><br>

<span style = "font-weight: bold;">
3) <성격의 자화상 검사> 및 <적응기제 검사>
</span>
성격의 자화상 검사는 객관식 177문항으로 구성, 
<span style = "text-decoration: underline">
약 20분 
</span>
정도가, 적응기제 검사는 객관식 125문항으로 구성, 
<span style = "text-decoration: underline">
약 15분 
</span>
정도가 소요됩니다. (총 약 35분)
<br>
* 링크: <a href="https://kupsychology.qualtrics.com/jfe/form/SV_5dTFcwOzSm3XGjI"> 성격의 자화상 및 적응기제 검사 링크</a>
<br><br>

본 메일 속
<span style = "font-weight: bold; color: blue;">
4가지 검사
</span>
결과를 바탕으로 해석상담(온라인/50분)이 진행됩니다. <br>
<span style = "font-weight: bold; color: red;">
모든 검사에 대한 응답을 완료하신 분을 대상으로 학생상담센터 진로  적성검사 프로그램 담당자가 해석상담 일정 예약
</span>
을 위해 연락드릴 것입니다. <br><br>

검사 진행 및 기타 궁금하신 사항은 이메일(kulifementoring@gmail.com)로 연락부탁드립니다. <br>
학생상담센터 진로적성 검사 프로그램 담당자가 회신해드릴 것입니다. <br>
감사합니다 :) <br><br>

학생상담센터 진로적성 검사 프로그램 담당자 드림

'

# ------------------
# list to send mail
# ------------------
sent_email <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
    sheet = '발송명단') %>% pull(email)

d <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
    sheet = '등록') %>% 
  rename(email = '이메일 주소') %>% 
  filter(!email %in% sent_email)

# ------------
# check email
#-------------

par <- d %>% 
  select(email)
   

par <- par %>% mutate(email = gsub(' ', '', email))

par <- par %>% 
  mutate(email = gsub(' ', '', email),
         email = gsub(',', '.', email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, '\\.con'))

par <- par %>%
  mutate(email = gsub('\\.con', '\\.com', email))

#check email
rbind(
    par %>% filter(str_detect(email, 'kroea\\.')),
    par %>% filter(str_detect(email, 'gamil\\.')),
    par %>% filter(str_detect(email, 'kprea\\.')),
    par %>% filter(str_detect(email, 'korra\\.')),
    par %>% filter(str_detect(email, 'korrea\\.')),
    par %>% filter(str_detect(email, 'koreq\\.')),
    par %>% filter(str_detect(email, 'korae\\.')),
    par %>% filter(str_detect(email, '\\.ke')),
    par %>% filter(str_detect(email, 'naveer\\.')),
    par %>% filter(str_detect(email, 'navaer\\.')),
    par %>% filter(str_detect(email, '\\.acc\\.kr')),
    par %>% filter(str_detect(email, '\\.ackr')),
    par %>% filter(str_detect(email, 'ac\\.jr')),
    par %>% filter(str_detect(email, 'korea\\.com')),
    par %>% filter(str_detect(email, 'korea\\.c\\.kr')),
    par %>% filter(str_detect(email, 'ac\\.mr')),
    par %>% filter(str_detect(email, '\\.ackr')),
    par %>% filter(str_detect(email, 'ac\\.r')),
    par %>% filter(str_detect(email, 'navee\\.')),
    par %>% filter(str_detect(email, 'portal\\.korea')),
    par %>% filter(str_detect(email, 'korea\\.co\\.kr')),
    par %>% filter(str_detect(email, '[가-힣]')),
    par %>% filter(!str_detect(email, '@')),
    par %>% mutate(string_length = nchar(email)) %>% 
      filter(string_length < 12) %>% 
      select(-string_length)) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

par <- par %>% 
  mutate(email = case_when(email == 'recht0316@korea.ac.ke' ~ 'recht0316@korea.ac.kr',
                           T ~ email))

par <- 
  rbind(tibble(email = 'js94park@naver.com', n = 0), par) %>% 
  mutate(title = '[고려대학교 학생상담센터] 진로 적성검사 프로그램 검사 안내',
         inhalt = email_inhalt)

# ------------------------
# send to the first email
# ------------------------
{
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[1], # 메일제목
            body         = par$inhalt[1],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = 'erfolgreich10!!',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            html = TRUE,
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            # attach.files = c(par$file_directory[1], par$file_directory_2[1]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                    
  
  
  print(paste0(1, 'th email sent to [', par$email[1],'], ', length(par$email) - 1, ' email left'))
}

# ----
# send
# ----
for(i in 1:length(par$email)){
  
  
  
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[i], # 메일제목
            body         = par$inhalt[i],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = 'erfolgreich10!!',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            # attach.files = c(par$file_directory[i], par$file_directory_2[i]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                
  
  
  print(paste0(i, 'th email sent to [', par$email[i],'], ', length(par$email) - i, ' email left'))
  
}

# ------------
# update list
# ------------
bind_rows(
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
  sheet = '발송명단'),
  par %>% 
  filter(email != 'js94park@naver.com') %>% 
  select(-c(title, inhalt))
  ) %>% 
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
    sheet = '발송명단')
  
