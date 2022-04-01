# --------
# setting
# --------
# Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home') #ZSU_17
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home') #ZSU_15

#JAVA 8로 환경 변수 설정되어 있는지 확인
Sys.getenv('JAVA_HOME')

pacman::p_load('rJava', 'mailR', 'devtools', 'magrittr', 'tidyverse')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------
# list to send mail
# ------------------

par <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1i8FBDs53et1bGf2S-QPLw8b6Thl93a0KXfBgje9g0Qk/edit#gid=470386533',
    sheet = 'aptitude_emotion_converted') %>% 
  filter(!is.na(공간지각))

par_info <- 
  par %>% 
  select(Name) %>% 
  left_join(
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1ba1dbf_LDjTqdzzbWIs4QlsbDv5O1hj4v4BA2Mqc81Q/edit#gid=449282500',
      sheet = '등록') %>% 
    select(Name, '이메일 주소') %>% 
    rename(email = '이메일 주소'),
    by = "Name"
  )

#결과지 주소
list <- list.files(path = "./html",
                   full.names = T,
                   pattern = "*.html")

#최종 DF
par <- 
  par_info %>% 
  left_join(data.frame(file_directory = list) %>% 
            mutate(Name = str_extract(
              pattern = "(?<=\\.\\/html\\/).*(?=\\.html)", string = file_directory)) %>% 
            mutate(Name = stringi::stri_trans_nfc(Name)),
            by = "Name")
  

#확인용 만들기
example_dir <- par[1,3]

 par <- 
   bind_rows(
    data.frame(Name = "확인용", 
             email = "js94park@naver.com", 
             file_directory = example_dir), 
    par)

# ------
# check 
# ------

par <- par %>% mutate(email = gsub(" ", "", email))

par <- par %>% 
  mutate(email = gsub(" ", "", email),
         email = gsub(",", ".", email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, "\\.con"))

par <- par %>%
  mutate(email = gsub("\\.con", "\\.com", email))

#check email
rbind(
  par %>% filter(str_detect(email, "kroea\\.")),
  par %>% filter(str_detect(email, "gamil\\.")),
  par %>% filter(str_detect(email, "kprea\\.")),
  par %>% filter(str_detect(email, "korra\\.")),
  par %>% filter(str_detect(email, "korrea\\.")),
  par %>% filter(str_detect(email, "koreq\\.")),
  par %>% filter(str_detect(email, "korae\\.")),
  par %>% filter(str_detect(email, "\\.ke")),
  par %>% filter(str_detect(email, "naveer\\.")),
  par %>% filter(str_detect(email, "navaer\\.")),
  par %>% filter(str_detect(email, "\\.acc\\.kr")),
  par %>% filter(str_detect(email, "\\.ackr")),
  par %>% filter(str_detect(email, "ac\\.jr")),
  par %>% filter(str_detect(email, "korea\\.com")),
  par %>% filter(str_detect(email, "korea\\.c\\.kr")),
  par %>% filter(str_detect(email, "ac\\.mr")),
  par %>% filter(str_detect(email, "\\.ackr")),
  par %>% filter(str_detect(email, "ac\\.r")),
  par %>% filter(str_detect(email, "navee\\.")),
  par %>% filter(str_detect(email, "portal\\.korea")),
  par %>% filter(str_detect(email, "korea\\.co\\.kr")),
  par %>% filter(str_detect(email, "[가-힣]")),
  par %>% filter(!str_detect(email, "@")),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

#check file_directory
par %>% filter(is.na(email))
par %>% filter(is.na(file_directory))

#최종 점검
par %>% View()

# ------------------------
# send to the first email
# ------------------------
{
  send.mail(from         = "psy_test@naver.com",                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = "js94park@naver.com",                    # 참조
            # bcc           = "js94park@naver.com",                   # 숨은 참조
            subject      = "[고려대학교 학생상담센터] 온라인 적성검사 프로그램 검사 결과지", # 메일제목
            body         =
              "안녕하세요, 고려대학교 학생상담센터입니다. <br><br>

진로 적성검사 프로그램에 참여해주셔서 다시 한 번 감사드립니다. <br>
여러가지 검사에 참여하시느라 고생 많으셨습니다.<br>
검사 결과지는 첨부 파일에서 확인하실 수 있으시며 아래 사항을 참고해 파일을 열어보시기 바랍니다.  <br><br>

1. 본 결과지는 핸드폰으로는 열리지 않으므로 컴퓨터나 노트북을 이용하여 다운 받으십시오. <br>
2. 본 결과지는 구글 크롬으로 열어야 오류 없이 확인하실 수 있습니다. <br>
3. 인터넷 익스프롤러에서는 파일이 열리지 않으며 크롬, 파이어폭스, 사파리 등 그 외 대부분의 브라우저에서는 정상적으로 열립니다. <br><br>

해석상담은 9월 이후 진행될 예정이며, 해석상담 일정을 잡기 위해 연락을 다시 드릴 예정입니다. <br>
문의사항이 있으신 경우 psy_test@naver.com 으로 해주시기 바랍니다. <br><br>

감사합니다. <br><br>",                         # 메일내용
            smtp         = list(host.name = "smtp.naver.com",         # 메일서버 연동 정보
                                port = 587,
                                user.name = "psy_test@naver.com",
                                passwd = "***************",
                                ssl = TRUE),
            encoding     = "utf-8",                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            attach.files = c(par$file_directory[1]),
            # file.names = c("option"),
            # file.descriptions = c("option"),
            debug = F)
  
  print(paste0(1, "th email sent to [", par$email[1],"], ", length(par$email) - 1, " email left"))
}


# ----
# send
# ----
for(i in 1:length(par$email)){

  send.mail(from         = "psy_test@naver.com",                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = "js94park@naver.com",                    # 참조
            # bcc           = "js94park@naver.com",                   # 숨은 참조
            subject      = "[고려대학교 학생상담센터] 온라인 적성검사 프로그램 검사 결과지", # 메일제목
            body         =
              "안녕하세요, 고려대학교 학생상담센터입니다. <br><br>

진로 적성검사 프로그램에 참여해주셔서 다시 한 번 감사드립니다. <br>
여러가지 검사에 참여하시느라 고생 많으셨습니다.<br>
검사 결과지는 첨부 파일에서 확인하실 수 있으시며 아래 사항을 참고해 파일을 열어보시기 바랍니다.  <br><br>

1. 본 결과지는 핸드폰으로는 열리지 않으므로 컴퓨터나 노트북을 이용하여 다운 받으십시오. <br>
2. 본 결과지는 구글 크롬으로 열어야 오류 없이 확인하실 수 있습니다. <br>
3. 인터넷 익스프롤러에서는 파일이 열리지 않으며 크롬, 파이어폭스, 사파리 등 그 외 대부분의 브라우저에서는 정상적으로 열립니다. <br><br>

해석상담은 9월 이후 진행될 예정이며, 해석상담 일정을 잡기 위해 연락을 다시 드릴 예정입니다. <br>
문의사항이 있으신 경우 psy_test@naver.com 으로 해주시기 바랍니다. <br><br>

감사합니다. <br><br>",                         # 메일내용
            smtp         = list(host.name = "smtp.naver.com",         # 메일서버 연동 정보
                                port = 587,
                                user.name = "psy_test@naver.com",
                                passwd = "erfolgreich10!!",
                                ssl = TRUE),
            encoding     = "utf-8",                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            attach.files = c(par$file_directory[i]),
            # file.names = c("option"),
            # file.descriptions = c("option"),
            debug = F)


  print(paste0(i, "th email sent to [", par$email[i],"], ", length(par$email) - i, " email left"))

}

