#### Telegram Bot Crawiling financial bot ####

#install.packages("telegram.bot")
library(telegram.bot)
library(HenryQuant)
library(dplyr)
library(base)
library(jsonlite)
library(ggplot2)
library(httr)
library(rvest)
library(stringr)

### 방법 : 텔레그램 -> 사용자검색 : BotFather -> 메세지 /start 보내기 -> /이름_bot 두번 쳐서 대화방 만들기
bot = Bot(token = "621151480:AAGTdxlPLgYEq84wdpG1MsTazUNfpyZd5Uc")
print(bot$getMe())

#위에서 만든 /이름_bot에 들어가서 테스트메시지 아무거ㄴ 보내야함

updates = bot$getUpdates()

updates[[1]]$message$chat

chat_id = updates[[1]]$message$chat$id
chat_id


#bot$sendMessage(chat_id = chat_id, text = "shin hyunjin genious") #한번 보내보기


# 한경 컨센서스 크롤링
url = 'http://hkconsensus.hankyung.com/apps.analysis/analysis.list?skinType=market'
down = GET(url) %>% read_html(encoding = 'EUC-KR')

data_url = down %>% html_nodes('.dv_input') %>% html_nodes('a') %>% html_attr('href') %>%
  paste0('http://hkconsensus.hankyung.com', .) 

data_date = down %>% html_nodes('.first.txt_number') %>% html_text()
data_title = down %>% html_nodes('.text_l') %>% html_nodes('a')%>%
  html_text() %>% str_remove_all('.pdf')

data = paste(data_date, data_title, data_url, sep="\n")
data
[1] "2019-03-13\n>> Global Asset Monitor l 2019.03.13\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516902"  
[2] "2019-03-13\n한투의 아침\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516894"                           
[3] "2019-03-13\nStart with IBKS\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516884"                       
[4] "2019-03-13\nKTB MORNING EXPRESS\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516874"                   
[5] "2019-03-13\nDaily KTB Morning Brief\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516873"               
[6] "2019-03-13\n미래에셋대우 Daily\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516862"                    
[7] "2019-03-13\n마감 시장 스크린\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516861"                      
[8] "2019-03-13\nSamsung Daily\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516856"                         
[9] "2019-03-13\n오늘의 테마: 갤럭시S10 판매호조\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516855"       
[10] "2019-03-13\n오늘의 특징주 CJ ENM(035760) 2019-03-12\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516854"
[11] "2019-03-13\n시동 걸린 Bear Market Rally 후반전\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516850"    
[12] "2019-03-13\n1분 브리핑\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516836"                            
[13] "2019-03-13\n마켓 레이더\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516822"                           
[14] "2019-03-13\n약해져도 변심은 아니다\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516820"                
[15] "2019-03-13\nMORNING CALL\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516791"                          
[16] "2019-03-13\n경기선행지수가 보여준 Bright side\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516784"     
[17] "2019-03-13\n KB Daily\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516762"                             
[18] "2019-03-12\n이은택의 그림으로 보는 전략\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516750"           
[19] "2019-03-12\nGeopolitical Monitor\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516744"                  
[20] "2019-03-12\n미래에셋대우 Daily\nhttp://hkconsensus.hankyung.com/apps.analysis/analysis.downpdf?report_idx=516743"

##

sapply(data, function(x) {bot$sendMessage(chat_id = '@hjfinancetoday',x)})

#sapply(data, function(x) {bot$sendMessage(chat_id = chat_id, x)})


