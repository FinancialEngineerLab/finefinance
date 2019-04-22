
### send news auto ###

tele_auto = file.path("C:/Users/shinhyunjin/Dropbox/data/telegram/example_send_time.R")
taskscheduler_create(taskname = "send_news", rscript = tele_auto,
                     schedule = "MINUTE", 
                     starttime = format(Sys.time() + 601, "%H:%M"),  ## 초 설정 
                     startdate = format(Sys.time(), "%Y/%m/%d"),
                     modifier = 5)



## tool ##
#taskscheduler_runnow('send_news')
#taskscheduler_stop('send_time')
#taskscheduler_delete("send_news")
