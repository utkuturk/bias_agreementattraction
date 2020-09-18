data_ugb <- read.csv("../Data/Experiment3_ugb/results/results", 
                    header = F, 
                    comment.char = "#", 
                    encoding = "UTF-8" , 
                    col.names = paste0("V",seq_len(11)), 
                    fill = TRUE, 
                    stringsAsFactors = FALSE)
colnames(data_ugb) = c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question","Answer", "RT")

subject_id <- with(data_ugb, { as.integer(as.factor(paste(Time, MD5))) })
data_ugb$subject <- sprintf("S[%d]", subject_id + 100)

df_forms <- data_ugb %>% subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
data_ugb %<>% subset(ControllerType == "DashedAcceptabilityJudgment")

age <- df_forms %>% dplyr::filter(Sentence == "age") %>% 
  dplyr::select(subject, age = Question)
natturk <- df_forms %>% dplyr::filter(Sentence == "natturk") %>% 
  dplyr::select(subject, natturk = Question) 
natturk$natturk <- ifelse(natturk$natturk == "male", "nat_turk", "nat_non_turk")

forms <- dplyr::left_join(age, natturk, by = "subject")

stopifnot( nrow(data_ugb) %% 2 == 0 )
rows_stim <- data_ugb[c(T,F),]
rows_resp <- data_ugb[c(F,T),]
stopifnot( all(is.na( rows_stim$RT )) )

data_ugb <- rows_resp %>% left_join(forms) %>% 
  dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
  dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
  dplyr::select(-ResponseCorrect)
data_ugb %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
data_ugb %<>% mutate( late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)) )

responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
data_ugb$Response %<>% as.character() %>% enc2native()
stopifnot( all(data_ugb$Response %in% responses | is.na(data_ugb$Response) ) )

data_ugb$response_yes <- ifelse(grepl("P'ye",data_ugb$Response) , T, 
                               ifelse(grepl("Q'ya",data_ugb$Response) , F, NA))
print( with(data_ugb, table(Response, response_yes)) )
data_ugb %<>% dplyr::select(-Response)

data_ugb %<>% dplyr::select(-SentenceNoInStimFile) %>% 
  subset(natturk == "nat_turk") %>% 
  subset(exp_condition != "practice") %>% 
  left_join(conditions_info, by = "exp_condition")
