data_n <- read.csv("../Data/Experiment3_neutral/results/results", 
                     header = F, 
                     comment.char = "#", 
                     encoding = "UTF-8" , 
                     col.names = paste0("V",seq_len(11)), 
                     fill = TRUE, 
                     stringsAsFactors = FALSE)
colnames(data_n) = c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question","Answer", "RT")

subject_id <- with(data_n, { as.integer(as.factor(paste(Time, MD5))) })
data_n$subject <- sprintf("S[%d]", subject_id + 200)

df_forms <- data_n %>% subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
data_n %<>% subset(ControllerType == "DashedAcceptabilityJudgment")

age <- df_forms %>% dplyr::filter(Sentence == "age") %>% 
  dplyr::select(subject, age = Question)
natturk <- df_forms %>% dplyr::filter(Sentence == "natturk") %>% 
  dplyr::select(subject, natturk = Question)
natturk$natturk <- ifelse(natturk$natturk == "male", "nat_turk", "nat_non_turk")
 
forms <- dplyr::left_join(age, natturk, by = "subject")

stopifnot( nrow(data_n) %% 2 == 0 )
rows_stim <- data_n[c(T,F),]
rows_resp <- data_n[c(F,T),]
stopifnot( all(is.na( rows_stim$RT )) )

data_n <- rows_resp %>% left_join(forms) %>% 
  dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
  dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
  dplyr::select(-ResponseCorrect)
data_n %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
data_n %<>% mutate( late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)) )

responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
data_n$Response %<>% as.character() %>% enc2native()
stopifnot( all(data_n$Response %in% responses | is.na(data_n$Response) ) )

data_n$response_yes <- ifelse(grepl("P'ye",data_n$Response) , T, 
                                ifelse(grepl("Q'ya",data_n$Response) , F, NA))
print( with(data_n, table(Response, response_yes)) )
data_n %<>% dplyr::select(-Response)

data_n %<>% dplyr::select(-SentenceNoInStimFile) %>% 
  subset(natturk == "nat_turk") %>% 
  subset(exp_condition != "practice") %>% 
  left_join(conditions_info, by = "exp_condition")
