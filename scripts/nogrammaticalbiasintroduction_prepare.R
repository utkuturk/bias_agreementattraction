
#### DATA -----
# Call data
data_noBiasinstruction <- read.csv("Data/Experiment3_neutral/results/results", #location
                                   header = F, #the first row is not the column names
                                   comment.char = "#", #disregard everything that starts with "#". Think about it why?
                                   encoding = "UTF-8" , #utf8 because turkish
                                   col.names = paste0("V",seq_len(11)), #create new column names just incase
                                   fill = TRUE, # When there is a blank line, just add it to the df and specify it as it is for later cleaning
                                   stringsAsFactors = FALSE) #do not take strings as factor. "why?"
# give columns names
colnames(data_noBiasinstruction) = c("Time", "MD5", "ControllerType",  #names for columns
                                     "SentenceNoInStimFile", "Element", "exp_condition", 
                                     "item", "Sentence", "Question","Answer", "RT")
# create subject ids
subject_id <- with(data_noBiasinstruction, { as.integer(as.factor(paste(Time, MD5))) })
data_noBiasinstruction$subject <- sprintf("S[%d]", subject_id + 0)

# extract form and the the real data
df_forms_noBiasinstruction <- data_noBiasinstruction %>% #extract form related stuff
  subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% # by specifying not dashedacceptability
  gdata::drop.levels()
data_noBiasinstruction %<>% subset(ControllerType == "DashedAcceptabilityJudgment") 
data_noBiasinstruction$subject %<>% as.factor()
data_noBiasinstruction$exp_condition %<>% as.factor()
data_noBiasinstruction$RT %<>% as.integer()
#extract ages and native language
age <- df_forms_noBiasinstruction %>% dplyr::filter(Sentence == "age") %>% 
  dplyr::select(subject, age = Question)
natturk <- df_forms_noBiasinstruction %>% dplyr::filter(Sentence == "natturk") %>% 
  dplyr::select(subject, natturk = Question)
natturk$natturk <- ifelse(natturk$natturk == "male", "nat_turk", "nat_non_turk")
forms_noBiasinstruction <- dplyr::left_join(age, natturk, by = "subject")

#extract stim and responses
stopifnot( nrow(data_noBiasinstruction) %% 2 == 0 )
rows_stim_noBiasinstruction <- data_noBiasinstruction[c(T,F),]
rows_resp_noBiasinstruction <- data_noBiasinstruction[c(F,T),]
stopifnot( all(is.na( rows_stim_noBiasinstruction$RT )) )

#check for practice accuracy
data_noBiasinstruction_practice <- rows_resp_noBiasinstruction %>% subset(exp_condition == "practice") 
data_noBiasinstruction_practice %<>% 
  left_join(forms_noBiasinstruction) %>% 
  dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
  dplyr::rename(ResponseCorrect=Answer, Response=Question)


# exclude participants who did less than 0.6 in practice items
data_noBiasinstruction_practice %>% subset(Response == "NULL")
data_noBiasinstruction_practice$ResponseCorrect[data_noBiasinstruction_practice$ResponseCorrect == "NULL"] <- 0
data_noBiasinstruction_practice$ResponseCorrect %<>%  as.integer()
bad_subjects_by_practice <- data_noBiasinstruction_practice %>% group_by(subject) %>% 
  summarize(p_yes = mean(ResponseCorrect), meanRT = mean(RT)) %>% subset(p_yes <=0.6) %>% .$subject
rows_resp_noBiasinstruction_nobadpractice <- rows_resp_noBiasinstruction %>% subset(!subject %in% bad_subjects_by_practice)
forms_noBiasinstruction_nobadpractice <- forms_noBiasinstruction %>% subset(!subject %in% bad_subjects_by_practice)

#prepare data for analysis.
data_noBiasinstruction_nobadpractice <- rows_resp_noBiasinstruction_nobadpractice %>% 
  left_join(forms_noBiasinstruction_nobadpractice) %>% 
  dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
  dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
  dplyr::select(-ResponseCorrect) %>% subset(exp_condition != "practice")

# add trial no
data_noBiasinstruction_nobadpractice %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
#identify late responses
data_noBiasinstruction_nobadpractice %<>% mutate( late_response = (Response == "NULL"), 
                                                  Response = ifelse(late_response, NA, as.character(Response)) )
#create response yes vector and control for it. 
responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
data_noBiasinstruction_nobadpractice$Response %<>% as.character() %>% enc2native()
stopifnot( all(data_noBiasinstruction_nobadpractice$Response %in% responses | is.na(data_noBiasinstruction_nobadpractice$Response) ) )

data_noBiasinstruction_nobadpractice$response_yes <- ifelse(grepl("P'ye",data_noBiasinstruction_nobadpractice$Response) , T, 
                                                            ifelse(grepl("Q'ya",data_noBiasinstruction_nobadpractice$Response) , F, NA))
print( with(data_noBiasinstruction_nobadpractice, table(Response, response_yes)) )
data_noBiasinstruction_nobadpractice %<>% dplyr::select(-Response)



conditions_info <- data.frame(
  exp_condition = c("condition_a", "condition_b", "condition_c", "condition_d", "filler_ung", "filler_g"),
  experiment =    c("AgrAttr",     "AgrAttr",     "AgrAttr",     "AgrAttr",     "filler",     "filler"),
  condition =     c("a",           "b",           "c",           "d",           "ung",        "g"),
  grammatical =   c("ungram",      "gram",        "ungram",      "gram",        "ungram",     "gram"),
  verb_num =      c("pl",          "sg",          "pl",          "sg",          "sg",         "pl"),
  attractor_num = c("pl",          "pl",          "sg",          "sg",          NA,           NA),
  stringsAsFactors = FALSE
)


data_noBiasinstruction_nobadpractice %<>% dplyr::select(-SentenceNoInStimFile) %>% 
  subset(natturk == "nat_turk") %>% 
  subset(exp_condition != "practice") %>% 
  left_join(conditions_info, by = "exp_condition")



