
read_experimental_data <- function(fname, subj_offset = 0)
{
  data <- read.csv(fname, 
                   header = F, 
                   comment.char = "#", 
                   encoding = "UTF-8" , 
                   col.names = paste0("V",seq_len(11)), 
                   fill = TRUE, 
                   stringsAsFactors = FALSE)
  colnames(data) = c("Time", "MD5", "ControllerType", "SentenceNoInStimFile", "Element", "exp_condition", "item", "Sentence", "Question","Answer", "RT")
  
  subject_id <- with(data, { as.integer(as.factor(paste(Time, MD5))) })
  data$subject <- sprintf("S[%d]", subject_id + subj_offset)

  df_forms <- data %>% subset(ControllerType != "DashedAcceptabilityJudgment" ) %>% gdata::drop.levels()
  data %<>% subset(ControllerType == "DashedAcceptabilityJudgment")
  
  age <- df_forms %>% dplyr::filter(Sentence == "age") %>% 
                      dplyr::select(subject, age = Question)
  natturk <- df_forms %>% dplyr::filter(Sentence == "natturk") %>% 
                          dplyr::select(subject, natturk = Question) %T>% 
                          { .$natturk %<>% recode(male ="nat_turk", female = "nat_non_turk") } 
  forms <- dplyr::left_join(age, natturk, by = "subject")

  stopifnot( nrow(data) %% 2 == 0 )
  rows_stim <- data[c(T,F),]
  rows_resp <- data[c(F,T),]
  stopifnot( all(is.na( rows_stim$RT )) )
  
  data <- rows_resp %>% left_join(forms) %>% 
                        dplyr::select(-MD5, -Time, -ControllerType, -Sentence, -Element) %>%
                        dplyr::rename(ResponseCorrect=Answer, Response=Question) %>%
                        dplyr::select(-ResponseCorrect)
  data %<>% group_by(subject) %>% mutate(trial_no = seq(subject))
  data %<>% mutate( late_response = (Response == "NULL"), Response = ifelse(late_response, NA, as.character(Response)) )
  
  responses <- c(yes="İYİ (P'ye basınız)", no="KÖTÜ (Q'ya basınız)")
  data$Response %<>% as.character() %>% enc2native()
  stopifnot( all(data$Response %in% responses | is.na(data$Response) ) )
  
  data$response_yes <- ifelse(grepl("P'ye",data$Response) , T, 
                             ifelse(grepl("Q'ya",data$Response) , F, NA))
  print( with(data, table(Response, response_yes)) )
  data %<>% dplyr::select(-Response)
  
  data
}

se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse()
  DV <- substitute(DV) %>% deparse()
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM )
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>% 
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     #Var = var(nDV, na.rm = T) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N) )
}