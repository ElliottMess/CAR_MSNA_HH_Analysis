

#' function to change value of select one questions if other value is in the choices
#'
#' @param question.name Column name of the select_one question
#' @param other.q.name Column name of the other question
#' @param old.value.name Column in cleaned values dataframe with the old values. Must match the values in the main dataframe
#' @param new.value.name Column in cleaned values dataframe with the new values
#' @param toadd.value List of choices names to be applied. Must be similar to the values in the form.
#' @param data Main dataframe
#' @param codes.df Dataframe with value to change. Each question to be recoded should have two columns with unique values: the orignal other column and the new values.

change_select_one_value <- function(question.name, other.q.name, old.value.name, new.value.name, toadd.value, data, codes.df){
  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")
  
  old.value <- variable.coded[[old.value.name]]
  
  new.value <- variable.coded[[new.value.name]]
  
  other.q <- data[[other.q.name]] 
  question.v <- data[[question.name]]
  data[[other.q.name]] <- plyr::mapvalues(other.q, old.value, new.value)
  question.v[which(!(is.null(other.q)))] <- !(is.null(data[[other.q.name]]))
  question.v[data[[other.q.name]] %in% toadd.value] <- data[[other.q.name]][data[[other.q.name]] %in% toadd.value]
  data[[question.name]] <- question.v
  return(data)
}

#' function to populate datacleaning logbook for change_select_one_value function
#'
#' @param logbook Column name of the select_one question. If doesn't exist, creating it. Must have the following columns "uuid", "index", "question.name", "Issue",	"feedback",	"changed",	"old.value",	"new.value"
#' @param data Main dataframe
#' @param codes.df Dataframe with value to change. Each question to be recoded should have two columns with unique values: the orignal other column and the new values.
#' @param other.q.name Column name of the other question
#' @param new.value.name Column in cleaned values dataframe with the new values
change_select_one_value_log <- function(logbook, data, codes.df, other.q.name, new.value.name, uuid_col = "X_submission__uuid", index_col = "X_index" ){
  

  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")
  
  variables.log <- data %>%
    inner_join(variable.coded)%>%
    select(UQ(sym(uuid_col)), UQ(sym(index_col)), other.q.name, new.value.name)%>%
    mutate(uuid = UQ(sym(uuid_col)), index = UQ(sym(index_col)), question.name = other.q.name, 
           feedback = "", Issue = "Autre a recoder", changed = "Oui", 
           old.value = UQ(sym(other.q.name)), new.value = UQ(sym(new.value.name)))%>%
    select(log_cols)
  
  rbind(logbook, variables.log)
  
  return(logbook)
}


#' function to change choices columns to +1.
#'
#' @param col_sm Column name of the select_multiple.choice question
#' @param col_sm_autre Column name of the other question
#' @param other.choice.name name of the other choice in the select_multiple (e.g. 'other')
#' @param data Main dataframe
#'
add_toselectmutlipe <- function(col_sm, col_sm_autre, other.choice.name, data){
  split_name <- gsub(".*?\\.","", names(data[col_sm]))
  data[[col_sm_autre]] <- trimws(data[[col_sm_autre]])
  in_col_sm_autre <- as.numeric(grepl(split_name, data[[col_sm_autre]], ignore.case=T))
  col_sm_value <- data[[col_sm]]
  value <- rowSums(cbind(in_col_sm_autre, col_sm_value))
  if(split_name == other.choice.name){
    value <- if_else(value >1,1,0)
  }else{
    value <- if_else(value >=1,1,0)
  }
  return(value)
}

#' function to change value of select_multiple questions if other value is in the choices
#'
#' @param question.name Column name of the select_one question
#' @param other.q.name Column name of the other question
#' @param old.value.name Column in cleaned values dataframe with the old values. Must match the values in the main dataframe
#' @param new.value.name Column in cleaned values dataframe with the new values
#' @param other.choice.name name of the other choice in the select_multiple (e.g. 'other')
#' @param toadd.value List of choices names to be applied. Must be similar to the values in the form.
#' @param data Main dataframe
#' @param codes.df Dataframe with value to change. Each question to be recoded should have two columns with unique values: the orignal other column and the new values.
#' 
change_select_multiple_value <- function(question.name, other.q.name, old.value.name, new.value.name, other.choice.name, toadd.value, data, codes.df){
  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")
  
  old.value <- variable.coded[[old.value.name]]
  new.value <- variable.coded[[new.value.name]]
  other.q <- data[[other.q.name]] 
  question.v <- data[[question.name]]
  data[[other.q.name]] <- plyr::mapvalues(other.q, old.value, new.value)
  list_sm_cols <- names(data%>%select(starts_with(paste0(question.name, "."))))
  cols_toadd <- paste0("sante_4_0_4_malades.", toadd.value)
  not_incols <- cols_toadd[!cols_toadd %in% list_sm_cols]
  data[,not_incols] <- NA
  list_sm_cols <- names(data%>%select(starts_with(paste0(question.name, "."))))
  
  data[,list_sm_cols] <- sapply(list_sm_cols, add_toselectmutlipe, col_sm_autre = other.q.name, other.choice.name = other.choice.name, data = data)
  
  return(data)
}


removed_nonUTF <- function(x){
  x <- gsub('[^ -~]', '', x)
  x <- gsub(' ', "", x )
  x <- gsub("\'", "", x)
  x <- gsub("[(),.]", "", x)
  x <- tolower(x)
  return(x)
}

