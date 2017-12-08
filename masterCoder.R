##' Modifies 'data' by adding new values supplied in recodeFileName
##'
##' recodeFileName is expected to have columns 
##' c(lookupVariable1,lookupOperator1, lookupValue1,
##' lookupVariable2,lookupOperator2, lookupValue2, 
##' lookupVariable3,lookupOperator3, lookupValue3, newVariable, newValue)
##' 
##' Within the column 'newVariable', replace values that
##' meet the condition[i] defined by all 'lookupOperator[i]' and 'lookupValue[i]' combinations 
##' within column 'lookupVariable[i]' with the value newValue'.  
##' If any 'lookupVariable[i]' is NA, then sets condition[i] to equal TRUE
##' 
##' Note that lookupVariable can be the same as newVariable.
##' 
##' Note that I have not yet figured out how best to ensure that there are no extra whitespaces in the 
##' data set that is being recoded.
##'
##' @param recodeDataFileName name of table with recode instructions
##' @param data existing data.frame
##' @return modified data.frame

masterRecoder <- function(recodeFileName, data){
  
  validFromNames <- names(data)
  import <- readRecode(recodeFileName, validFromNames)
  
  if( !is.null(import)){    
    for(i in seq_len(nrow(import))){  #Make replacements
      #print(import[i,])
      col.to <- import$newVariable[i] 
      ##colsFrom <- paste("col.from",c(1,2,3), sep="")
      lookupVariableColNames <- paste("lookupVariable", 1:3, sep="")
      lookupVariableValues <- as.list(import[i,lookupVariableColNames])
      lookupOperatorColNames <- paste("lookupOperator", 1:3, sep="")
      OperatorValues <- as.list(import[i,lookupOperatorColNames])
      lookupValueColNames <- paste("lookupValue", 1:3, sep="")
      lookupValueValues <- as.list(import[i,lookupValueColNames])
      criteria <- vector("list", 3)
      for(j in 1:3) {
        if(is.na(lookupVariableValues[[j]])) { # set criteria[j] to TRUE if there is no lookupVariable specified
          criteria[[j]] <- TRUE
        } else { # determine subset of records where criteria[j] applies
          criteria[[j]] <- !is.na(data[[lookupVariableValues[[j]]]]) &  # Note that invalid values in the data frame need to be coded as "NA"
            match.fun(OperatorValues[[j]])(data[[lookupVariableValues[[j]]]], lookupValueValues[[j]])
        }
      }
      rows <- criteria[[1]] & criteria[[2]] & criteria[[3]]
      data[rows,col.to] <- import$newValue[i]
      
    }
  }   
  data
}      


##' Utility function to read/process recodeFileName for masterRecoder
##' 
##' @param recodeFileName name of table with recode instructions
##' @param validFromNames vector of existing variable names in the data frame to be recoded
##' @return data.frame with columns c(lookupVariable1,lookupOperator1, lookupValue1,
##' lookupVariable2,lookupOperator2, lookupValue2, 
##' lookupVariable3,lookupOperator3, lookupValue3, newVariable, newValue)
readRecode <- function(recodeFileName, validFromNames){
  
  if(file.exists(recodeFileName)){
    import <- read.csv(recodeFileName, header=TRUE, stringsAsFactors=FALSE,
                       strip.white=TRUE)
    if(nrow(import)> 0 ){
      
      #Check columns names for import are right
      expectedColumns<- c("lookupVariable1","lookupOperator1","lookupValue1",
                          "lookupVariable2","lookupOperator2","lookupValue2", 
                          "lookupVariable3","lookupOperator3","lookupValue3",
                          "newVariable", "newValue")
      nameIsOK <-  names(import) %in% expectedColumns
      if(any(!nameIsOK))
        stop("Incorrect name in lookup table for ",
             recodeFileName, "--> ", paste(names(import)[!nameIsOK],
                                            collapse=", "))
      #Check values of lookupVariable[i] are in list of allowed variable names
      lookupVariableColNames <- paste("lookupVariable",c(1,2,3), sep="")
      for(colname in lookupVariableColNames) {
        import[colname][import[colname] == ""] <- NA
        nameIsOK <- import[colname] %in% validFromNames | is.na(import[colname])
        if(any(!nameIsOK))
          stop("Names in the recode file do not match variable names in the dataset. ", 
               "Column: ", colname, "Variable name: ", paste(import[colname][!nameIsOK], collapse=", "))
      }
      
    } else {
      import <- NULL
    }
  } else {
    import <- NULL
  }
  import
}