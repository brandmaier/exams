#
# R exams Evaluationsfunktion
#


eval_simple <- function() {
  
  #inhert from exams_eval
  parent <- exams::exams_eval(partial=TRUE, negative=FALSE, rule="none")
  
  ## convenience function for determining exercise type
  extype <- function(correct, answer = NULL, type = NULL) {
    ## convenience function
    mchoice01 <- function(x) as.numeric(strsplit(unlist(x), "")[[1L]])
    
    ## if type not given: auto-detect from correct
    if(is.null(type)) {
      type <- if(is.numeric(correct)) {
        "num"
      } else if(is.logical(correct)) {
        "mchoice"
      } else if(is.character(correct)) {
        if(all(strsplit(correct, "")[[1L]] %in% c("0", "1"))) "mchoice" else "string"
      } else {
        "unknown"
      }
    }
    if(!(type %in% c("num", "mchoice", "schoice", "string"))) stop("Unknown exercise type.")
    
    ## canonicalize correct
    if(type != "string" && is.character(correct)) {
      correct <- if(type == "num") as.numeric(correct) else as.logical(mchoice01(correct))    
    }
    
    ## process answer (if any)
    if(!is.null(answer)) {
      answer <- switch(type,
                       "num" = {
                         if(is.character(answer)) answer <- gsub(",", ".", answer, fixed = TRUE)
                         as.numeric(answer)	
                       },
                       "mchoice" = {
                         if(is.character(answer)) answer <- mchoice01(answer)
                         as.logical(answer)
                       },
                       "schoice" = {
                         if(is.character(answer)) answer <- mchoice01(answer)
                         as.logical(answer)
                       },
                       "string" = {
                         as.character(answer)
                       }
      )
      if(!any(is.na(answer)) && (length(correct) != length(answer))) stop(
        "Length of 'correct' and given 'answer' do not match.")
    }
    
    return(list(type = type, correct = correct, answer = answer))
  }
  
  # exams check answer codes:
  # 1 = Hit 
  # -1 = False Alarm
  # 0 = No answer (Miss or correct rejection)
  
  checkanswer <- function(correct, answer, tolerance = 0, type = NULL) {
    type <- extype(correct, answer, type = type)
    correct <- type$correct
    answer <- type$answer
    type <- type$type
    
    #if (!partial) stop("Not implemented")
    if (!(type %in% c("mchoice", "schoice"))) stop("Not implemented")
        
    if (type == "mchoice") {
          rval <- rep.int(0L, length(answer))
          #if (all(!answer)) 
          #  return(rval)
          #rval[which(correct & answer)] <- 1L
          #rval[which(!correct & answer)] <- -1L
          rval[which(correct & answer)] <- 1L # Hit
          rval[which(!correct & answer)] <- 0L # False Alarm
          rval[which(correct & !answer)] <- 0L # Miss
          rval[which(!correct & !answer)] <- 1L # Correct Rejection
          return(rval)
        } else {
          if (any(is.na(answer))) 
            return(0)
          if (negative < 0 & all(!answer)) 
            return(0)
          return(ifelse(all(correct == answer), 1L, -1L))
        }   
  }
  
  pointvec <- parent$pointvec
  
  pointsum <- function(correct, answer, tolerance = 0, type = NULL) {
  
   # browser()
    
    pts <- pointvec(correct, type = type)
    chk <- as.character(checkanswer(correct, answer, tolerance = tolerance, 
                                    type = type))
    res <- rep(0, length.out = length(chk))
    res[which(chk == "1")] <- 1/nchar(answer)
    #cat(res,"\n")
    #res[which(chk == "-1")] <- pts["neg"]
    #pmax(sum(res), negative)
    sum(res)
  }
  
  ## return (processed) parameters and functions
  list(
    #partial = partial,
    #negative = negative,
    #rule = rule,
    checkanswer = checkanswer,
    pointvec = pointvec,
    pointsum = pointsum
  )
}

