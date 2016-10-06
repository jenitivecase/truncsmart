##' Cuts a string at a specified linewidth, trying to align cut with a separator
##' 
##' Some strings are simply too long. Let's shorten them and attempt to cut at a
##' specified separator. Use vectorized function, shortenv, to apply to non-scalar
##' @param textstring target string to shorten
##' @param linewidth approximate length to shorten taking into account tolerance
##' @param tol number of characters back/forward to check; if single value then only backwards checking
##' JB NOTE - this configuration of tol seemed more logical to me. tol = 1 char for backwards, or two char for back/forwards
##' @param capwidth integer specifying the width of capital letters
##' @param separator accepts 1- or 2-element vector to separate string; default = c(" ", "_")
##' @return shortened character string
##' @author Brent Kaplan, Ben Kite, Paul Johnson & JENNIFER BRUSSOW!!!!
##' @examples
##' test <- "123_567_9"
##' (truncsmart(test, 5, tol = c(1,2)))
##'
##' edited 2016-10-04

### The actual result ################################################################################################################
# Instead of making a bunch of possible strings, my script focuses on finding the appropriate "index" value 
# where the string should be cut. This strategy makes it easier to paste things back together at the end.
# Also, it's fewer lines of code, and it appears to be marginally faster. Run times are similar with comments removed.
# A comment-free version can be found starting on line 157.

truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
  lets <- unlist(strsplit(textstring, split = "")) #makes character vector into literally a vector of characters
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) #assigns larger width values for capital characters
  linelength <- cumsum(widthval) #calculates the cumulative length after each character
  if(linelength[length(linelength)] <= linewidth) return(textstring) #if the string is already short enough, return it
  index <- Position(function(x){x < linewidth}, linelength, right = TRUE) #sets the index position to the first adj value > linewidth
  breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) #creates a vector showing where break characters are
  if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])], na.rm = TRUE) > 0){ #if are two values and a break in the forward check...
    #when looking forward, NAs may be encountered. ref is the last non-NA value in the range to avoid these
    ref <- Position(function(x){!is.na(x)}, breaks[(index+1):(index+tol[2])], right = TRUE) #the last non-NA value in the forward tol
    #have to add 1 to index so we aren't evaluating it. have to subtract 1 from result to remove trailing break
    index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+ref)], right = TRUE)+index-1 #set index to last break found in range
  } else if(sum(breaks[(index-tol[1]):(index-1)], na.rm = TRUE) > 0){ #if no 2nd tol value or no breaks in forward tol, look backwards
    #have to subtract 1 from index so we aren't evaluating it. have to subtract 2 from result to remove trailing break & acct for index
    index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-2
  } #if neither condition is satisfied, index remains in its initial position.
  out <- paste0(lets[1:index], collapse = "") #put the string back together up to the index point
  return(out)
}

######################################################################################################################################

#testing the new version
truncsmart("This is a string with l o t s o f b r e a k s in it", 30)
truncsmart("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30, 3)
truncsmart("This is a string with nobreakswhereyoumightwantthemtobe", 30)
truncsmart("This is a string with breaks where you might think", 30)
truncsmart("This is too short", 30)
truncsmart("This is not too short but medium", 30)

#I am unfamiliar with the vectorize command, but it appears to be making the function slower?? I am not using it.
# truncsmart <- Vectorize(truncsmart, USE.NAMES=FALSE)

### line breaking function - just a cleaned up truncsmart with a better out pasting. #################################################
#new param newline is the break you want to insert.
linetrunc <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_"), newline = "\n") {
  if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
  lets <- unlist(strsplit(textstring, split = ""))
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1)
  if(sum(widthval) <= linewidth){return(textstring)}
  max <- ceiling(sum(widthval)/linewidth) #how many reps to do/substrings to complete
  out <- vector("list", max) #we are going to store the substrings in a list called out
  for(i in 1:max){
    lets <- unlist(strsplit(textstring, split = ""))
    widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1)
    linelength <- cumsum(widthval) 
    if(linelength[length(linelength)] <= linewidth){
      out[[i]] <- textstring
    } else {
      index <- Position(function(x){x < linewidth}, linelength, right = TRUE)
      breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) 
      if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])], na.rm = TRUE) > 0){ 
        ref <- Position(function(x){!is.na(x)}, breaks[(index+1):(index+tol[2])], right = TRUE) 
        index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+ref)], right = TRUE)+index-1 
      } else if(sum(breaks[(index-tol[1]):(index-1)], na.rm = TRUE) > 0){ 
        index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-2
      } 
      out[[i]] <- paste0(paste0(lets[1:index], collapse = ""), newline) 
      textstring <- paste0(lets[(index+1):length(lets)], collapse = "")
      textstring <- gsub(paste0("^",separator[1]), "", textstring) 
      if(!is.na(separator[2])) {
        textstring <- gsub(paste0("^",separator[2]), "", textstring)  #removing the leading space if applicable. necessary for cases w/ no good break
      }
    }
  }
  out <- paste0(unlist(out), collapse = "")
  return(out)
}

#testing linetrunc
linetrunc("Here is a REALLY long test string that needs to be BROKEN into multiple LINES so that everything will WORK OUT look at the test", 30)
linetrunc("THIS IS A DIFFERENT_LONG_STRING THAT HAS ALL CAPS BECAUSE CAPS ARE DIFFICULT_RIGHT_YES_THEY SURE ARE LOOK AT THE CAPS", 30)
linetrunc("This is a string with nobreakswhereyoumightwantthemtobe", 30)
linetrunc("Here is a short string", 30)
linetrunc("What is the optimal length of string? I have no idea.", 30, 3)

# the original version ###############################################################################################################
truncsmart_old <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
  lets <- unlist(strsplit(textstring, split = ""))
  caps <- sapply(lets, function(x) x %in% LETTERS)
  widthval <- ifelse(caps == TRUE, capwidth, 1)
  linelength <- cumsum(widthval)
  if(linelength[length(linelength)] <= linewidth) return(textstring)
  withinlength <- linelength  <= linewidth
  ifelse(length(tol) == 2, withintol <- linelength <= (linewidth + tol[2]),
         withintol <- linelength <= (linewidth + tol))
  letswl <- lets[withinlength]
  letso <- letswl
  letswt <- lets[withintol]
  if(length(tol) == 2) {
    for (j in 0:tol[1]){
      if(identical(letswl[length(letswl)], separator[1]) ||
         identical(letswl[length(letswl)], separator[2])) {
        letso1 <- letswl[-length(letswl)]
        break()
      }
      letswl <- letswl[-length(letswl)]
    }
    for (j in 0:tol[2]) {
      if(identical(letswt[length(letswt)], separator[1]) ||
         identical(letswt[length(letswt)], separator[2])) {
        letso2 <- letswt[-length(letswt)]
        break()
      }
      letswt <- letswt[-length(letswt)]
    }
    ifelse(!exists("letso1") && !exists("letso2"), letso,
           ifelse(exists("letso1") && !exists("letso2"), letso <- letso1,
                  ifelse(!exists("letso1") && exists("letso2"), letso <- letso2,
                         ifelse(abs((length(letso1) - length(lets[withinlength]))) <= (length(letso2) - length(lets[withintol])), letso <- letso1, letso <- letso2))))
  } else {
    for (j in 0:tol){
      if(identical(letswl[length(letswl)], separator[1]) ||
         identical(letswl[length(letswl)], separator[2])) {
        letso <- letswl[-length(letswl)]
        break()
      }
      letswl <- letswl[-length(letswl)]
    }
  }
  out <- paste0(letso, collapse  = "")
  out
}

truncsmart_old("This is a string with l o t s o f b r e a k s in it", 30)
truncsmart_old("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)
truncsmart_old("This is a string with nobreakswhereyoumightwantthemtobe", 30)
truncsmart_old("This is a string with breaks where you might think", 30)
truncsmart_old("This is too short", 30)
truncsmart_old("This is not too short but medium", 30)

####### Speed testing ################################################################################################################
#redefine without comments for accurate estimates, since comments appear to slow it down??
truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
  lets <- unlist(strsplit(textstring, split = "")) 
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) 
  linelength <- cumsum(widthval) 
  if(linelength[length(linelength)] <= linewidth) return(textstring) 
  index <- Position(function(x){x < linewidth}, linelength, right = TRUE) 
  breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) 
  if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])], na.rm = TRUE) > 0){ 
    ref <- Position(function(x){!is.na(x)}, breaks[(index+1):(index+tol[2])], right = TRUE) 
    index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+ref)], right = TRUE)+index-1 
  } else if(sum(breaks[(index-tol[1]):(index-1)], na.rm = TRUE) > 0){ 
    index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-2
  }
  out <- paste0(lets[1:index], collapse = "") 
  return(out)
}

time1 <- 0
for(j in 1:100){
  time1 <- time1 + system.time(for(i in 1:10000){truncsmart("This is a string with l o t s o f b r e a k s in it", 30)})
}
time1/100

time2 <- 0
for(j in 1:100){
  time2 <- time2 + system.time(for(i in 1:10000){truncsmart_old("This is a string with l o t s o f b r e a k s in it", 30)})
}
time2/100

GRF <- readRDS("S:/Projects/DLM Secure/1-ELA and Math/Scoring/GRF/GRF Output/2016/Batch 2/Internal DLM GRF/Full_Batch_2_GRF_20160722.Rds")
test_frame <- GRF[c("District", "School")]
rm(GRF)
gc()

#this results in basically equal times for each function on my machine - approx 4.1 seconds for new & 4.4 for old
system.time(test_frame$dist_test2 <- sapply(X = test_frame$District, FUN = truncsmart, linewidth=30))
system.time(test_frame$dist_test2 <- sapply(X = test_frame$District, FUN = truncsmart_old, linewidth=30))


