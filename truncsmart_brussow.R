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

#instead of making a bunch of possible strings, my script focuses on finding the appropriate "index" value where 
truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  lets <- unlist(strsplit(textstring, split = "")) #makes character vector into literally a vector of characters
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) #assigns larger width values for capital characters
  linelength <- cumsum(widthval) #calculates the cumulative length after each character
  if(linelength[length(linelength)] <= linewidth) return(textstring) #if the string is already short enough, return it
  index <- Position(function(x){x < linewidth}, linelength, right = TRUE) #sets the index position to the first adj value > linewidth
  breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) #creates a vector showing where break characters are
  if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])]) > 0){ #if there are 2 tol values and a break in the forward check...
    #have to add 1 to index so we aren't evaluating it. have to subtract 1 from result to remove trailing break
    index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+tol[2])], right = TRUE)+index-1 #set index to last break found in range
  } else if(sum(breaks[(index-tol[1]):(index-1)]) > 0){ #if there is no second tol value or no breaks in forward tol, look backwards
    #have to subtract 1 from index so we aren't evaluating it. have to subtract 1 from result to remove trailing break
    index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-1
  }#if neither condition is satisfied, index remains in its initial position.
  out <- paste0(lets[1:index], collapse = "") #put the string back together
  return(out)
}

#testing the new version
system.time(for(i in 1:10000){truncsmart("This is a string with l o t s o f b r e a k s in it", 30)})
system.time(for(i in 1:10000){truncsmart("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)})
system.time(for(i in 1:10000){truncsmart("This is a string with nobreakswhereyoumightwantthemtobe", 30)})
system.time(for(i in 1:10000){truncsmart("This is a string with breaks where you might think", 30)})


#I am unfamiliar with the vectorize command, but it appears to be making the function spit out two results???
truncsmart <- Vectorize(truncsmart, USE.NAMES=FALSE)


#line breaking test
linetrunc <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_"), newline = "/n") {
  lets <- unlist(strsplit(textstring, split = "")) 
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) 
  linelength <- cumsum(widthval) 
  if(linelength[length(linelength)] <= linewidth) return(textstring)
  index <- Position(function(x){x < linewidth}, linelength, right = TRUE) 
  breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) 
  if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])]) > 0){ 
    index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+tol[2])], right = TRUE)+index-1 
  } else if(sum(breaks[(index-tol[1]):(index-1)]) > 0){ 
    index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-1
  }
  out <- paste0(paste0(lets[1:index], collapse = ""), newline, paste0(lets[(index+2):length(lets)], collapse = "")) 
  return(out)
}

#test
linetrunc("This is a string with l o t s o f b r e a k s in it", 30)
linetrunc("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)
linetrunc("This is a string with nobreakswhereyoumightwantthemtobe", 30)


######################################################################################################################################
#clocking the original version
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

system.time(for(i in 1:10000){truncsmart_old("This is a string with l o t s o f b r e a k s in it", 30)})
system.time(for(i in 1:10000){truncsmart_old("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)})
system.time(for(i in 1:10000){truncsmart_old("This is a string with nobreakswhereyoumightwantthemtobe", 30)})
system.time(for(i in 1:10000){truncsmart_old("This is a string with breaks where you might think", 30)})
