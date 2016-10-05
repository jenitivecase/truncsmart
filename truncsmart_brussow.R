##' Cuts a string at a specified linewidth, trying to align cut with a separator
##' 
##' Some strings are simply too long. Let's shorten them and attempt to cut at a
##' specified separator. Use vectorized function, shortenv, to apply to non-scalar
##' @param textstring target string to shorten
##' @param linewidth approximate length to shorten taking into account tolerance
##' @param tol number of characters forward/back to check; if single value then only backwards checking
##' @param capwidth integer specifying the width of capital letters
##' @param separator accepts 1- or 2-element vector to separate string; default = c(" ", "_")
##' @return shortened character string
##' @author Brent Kaplan, Ben Kite, Paul Johnson & JENNIFER BRUSSOW!!!!
##' @examples
##' test <- "123_567_9"
##' (truncsmart(test, 5, tol = c(1,2)))
##'
##' edited 2016-10-04
##' JB concern - this still just chops it if no logical space is found within tolerance :/


truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  lets <- unlist(strsplit(textstring, split = "")) #makes character vector into literally a vector of characters
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) #assigns larger width values for capital characters
  linelength <- cumsum(widthval) #calculates the cumulative length after each character
  if(linelength[length(linelength)] <= linewidth) return(textstring) #if the string is already short enough, return it
  letswl <- lets[linelength  <= linewidth] #cuts the string at the length point given by the user
  letswt <- lets[linelength <= (linewidth + tol[length(tol)])] #cuts the string at the length point + tolerance
  letso <- letswl #makes another reference variable at the user-given length - what is returned if no breaks are found
  index_swl <- tail(which(grepl(as.character(separator[1]), letswl[(length(letswl)-tol[1]):(length(letswl))])), n=1)
  if(length(index_swl == 0)){index_swl <- tail(which(grepl(as.character(separator[2]), letswl[(length(letswl)-tol[1]):(length(letswl))])), n=1)}
  if(!is.na(tol[2])){
    index_swt <- tail(which(grepl(as.character(separator[1]), letswt[(length(letswt)-tol[2]):(length(letswt))])), n=1)
    if(length(index_swt) == 0){index_swt <- tail(which(grepl(as.character(separator[2]), letswt[(length(letswt)-tol[2]):(length(letswt))])), n=1)}
  }
  if(length(index_swt) != 0){
    letso <- letswt[1:((length(letswt)-tol[2])+index-2)]
  } else if(length(index_swl) != 0){
    letso <- letswl[1:((length(letswl)-tol[1])+index-2)]
  }
  letso <- paste0(letso, collapse  = "")
  return(letso)
}

#test
system.time(for(i in 1:10000){truncsmart("This is a string with l o t s o f b r e a k s in it", 30)})
system.time(for(i in 1:10000){truncsmart("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)})
system.time(for(i in 1:10000){truncsmart("This is a string with nobreakswhereyoumightwantthemtobe", 30)})

#I am unfamiliar with the vectorize command, but it appears to be making the function spit out two results???
truncsmart <- Vectorize(truncsmart, USE.NAMES=FALSE)


#line breaking test
linetrunc <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
  lets <- unlist(strsplit(textstring, split = "")) #makes character vector into literally a vector of characters
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1) #assigns larger width values for capital characters
  linelength <- cumsum(widthval) #calculates the cumulative length after each character
  if(linelength[length(linelength)] <= linewidth) return(textstring) #if the string is already short enough, return it
  withinlength <- linelength  <= linewidth #vector of T/F values showing the desired cut location
  withintol <- linelength <= (linewidth + tol[length(tol)]) #extends the acceptable length according to the tolerance value specified
  letswl <- lets[withinlength] #cuts the string at the length point given by the user
  letswt <- lets[withintol] #cuts the string at the length point + tolerance
  if(sum(grepl(as.character(separator[1]), letswl[(length(letswl)-tol[1]):(length(letswl))])) > 0 |
     sum(grepl(as.character(separator[2]), letswl[(length(letswl)-tol[1]):(length(letswl))])) > 0){
    #if a logical cut exists at the lower end of the tolerance, take that
    index <- tail(which(grepl(as.character(separator[1]), letswl[(length(letswl)-tol[1]):(length(letswl))])), n=1) #must use tail in case two breaks exist
    if(length(index) == 0){index <- tail(which(grepl(as.character(separator[2]), letswl[(length(letswl)-tol[1]):(length(letswl))])), n=1)}
    letso <- paste0(paste0(lets[1:((length(letswl)-tol[1])+index-2)], collapse = ""), "/n", 
                    paste0(lets[((length(letswl)-tol[1])+index):length(lets)], collapse = ""))
  } 
  if(!is.na(tol[2]) && sum(grepl(as.character(separator[1]), letswt[(length(letswt)-tol[2]):(length(letswt))])) > 0 |
     sum(grepl(as.character(separator[2]), letswt[(length(letswt)-tol[2]):(length(letswt))])) > 0){
    #if a logical cut exists at the higher end of the tolerance, that is better!!
    index <- tail(which(grepl(as.character(separator[1]), letswt[(length(letswt)-tol[2]):(length(letswt))])), n=1)
    if(length(index) == 0){index <- tail(which(grepl(as.character(separator[2]), letswt[(length(letswt)-tol[2]):(length(letswt))])), n=1)}
    letso <- paste0(paste0(lets[1:((length(letswt)-tol[2])+index-2)], collapse = ""), "/n", 
                   paste0(lets[((length(letswt)-tol[2])+index):length(lets)], collapse = ""))
  }
  if(!exists("letso")){
    letso <- paste0(paste0(lets[1:length(letswt)], collapse = ""), "/n", paste0(lets[length(letswt):length(lets)], collapse = ""))
  }
  return(letso)
}

#test
linetrunc("This is a string with l o t s o f b r e a k s in it", 30)
linetrunc("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)
linetrunc("This is a string with nobreakswhereyoumightwantthemtobe", 30)


######################################################################################################################################
#clocking the original version
truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
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

system.time(for(i in 1:10000){truncsmart("This is a string with l o t s o f b r e a k s in it", 30)})
system.time(for(i in 1:10000){truncsmart("This_is_a_string_with_l_o_t_s_o_f_b_r_e_a_k_s_in_it", 30)})
system.time(for(i in 1:10000){truncsmart("This is a string with nobreakswhereyoumightwantthemtobe", 30)})
