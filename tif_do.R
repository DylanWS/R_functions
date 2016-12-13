tif.do <- function (first_function, if_condition, second_function, f1_arguments, f2_arguments, print="on fail")
{
	functions_complete = ifelse(missing(f1_arguments),T,F)
	if(functions_complete)
	{
		attempt_1 <- try(first_function,T)
		
		if(missing(if_condition)|| if_condition=="error")
		{
			if(class(attempt_1) == "try-error")
			{
				if(print=="on fail"|print==T) cat("First function failed, returning second funciton result: \n\n") #
				return(second_function)
			} else {
				if(print==T) cat("First function attempt successful, returning first funciton result: \n\n") #
				return(attempt_1)
			}
		
		} else {
		
			if(deparse(attempt_1)[1]==if_condition)
			{
				if(print=="on fail"|print==T) cat("Specified if condition occured, returning second funciton result: \n\n") #
				return(second_function)

			} else {
				if(print==T) cat("If condition not met, returning first funciton result: \n\n")
				if(class(attempt_1) == "try-error")
				{
					stop(attempt_1,call.=F)
				} else {
					return(attempt_1)
				}
			}
		}
		
	} else {
		
		if(missing(f2_arguments)) f2_arguments=f1_arguments
		attempt_1 <- try(first_function(f1_arguments),T)
		
		if(missing(if_condition)|| if_condition=="error")
		{
			if(class(attempt_1) == "try-error")
			{
				if(print=="on fail"|print==T) cat("First function failed, returning second funciton result: \n\n") #
				return(second_function(f2_arguments))
			} else {
				if(print==T) cat("First function attempt successful, returning first funciton result: \n\n") #
				return(attempt_1)
			}
		
		} else {
		
			if(deparse(attempt_1)[1]==if_condition)
			{
				if(print=="on fail"|print==T) cat("Specified if condition occured, returning second funciton result: \n\n") #
				second_function(f2_arguments)

			} else {
				if(print==T) cat("If condition not met, returning first funciton result: \n\n")
				if(class(attempt_1) == "try-error")
				{
					stop(attempt_1,call.=F)
				} else {
					return(attempt_1)
				}

			}
		}
	}
	
}

	

# If condition not met, first function used.
# tif.do(dim, "NA", length, matrix(c(1,4,2,8),1), print=T)
# 
# Critical error in first function resulting in second function being used.
# tif.do(sum, , length, "text",print=T)
# tif.do(sum, "error", length, "text",print=T)
#
# If condition not met but critical error produced.
# tif.do(sum, "NULL", length, "text",print=T)
#
# If condition caught and second function used.
# tif.do(dim, "NULL", length, c(1,4,2,8),print=T)
#
# If condition caught and second function used. Two seperate argument sets given.
# tif.do(dim, "NULL", length, c(1,4,2,8), 1:10,T)
# 
# If condition caught and second function used. Bit of a hack and a pour example.
# tif.do(dim, "NULL", dim, c(1,4,2,8), matrix(c(1,4,2,8),1),T)
# 
# No critical error but possibly undesired results.
# tif.do(dim(c(1,4,2,8)),, length(c(1,4,2,8)), print=T)
#
# If condition not met but possibly undesired result.
# tif.do(dim, "NA", length, c(1,4,2,8), print=T)
# 
# If condition met but possibly undesired result.
# tif.do(names(c(1,4,2,8)),"NULL", dimnames(c(1,4,2,8)), print=T)
