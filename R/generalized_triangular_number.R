
#' @name gtn
#' 
#' @title Compute the generalized triangular numbers
#'
#' @description
#' The triangular numbers are 1,3,6,10,15,21... 
#' That is, the index \code{n} triangular number is the sum of the first \code{n} positive integers.
#' The sequence is named as such because it counts the number of dots in triangular
#' arrays with increasing numbers of rows. This sequence can be generalized by 
#' altering the dimension under consideration. The triangular numbers are the two 
#' dimensional case, but in three dimensions the tetrahedral numbers are obtained: 
#' 1,4,10,20,35,56... In fact, the generalization can be made so extreme as to allow
#' a "dimension" equal to any complex number. In the case of numbers which are not 
#' positive integers, the dot-counting interpretation breaks down, but the sequence 
#' is still well-defined. \cr \cr
#' While these numbers are interesting in a pure sense,
#' one application is related to cumulative sums and differences.
#' If the cumulative sum is thought of as an operator with the difference being its
#' inverse operator, then iterated cumulative sums and differences of a vector \code{v}
#' can be computed by convolving with the appropriate vector of 
#' generalized triangular numbers. For example, this will compute the 
#' cumulative sum of \code{v} iterated \code{m} times: \cr \cr
#' \code{
#' foo <- function(v,m){ \cr
#' \verb{    }n <- length(v) \cr
#' \verb{    }pow.2 <- 2^(1+ceiling(log(n,2))) \cr
#' \verb{    }convolve(c(v,numeric(pow.2-n)),c(gtn(m-1,n),numeric(pow.2-n)),conj=F)[seq_len(n)] \cr
#' } \cr \cr
#' }
#' And this will compute the difference of \code{v} iterated \code{m} times: \cr \cr
#' \code{
#' bar <- function(v,m){ \cr
#' \verb{    }n <- length(v) \cr
#' \verb{    }pow.2 <- 2^(1+ceiling(log(n,2))) \cr
#' \verb{    }convolve(c(v,numeric(pow.2-n)),c(gtn(-(m+1),n),numeric(pow.2-n)),conj=F)[seq_len(n)] \cr
#' } 
#' } \cr \cr
#' The nth generalized triangular number in dimension z is
#' given by \code{1/(z*beta(n,z))}. However, the base R function \code{beta} does not
#' support arbitrary complex arguments.
#' 
#' @usage 
#' gtn(0, 10)
#' gtn(1, 10)
#' gtn(2, 10)
#' gtn(3, 10)
#' 
#' @param 
#' z A length \code{1} \code{complex} indicating the dimension in which to calculate the sequence
#' 
#' @param 
#' n A length \code{1} \code{integer} indicating the desired index in the sequence - must be positive 
#' 
#' @param 
#' sequence A length \code{1} \code{logical}. 
#' If \code{TRUE}, a length \code{n} \code{complex} will be returned: the first \code{n} generalized triangular numbers in dimension \code{z}.
#' If \code{FALSE}, a length \code{1} \code{complex} will be returned: the index \code{n} generalized triangular number in dimension \code{z}.
#' Defaults to \code{TRUE}.
#' 
#' @return
#' Depending on the value of \code{sequence}, either 
#' the first \code{n} generalized triangular numbers in dimension \code{z}
#' or the index \code{n} generalized triangular number in dimension \code{z}
#' 
gtn <- function(z, n, sequence = TRUE){

    if(!(is.numeric(z) || is.complex(z))){
        stop("Argument 'z' must be a length 1 complex")
    } else if(length(z) != 1){
        stop("Argument 'z' must be a length 1 complex")
    }

    if(!is.numeric(n)){
        stop("Argument 'n' must be a length 1 positive integer")
    } else if(length(n) != 1){
        stop("Argument 'n' must be a length 1 positive integer")
    } else if((n %% 1) != 0){
        stop("Argument 'n' must be a length 1 positive integer")
    } else if(n <= 0){
        stop("Argument 'n' must be a length 1 positive integer")
    }

    if(!is.logical(sequence)){
        stop("Argument 'sequence' must be a length 1 logical")
    } else if(length(sequence) != 1){
        stop("Argument 'sequence' must be a length 1 logical")
    } else if(is.na(sequence)){
        stop("Argument 'sequence' must be a length 1 logical")
    }

    if(sequence){
        return(generalized_triangular_numbers_sequence(z+1, n))
    } else{
        return(generalized_triangular_numbers_point(z+1, n))
    }

}
