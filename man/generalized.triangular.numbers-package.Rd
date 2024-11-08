\name{generalized.triangular.numbers-package}
\alias{generalized.triangular.numbers-package}
\alias{generalized.triangular.numbers}
\docType{package}
\title{
  Compute the generalized triangular numbers
}
\description{
  Given a complex dimension \code{z} and an index \code{n},
  compute the relevant generalized triangular number.
}
\details{
  The triangular numbers are 1,3,6,10,15,21... 
  That is, the nth triangular number is the sum of the first n positive integers.
  The sequence is named as such because it counts the number of dots in triangular
  arrays with increasing numbers of rows. This sequence can be generalized by 
  altering the dimension under consideration. The triangular numbers are the two 
  dimensional case, but in three dimensions the tetrahedral numbers are obtained: 
  1,4,10,20,35,56... In fact, the generalization can be made so extreme as to allow
  a "dimension" equal to any complex number. In the case of numbers which are not 
  positive integers, the dot-counting interpretation breaks down, but the sequence 
  is still well-defined. The function \link{\code{gtn}} may be used to calculate generalized triangular numbers.
}
\author{
Maintainer: Chase Benoist <chase.s.benoist@gmail.com>
}
\keyword{ package }
\seealso{
  \link{\code{gtn}}
}
