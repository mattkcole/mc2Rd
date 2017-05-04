#' @title wrapper for MCMCpack's rdirichlet func
#'
#' @description Generate dirichlet random vectors without specifying 'n' (assumed
#'     to be 1)
#'
#' @param alpha the alpha matrix to be supplied to the rdirichlet function
#'
#' @return A dirichlet random vector
#'
#' @importFrom MCMCpack rdirichlet
h.rsmatrix <- function(alpha){
        return(rdirichlet(1,alpha))
}


#' @title Generate random strucutred corr plots
#'
#' @description Easy method to generate random strucuted correlation matricies
#'
#' @param struc the 'weights' matrix
#'
#' @return A correlation matrix
#'
#' @examples
#' struc <- matrix(c(5,1,2,
#'                   4,1,1,
#'                   7,1,1.5,
#'                   1,6,1,
#'                   1,7,1,
#'                   1,5,1,
#'                   1,1,9,
#'                   1,1,9,
#'                   1,1,5)
#'                 , ncol= 3)
#' rsmatrix(struc)
#'
#'
#' @export
rsmatrix <- function(struc){
        # structure needs to be a p X m matrix of latent 'factor loading'
        # strucute

        # NOTE: must be vectorized to be fast (maybe use )

        laodings <- t(apply(struc, 1, h.rsmatrix))

        return(l2c(laodings))

}
