#' @title l2c helper function
#'
#' @description determines if a proper loadings matrix has been provided
#'
#' @param loadings the loading matrix to be utilized.
#'
#' @description tests to make sure a valid factor loadings matrix is provided
tester.l2c <- function(loadings){
        if (ncol(loadings) >= nrow(loadings)){
                stop("Number of rows must be greater than columns")
        }
}

#' @title Loadings to Correlation Matrix
#'
#' @description Easy method to generate correlation matricies from factor
#'    loadings (either made up or real)
#'
#' @param loadings the loading matrix to be utilized.
#'
#' @return A correlation matrix
#'
#' @examples
#' loadings <- matrix(c(0.6,0.7,0.1,0.1,0.2,
#'                      0.3,0.1,0.7,0.7,0.1,
#'                      0,0.1,0.2,0.2,0.8),
#'                    ncol = 3)
#' l2c(loadings)
#'
#'
#' @export
#'

l2c <- function(loadings){
        # testing input
        tester.l2c(loadings)

        # calculating the communalities
        communalities <- apply(loadings, 1, function(x) sum(x^2))

        if (sum(communalities >= 1) >= 1){
                warning("heywood case detected... Check loadings")
        }

        # calculating the unique variance
        unique <- 1 - communalities

        # here we will assume correlation between factors is null (can/will change to be more flexable)
        factor_cor <- as.matrix(diag(x = 1, nrow = ncol(loadings), ncol = ncol(loadings)))

        corr_mat <- loadings %*% factor_cor %*% t(loadings) + diag(unique)

        if (sum(corr_mat > 1) >= 1){
                warning("not valid correlation matrix")
        }

        return(corr_mat)
}
