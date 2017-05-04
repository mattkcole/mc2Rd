context("correlation matrix is positive semi-definite")

test_that("output is a valud correlation matrix", {

        loadings1 <- matrix(c(0.6,0.7,0.1,0.1,0.2,
                             0.3,0.1,0.7,0.7,0.1,
                             0,0.1,0.2,0.2,0.8),
                           ncol = 3)

        # generating corr matrix
        cor.1 <- l2c(loadings1)

        # generating eigen values
        evals.1 <- base::eigen(cor.1)$values

        expect_gte(min(evals.1), 0, "Eigen values greather than zero")

        expect_gte(min(cor.1), -1, "min value greather than -1")

        expect_lte(max(cor.1), 1, "max value less than 1")

})
