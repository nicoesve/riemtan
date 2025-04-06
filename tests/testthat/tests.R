# Create test data
test_pd_mats <- list(
    Matrix::Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
        Matrix::nearPD() |> _$mat |> Matrix::pack(),
    Matrix::Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
        Matrix::nearPD() |> _$mat |> Matrix::pack()
)

# Create metric objects for each geometry
airm <- metric(
    log = airm_log,
    exp = airm_exp,
    vec = airm_vec,
    unvec = airm_unvec
)

log_euclidean <- metric(
    log = log_euclidean_log,
    exp = log_euclidean_exp,
    vec = log_euclidean_vec,
    unvec = log_euclidean_unvec
)

euclidean <- metric(
    log = euclidean_log,
    exp = euclidean_exp,
    vec = euclidean_vec,
    unvec = euclidean_unvec
)

log_cholesky <- metric(
    log = log_cholesky_log,
    exp = log_cholesky_exp,
    vec = log_cholesky_vec,
    unvec = log_cholesky_unvec
)

bures_wasserstein <- metric(
    log = bures_wasserstein_log,
    exp = bures_wasserstein_exp,
    vec = bures_wasserstein_vec,
    unvec = bures_wasserstein_unvec
)

# Function to generate tests for a given metric
test_metric <- function(metric_obj, metric_name) {
    test_that(sprintf("%s exponential map preserves matrix dimensions and positive definiteness", metric_name), {
        result <- metric_obj$exp(test_pd_mats[[1]], test_pd_mats[[2]])
        expect_equal(result@Dim, c(2, 2))
        expect_true(inherits(result, "dppMatrix"))
    })

    test_that(sprintf("%s logarithm map preserves matrix dimensions and symmetry", metric_name), {
        result <- metric_obj$log(test_pd_mats[[1]], test_pd_mats[[2]])
        expect_equal(result@Dim, c(2, 2))
        expect_true(inherits(result, "dspMatrix"))
    })

    test_that(sprintf("%s logarithm and exponential are mutual inverses", metric_name), {
        # log(exp(v)) ≈ v
        v <- metric_obj$log(test_pd_mats[[1]], test_pd_mats[[2]])
        x <- metric_obj$exp(test_pd_mats[[1]], v)
        comp1 <- metric_obj$log(test_pd_mats[[1]], x)
        expect_lt(Matrix::norm(v - comp1, "F") / Matrix::norm(v, "F"), 1e-1)

        # exp(log(p)) ≈ p
        comp2 <- metric_obj$exp(
            test_pd_mats[[1]],
            metric_obj$log(test_pd_mats[[1]], test_pd_mats[[2]])
        )
        expect_lt(Matrix::norm(test_pd_mats[[2]] - comp2, "F") /
            Matrix::norm(test_pd_mats[[2]], "F"), 1e-1)
    })

    test_that(sprintf("%s vectorization produces correct output dimensions", metric_name), {
        v <- metric_obj$log(test_pd_mats[[1]], test_pd_mats[[2]])
        w <- metric_obj$vec(test_pd_mats[[1]], v)
        expect_true(inherits(w, c("vector", "numeric")))
        expect_equal(length(w), 3) # For 2x2 symmetric matrices
    })

    test_that(sprintf("%s vectorization and unvectorization are mutual inverses", metric_name), {
        # vec -> unvec -> vec
        v <- metric_obj$log(test_pd_mats[[1]], test_pd_mats[[2]])
        vec_result <- metric_obj$vec(test_pd_mats[[1]], v)
        unvec_result <- metric_obj$unvec(test_pd_mats[[1]], vec_result)
        vec_again <- metric_obj$vec(test_pd_mats[[1]], unvec_result)
        expect_lt(norm(as.matrix(vec_result - vec_again, ncol = 1)) /
            norm(as.matrix(vec_result, ncol = 1)), 1e-1)

        # unvec -> vec -> unvec
        w <- rep(1, 3)
        unvec_first <- metric_obj$unvec(test_pd_mats[[1]], w)
        vec_mid <- metric_obj$vec(test_pd_mats[[1]], unvec_first)
        unvec_again <- metric_obj$unvec(test_pd_mats[[1]], vec_mid)
        expect_lt(Matrix::norm(unvec_first - unvec_again, "F") /
            Matrix::norm(unvec_first, "F"), 1e-1)
    })

    test_that(sprintf("%s CSample operations work correctly", metric_name), {
        sample <- CSample$new(conns = test_pd_mats, metric = metric_obj)

        # Test basic initialization
        expect_identical(sample$connectomes, test_pd_mats)
        expect_equal(sample$sample_size, 2)
        expect_equal(sample$matrix_size, 2)
        expect_equal(sample$mfd_dim, 3)

        # Test tangent space operations
        sample$compute_tangents()
        expect_false(is.null(sample$tangent_images))
        expect_true(inherits(sample$tangent_images[[1]], "dspMatrix"))

        # Test vectorization
        sample$compute_vecs()
        expect_false(is.null(sample$vector_images))
        expect_true(is.matrix(sample$vector_images))

        # Test statistical operations
        sample$compute_fmean()
        expect_false(is.null(sample$frechet_mean))
        expect_true(inherits(sample$frechet_mean, "dppMatrix"))

        sample$compute_variation()
        expect_false(is.null(sample$variation))
        expect_true(is.numeric(sample$variation))
        expect_true(sample$variation > 0)

        sample$compute_sample_cov()
        expect_false(is.null(sample$sample_cov))
        expect_true(isSymmetric(sample$sample_cov))
    })
}

# Run tests for each metric
test_metric(airm, "AIRM")
test_metric(log_euclidean, "Log-Euclidean")
test_metric(euclidean, "Euclidean")
test_metric(log_cholesky, "Log-Cholesky")
test_metric(bures_wasserstein, "Bures-Wasserstein")
