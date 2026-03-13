test_that("check_python_deps returns a logical value", {
  result <- check_python_deps()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("ensure_python_env is a function", {
  expect_true(is.function(ensure_python_env))
})

test_that("import_python is a function", {
  expect_true(is.function(import_python))
})
