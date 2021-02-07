require(testthat)

test_that("Server operation - local UNIX", {

  skip_on_cran()
  skip_if(get_os() == 'windows', message = "Windows require additional system auth, skipped.")

  # test port
  default_host("127.0.0.1")
  default_port(7035)
  default_protocol('http')

  # ensure the server is on
  if(ensure_server()){ on.exit({ try({
    kill_server(port = 7035)
  }, silent = TRUE) }, add = TRUE) }

  testthat::expect_true(isTRUE(server_alive()))

  # Make a task
  task <- new_task2(function(x){ x + 1 }, 1)
  on.exit({
    if(dir.exists(task$task_dir)){
      task$remove()
    }
  }, add = TRUE)


  expect_false(task$submitted)
  expect_error(task$resolved())
  expect_error(task$collect())
  expect_error(task$download(tempfile()))
  expect_false(task$locally_resolved())

  res <- task$submit(pack = TRUE)
  expect_true(res$status_code == 200)

  ret <- task$collect()
  f <- tempfile()
  task$download(f, TRUE)
  s <- task$server_status()
  # expect_equal(s$status, 'finish')
  # This is local task. It's possible that the task hasn't synced
  expect_true(s$status %in% c("finish", "running"))
  expect_false(s$error)
  expect_true(s$message == 'OK')

  expect_equal(ret[[1]], 2)

  task$remove()

  # start another task
  task <- new_task2(function(x){
    if(x == 2){ stop() }
    x + 1
  }, 1:10)
  res <- task$submit()
  expect_true(res$status_code == 200)
  ret <- task$collect()
  expect_true(task$locally_resolved())
  expect_true(task$collected)

  res <- task$collect()
  expect_true(inherits(res[[2]], 'simpleError'))

  kill_server(port = 7035, host = '127.0.0.1')
  task$remove()

})


test_that("Server operation - WINDOWS/CRAN", {

  testthat::expect_true(isTRUE(!server_alive()))

  # Make a task
  task <- new_task2(function(x){ x + 1 }, 1)
  on.exit({
    if(dir.exists(task$task_dir)){
      task$remove()
    }
  }, add = TRUE)


  expect_false(task$submitted)
  expect_error(task$resolved())
  expect_error(task$collect())
  expect_error(task$download(tempfile()))
  expect_false(task$locally_resolved())

  # Not submitted warning
  expect_warning(task$server_status())

  task$remove()
  expect_false(dir.exists(task$task_dir))
  expect_silent(task$remove())

})
