# R package `restbatch` - A small RESTful framework to run batch jobs in the background

> This package is still under development. A road map will be built on Github soon.

Start a simple [RESTful](https://restfulapi.net/) R server running batch jobs anywhere to unleash the computing power

* On local machine
* Runs within a lan that has idle computers or servers (see [remote setups](#remote-setups))
* Runs on a public server, see [safety](#safety) and [remote setups](#remote-setups) (under construction)

The package has the following features

* Schedule tasks without blocking the session
* Runs even the main R session is quitted
* Easy to check task status on browsers or to be integrated into websites (REST api)
* Customizable (see the sections below)

## 1. Installation

```r
# install.packages("remotes")
remotes::install_github("dipterix/restbatch")
```

The package is to be on CRAN once fully tested

## 2. Basic usage

In R or RStudio console

```r
library(restbatch)

default_host("127.0.0.1")
default_port(7033)

start_server()

# wait for a second to ramp up the service
# check if the server is alive
server_alive()
```

A local server will run locally at port `7033`. By default the server does not accept requests from the outside. 

### 2.1 Schedule a task

First, let's create a new task that simply return the process IDs after 2 seconds. The task contains 10 such jobs (`x=1:10`) and the readable name is `Test`.

```r
task <- new_task2(function(x){
  Sys.sleep(2)
  Sys.getpid()
}, x = 1:10, task_name = "Test")

task$readable_name

#> [1] "Test"
```

The task is created locally. However, it will not run until submitted to the restbatch server. To send the task, run

```r
task$submit()

#> Response [http://127.0.0.1:7033/jobs/new]
#> Date: 2021-02-04 11:04
#> Status: 200
#> Content-Type: application/json
#> Size: 30 B
```

If you print the `task` now, there will be messages as follows:

```
Task (client proxy from the `restbatch` package)
Task name : [e5f6226c9f2e6874dd3a7f0944b13dcb__Test]
Total jobs: 10
Submitted to: http://127.0.0.1:7033/
Status for 10 jobs at 2021-02-04 05:04:55:
  Submitted    :  0 (  0.0%)
  -- Queued    :  0 (  0.0%)
  -- Started   :  0 (  0.0%)
  ---- Running :  0 (  0.0%)
  ---- Done    :  0 (  0.0%)
  ---- Error   :  0 (  0.0%)
  ---- Expired :  0 (  0.0%)

*Use `task$resolved()` to check whether the task has finished.
```

Keep down the task name. In this example, the task name is `e5f6226c9f2e6874dd3a7f0944b13dcb__Test`. Now it's safe to close your R session.

Open another R session, the task can be restored via

```r
library(restbatch)
task <- restore_task2("e5f6226c9f2e6874dd3a7f0944b13dcb__Test")
```

To obtain the task results, it is always a good practice to check whether a task has been finished ot not. This could be achieved via methods `resolved` or `server_status` (assuming the server is still running).

```r
task$resolved()
task$server_status()
```

If resolved, or the server status is "finish", you may now collect results by simply call

```r
task$collect()
```

If not resolved, this step will block your current session until the task is finished or errors occur. The result will be a list of length 10 (length of `x` variable).


