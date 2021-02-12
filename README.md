# A small 'RESTful' framework to run batch R scripts in the background

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/restbatch)](https://CRAN.R-project.org/package=restbatch)
[![R-CMD-check](https://github.com/dipterix/restbench/workflows/R-CMD-check/badge.svg)](https://github.com/dipterix/restbench/actions)
<!-- badges: end -->


Starts simple '[RESTful](https://restfulapi.net/)' R servers running batch jobs anywhere to unleash the computing power:

* On local machine
* Runs within a lab that has idle computers or servers (see [remote setups](#3-remote-setups))
* Runs on a public server, see [network safety](#5-network-safety) and [remote setups](#3-remote-setups) (under construction)

The package automatically queues and schedules R tasks that would run hours or days without blocking the main session. It runs even if you exit the main R session. For example, you can run `shiny` app, schedule tasks that run for minutes without freezing the browser. If you have a server running the service, you can schedule batch jobs that runs over-night and pick the results up next morning. The tasks can be monitored and inspected in multiple ways.

Try `restbatch` if you need to run R scripts in the background that take minutes, hours, or even days. Don't use `restbatch` if the tasks are micro-operations (use `clustermq` instead).

## 1. Installation

```r
install.packages("restbatch")
```

The package is to be on CRAN once fully tested

## 2. Basic usage

### 2.1 Start a `restbatch` server

In `R` or `RStudio` console

```r
library(restbatch)

default_host("127.0.0.1")
default_port(7033)

start_server()

# wait for a second to ramp up the service
# check if the server is alive
server_alive()
```

A local server will run locally at port `7033`. 

### 2.2 Schedule a task

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

#> Response [http://127.0.0.1:7033/task/new]
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

To obtain the task results, it is always a good practice to check whether a task has been finished or not. This could be achieved via methods `resolved` or `server_status` (assuming the server is still running).

```r
task$resolved()
task$server_status()
```

If resolved, or the server status is "finish", you may now collect results by simply call

```r
task$collect()
```

If not resolved, this step will block your current session until the task is finished or errors occur. The result will be a list of length 10 (length of `x` variable).

### 2.3 Shutdown a server

Kill the server right now

```r
kill_server(host = "127.0.0.1", port = 7033)
```

All unfinished tasks will be marked as `canceled` immediately. You need to submit the task again to resume them (see section 2.5 below).


If you have previously set the default `host` and `port`, simply call `kill_server()`

To kill the server once your current R session is closed,

```r
autoclose_server(host = "127.0.0.1", port = 7033, auto_close = TRUE)
```

### 2.4 Monitor task(s)

An interactive client viewer is available using R `shiny` package. This viewer can be enabled via

```r
# Install shinydashboard if you haven't done so
# install.packages("shinydashboard")

source(system.file('dashboards/client/app.R', package = 'restbatch'))
```

<img src="https://user-images.githubusercontent.com/8163576/106888900-d6dabe80-66ac-11eb-8c1f-27716a3a7112.png" width="50%">

Alternatively, `task$monitor()` function integrates `RStudio`, creating job panels in via package `rstudioapi`.

```r
# install.packages("rstudioapi")
task$monitor()
```

### 2.5 Resume a task

If a server is killed, all unfinished tasks will be canceled. The tasks can be resumed without re-run everything. This requires to submit with flag `pack=FALSE` and `force=TRUE`:

```r
task$submit(pack=FALSE, force=TRUE)
```

## 3. Remote setup

If you have a spare computer or a server, you can set up a `restbatch` service on that remote computer while submitting the tasks from the local computer. 

<img src="https://user-images.githubusercontent.com/8163576/107587794-08311e00-6bc8-11eb-8677-d22c04659d49.png" width="90%">

This setup requires that the remote server has a dedicated public IP (your own server) or shares the same intranet as your local computer (like computers in the same lab, or home WiFi). 

Deploying a server on the internet may cause security issues. To protect your data, `restbatch` uses `RSA` to authorize the connection and requires both remote server and local computer to possess the same private keys. If authentication is enabled, then you'll need to add a private key to both remote server and local machine. 

### 3.1 Add private keys to both local machine and remote server

* Step 1: you need to get the user ID on your **local computer**.

```r
restbatch::my_userid()
#> [1] "e5f6226c9f2e6874dd3a7f0944b13dcb"
```

* Step 2: on the **remote server**, run the following R command. Remember to replace the `userid` with your actual ID above. Give your local computer a nice nick name using only letters digits and underscore.

```r
pem <- restbatch::generate_pem(userid = "e5f6226c9f2e6874dd3a7f0944b13dcb", 
                               username = "A_nickname", overwrite = TRUE,
                               pem_file = "~/Downloads/restbatch.pem")
                               
pem$password
#> [1] "23ae8b0f"

cat(pem$pem_file)
#> ~/Downloads/restbatch.pem
```


* Step 3: Go to your `~/Download` directory on the **remote server**, where will be a file `restbatch.pem`. The file looks like the followings if opened with `textEdit` or `notepad`. 

```
-----BEGIN ENCRYPTED PRIVATE KEY-----
MIIFHDBOBgkqhkiG9w0BBQ0wQTApBgkqhkiG9w0BBQwwHAQIvzx03ePC40ACAggA
... [omitted]
LsPKHCxgtRZbY9iKzGqOeg==
-----END ENCRYPTED PRIVATE KEY-----
```

Send this file and the password `pem$password` together to your local computer.

* Step 4: on your **local computer**, download the `restbatch.pem`, and run the following R command to add the private key:

```r
restbatch::add_pem(pem_file = "<path to your 'restbatch.pem'>", 
                   password = "<the password created by the server>", 
                   username = "A_nickname")
```

Now, the private key is added to your local machine

### 3.2 Connect to the remote server

To start the service on the **remote server**, run 

```r
restbatch::ensure_server(host="0.0.0.0", port=7033)
```

Once the server is on, you can now send tasks from the local computer. By default, tasks will be sent to your local host ("127.0.0.1"). To set your default host as the remote server, you need to get the server IP. 

I wrote a tool function that can get your server IP address: (run on your server)

```r
restbatch:::get_ip(TRUE)
#> $available
#> [1] "127.0.0.1"    "0.0.0.0"      "10.0.0.29"
#> 
#> $public
#> [1] "128.100.100.101"
```

> If your server is in the intranet, usually the IP is the third one in the first line (in this example, it's "10.0.0.29"). If your server has a public IP address, then use the second line (in this example, it's "128.100.100.101")

Now, go to your **local computer**, run the following command. Remember to change `"10.0.0.29"` to your server IP.

```r
library(restbatch)

# set default host to be the remote server
default_host("10.0.0.29")
default_port(7033)

restbatch::server_alive()
```

If the connect is successful, you will see the following result:

```
[1] TRUE
attr(,"response")
Response [http://10.0.0.29:7033/validate/ping]
  Date: 2021-02-11 00:46
  Status: 200
  Content-Type: application/json
  Size: 2.14 kB
```

Now everything will be the same.

## 4 Service configuration

A `restbatch` service can be customized via a settings file like follows. 

```yml
host: '127.0.0.1'
port: 7033
protocol: http
server_scripts:
  startup_script: '{system.file("scheduler/startup.R", package = "restbatch")}'
  batch_cluster: '{system.file("scheduler/cluster.R", package = "restbatch")}'
modules:
  task: '{system.file("scheduler/task.R", package = "restbatch")}'
  validate: '{system.file("scheduler/validate.R", package = "restbatch")}'
  info: '{system.file("scheduler/info.R", package = "restbatch")}'
options:
  debug: no
  require_auth: yes
  anonymous_request: no
  modules_require_auth: 'task, validate'
  max_concurrent_tasks: 4
  max_concurrent_jobs: 2
  task_queue_interval: 0.5
  max_release_tasks_per_second: 100
  keep_alive: 1800
  task_root: '{restbatch::restbatch_getopt("task_root")}'
  max_nodetime: 864000
  func_newjob: restbatch:::run_task
  func_validate_server: restbatch::handler_validate_server
```

To load a settings file, save the above text in a file `settings.yaml` somewhere, and assign `settings` when starting the server:

```r
# start_server or ensure_server
restbatch::start_server(port, host, settings = "<path to settings.yaml>")
```

I'll explain the main options in this file.

### 4.1 Startup settings

* `startup_script`: the file path to an R script to set up the server before launching services (like connect to databases, or remote nodes etc.). An example (also default script) can be found from `system.file("scheduler/startup.R", package = "restbatch")` (run this command in R to get its dynamic path)
* `batch_cluster`: script to schedule each job within a task. By default, each job will spawn a process locally (on the server). It is also possible to use `docker`, `SGE`, `Slurm`, `OpenLava`, etc. if your server is connected to some computing nodes and provides high-performance parallel services (please check `?batchtools::makeClusterFunctions`)


### 4.2 Web modules & Authentication

The `modules` list contains R [plumber](https://www.rplumber.io/) scripts that provide different web entry points. The default entry points are `task`, `validate`, and `info`. Those module files will be mounted with `plumber::pr_mount`. You can always insert new entry points/modules to the service by including `plumber` files.

* `task`: a `plumber` module that receives, schedules, executes, collects, queries, and removes tasks
* `validate`: a `plumber` module that provides authentication filters (middle-layers) and some server-level operations (shutdown etc.)
* `info`: currently serialize task results as `JSON` formats that can be viewed or downloaded from website

Authentication-related settings:

* `require_auth`: whether modules require authentications. It's highly recommended to be on if deployed remotely, otherwise anyone can run commands on your server.
* `anonymous_request`: whether to use a fake `RSA` key. Only set to `yes` for debug use
* `modules_require_auth`: which modules require authentication? Use `,` to separate. Default are `task` and `validate` modules.
* `keep_alive`: once authorized, how long (seconds) will the token be valid until expired? Default is 1800 seconds (30 minutes). Once expired, the token needs to be renewed.

### 4.3 Parallel settings

* `max_concurrent_tasks`: how many tasks are allowed to run at the same time
* `max_concurrent_jobs`: for each task, how many jobs are allowed.

A task is created from the function `new_task2`, and each task may contain multiple jobs. For example, the following task contains 10 jobs (`x=1:10`, each job corresponds to an element):

```r
task <- restbatch::new_task2(function(x){
  # do something
}, x = 1:10)

task$njobs
#> [1] 10
```

## 5 Network safety

### Scenario 1: Running as a local service

If the `restbatch` service is running locally and the host IP is set to `127.0.0.1`, it is relatively safe. In such case, connections from outside of your machine will not be granted access to the service and you don't need any authentication. However, if you dispatch the service via anything other than `127.0.0.1`, then it is possible that someone outside will try to connect to your machine. Therefore it is highly recommended that `require_auth` is on, `anonymous_request` is off, and `modules_require_auth` covers both `task` and `validate`. 

If you run locally, you don't need to set up private keys because there will be a key automatically generated for local use.

### Scenario 2: Running in an intranet or on a public domain for personal/lab use

Default authentication is required. Don't share your private keys to anyone that you don't trust. If your network is hijacked or compromised, use `generate_pem` with `overwrite=TRUE` on the server to reset your credentials as soon as possible.

### Scenario 3: Public service for arbitrary users

Not recommended to run bare-bone. This is a `restbatch` task is essentially a collection of R scripts that has the potential to run anything. It's highly recommended that you wrap the `restbatch` service into [docker containers](https://www.docker.com/) for each user, such that users' activities are limited to docker container environments.
