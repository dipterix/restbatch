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

## Installation

```r
# install.packages("remotes")
remotes::install_github("dipterix/restbatch")
```

The package is to be on CRAN once fully tested

## Basic usage

