# tsa

[![Travis CI build status](https://travis-ci.org/dzhus/tsa.svg)](https://travis-ci.org/dzhus/tsa)
[![Hackage](https://img.shields.io/hackage/v/tsa.svg?colorB=5e5184&style=flat)](https://hackage.haskell.org/package/tsa)
[![Hackage deps](https://img.shields.io/hackage-deps/v/tsa.svg)](http://packdeps.haskellers.com/feed?needle=tsa)

Time Series Analysis and Forecasting in Haskell.

## Features

- [ ] stationarizing, de-trending (log)

- [ ] Standard error, confidence intervals

- [ ] ACF, PACF

- [ ] MLE

- [ ] residual plots

- [ ] a Jupyter Notebook showcasing library features

## Box-Jenkins approach

The general approach to time series forecasting consists of three
stages:

- Identification (observing autocorrelation patterns, choosing a model)

- Estimation (fitting the model using MLE)

- Diagnosis (examining residuals and checking if the fit is good)

## Implementation notes

- The general form of ACF drives the choice of model.

- There're special cases of a more general ARIMA(p, q, d) model which
  are simpler.

- In general case, ARIMA parameters are estimated by MLE. OLS will
  *not* give the same results.

- AR(1) is a linear model which in theory can be solved by OLS. MA(1)
  is not though and needs MLE.

- Employ unit root test to see if series is difference-stationary.

## Alternatives

There're other Haskell packages for working with time series:

- [timeseries][]:

    - Uses custom type for series

    - Provides several simple operations

- [kalman][]:

    - Probably usable for building MLE equations

## Running notebooks

```
cd notebooks
pipenv install
stack build ihaskell
pipenv run stack exec ihaskell install
pipenv run stack exec jupyter notebook
```

[timeseries]: https://hackage.haskell.org/package/timeseries

[kalman]: https://hackage.haskell.org/package/kalman
