# taganomaly
Anomaly detection labeling tool, specifically for multiple time series (one time series per category).

Taganomaly is a tool for creating labeled data for anomaly detection models. It allows the labeler to select points on a time series, further inspect them by looking at the behavior of other times series at the same time range, or by looking at the raw data that created this time series (assuming that the time series is an aggregated metric, counting events per time range)

:exclamation: **Note: This tool was built as a part of a [customer engagement](https://www.microsoft.com/developerblog/2019/01/02/real-time-time-series-analysis-at-scale-for-trending-topics-detection/), and is not maintained on a regular basis.**


Click here to deploy on Azure using [Azure Container Instances](https://azure.microsoft.com/en-us/services/container-instances/):
[![Deploy to Azure](http://azuredeploy.net/deploybutton.png)](https://azuredeploy.net/?repository=https://github.com/omri374/taganomaly)

## Table of contents

- [Using the app](#using-the-app)
  * [The labeling window](#the-labeling-window)
    + [Time series labeling](#time-series-labeling)
    + [Selected points table view](#selected-points-table-view)
    + [View raw data for window (if exists)](#view-raw-data-for-window-if-exists)
  * [Compare this category with others over time](#compare-this-category-with-others-over-time)
  * [Find proposed anomalies using the Twitter AnomalyDetection package](#find-proposed-anomalies-using-the-twitter-anomalydetection-package)
  * [Observe the changes in distribution between categories](#observe-the-changes-in-distribution-between-categories)
- [How to run locally](#how-to-run-locally)
  * [using R](#using-r)
  * [Using Docker](#using-docker)
- [How to deploy using docker](#how-to-deploy-using-docker)
  * [Deploy to Azure](#deploy-to-azure)
  * [Pull the image manually](#pull-the-image-manually)
- [Building from source](#building-from-source)
- [Instructions of use](#instructions-of-use)
- [Current limitations](#current-limitations)
- [Contributing](#contributing)

## Using the app
The app has four main windows:
### The labeling window
![UI](https://github.com/Microsoft/taganomaly/raw/master/assets/ui.png)
#### Time series labeling
![Time series](https://github.com/Microsoft/taganomaly/raw/master/assets/ts.png)

#### Selected points table view
![Selected points](https://github.com/Microsoft/taganomaly/raw/master/assets/selected.png)

#### View raw data for window if exists
![Detailed data](https://github.com/Microsoft/taganomaly/raw/master/assets/detailed.png)


### Compare this category with others over time
![Compare](https://github.com/Microsoft/taganomaly/raw/master/assets/compare.png)

### Find proposed anomalies using the Twitter AnomalyDetection package
![Reference results](https://github.com/Microsoft/taganomaly/raw/master/assets/twitter.png)

### Observe the changes in distribution between categories
This could be useful to understand whether an anomaly was univariate or multivariate
![Distribution comparison](https://github.com/Microsoft/taganomaly/raw/master/assets/dist.png)

## How to run locally

### using R

This tool uses the [shiny framework](https://shiny.rstudio.com/) for visualizing events.
In order to run it, you need to have [R](https://mran.microsoft.com/download) and preferably [Rstudio](https://www.rstudio.com/products/rstudio/download/).
Once you have everything installed, open the project ([taganomaly.Rproj](taganomaly/taganomaly.Rproj)) on R studio and click `Run App`, or call runApp() from the console. You might need to manually install the required packages

#### Requirements
- R (3.4.0 or above)
#### Used packages: 
- shiny
- dplyr
- gridExtra
- shinydashboard
- DT
- ggplot2
- shinythemes
- AnomalyDetection

### Using Docker

Pull the image from Dockerhub:

```sh
docker pull omri374/taganomaly
```

Run:

```sh
docker run --rm -p 3838:3838 omri374/taganomaly
```

## How to deploy using docker

### Deploy to Azure

[Deploy to Azure Web App for Containers or Azure Container Instances](https://azuredeploy.net/). More details [here (webapp)](https://azure.microsoft.com/en-us/services/app-service/containers/) and [here (container instances)](https://azure.microsoft.com/en-us/services/container-instances/)

### Pull the image manually

Deploy [this image](https://hub.docker.com/r/omri374/taganomaly/) to your own environment.

## Building from source
In order to build a new Docker image, run the following commands from the root folder of the project:

```sh
sudo docker build -t taganomaly .
```

If you added new packages to your modified TagAnomaly version, make sure to specify these in the Dockerfile.

Once the docker image is built, run it by calling

```sh
docker run -p 3838:3838 taganomaly
```

Which would result in the shiny server app running on port 3838.


## Instructions of use
1. Import time series CSV file. Assumed structure:
- date ("%Y-%m-%d %H:%M:%S")
- category
- value

2. (Optional) Import raw data time series CSV file. If the original time series is an aggreation over time windows, this time series is the raw values themselves. This way we could dive deeper into an anomalous value and see what it is comprised of.
Assumed structure:
- date ("%Y-%m-%d %H:%M:%S")
- category
- value

2. Select category (if exists)

3. Select time range on slider

4. Inspect your time series:
(1): click on one time range on the table below the plot to see raw data on this time range
(2): Open the "All Categories" tab to see how other time series behave on the same time range.

4.Select points on plot that look anomalous.

5. Click "Add selected points" to add the marked points to the candidate list.

7. Once you decide that these are actual anomalies, save the resulting table to csv by clicking on "Download labels set" and continue to the next category.

## Current limitations
Points added but not saved will be lost in case the date slider or categories are changed, hence it is difficult to save multiple points from a complex time series. Once all segments are labeled, one can run the provided [prep_labels.py](https://github.com/Microsoft/TagAnomaly/blob/master/prep_labels.py) file in order to concatenate all of TagAnomaly's output file to one CSV.


## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.microsoft.com.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
