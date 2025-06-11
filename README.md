# Leeds MATSim-DRT Model

This repository contains the codebase for a MATSim model of the city of Leeds, UK. The model is used to study the impact of introducing service-area based Demand-Responsive Transport (DRT) under different service configurations.

## Table of Contents

- [Overview](#overview)
- [Input Data](#input-data)
  - [Transport Supply](#transport-supply)
  - [Travel Demand](#travel-demand)
- [Model Calibration](#model-calibration)
- [Getting Started](#getting-started)

## Overview

- This MATSim model leverages the [Discrete Mode Choice contrib](https://github.com/matsim-org/matsim-libs/tree/main/contribs/discrete_mode_choice) for replanning, instead of traditional MATSim scoring.
- The mode choice model is based on [Tsoleridis et al. 2022](https://doi.org/10.1016/j.tra.2022.08.016).
- DRT is integrated into the choice model and can be used for standalone trips or as feeder trips to public transport, using [eqasim](https://github.com/eqasim-org/eqasim-java/blob/develop/docs/on_demand_mobility.md).

## Input Data

### Transport Supply

- **Bus:** GTFS data from [Bus Open Data Service](https://www.gov.uk/guidance/find-and-use-bus-open-data)
- **Rail:** CIF data from [Rail Delivery Group](https://data.atoc.org/), converted to GTFS using [UK2GTFS](https://itsleeds.github.io/UK2GTFS/)

### Travel Demand

- Generated using a bespoke [activity-based pipeline](https://github.com/Urban-Analytics-Technology-Platform/acbm), which can produce MATSim input for any area in England.

## Model Calibration

- The base model (without DRT) is calibrated to match aggregate Leeds mode shares. See [Calibration script](https://github.com/Hussein-Mahfouz/matsim-drt/blob/calibration-iterative/matsim-leeds/bash/Calibration/CalibrationIterativeCluster.sh).
- Future calibration improvements could include:
    - Mode share by distance bracket or area
    - Traffic assignment calibration (using [cadyts](https://github.com/matsim-org/matsim-libs/blob/main/contribs/cadytsIntegration/README.md))

## Getting Started

See the [Wiki](https://github.com/Hussein-Mahfouz/matsim-drt/wiki) for detailed installation instructions, setup steps, and script run order.

> **Warning:** The instructions and notes in the [Wiki](https://github.com/Hussein-Mahfouz/matsim-drt/wiki) were compiled by me as I was learning to use MATSim and its relevant extensions. They contain observations / options for how to do things. I think it is useful as not everything is documented well when it comes to data preperation, but it is not a fully fledged instruction manual :)

## Using the model

I plan to develop the documentation in the wiki so that anyone can run this model on their own. In the meantime, if you are interested in using it, reach out and I can help you get started
