# Introduction
The code in this repository analyses the effect of house price
increases on wealth inequality.

In particular, it analyses a decomposition of the household wealth
Gini coefficient into within and between components of inequality
for owners and non-owners of housing wealth.

It also analyses the distribution of household wealth,
jointly with material hardship status and with housing costs status,
for housing owners and non-owners.

# Overview
The scripts are organised in the following order:

0. Install required dependencies
1. Run the TAWA model.
2. Gather extra IDI data needed for the analysis.
3. Carry out analysis of the effect of house price increases on the
Gini coefficient of wealth.
4. Analyse the distribution of wealth, including by material hardship status
and housing cost status.

# Requirements
This code is intended to be run in the IDI (Statistics New Zealand's "Integrated Data Infrastructure"), and has some dependencies on packages and data developed by the Treasury's Analytics & Insights team.

The first script requires access to particular input files and scripts for
running TAWA (the Treasury's Tax and Welfare Analysis model), including
the R package `TAWArun`. Subsequent scripts require weights generated
by the TAWA team. These requirements may be substituted with
Stats-derived data by an expert user, which would be expected to
generate broadly comparable results, but this is not explicitly supported.

# Disclaimer
This code can be modified and customised by users to meet the needs of specific
projects, and in all cases the Analytics and Insights Team,
Te Tai Ōhanga, New Zealand Treasury must be acknowledged as a source.
While all care and diligence has been used in developing this code,
The Treasury gives no warranty it is error free and will not be liable for any
loss or damage suffered as a result of its use, either directly or indirectly.
