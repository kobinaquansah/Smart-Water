# Smart-Water
Using Machine Learning Models  on Water Treatment
This project seeks to investigate the production of smartwater systems using different energy and water saving mechanisms while preserving water supply, treatment and disposal effectiveness. The dataset named "hourly_in.csv" consists of the volume of sewage pumped through the "Eastern Treatment Plant - Melbourne, Australia" on an hourly basis.

# Project Information
The aim of this branch was to create a codebase on top of which a Time Series Forecasting model can be implemented with further improvements. The data in question is the hourly inlet wastewater flow rate at Eastern Treatment Plant, Melbourne, Australia from 2009 to 2018. The Box - Jenkins (specifically AR) model was selected.

# Download
Open "ETP_TS_forecast.zip" and click on "Raw"

# Installation

## Dependencies
The following IDE's need to be installed:
	1. RStudio (only tested on version 3.6)
	2. MySQL Workbench (only tested on version 8.0)

## Instructions
1. From your File Explorer, unzip the file named "ETP_forecast.zip"

2. Open MySQL workbench

	i.Access the MySQL Import/Export Wizard.
	
	ii.Import the file named "hourly_flow_in.csv". Save as "hourly_flow_in" with UTF-8 encoding.
	
	iii.After import is complete, open the SQL script named "FlowTransmission.csc" and run it.

3. Open the R project named "ETP_TS_forecast.Rproj"

	i.Navigate to Session -> Set Working Directory -> To Project Directory
	
	ii.From the "Files" tab/files pane (usually in lower right window), open "database_connector.Rmd",
	"ar_script.R" and "arima_script.R"
	
	iii. Always run database_connector.Rmd first
	
	iv. You are now ready to run "ar_script.R" and/or "arima_script.R"
	
NB. You will be asked for the following information by the ODBC to create a connection between R and the server
	using the following information:
	
	i.Server Address
	
	ii.Username
	
	iii.Password
	
	iv.Port
