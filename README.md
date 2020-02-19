# Smart-Water
Using Machine Learning Models  on Water Treatment
This project seeks to investigate the production of smartwater systems using different energy and water saving mechanisms while preserving water supply, treatment and disposal effectiveness. The dataset named "Hourly Flowrate" consists of the volume of sewage pumped through the "Eastern Treatment Plant - Melbourne, Australia" on an hourly basis. Data consists of roughly 10 years of data. Training and testing of an AR time series model has been performed on the dataset, but I am yet to perform cross validation. 


The Ammonium Concentration Prediction uses Linear Regression to predict ammonium comcentrations in Ukranian river water. Ammonium ions concentration is measured in mg/cub. dm (ie milligrams in the cubic decimeter).The maximum permissible value of Ammonium ions concentration (NH4) in Ukraine is 0.5 mg/cub.dm. Id - the unique id of a given monthly averaged data; target - a values of monthly 
averaged data of NH4 in target station, mg/cub. dm; 1-7 - a values of monthly averaged data of NH4 in stations 1-7 (in seven stations located from the target station upstream), mg/cub. dm. This dataset can be used to reduce the number of required sensors and preemtively predict ammonium concentrations upstream.


