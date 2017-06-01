# EleMovement
Guidance to the files included in this repository

ASCII files (mXXXXXXXndviT.asc) - these files hold Terra-MODIS 16-day composite NDVI values across the Amboseli Ecosystem. Each file represents a 16-day period within the collar period - the date is illuded to in the file name: mXXXXXXXndviT.asc where XXXXXXX provides the Julian date on which the NDVI data collected.

TerraModisNDVI_ImageRetrieve.py - python script used to define the date range and area over which NDVI values should be retrieved. Start and end date, longitude and latitude, and image size are specified. Running the script will output the ASCII files described above.

modisWSDL.py - python script which sits behind the TerraModisNDVI_ImageRetrieve.py script to retrieve the data specified from the Oak Ridge National Laboratory MODIS web service.

AP_collar_model.R - R script used to define projection of NDVI data; extract median NDVI values from the swamp edge and dispersal areas for each 16-day composite throughout the collar period; subset rainfall data; create daily values of NDVI, rainfall and protein content of vegetation; calculate daily motivation indices; model elephant absence or presence from MI model; extract actual absence or presence from collar data; compare model and data using Phi coefficient and Chi-squared.
