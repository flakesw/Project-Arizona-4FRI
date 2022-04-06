rem set the replicate number
set rep=41

rem (optional) verify that you want to run this replicate (will overwrite if already exists)
set /p=Running Replicate %rep%. Press ENTER to continue...

rem set the drive letter and working directory
set drive=E:\
set workingdir=E:\Landis
set resultsdir=E:\Landis\Harvest_moderateIPSLCM5ALR
rem create the replicate folder Rep#
if not exist %resultsdir%\replicate%rep% mkdir %resultsdir%\replicate%rep%

rem navigate to the replicate folder
E:
cd %resultsdir%\replicate%rep%

rem copy the scenario file into the replicate folder
copy %workingdir%\scenario_moderateIPSLCM5ALR_30yrspinup.txt

rem run LANDIS
call landis-ii-7 scenario_moderateIPSLCM5ALR_30yrspinup.txt

rem navigate back to the working directory
E:
cd %workingdir%

pause
