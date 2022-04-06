rem set the replicate number
set rep=1

rem (optional) verify that you want to run this replicate (will overwrite if already exists)
set /p=Running Replicate %rep%. Press ENTER to continue...

rem set the drive letter and working directory
set drive=C:\
set workingdir=C:\Landis\Test_Landscape
set resultsdir=C:\Landis\Test_Landscape\Harvest_fastIPSLCM5ALR
rem create the replicate folder Rep#
if not exist %resultsdir%\replicate%rep% mkdir %resultsdir%\replicate%rep%

rem navigate to the replicate folder
C:
cd %resultsdir%\replicate%rep%

rem copy the scenario file into the replicate folder
copy %workingdir%\scenario_fastIPSLCM5ALR.txt

rem run LANDIS
call landis-ii scenario_fastIPSLCM5ALR.txt

rem navigate back to the working directory
C:
cd %workingdir%

pause
