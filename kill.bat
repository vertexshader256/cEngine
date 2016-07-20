@echo off
if exist RUNNING_PID (
  setlocal EnableDelayedExpansion
  set /p PLAY_PID=<RUNNING_PID
  echo killing pid !PLAY_PID!
  taskkill /F /PID !PLAY_PID!
  del RUNNING_PID
  endlocal
) 