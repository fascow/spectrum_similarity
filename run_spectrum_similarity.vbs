' windows script to hide cmd window on execution of commands
' doublequotes (chr(34)) need to be added literally to the string

Dim oShell
Dim strArgs

Set oShell = CreateObject("Wscript.Shell") 
strArgs = "cmd /c " + chr(34) + chr(34) + "C:\Program Files\R\R-3.0.2\bin\Rscript.exe" + chr(34) + " --slave --vanilla " + chr(34) + "run_spectrum_similarity.R" + chr(34) + chr(34)

oShell.Run strArgs, 0, false
