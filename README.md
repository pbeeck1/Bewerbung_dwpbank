Dieses Github-Repository dient vorrangig dazu, meinen in COBOL verfassten Lebenslauf (CV.exe) zur Verfügung zu stellen. Darüber hinaus lassen sich hier aber auch weitere Bewerbungsunterlagen im PDF-Format finden und abrufen.

Die CBL-Datei wurde in VS Code erstellt. Als Compiler wurde GnuCOBOL verwendet. Um die Anwendung "CV" zu erstellen und diese wiederum über das Terminal zu starten, benötigte es die folgenden Schritte:

1. Die Datei "CV.cbl" in VS Code öffnen.

2. Ein neues Terminal starten.

3. Zum Ordner von GnuCOBOL navigieren. Der Ordner wurde unter C:\gnucobol angelegt. Entsprechend lautet der eingegebene Navigationsbefehl 'cd C:\gnucobol'.

4. Mittels des Befehls 'set_env' werden die Umgebungsvariablen gesetzt und abgerufen.

5. Optional erfolgt die Navigation zum Ablageordner der erstellten COBOL-Datei, bspw. den Unterordner C:\gnuCobol\cobol files. Zu diesem kann mit dem Befehl 'cd cobol files'navigiert werden.

6. Mit dem Befehl 'cobc -x CV.cbl' wird der COBOL-Compiler gestartet und die Datei 'CV.cbl' durch den Compiler verarbeitet.

7. Anschließend kann mittels '.\CV' die CV-Anwendung im Terminal gestartet werden. Die Interaktion mit der CV-Anwendung erfolgt dann rein über das Terminal.

8. Nach Auswahl des Programmendes im Menü der Anwendung (Ziffer 9, ENTER) wird das Terminal durch Eingabe von 'exit' geschlossen.

