      ******************************************************************
      *----------------------------------------------------------------*
      *                                                                *
      *       Copyright(c) by Patrick Beeck                            *
      *       Version: 2.0                                   *
      *                                                                *
      *----------------------------------------------------------------*
      
      *Die korrekte Anzeige von Umlauten über die Anbindung ASCI-Zeichen
      *funktioniert noch nicht, soll aber in einer zukünftigen 
      *Überarbeitung implementiert werden.

      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     HirePatrickBeeckAsTrainee.
       AUTHOR.         PatrickBeeck.
       DATE-WRITTEN.   06-12-2024.

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. PB-PC-COMP.
        OBJECT-COMPUTER. DWPBANK-PC-COMP.
      *
       INPUT-OUTPUT SECTION.
      *---------------------
      *
       FILE-CONTROL.
      *-------------
      *
      *An dieser Stelle kann in einem nächsten Schritt eine separate
      *Daten-Datei zugewiesen werden, welche die unten stehenden Daten 
      *der Working Stoarage Section (oder zumindest einen Teil derer)
      *enthält.
      *
      *z.B. wie folgt:
      *SELECT WORK-EXPERIENCE 
      *ASSIGN TO 'C:\user_name\..\work-experience.txt
      *ORGANIZATION IS LINE SEQUTENTIAL
      *ACCESS MODE IS SEQUENTIAL.
      *
      *SELECT JOB-RESPONSIBILITIES
      *ASSIGN TO 'C:\user_name\..\job-responsibilities.txt
      *ORGANIZATION IS SEQUTENTIAL
      *ACCESS MODE IS SEQUENTIAL:


       DATA DIVISION.
      *==============
      *
       FILE SECTION.
      *-------------
      *
      *An dieser Stelle kann in einem nächsten Schritt die Struktur 
      *einer separaten Daten-Datei beschrieben werden. Es bietet sich
      *an, eine .txt-Datei zu hinterlegen. So muss die Datenstruktur:
      *nur ein einzelnes Mal festgelegt werden und anschließend können
      * die Daten dem Dateiformat entsprechend in einer Daten-Datei
      *erfasst werden. Durch den Wegfall der erneuten Betitelung und
      *Datenstrukturierung im Falle eines neuen, gleichartigen 
      *Datensatzes lässt sich der Schreibaufwand insgesamt reduzieren.
      *
      *FD WORK-EXPERIENCE:
      *01  WORK-EXPERIENCE-RECORD.
      *    05 JOB-ID PIC X(2).
      *    05 FILLER PIC X(3).
      *    05 JOB-TITLE  PIC X(50).
      *    05 FILLER PIC X(3).
      *    05 WS-JOB-EMPLOYER PIC X(27).
      *    05 FILLER PIC X(3).
      *    05 WS-JOB-LOCATION PIC X(11).
      *
      *FD JOB-RESPONSIBILITIES
      *01   RESPONSIBILITIES-RECORD.
      *    05 JOB-ID PIC X(2).
      *    05 FILLER PIC X(3).
      *    05 RESPONSIBILITY-NO PIC X(2).
      *    05 FILLER PIC X(3).
      *    10 JOB-RESPONSBLT PIC X(250).
      

       WORKING-STORAGE SECTION.
      *------------------------
      * 
      *In diesem Bereich werden alle für die Anwendung benötigten Daten
      *deklariert und initialisiert. Die vorliegende Datei soll vollum-
      *fänglich sein und ohne externe Daten-Dateien auskommmen.
      *     

      *Zunächst wird der Inhalt des Kurzprofils festgelegt. Dies erfolgt
      *über alphanumerische Werte in unterschiedlicher Länge. Für jeden
      *Satz wird eine neue Variable angelegt. Es besteht somit ein Satz-
      *limit von 250 Zeichen.
       01 WS-SHORT-PROFILE USAGE IS DISPLAY.
           05 WS-SHORT-PROFILE-SENTENCE1 PIC X(91) VALUE "Im naechsten J
      -       "ahr strebe ich den Einstieg in die IT und damit einen fac 
      -       "hlichen Wechsel an. ".
           05 WS-SHORT-PROFILE-SENTENCE2 PIC X(133) VALUE "Seit Beginn d
      -       "es Bachelorstudiums habe ich immer wieder gemerkt, wie se
      -       "hr mich das Coding, unabhaengig von der Sprache, fesseln 
      -       "kann. ".
           05 WS-SHORT-PROFILE-SENTENCE3 PIC X(86) VALUE "Wohl auch weil
      -       " es den Analytiker, Problemloeser und Perfektionisten in
      -       "mir anspricht. ".
           05 WS-SHORT-PROFILE-SENTENCE4 PIC X(76) VALUE "Ich habe mich
      -       "als vorrangig intrinsisch motivierten Menschen kennengele
      -       "rnt. ".
           05 WS-SHORT-PROFILE-SENTENCE5 PIC X(155) VALUE "Deswegen bin 
      -       "ich der festen Ueberzeugung, dass ich meine Kenntnisse un
      -       "d Faehigkeiten im Bereich der IT und der Anwendungsentwic
      -       "klung zuegig ausbauen kann. ".
           05 WS-SHORT-PROFILE-SENTENCE5 PIC X(195) VALUE "Denn wenn mic
      -        "h etwas interessiert und begeistert, nutze ich meine sch
      -        "nelle Auffassungsgabe, meine Wissbegierde und meine Freu
      -        "de am Lernen, um mir moeglichst schnell viel neues Wisse
      -        "n anzueignen. ".
           05 WS-SHORT-PROFILE-SENTENCE6 PIC X(171) VALUE "Ich freue mic
      -       "h darauf, Teil des Teams zu werden an einem Ort, an dem m
      -       "ir die Chance zum fachlichen Wechsel eingeraeumt wird und
      -       " wo ich einen Platz zum Lernen finden darf. ".
           05 WS-SHORT-PROFILE-NewLine PIC X VALUE X'0A'.
           05 WS-SHORT-PROFILE-NewLine PIC X VALUE X'0A'.
           05 WS-SHORT-PROFILE-SENTENCE7 PIC X(187) VALUE "PS: Der vorli
      -       "egende COBOL-Code ist das Ergebnis meiner ersten Stunden 
      -       "im Umgang mit COBOL und bietet bestimmt noch viel Verbess
      -       "erungspotenzial. Aber erste Ideen hierzu habe ich bereits
      -       "...".     
      
      *Die Berufserfahrung wird unter Angabe eines Jobtitels, der Firma
      *des Arbeitgebers, des Beschäftigungsortes, der Tätigkeiten/
      *Verantwortlichkeiten sowie des Beschäftigungszeitraums angegeben.
      *Es beginnt mit der zuletzt ausgeübten Tätigkeit. Die Auflistung 
      *erfolgt antichronologisch entsprechend des Startdatums.
       01 WS-WORK-EXPERIENCE USAGE IS DISPLAY.
           05 WS-JOB-1.
              10 WS-JOB-TITLE  PIC X(50) VALUE "Associate im Bereich 'Fi
      -          "nancial Advisory Services'".
              10 WS-JOB-EMPLOYER PIC X(27) VALUE "Forvis Mazars GmbH & C
      -          "o. KG".        
              10 WS-JOB-LOCATION PIC X(11) VALUE "Duesseldorf".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-3 PIC X(140) VALUE "- Unterstuetzu 
      -             "ng bei der Erstellung von Unternehmensbewertungen m
      -             "ittels unterschiedlicher Verfahren und der Aufstell 
      -             "ung von Operating Models".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-1 PIC X(109) VALUE "- Wuerdigung v
      -             "on Immobiliengutachten hinsichtlich der Plausibilit
      -             "aet von Annahmen und Wertermittlungsmethodik".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(50) VALUE "- Plausibilisie
      -             "rung von Purchase Price Allocations".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-4 PIC X(104) VALUE "- Datenrecherc
      -             "he fuer die Erstellung von Unternehmensbewertungen, 
      -             " Operating Models und Markenbewertungen".
      *Die Daten des Beschäftigungszeitraumes werden hier auf mehrere 
      *Variablen aufgeteilt. Es ist auch eine einzelne Wertzuweisung
      *denkbar, welche den Beschäftigungszeitraum als Zeichenfolge ohne
      *Leerzeichen beinhaltet im Format MM/YYYY-MM/YYYY. In der 
      *PROCEDURE DIVISION könnte dann unterschiedliche Teile dieses 
      *Wertes verschiedenen Variablen zugewiesen werden, um das Datum in 
      *einer Textausgabe auch mit Leerzeichen anzugeben. 
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 05.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC   9(4) VALUE 2024.
              10 WS-JOB-END.
                 15 WS-JOB-END-MONTH   PIC  9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC  9(4) VALUE 2024.
      
      *Tätigkeit 2, welche als vorletztes ausgeübt wurde und somit am
      *zweitjüngsten ist.
           05 WS-JOB-2.
              10 WS-JOB-TITLE  PIC X(63) VALUE "Werkstudent im Bereich '
      -          "Technology Consulting - Cyber Security'".
              10 WS-JOB-EMPLOYER PIC X(51) VALUE "Ernst & Young GmbH Wir
      -          "tschaftspruefungsgesellschaft".
              10 WS-JOB-LOCATION PIC X(11) VALUE "Duesseldorf".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(230) VALUE "- Vorbereitung
      -             ", Unterstuetzung, Dokumentation und Qualitaetssiche
      -             "rung bei der Durchfuehrung von IT-Grundschutz-Check
      -             "s, Risikoanalysen und Schutzbedarfsfeststellungen s
      -             "owie Erstellung von Prozesssteckbriefen fuer Gescha
      -             "eftsprozesse".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(74) VALUE "- Mitwirkung an
      -             " der Erstellung von IT-Grundschutzchecks und Risiko
      -             "analysen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(120) VALUE "- Erarbeitung 
      -             "und Einfuehrung eines Klassifizierungsschemas fuer 
      -             "die Anforderungen aus dem BSI IT-Grundschutz-Kompen
      -             "dium".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-4 PIC X(176) VALUE "- Aufbereitung
      -             " von Projektergebnissen, Unterstuetzung bei der Ers
      -             "tellung von Angeboten und Erstellung eines internen
      -             " Abwesenheitsplaners unter Verwendung von VBA-Progr
      -             "ammierung".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2022.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 03.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2024.
      
      *Tätigkeit 3, die am drittjüngsten ist.
           05 WS-JOB-3.
              10 WS-JOB-TITLE  PIC X(62) VALUE "Werkstudent im Bereich '
      -          "Asset, Liability & Capital Management'".
              10 WS-JOB-EMPLOYER PIC X(30) VALUE "HSBC Trinkaus & Burkha
      -          "rdt GmbH".
              10 WS-JOB-LOCATION PIC X(11) VALUE "Duesseldorf".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(79) VALUE "- Ueberwachung 
      -             "der Liquiditaet, des Liquiditaetsrisikos und der Ka 
      -             "pitalstruktur".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(55) VALUE "- Erstellung au
      -             "fsichtsrelevanter Berichte und Dokumente".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(31) VALUE "- Datenaufberei
      -             "tung und -pflege".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 07.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2021.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 09.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2022.

      *Tätigkeit 4, die am viertjüngsten ist.
           05 WS-JOB-4.
              10 WS-JOB-TITLE  PIC X(81) VALUE "Praktikant im Bereich 'F
      -          "inancial Services Deal Advisory - Mergers & Acquisitio
      -          "ns'".
              10 WS-JOB-EMPLOYER PIC X(40) VALUE "KPMG AG Wirtschaftspru
      -          "efungsgesellschaft".
              10 WS-JOB-LOCATION PIC X(20) VALUE "Frankfurt am Main".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(70) VALUE "- Aufbau einer
      -             " Datenbank inkl. der Aufbereitung relevanter Kennza
      -             "hlen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(107) VALUE "- Unterstuetzu 
      -             "ng bei der Erstellung von Projektunterlagen sowie D
      -             "urchfuehrung einer Financial Due Diligence".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(63) VALUE "- Informationsr
      -             "echerche bei Datendiensten (Orbis, Mergermarket)".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-4 PIC X(100) VALUE "- Unterstuetzu
      -             "g bei Unternehmensbewertungen und der Aufbereitung 
      -             "von Unternehmensbewertungsmodellen".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2020.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 01.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2021.
      
      *Tätigkeit 5, die am fünftjüngsten ist.
           05 WS-JOB-5.
              10 WS-JOB-TITLE  PIC X(52) VALUE "Werkstudent in der Strat
      -          "egie- und Managementberatung".
              10 WS-JOB-EMPLOYER PIC X(30) VALUE "MOONROC Advisory Partn
      -          "ers GmbH".
              10 WS-JOB-LOCATION PIC X(10) VALUE "Muenchen".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(37) VALUE "- Erstellung vo
      -             "n Wettbewerbsanalysen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(46) VALUE "- Erstellung in
      -             "terner Unterlagen und Dokumente".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 02.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2020.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 07.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2020.
      
      *Tätigkeit 6, die am sechstjüngsten ist.
           05 WS-JOB-6.
              10 WS-JOB-TITLE  PIC X(50) VALUE "Praktikant im Bereich 'A
      -          "ssurance - Audit Services'".
              10 WS-JOB-EMPLOYER PIC X(51) VALUE "Ernst & Young GmbH Wir
      -          "tschaftspruefungsgesellschaft".
              10 WS-JOB-LOCATION PIC X(11) VALUE "Duesseldorf".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(127) VALUE "- Mitwirkung b
      -             "ei Pruefungen von Einzel- und Konzernabschluessen b
      -             "ei Gesellschaften unterschiedlicher Rechtsformen un 
      -             "d Groessen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(79) VALUE "- Mitarbeit bei
      -             " der Berichterstellung zu Jahres- und Konzernabschl
      -             "usspruefungen".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 03.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2019.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 04.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2019.
      
      *Tätigkeit 7, die am siebtjüngsten ist.
                 05 WS-JOB-7.
              10 WS-JOB-TITLE  PIC X(28) VALUE "Wissenschaftliche Hilfsk
      -          "raft".
              10 WS-JOB-EMPLOYER PIC X(53) VALUE "Kernkompetenzzentrum F
      -          "inanz- & Informationsmanagement".
              10 WS-JOB-LOCATION PIC X(10) VALUE "Bayreuth".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(57) VALUE "- Teilverantwor
      -             "tung im Vertrags- und Rechnungsmanagement".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(47) VALUE "- Tutor fuer Gr
      -             "undlagen der Java-Programmierung".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(32) VALUE "- Betreuung der
      -             " Internetpraesenz".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 11.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2018.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 08.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2019.
      
      *Tätigkeit 8, die am achtjüngsten ist.
           05 WS-JOB-8.
              10 WS-JOB-TITLE  PIC X(51) VALUE "Praktikant in der Strate
      -          "gie- und Managementberatung".
              10 WS-JOB-EMPLOYER PIC X(30) VALUE "MOONROC Advisory Partn
      -          "ers GmbH".
              10 WS-JOB-LOCATION PIC X(10) VALUE "Muenchen".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(46) VALUE "- Erstellung vo
      -             "n Use Cases und Rechenmodellen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(73) VALUE "- Unterstuetzun
      -             "g der Betreuung des Business Case und im Projektman
      -             "agement".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(39) VALUE "- Anfertigung v
      -             "on Kundenpraesentationen".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 08.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2018.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2018.
       
      *Die Ausbildungen werden nummeriert und unter Angabe der 
      *ausbildenden Institution, des Programmnamens (üblicherweise 
      *Ausbildungsthema + Abschlussbezeichnung), einer Zusatzinformation
      *sowie des Ausbildungszeitraumes angegeben.
      *Es beginnt mit der zuletzt begonnenen Ausbildung. Die Auflistung 
      *erfolgt antichronologisch entsprechend des Startdatums.

      *Tätigkeit 1, die aktuell noch laufende Ausbildung ist. Dies ist 
      *daran zu erkennen, dass das Ende des Ausbildungszeitraumes mit
      *"heute" und nicht mit einem Datum im Format MM/JJJJ angegeben 
      *wird.
       01 WS-EDUCATION USAGE IS DISPLAY.
           05 WS-DEGREE-1.
              10 WS-SCHOOL   PIC X(21) VALUE "Universitaet zu Koeln".
              10 WS-COURSE-OF-STUDY   PIC X(40) VALUE "Business Administ
      -          "ration: Finance (M.Sc.)".
              10 WS-COURSE-OF-STUDY-ADDITION   PIC X(39) VALUE "Ergaenzu
      -          "ngsbereich: Information Systems".           
              10 WS-DEGREE-START .
                 15 WS-DEGREE-START-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-START-YEAR    PIC 9(4) VALUE 2021.
              10 WS-DEGREE-END  PIC X(5) VALUE "heute".
      
      *Tätigkeit 2, die am zweitjüngsten ist.
           05 WS-DEGREE-2.
              10 WS-SCHOOL   PIC X(31) VALUE "Linnaeus University, (Schw
      -          "eden)".
              10 WS-COURSE-OF-STUDY   PIC X(7) VALUE "Finance".
              10 WS-COURSE-OF-STUDY-ADDITION PIC X(16) VALUE "Auslandsse
      -             "mester".           
              10 WS-DEGREE-START .
                 15 WS-DEGREE-START-MONTH   PIC 9(2) VALUE 09.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-START-YEAR    PIC 9(4) VALUE 2019.
              10 WS-DEGREE-END.
                 15 WS-DEGREE-END-MONTH   PIC 9(2) VALUE 01.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-END-YEAR    PIC 9(4) VALUE 2020.
           
      *Tätigkeit 3, die am drittjüngsten ist.
           05 WS-DEGREE-3.
              10 WS-SCHOOL   PIC X(21) VALUE "Universitaet Bayreuth".
              10 WS-COURSE-OF-STUDY   PIC X(32) VALUE "Betriebswirtschaf
      -          "tslehre (B.Sc.)".
              10 WS-COURSE-OF-STUDY-ADDITION PIC X(56) VALUE "Schwerpunk
      -             "te: Finanzen und Banken, Wirtschaftsinformatik".
              10 WS-DEGREE-START.
                 15 WS-DEGREE-START-MONTH   PIC 9(2) VALUE 04.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-START-YEAR    PIC 9(4) VALUE 2017.
              10 WS-DEGREE-END.
                 15 WS-DEGREE-END-MONTH   PIC 9(2) VALUE 09.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-END-YEAR    PIC 9(4) VALUE 2021.
           
      *Tätigkeit 4, die am viertjüngsten ist.
           05 WS-DEGREE-4.
              10 WS-SCHOOL   PIC X(17) VALUE "Sparkasse Krefeld".
              10 WS-COURSE-OF-STUDY   PIC X(27) VALUE "Ausbildung zum Ba
      -          "nkkaufmann".
              10 WS-COURSE-OF-STUDY-ADDITION PIC X(16) VALUE "IHK-Besten
      -             "ehrung".
              10 WS-DEGREE-START.
                 15 WS-DEGREE-START-MONTH   PIC 9(2) VALUE 08.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-START-YEAR    PIC 9(4) VALUE 2014.
              10 WS-DEGREE-END.
                 15 WS-DEGREE-END-MONTH   PIC 9(2) VALUE 01.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-DEGREE-END-YEAR    PIC 9(4) VALUE 2017.
      
      *Die Auflistung der Kenntnisse und Fähigkeiten hat die Bereiche
      *IT-bezogene Sprachen ("WS-IT-LANGUAGES"), weitere IT-Kenntnisse 
      *(WS-IT-SKILLS) und Kenntnisse (natürlicher, also durch den  
      *Menschen gesprochener) Sprachen (WS-NATURAL-LANGUAGES). Die  
      *Auflistung der Kenntnisse berücksichtigt den Zeitpunkt des 
      *Erwerbs einer Fähigkeit und/oder der Relevanz einer Kenntnis
      *Fähigkeit.
       01 WS-SKILLS USAGE IS DISPLAY.
           05 WS-IT-LANGUAGES.
              10 WS-IT-LANG-1.
                 15 WS-IT-LANG-1-NAME     PIC X(20) VALUE "Java".
                 15 WS-IT-LANG-1-LEVEL    PIC X(15) VALUE "Grundkenntnis
      -             "se".
              10 WS-IT-LANG-2.
                 15 WS-IT-LANG-2-NAME     PIC X(20) VALUE "C++".
                 15 WS-IT-LANG-2-LEVEL    PIC X(15) VALUE "Grundkenntnis
      -             "se".
               10 WS-IT-LANG-3.
                 15 WS-IT-LANG-3-NAME     PIC X(20) VALUE "Python".
                 15 WS-IT-LANG-3-LEVEL    PIC X(15) VALUE "Grundkenntnis
      -             "se".
      *Erlangung der WS-IT-LANG-4 im Rahmen des abgelegten 
      *IT-Zertifikats (siehe Anhänge der Bewerbung)
              10 WS-IT-LANG-4.
                 15 WS-IT-LANG-4-NAME     PIC X(20) VALUE "HTML/CSS/JS/A
      -             "JAX".
                 15 WS-IT-LANG-4-LEVEL    PIC X(15) VALUE "Grundkenntnis
      -             "se".

           05 WS-IT-SKILLS.
      *Erlangung des WS-IT-SKILL-1 im Rahmen des abgelegten 
      *IT-Zertifikats (siehe Anhänge der Bewerbung)
              10 WS-IT-SKLL-1.
                 15 WS-IT-SKILL-1-NAME     PIC X(55) VALUE "Web-Developm
      -             "ent".
                 15 WS-IT-SKILL-1-LEVEL    PIC X(15) VALUE "Grundkenntni
      -             "sse".
              10 WS-IT-SKLL-2.
                 15 WS-IT-SKILL-2-NAME     PIC X(55) VALUE "Zertifiziert
      -             "er BSI IT-Grundschutz-Praktiker".
                 15 WS-IT-SKILL-2-LEVEL    PIC X(15) VALUE SPACE.
              10 WS-IT-SKLL-3.
                 15 WS-IT-SKILL-3-NAME     PIC X(55) VALUE "Microsoft Of
      -             "fice (Excel inkl. VBA, Power Point, Word)".
                 15 WS-IT-SKILL-3-LEVEL    PIC X(20) VALUE "Eweiterte Ke
      -             "nntnisse".

           05 WS-NATURAL-LANGUAGES.
              10 WS-NAT-LANG-1.
                 15 WS-NAT-LANG-1-NAME     PIC X(35) VALUE "Deutsch".
                 15 WS-NAT-LANG-1-LEVEL    PIC X(15) VALUE "Muttersprach
      -          "ler".
              10 WS-NAT-LANG-2.
                 15 WS-NAT-LANG-2-NAME     PIC X(35) VALUE "Englisch".
                 15 WS-NAT-LANG-2-LEVEL    PIC X(09) VALUE "Fliessend".
              10 WS-NAT-LANG-3.
                 15 WS-NAT-LANG-3-NAME     PIC X(35) VALUE "Schwedisch/F
      -          "ranzoesisch/Chinesisch".           
                 15 WS-NAT-LANG-3-LEVEL    PIC X(15) VALUE "Grundkenntni
      -          "sse".
      
      *Die Auflistung meiner ehrenamtlichen Arbeit umfasst zwei 
      *wesentliche Positionen und erfolgt unter Angabe eines Titels
      *(welcher grundsätzlich darüber Aufschluss gibt, wo das Ehrenamt
      *ausgeführt wurde), der Verantwortlichkeiten sowie des Ausübungs-
      *zeitraumes. Die Auflistung erfolgt antichronologisch entsprechend
      *des Startdatums. Die zuerst genannte ehrenamtliche Tätigkeit ist
      *nicht beendet und wird ausgeübt.
       01 WS-VOLUNTARY-WORK USAGE IS DISPLAY.
           05 WS-VOLUNTARY-WORK-1.
              10 WS-VOLTWK-TITLE  PIC X(68) VALUE "Mitglied des Deutsche
      -          "n Roten Kreuzes (DRK-Ortsverein Meerbusch e.V.)".
              10 WS-VOLTWK-RESPONSIBLITIES.
                 15 WS-VOLTWK-RESPONSBLT-1 PIC X(55) VALUE "- Vorstandsm
      -             "itglied und Schatzmeister (11/2024 - heute)".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-VOLTWK-RESPONSBLT-2 PIC X(27) VALUE "- Mitglied d
      -             "er Bereitschaft".
              10 WS-VOLTWK-START.
                 15 WS-VOLTWK-START-MONTH   PIC 9(2) VALUE 08.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-VOLTWK-START-YEAR    PIC 9(4) VALUE 2023.
              10 WS-VOLTWK-END   PIC X(5) VALUE "heute".

           05 WS-VOLUNTARY-WORK-2.
              10 WS-VOLTWK-TITLE  PIC X(122) VALUE "Fachschaft der recht
      -          "s- und wirtschaftswissenschaftlichen Fakultaet (Ressor 
      -          "t fuer Studienzuschuesse), Universitaet Bayreuth".
              10 WS-VOLTWK-RESPONSIBLITIES.
                 15 WS-VOLTWK-RESPONSBLT-1 PIC X(116) VALUE "- Mitglied 
      -          "der Studienzuschusskommission der rechts- und wirtscha
      -          "ftswissenschaftlichen Fakultaet (11/2018 - 11/2019)".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-VOLTWK-RESPONSBLT-2 PIC X(57) VALUE "- Vorstandsm
      -          "itglied und Ressortleiter (10/2018 - 09/2019)".
              10 WS-VOLTWK-START.
                 15 WS-VOLTWK-START-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-VOLTWK-START-YEAR    PIC 9(4) VALUE 2017.
              10 WS-VOLTWK-END.
                 15 WS-VOLTWK-END-MONTH   PIC 9(2) VALUE 09.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-VOLTWK-END-YEAR    PIC 9(4) VALUE 2021.
       
      *Um das Bild zu meiner Person noch etwas zu erweitern, sind 
      *nachfolgend meine Hobbys aufgelistet.
       01 WS-HOBBIES USAGE IS DISPLAY.
           05 WS-SPORT          PIC A(4) VALUE "Judo".
           05 WS-INSTRUMENT     PIC A(15) VALUE "Gitarre spielen".
           05 WS-VOLUNTEERING   PIC X(16) VALUE "Sanitaetsdienste".
           05 WS-OTHER-HOBBIES  PIC X(33) VALUE "Neues lernen und Wissen
      -    " anhaeufen".
      
      *Bei einer Interaktion des Users mit dem Programm wird ein User 
      *Input in alphanumerischer Form gegeben. Dabei soll eine einfache
      *Ja-oder-Nein-Entscheidung mittels einer Boolean-Variable 
      *getroffen werden. Es sollen unterschiedliche Varianten der 
      *Antworten "Ja" und "Nein" möglich sein. Diese werden nachfolgend
      *definiert.
       01 WS-USER-INPUT-TEXT PIC X(5).
       88 WS-TRUE-ANSWER  VALUE "Ja" "ja" "J" "j" "Yes" "yes" "Y" "y"
      -    "True" "TRUE" "true" "1".
       88 WS-FALSE-ANSWER VALUE "Nein" "nein" "N" "n" "No" "no" "False"
      -    "False" "FALSE" "false" "0".
      *Um nicht nur abfragen zu können, ob eine Antwort "Ja" oder "Nein" 
      *lautet, werden alle grundsätzlich validen Eingaben in einer 
      *Variable "WS-VALID-ANSWER" zusammengefasst. Diese kann genutzt
      *werden, um zu entscheiden, ob eine valide Eingabe durch den User
      *getätigt wurde.
       88 WS-VALID-ANSWER VALUE "Ja" "ja" "J" "j" "Yes" "yes" "Y" "y"
      -    "True" "TRUE" "true" "1" "Nein" "nein" "N" "n" "No" "no" 
      -    "0" "False" "FALSE" "false".
      
      *Zur Auswahl einer Nummer in einem Auswahlmenü, erfolgt die 
      *Deklaration einer einstelligen Integer-Variable, welche den
      *User-Input erfasst.
       01 WS-USER-INPUT-NO PIC 9(1).
      
      *Im Auswahlmenü kann die Auswahl erfolgen, dass alle Informationen
      *ausgegeben werden sollen. Wird dies ausgewählt, wird der Wert der
      *Variable "WS-SHOW-ALL" von 0 auf 1 gesetzt. Der Wert der Variable
      *wird anschließend in den Ausgaben der jeweiligen Teilbereiche, 
      *abgefragt. Der erste Abschnitt "PERSONAL-INFO-para" mit der 
      *Information zu den persönlichen Daten wird über die Verarbeitung 
      *des User-Inputs zur Menüauswahl angestoßen. Wenn der Wert der Var
      *"WS-SHOW-All" 1 ist, wird die Ausgabe des nächsten Teilbereichs 
      *angestoßen.
       01 WS-SHOW-ALL PIC 9(1) VALUE 0.

      *
      ******************************************************************
       PROCEDURE DIVISION.
      *===================
           
      * MAIN SECTION.
      **----------------------
      *    PERFORM DIALOG.      
          
      *    STOP RUN.

      ******************************************************************
       DIALOG SECTION.
      *----------------
      *Dieser Dialog ist die Einleitung des Programms und leitet in
      *jedem Fall auf die Auswahl der Informationsbereiche (AUSWAHL 
      *SECTION) weiter.
           DISPLAY "Guten Tag! Suchen Sie einen neuen Trainee?".
           ACCEPT WS-USER-INPUT-TEXT.
           IF NOT WS-VALID-ANSWER
              DISPLAY X'0A' "Bitte geben Sie eine valide Antwort ein. Si
      -               "e koennen die Frage mit {Ja,ja,J,j,Yes,yes,Y,y,1,
      -               "True,TRUE,true} bejahen oder mit {Nein,nein,N,n,N
      -               "o,no,0,False,FALSE,false} verneinen." X'0A'
              GO TO DIALOG
           ELSE 
              IF WS-TRUE-ANSWER
                 DISPLAY X'0A' "Dann habe ich hier auch schon einen pass
      -                  "enden Kandidaten fuer Sie."
              ELSE
                 DISPLAY X'0A' "Schauen Sie sich gerne trotzdem meinen f
      -          "olgenden Vorschlag an."
                 DISPLAY "Vielleicht passt die Person ja in Ihr Team." 
                 DISPLAY "Durch Wissbegierde, Lernbereitschaft und eine 
      -          "schnelle Auffassungsgabe hat die Person bestimmt das P
      -          "otenzial, Ihr Team langfristig gut zu verstaerken."
              END-IF
           END-IF.
           
           PERFORM NEXT-STEP-para.

      ******************************************************************
       AUSWAHL SECTION.
      *----------------
      *Das nachfolgende Menü ist die Ebene, von der aus die 
      *unterschiedlichen Informationen zu meiner Person abegrufen werden 
      *können. Auf dieses wird der User, nach kurzer Bestätigung durch 
      *ENTER, nach einer erfolgten Informationsausgabe zurück- bzw. 
      *weitergeleitet.
      *
           DISPLAY "***************************************************"
           DISPLAY SPACE
           DISPLAY "Welche Informationen moechten Sie erhalten?"
           DISPLAY "(1) Alle"
           DISPLAY "(2) Persoenliche Daten"
           DISPLAY "(3) Kurzprofil"
           DISPLAY "(4) Berufserfahrung"
           DISPLAY "(5) Ausbildung"
           DISPLAY "(6) Kenntnisse"
           DISPLAY "(7) Ehrenamtliche Aktivitaeten"
           DISPLAY "(8) Hobbys"
           DISPLAY "(9) Programm beenden"
           
           DISPLAY X'0A' "Geben Sie bitte eine Nummer von 1 bis 9 ein un
      -            "d bestaetigen Sie mit ENTER.".
           ACCEPT WS-USER-INPUT-NO.
           
           EVALUATE WS-USER-INPUT-NO 
              WHEN 1
                 SET WS-SHOW-ALL TO 1
                 DISPLAY "*********************************************"
                 DISPLAY "*   GESAMTER LEBENSLAUF VON PATRICK BEECK   *"
                 DISPLAY "*********************************************"
      *       Der erste Informationsabschnitt wird hier noch direkt
      *       angestoßen. Anschließend erfolgt in jedem Abschnitt eine 
      *       kurze Abfrage, ob alle Informationen ausgegeben werden
      *       sollen. Falls dies der Fall ist, wird in jedem Abschnitt
      *       auf den nächsten Abschnitt verwiesen, der in der Ausgabe
      *       folgen soll.
                 PERFORM PERSONAL-INFO-para
              WHEN 2
                 PERFORM PERSONAL-INFO-para
              WHEN 3
                 PERFORM SHORT-PROFILE-para
              WHEN 4
                 PERFORM EXPERIENCE-para
              WHEN 5   
                 PERFORM EDUCATION-para
              WHEN 6   
                 PERFORM SKILLS-para
              WHEN 7   
                 PERFORM VOLUNTARY-WORK-para
              WHEN 8   
                 PERFORM HOBBIES-para
              WHEN 9
      *       Das Programm wird nach Ausgabe einer kurzen Nachricht 
      *       beendet.
                 DISPLAY "*********************************************"
                 DISPLAY X'0A' "Ich freue mich auf ein persoenliches Ges
      -                  "praech und den Austausch mit Ihnen." X'0A'
                 DISPLAY "*********************************************"
                 STOP RUN
              WHEN OTHER
                 DISPLAY "Bitte geben Sie eine Nummer von 1 bis 9 ein."
           END-EVALUATE 
           
           PERFORM NEXT-STEP-para.
              
      ******************************************************************
       PERSONAL-INFO-para.
      *----------------
      *Ausgabe einer Information zu den persönlichen Daten
           DISPLAY "***************************************************"
           DISPLAY "Meine persoenlichen Daten koennen dem Anschreiben so
      -            "wie dem Lebenslauf entnommen werden."
           DISPLAY SPACE
           DISPLAY "***************************************************"        
           
           IF WS-SHOW-ALL > 0
              PERFORM SHORT-PROFILE-para
           END-IF
           
           PERFORM NEXT-STEP-para.
                  
      ******************************************************************
       SHORT-PROFILE-para.
      *----------------
      *Ausgabe des Abschnitts "Kurzprofil"
           DISPLAY "***************************************************"
           DISPLAY  WS-SHORT-PROFILE
           DISPLAY SPACE
           DISPLAY "***************************************************"

           IF WS-SHOW-ALL > 0
              PERFORM EXPERIENCE-para
           END-IF
           
           PERFORM NEXT-STEP-para.
             
      ******************************************************************
       EXPERIENCE-para.
      *------------------
      *Ausgabe des Abschnitts "Berufserfahrung"
      *Insbesondere in Abschnitten wie diesem kann der repetitive 
      *Schreibaufwand durch die Einbindung externer Daten-Dateien  
      *verringert werden, da der dynamische Aufruf von Variablen z.B. im 
      *Rahmen einer For-Schleife nicht möglich zu sein scheint in COBOL 
      *(so zumindest ist mein bisheriges Verständnis der Sprache).
           DISPLAY "***************************************************"
           DISPLAY "Berufserfahrung:"
           DISPLAY SPACE
           DISPLAY WS-JOB-START IN WS-JOB-1 " - " WS-JOB-END IN WS-JOB-1
           DISPLAY WS-JOB-TITLE IN WS-JOB-1 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-1 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-1 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-1
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-2 " - " WS-JOB-END IN WS-JOB-2
           DISPLAY WS-JOB-TITLE IN WS-JOB-2 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-2 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-2 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-2
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-3 " - " WS-JOB-END IN WS-JOB-3
           DISPLAY WS-JOB-TITLE IN WS-JOB-3 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-3 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-3 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-3
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-4 " - " WS-JOB-END IN WS-JOB-4
           DISPLAY WS-JOB-TITLE IN WS-JOB-4 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-4 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-4 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-4
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-5 " - " WS-JOB-END IN WS-JOB-5
           DISPLAY WS-JOB-TITLE IN WS-JOB-5 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-5 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-5 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-5
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-6 " - " WS-JOB-END IN WS-JOB-6
           DISPLAY WS-JOB-TITLE IN WS-JOB-6 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-6 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-6 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-6
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-7 " - " WS-JOB-END IN WS-JOB-7
           DISPLAY WS-JOB-TITLE IN WS-JOB-7 
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-7 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-7 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-7
           DISPLAY SPACE
           DISPLAY "***************************"
           DISPLAY WS-JOB-START IN WS-JOB-8 " - " WS-JOB-END IN WS-JOB-8
           DISPLAY WS-JOB-TITLE IN WS-JOB-8
           DISPLAY WS-JOB-EMPLOYER IN WS-JOB-8 
           DISPLAY WS-JOB-LOCATION IN WS-JOB-8 
           DISPLAY WS-JOB-RESPONSIBLITIES IN WS-JOB-8
           DISPLAY SPACE
           DISPLAY "***************************************************"

           IF WS-SHOW-ALL > 0
              PERFORM EDUCATION-para
           END-IF
           
           PERFORM NEXT-STEP-para.
           
      ******************************************************************
       EDUCATION-para.
      *-----------------
      *Ausgabe des Abschnitts "Ausbildung"
      *(siehe Anmerkung zur Reduktion repetitiven Schreibaufwands im 
      *Abschnitt "EXPERIENCE-para")
           DISPLAY "***************************************************"
           DISPLAY "Ausbildung:"
           DISPLAY SPACE
           DISPLAY WS-DEGREE-START IN WS-DEGREE-1 " - " 
      -            WS-DEGREE-END IN WS-DEGREE-1 
           DISPLAY WS-SCHOOL IN WS-DEGREE-1 
           DISPLAY WS-COURSE-OF-STUDY IN WS-DEGREE-1 
           DISPLAY WS-COURSE-OF-STUDY-ADDITION IN WS-DEGREE-1
           DISPLAY SPACE
           DISPLAY WS-DEGREE-START IN WS-DEGREE-2 " - " 
      -            WS-DEGREE-END IN WS-DEGREE-2 
           DISPLAY WS-SCHOOL IN WS-DEGREE-2 
           DISPLAY WS-COURSE-OF-STUDY IN WS-DEGREE-2 
           DISPLAY WS-COURSE-OF-STUDY-ADDITION IN WS-DEGREE-2
           DISPLAY SPACE
           DISPLAY WS-DEGREE-START IN WS-DEGREE-3 " - " 
      -            WS-DEGREE-END IN WS-DEGREE-3 
           DISPLAY WS-SCHOOL IN WS-DEGREE-3 
           DISPLAY WS-COURSE-OF-STUDY IN WS-DEGREE-3 
           DISPLAY WS-COURSE-OF-STUDY-ADDITION IN WS-DEGREE-3
           DISPLAY SPACE
           DISPLAY WS-DEGREE-START IN WS-DEGREE-4 " - " 
      -            WS-DEGREE-END IN WS-DEGREE-4 
           DISPLAY WS-SCHOOL IN WS-DEGREE-4 
           DISPLAY WS-COURSE-OF-STUDY IN WS-DEGREE-4 
           DISPLAY WS-COURSE-OF-STUDY-ADDITION IN WS-DEGREE-4
           DISPLAY SPACE
           DISPLAY "***************************************************"
      
           IF WS-SHOW-ALL > 0
              PERFORM SKILLS-para
           END-IF
           
           PERFORM NEXT-STEP-para.
           
      ******************************************************************
       SKILLS-para.
      *-----------------
      *Ausgabe des Abschnitts "Kenntnisse und Fähigkeiten"
           DISPLAY "***************************************************"
           DISPLAY "Kenntnisse und Faehigkeiten:"
           DISPLAY X'0A' "IT-bezogene Sprachen:"
           DISPLAY WS-IT-LANG-1-NAME " (" WS-IT-LANG-1-LEVEL ")"
           DISPLAY WS-IT-LANG-2-NAME " (" WS-IT-LANG-2-LEVEL ")"
           DISPLAY WS-IT-LANG-3-NAME " (" WS-IT-LANG-3-LEVEL ")"
           DISPLAY WS-IT-LANG-4-NAME " (" WS-IT-LANG-4-LEVEL ")"
           DISPLAY X'0A' "Weitere IT-Kenntisse:"
           DISPLAY WS-IT-SKILL-1-NAME " (" WS-IT-SKILL-1-LEVEL ")"
           DISPLAY WS-IT-SKILL-2-NAME
           DISPLAY WS-IT-SKILL-3-NAME " (" WS-IT-SKILL-3-LEVEL ")"
           DISPLAY X'0A' "Natuerliche Sprachen:"
           DISPLAY WS-NAT-LANG-1-NAME " (" WS-NAT-LANG-1-LEVEL ")"
           DISPLAY WS-NAT-LANG-2-NAME " (" WS-NAT-LANG-2-LEVEL ")"
           DISPLAY WS-NAT-LANG-3-NAME " (" WS-NAT-LANG-3-LEVEL ")"
           DISPLAY SPACE
           DISPLAY "***************************************************"
      
           IF WS-SHOW-ALL > 0
              PERFORM VOLUNTARY-WORK-para
           END-IF
           
           PERFORM NEXT-STEP-para.
           
      ******************************************************************
       VOLUNTARY-WORK-para.
      *-----------------
      *Ausgabe des Abschnitts "Ehrenamtliche Aktivitäten"
           DISPLAY "***************************************************"
           DISPLAY "Ehrenamtliche Aktivitaeten:"
           DISPLAY SPACE
           DISPLAY WS-VOLTWK-START IN WS-VOLUNTARY-WORK-1 " - "
      -            WS-VOLTWK-END IN WS-VOLUNTARY-WORK-1 
           DISPLAY WS-VOLTWK-TITLE IN WS-VOLUNTARY-WORK-1 
           DISPLAY WS-VOLTWK-RESPONSIBLITIES IN WS-VOLUNTARY-WORK-1
           
           DISPLAY SPACE
           DISPLAY WS-VOLTWK-START IN WS-VOLUNTARY-WORK-2 " - "
      -            WS-VOLTWK-END IN WS-VOLUNTARY-WORK-2
           DISPLAY WS-VOLTWK-TITLE IN WS-VOLUNTARY-WORK-2 
           DISPLAY WS-VOLTWK-RESPONSIBLITIES IN WS-VOLUNTARY-WORK-2
           DISPLAY SPACE
           DISPLAY "***************************************************"
           
           IF WS-SHOW-ALL > 0
              PERFORM HOBBIES-para
           END-IF
           
           PERFORM NEXT-STEP-para.
           
      ******************************************************************
       HOBBIES-para.
      *-----------------
      *Ausgabe des Abschnitts "Hobbys"
           DISPLAY "***************************************************"
           DISPLAY "Hobbys:" WS-SPORT ", " WS-INSTRUMENT ", " 
      -    WS-VOLUNTEERING ", " WS-OTHER-HOBBIES
           DISPLAY SPACE
           DISPLAY "***************************************************"
           
           PERFORM NEXT-STEP-para.
      
      ******************************************************************
       NEXT-STEP-para.
      *-----------------
      *Ein Abschnitt, welcher den User zur Betätigung der ENTER-Taste
      *aufruft, um fortzufahren. Dieser wird eingebunden, sodass neue
      *Ausgaben möglichst weit unten und nicht mittig des Terminals,
      *oberhalb der erneuten Anzeige des Menüs erscheinen. So soll der 
      *User mehr Übersicht behalten und den Blick hauptsächlich auf der 
      *unteren Hälfte des Terminals belassen können.
           DISPLAY SPACE
           DISPLAY "Druecken Sie die ENTER-Taste, um fortzufahren..."
           ACCEPT WS-USER-INPUT-TEXT
      
           GO TO AUSWAHL.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************


