      ******************************************************************
      *----------------------------------------------------------------*
      *                                                                *
      *       Copyright(c) by Patrick Beeck                            *
      *                       An der Holzung 3, 40668 Meerbusch        *
      *                       +49 170 7693508                          *
      *                       patrick.beeck@web.de                     *
      *                                                                *
      *----------------------------------------------------------------*
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     HirePatrickBeeckAsTrainee.
       AUTHOR.         PatrickBeeck.
       DATE-WRITTEN.   03-12-2024.

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
       DATA DIVISION.
      *==============
      *
       FILE SECTION.
      *-------------
      *
       WORKING-STORAGE SECTION.
      *------------------------
      *
       01 WS-PERSONAL-INFO USAGE IS DISPLAY.
           05 WS-NAME     PIC A(13) VALUE "Patrick Beeck".
           05 WS-ADDRESS  PIC X(33) VALUE "An der Holzung 3, 40668 Meerb
      -                                   "usch".
           05 WS-PHONE    PIC X(20) VALUE "+49 170 7693508".
           05 WS-EMAIL    PIC X(50) VALUE "patrick.beeck@web.de".
       
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
                 15 WS-JOB-RESPONSBLT-1 PIC X(100) VALUE "- Wuerdigung v
      -             "on Immobiliengutachten hinsichtlich der Plausibilit
      -             "aet von Annahmen und Wertermittlung".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(75) VALUE "- Wuerdigung vo 
      -             "n Purchase Price Allocations hinsichtlich der Plaus
      -             "ibilitaet".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-4 PIC X(103) VALUE "- Datenrecherc
      -             "he für die Erstellung von Unternehmensbewertungen, 
      -             "Operating Models und Markenbewertungen".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 05.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC   9(4) VALUE 2024.
              10 WS-JOB-END.
                 15 WS-JOB-END-MONTH   PIC  9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC  9(4) VALUE 2024.

           05 WS-JOB-2.
              10 WS-JOB-TITLE  PIC X(63) VALUE "Werkstudent im Bereich '
      -          "Technology Consulting - Cyber Security'".
              10 WS-JOB-EMPLOYER PIC X(51) VALUE "Ernst & Young GmbH Wir
      -          "tschaftspruefungsgesellschaft".
              10 WS-JOB-LOCATION PIC X(11) VALUE "Duesseldorf".
              10 WS-JOB-RESPONSIBLITIES.
                 15 WS-JOB-RESPONSBLT-1 PIC X(70) VALUE "- Ausarbeitung
      -             " eines Leitfadens zum Umgang mit dem BSI IT-Grundsc
      -             "hutz".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-2 PIC X(74) VALUE "- Mitwirkung an
      -             " der Erstellung von IT-Grundschutzchecks und Risiko
      -             "analysen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-3 PIC X(73) VALUE "- Unterstuetzun 
      -             "g bei der Protokollerstellung im Rahmen von OT-Bewe
      -             "rtungen".
                 15 Filler PIC X VALUE X'0A'.
                 15 WS-JOB-RESPONSBLT-4 PIC X(67) VALUE "- Mitarbeit an 
      -             "Projektunterlagen, Angeboten und internen Dokumente
      -             "n".
              10 WS-JOB-START.
                 15 WS-JOB-START-MONTH   PIC 9(2) VALUE 10.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-START-YEAR    PIC 9(4) VALUE 2022.
              10 WS-JOB-END .
                 15 WS-JOB-END-MONTH   PIC 9(2) VALUE 03.
                 15 Filler               PIC X VALUE "/". *> Trenn-Slash
                 15 WS-JOB-END-YEAR    PIC 9(4) VALUE 2024.

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
                 15 WS-IT-LANG-3-NAME     PIC X(20) VALUE "HTML/CSS/JS/A
      -             "JAX".
                 15 WS-IT-LANG-3-LEVEL    PIC X(15) VALUE "Grundkenntnis
      -             "se".

           05 WS-IT-SKILLS.
              10 WS-IT-SKLL-1.
                 15 WS-IT-SKILL-1-NAME     PIC X(55) VALUE "Zertifiziert
      -             "er BSI IT-Grundschutz-Praktiker".
                 15 WS-IT-SKILL-1-LEVEL    PIC X(15) VALUE "Grundkenntni
      -             "sse".
              10 WS-IT-SKLL-2.
                 15 WS-IT-SKILL-2-NAME     PIC X(55) VALUE "Microsoft Of
      -             "fice (Excel inkl. VBA, Power Point, Word)".
                 15 WS-IT-SKILL-2-LEVEL    PIC X(20) VALUE "Eweiterte Ke
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
              10 WS-VOLTWK-TITLE  PIC X(99) VALUE "Fachschaft der recht
      -          "s- und wirtschaftswissenschaftlichen Fakultaet, Ressor 
      -          "t fuer Studienzuschuesse".
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
       
       01 WS-HOBBIES USAGE IS DISPLAY.
           05 WS-SPORT          PIC A(4) VALUE "Judo".
           05 WS-INSTRUMENT     PIC A(7) VALUE "Gitarre".
           05 WS-VOLUNTEERING   PIC X(16) VALUE "Sanitaetsdienste".
           05 WS-OTHER-HOBBIES  PIC X(33) VALUE "Neues lernen und Wissen
      -    " anhaeufen".

       01 WS-USER-INPUT-TEXT PIC X(5).
       88 WS-TRUE-ANSWER  VALUE "Ja" "ja" "J" "j" "Yes" "yes" "Y" "y"
      -    "True" "TRUE" "true" "1".
       88 WS-FALSE-ANSWER VALUE "Nein" "nein" "N" "n" "No" "no" "False"
      -    "False" "FALSE" "false" "0".
       88 WS-VALID-ANSWER VALUE "Ja" "ja" "J" "j" "Yes" "yes" "Y" "y"
      -    "True" "TRUE" "true" "1" "Nein" "nein" "N" "n" "No" "no" 
      -    "0" "False" "FALSE" "false".
      
       01 WS-USER-INPUT-NO PIC 9(1).

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
           DISPLAY "Guten Tag! Suchen Sie einen neuen Trainee?".
           ACCEPT WS-USER-INPUT-TEXT.
           IF NOT WS-VALID-ANSWER
              DISPLAY X'0A' "Bitte geben Sie eine valide Antwort ein. Si
      -               "e die Frage mit {Ja,ja,J,j,Yes,yes,Y,y,1,True,TRU
      -               "E,true} bejahen oder mit {Nein,nein,N,n,No,no,
      -               "0,False,FALSE,false} verneinen." X'0A'
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
           
           DISPLAY X'0A' "Geben Sie bitte eine Nummer von 1 bis 9 ein
      -            "und bestaetigen Sie mit ENTER.".
           ACCEPT WS-USER-INPUT-NO.
           
           EVALUATE WS-USER-INPUT-NO 
              WHEN 1
                 SET WS-SHOW-ALL TO 1
                 DISPLAY "*********************************************"
                 DISPLAY "*   GESAMTER LEBENSLAUF VON PATRICK BEECK   *"
                 DISPLAY "*********************************************"
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
           DISPLAY "***************************************************"
           DISPLAY "Zur Person:"
           DISPLAY X'0A' "Name: ", WS-NAME 
           DISPLAY "Adresse: ", WS-ADDRESS
           DISPLAY "Mobil: ", WS-PHONE
           DISPLAY "E-Mail: ", WS-EMAIL
           DISPLAY SPACE
           DISPLAY "***************************************************"        
           
           IF WS-SHOW-ALL > 0
              PERFORM SHORT-PROFILE-para
           END-IF
           
           PERFORM NEXT-STEP-para.
                  
      ******************************************************************
       SHORT-PROFILE-para.
      *----------------
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
           DISPLAY "***************************************************"
           DISPLAY "Kenntnisse und Faehigkeiten:"
           DISPLAY X'0A' "IT-bezogene Sprachen:"
           DISPLAY WS-IT-LANG-1-NAME " (" WS-IT-LANG-1-LEVEL ")"
           DISPLAY WS-IT-LANG-2-NAME " (" WS-IT-LANG-2-LEVEL ")"
           DISPLAY WS-IT-LANG-3-NAME " (" WS-IT-LANG-3-LEVEL ")"
           DISPLAY X'0A' "Weitere IT-Kenntisse:"
           DISPLAY WS-IT-SKILL-1-NAME " (" WS-IT-SKILL-1-LEVEL ")"
           DISPLAY WS-IT-SKILL-2-NAME " (" WS-IT-SKILL-2-LEVEL ")"
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
           DISPLAY "***************************************************"
           DISPLAY "Ehrenamtliche Aktivitaeten:"
           DISPLAY SPACE
           DISPLAY WS-VOLTWK-START IN WS-VOLUNTARY-WORK-1 " - "
      -            WS-VOLTWK-END IN WS-VOLUNTARY-WORK-1 
           DISPLAY WS-VOLTWK-TITLE IN WS-VOLUNTARY-WORK-1 
           DISPLAY WS-VOLTWK-RESPONSIBLITIES IN WS-VOLUNTARY-WORK-1
           
           DISPLAY SPACE
           DISPLAY WS-VOLTWK-START IN WS-VOLUNTARY-WORK-1 " - "
      -            WS-VOLTWK-END IN WS-VOLUNTARY-WORK-1
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
           DISPLAY "***************************************************"
           DISPLAY "Hobbys:" WS-SPORT ", " WS-INSTRUMENT ", " 
      -    WS-VOLUNTEERING ", " WS-OTHER-HOBBIES
           DISPLAY SPACE
           DISPLAY "***************************************************"
           
           PERFORM NEXT-STEP-para.
      
      ******************************************************************
       NEXT-STEP-para.
      *-----------------
           DISPLAY SPACE
           DISPLAY "Druecken Sie die ENTER-Taste, um fortzufahren..."
           ACCEPT WS-USER-INPUT-TEXT
      
           GO TO AUSWAHL.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************


