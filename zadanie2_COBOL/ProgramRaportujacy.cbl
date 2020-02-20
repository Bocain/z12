IDENTIFICATION DIVISION.
PROGRAM-ID. DKTS.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

        SELECT DANEKLIENTOWFILE ASSIGN TO "DaneKlientow"
                ORGANIZATION IS LINE SEQUENTIAL.

        SELECT TRANSAKCJEKLIENTOWFILE ASSIGN TO "TransakcjeKlientow"
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.

FILE SECTION.

FD DANEKLIENTOWFILE.
01 KLIENCI-RECORD.
        05 KOD-KLIENTA-DK           PIC A(16).
        05 NAZWA-KLIENTA            PIC A(16).
        05 ADRES-KLIENTA            PIC A(16).
        05 NIP                      PIC 9(10).

FD TRANSAKCJEKLIENTOWFILE.
01 TRANSAKCJE-RECORD.
        05 KOD-KRAJU                PIC A(16).
        05 KOD-KLIENTA-TK           PIC 9(16).
        05 WALUTA-TRANSAKCJI        PIC A(17).
        05 DEBET-KREDYT-FLAGA       PIC A(14).
        05 WARTOSC-TRANSAKCJI       PIC 9(4).


WORKING-STORAGE SECTION.

77 SELECT-COUNTRY       PIC A(3).
77 USER-OPTION          PIC 9 VALUE ZERO.

01 TABLICA-TRANSAKCJE.
        02 WIERSZ-TT                    OCCURS 33 TIMES.
                03 KOLUMNA-TT           OCCURS 5  TIMES.
                        04 TRANSAKCJAKRAJ   PIC X(17).

01 TABLICA-KLIENCI.
        02 WIERSZ-TK                    OCCURS 6 TIMES.
                03 KOLUMNA-TK           OCCURS 4 TIMES.
                        04 KLIENT       PIC X(16).

01 TABLICA-UNIQUEKLIENCI.
        02 WIERSZ-UK.                    
                03 KOLUMNA-UK           OCCURS 10 TIMES.
                        04 SUMAKLIENTOW PIC 9(3).

01 TABLICA-UNIQUEWALUTY.
        02 WIERSZ-UW.                    
                03 KOLUMNA-UW           OCCURS 10 TIMES.
                        04 SUMAWALUT    PIC A(17).
01 TABLICA-RAPORT.
        02 WIERSZ-R                     OCCURS 12 TIMES.
                03 KOLUMNA-R            OCCURS 4 TIMES.
                        04 RAPORT       PIC X(13).

01 SWITCHES.
        05 EOF-SWITCH-DKF   PIC A VALUE "N".
        05 EOF-SWITCH-TKF   PIC A VALUE "N".

01 COUNTERS.
        05 REC-COUNTER-DKF  PIC 9(3) VALUE 0.
        05 REC-COUNTER-TKF  PIC 9(3) VALUE 0.
        05 I PIC 9(3) VALUE 1.
        05 J PIC 9(3) VALUE 1.
        05 W PIC 9(3) VALUE 1.
        05 K PIC 9(3) VALUE 1.
        05 ZLICZKLIENTA PIC 9(2) VALUE ZERO.
        05 ZLICZWALUTE  PIC 9(2) VALUE ZERO.
        05 L PIC 9(2) VALUE 1.
        05 P PIC 9(2) VALUE 1.
        05 LICZBATRANSAKCJIKRAJ PIC 9(3) VALUE ZERO.
        05 NOWYWIERSZ PIC 9(2) VALUE 1.
        05 BIEZACYWIERSZ PIC 9(2) VALUE 1.


01 NALICZONE.
        05 ILOSCKLIENTOW PIC 9(2) VALUE 0.
        05 ILOSCWALUT    PIC 9(2) VALUE 0.
        05 KLIENTXWALUTA PIC 9(2) VALUE 12.
        05 SUMAOPERACJI  PIC 9(6) VALUE 0.
        05 SUMARAPORT    PIC 9(6) VALUE 0.
        05 SUMATRANSAKCJA PIC 9(6) VALUE 0.       

 
PROCEDURE DIVISION.

000-MAIN.

        PERFORM 400-INTRO.
        PERFORM 100-WCZYTAJ-DANEKLIENTOW.
        PERFORM 200-PROCESS-DANEKLIENTOW
                UNTIL EOF-SWITCH-DKF = "Y".
        PERFORM 300-TERMINATE-PROCESS-DANEKLIENTOW.
        
        PERFORM 101-WCZYTAJ-TRANSAKCJEKLIENTOW.
        PERFORM 201-PROCESS-UNIQUE-KRAJTRANSAKCJE
                UNTIL EOF-SWITCH-TKF = "Y".
        PERFORM 202-PROCESS-LISTAKLIENCIWALUTY.
        PERFORM 203-PROCESS-RAPORT.
        PERFORM 301-TERMINATE-PROCESS-UNIQUE-KRAJTRANSAKCJE.
        PERFORM 204-CHANGECODENAME-FULLNAME.        

        PERFORM 600-WYSWIETLTABLICE.

        STOP RUN.

100-WCZYTAJ-DANEKLIENTOW.
        
        OPEN INPUT DANEKLIENTOWFILE.
        
        READ DANEKLIENTOWFILE
                AT END
                        MOVE "Y" TO EOF-SWITCH-DKF
                NOT AT END
                        COMPUTE REC-COUNTER-DKF = REC-COUNTER-DKF + 1
        END-READ.

101-WCZYTAJ-TRANSAKCJEKLIENTOW.

        OPEN INPUT TRANSAKCJEKLIENTOWFILE.
        
        READ TRANSAKCJEKLIENTOWFILE
                AT END
                        MOVE "Y" TO EOF-SWITCH-TKF
                NOT AT END
                        COMPUTE REC-COUNTER-TKF = REC-COUNTER-TKF + 1
        END-READ.

200-PROCESS-DANEKLIENTOW.

        
        MOVE KOD-KLIENTA-DK TO KLIENT(I,1)
        MOVE NAZWA-KLIENTA  TO KLIENT(I,2)
        MOVE ADRES-KLIENTA  TO KLIENT(I,3)
        MOVE NIP            TO KLIENT(I,4)
        
        COMPUTE I = I + 1

        READ DANEKLIENTOWFILE
                AT END
                        MOVE "Y" TO EOF-SWITCH-DKF
                        COMPUTE I = 1
                NOT AT END
                        COMPUTE REC-COUNTER-DKF = REC-COUNTER-DKF + 1
        END-READ.

201-PROCESS-UNIQUE-KRAJTRANSAKCJE.
        
        IF KOD-KRAJU EQUALS SELECT-COUNTRY THEN                                
                MOVE KOD-KRAJU          TO TRANSAKCJAKRAJ(J,1)
                MOVE KOD-KLIENTA-TK     TO TRANSAKCJAKRAJ(J,2)
                MOVE WALUTA-TRANSAKCJI  TO TRANSAKCJAKRAJ(J,3)
                MOVE DEBET-KREDYT-FLAGA TO TRANSAKCJAKRAJ(J,4)
                MOVE WARTOSC-TRANSAKCJI TO TRANSAKCJAKRAJ(J,5)
                COMPUTE J = J + 1
        END-IF.


        READ TRANSAKCJEKLIENTOWFILE
                AT END
                        MOVE "Y" TO EOF-SWITCH-TKF
                        MOVE J TO LICZBATRANSAKCJIKRAJ
                NOT AT END
                        COMPUTE REC-COUNTER-TKF = REC-COUNTER-TKF + 1
        END-READ.

202-PROCESS-LISTAKLIENCIWALUTY.
       COMPUTE J = 1.

       PERFORM VARYING J FROM 1 BY 1 UNTIL J > LICZBATRANSAKCJIKRAJ 
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
         IF TRANSAKCJAKRAJ(J,2) IS EQUAL TO SUMAKLIENTOW(I) THEN
          COMPUTE ZLICZKLIENTA = ZLICZKLIENTA + 1   
         END-IF
 
         IF TRANSAKCJAKRAJ(J,3) IS EQUAL TO SUMAWALUT(I) THEN
          COMPUTE ZLICZWALUTE = ZLICZWALUTE + 1   
         END-IF        
        END-PERFORM

         IF ZLICZKLIENTA = 0 THEN
          MOVE TRANSAKCJAKRAJ(J,2) TO SUMAKLIENTOW(L)
          COMPUTE L = L + 1
          COMPUTE ZLICZKLIENTA = 0
         ELSE
          COMPUTE ZLICZKLIENTA = 0
         END-IF

         IF ZLICZWALUTE = 0 THEN
          MOVE TRANSAKCJAKRAJ(J,3) TO SUMAWALUT(P)
          COMPUTE P = P + 1
          COMPUTE ZLICZWALUTE = 0
         ELSE
          COMPUTE ZLICZWALUTE = 0
         END-IF
       END-PERFORM.
        
        COMPUTE ILOSCKLIENTOW = L.
        COMPUTE ILOSCWALUT = P.

203-PROCESS-RAPORT.
        
        COMPUTE J = 1.
        COMPUTE I = 1.
        COMPUTE P = 1.
        COMPUTE L = 1.
        COMPUTE W = 0.
        COMPUTE K = 0.
        COMPUTE NOWYWIERSZ = 1.

       PERFORM VARYING J FROM 1 BY 1 UNTIL J > LICZBATRANSAKCJIKRAJ 
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > ILOSCKLIENTOW
         PERFORM VARYING P FROM 1 BY 1 UNTIL P > ILOSCWALUT
          IF TRANSAKCJAKRAJ(J,2) IS EQUAL TO SUMAKLIENTOW(I) AND
          TRANSAKCJAKRAJ(J,3) IS EQUAL TO SUMAWALUT(P) THEN
            PERFORM VARYING L FROM 1 BY 1 UNTIL L > KLIENTXWALUTA

                 IF TRANSAKCJAKRAJ(J,2) IS EQUAL TO RAPORT(L,1) AND 
                    TRANSAKCJAKRAJ(J,3) IS EQUAL TO RAPORT(L,4) THEN
                        COMPUTE W = 1  
                 END-IF
             END-PERFORM

                 IF W EQUALS 0 THEN
                        MOVE TRANSAKCJAKRAJ(J,2) TO RAPORT(NOWYWIERSZ,1)
                        MOVE TRANSAKCJAKRAJ(J,3) TO RAPORT(NOWYWIERSZ,4)

                          IF TRANSAKCJAKRAJ(J,4) IS EQUAL TO "D" THEN
                           MOVE TRANSAKCJAKRAJ(J,5) TO RAPORT(NOWYWIERSZ,2)
                          ELSE
                           MOVE TRANSAKCJAKRAJ(J,5) TO RAPORT(NOWYWIERSZ,3)
                          END-IF

                        MOVE NOWYWIERSZ TO BIEZACYWIERSZ
                        COMPUTE NOWYWIERSZ = NOWYWIERSZ + 1
                        COMPUTE W = 0
                        COMPUTE K = 0
                END-IF

                IF W IS NOT EQUAL 0 THEN
                   IF TRANSAKCJAKRAJ(J,4) IS EQUAL TO "D" THEN
                      MOVE RAPORT(BIEZACYWIERSZ,2) TO SUMARAPORT
                      MOVE TRANSAKCJAKRAJ(J,5) TO SUMATRANSAKCJA
                      COMPUTE SUMAOPERACJI = SUMARAPORT + SUMATRANSAKCJA
                      MOVE SUMAOPERACJI TO RAPORT(BIEZACYWIERSZ,2)
                      COMPUTE SUMAOPERACJI = 0
                      ELSE
                      MOVE RAPORT(BIEZACYWIERSZ,3) TO SUMARAPORT
                      MOVE TRANSAKCJAKRAJ(J,5) TO SUMATRANSAKCJA
                      COMPUTE SUMAOPERACJI = SUMARAPORT + SUMATRANSAKCJA
                      MOVE SUMAOPERACJI TO RAPORT(BIEZACYWIERSZ,3)
                      COMPUTE SUMAOPERACJI = 0
                   END-IF     
               END-IF

                COMPUTE K = 0
                COMPUTE W = 0
          END-IF

         END-PERFORM
        END-PERFORM
       END-PERFORM.
 
204-CHANGECODENAME-FULLNAME.
        
        COMPUTE K = 1.        
        COMPUTE W = 1.

        PERFORM VARYING K FROM 1 BY 1 UNTIL K > 6
                PERFORM VARYING W FROM 1 BY 1 UNTIL W > 10
                        IF KLIENT(K,1) IS EQUAL TO RAPORT(W,1) THEN
                                MOVE KLIENT(K,2) TO RAPORT(W,1)
                        END-IF
                END-PERFORM
        END-PERFORM.
        

300-TERMINATE-PROCESS-DANEKLIENTOW.

        CLOSE DANEKLIENTOWFILE.

301-TERMINATE-PROCESS-UNIQUE-KRAJTRANSAKCJE.

        CLOSE TRANSAKCJEKLIENTOWFILE.

400-INTRO.

        DISPLAY "*******************************"
        DISPLAY "Witaj w programie raportujacym."
        DISPLAY "Raport dla Polski,  wprowadz 1."
        DISPLAY "Raport dla Niemiec, wprowadz 2."
        DISPLAY "*******************************"
        ACCEPT USER-OPTION.
        
        EVALUATE USER-OPTION
                WHEN 1 MOVE "PL" TO SELECT-COUNTRY
                WHEN 2 MOVE "DE" TO SELECT-COUNTRY.

600-WYSWIETLTABLICE.

        DISPLAY "NazwaKlienta SumaTransakcji SumaTransakcji "
                "Waluta".

        DISPLAY "             Debetowych     Kredytowych    Transakcji".

        PERFORM VARYING W FROM 1 BY 1 UNTIL W > KLIENTXWALUTA
                        DISPLAY RAPORT(W,1)"  "
                                RAPORT(W,2)"  "
                                RAPORT(W,3)"  "
                                RAPORT(W,4)
        END-PERFORM.
        
