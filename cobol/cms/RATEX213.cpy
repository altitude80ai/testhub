      ********************************************************
      *                                                      *
      * IPPS PRICER RATE TABLES COPYBOOK                     *
      * RATES EFFECTIVE FOR FY 2021 (10/01/2020)             *
      * MEMBER NAME: RATEX213                                *
      *                                                      *
      ********************************************************
      *   NOTE EHR = BLANK NO REDUCTION                      *
      *   NOTE EHR = Y     REDUCTION                         *
      ********************************************************
      * TABLE 1 (TB1)                                        *
      * NATIONAL (69.6% LABOR SHARE/30.4% NONLABOR SHARE)    *
      * PUERTO RICO (63.2% LABOR SHARE/36.8% NONLABOR SHARE) *
      *    (FULL    UPDATE                                   *
      *    (QUALITY = 1 WAGE INDEX > 1 EHR = BLANK           *
      ********************************************************
      * TABLE 2 (TB2)                                        *
      * NATIONAL (69.6% LABOR SHARE/30.4% NONLABOR SHARE)    *
      * PUERTO RICO  (63.2% LABOR SHARE/36.8% NONLABOR SHARE)*
      *    (REDUCED UPDATE                                   *
      *    (QUALITY NOT = 1 WAGE INDEX > 1 EHR = BLANK       *
      ********************************************************
      * TABLE 3 (TB3)                                        *
      *    (62% LABOR SHARE/38% NONLABOR SHARE)              *
      *    (FULL UPDATE                                      *
      *    (QUALITY = 1 WAGE INDEX <= 1 EHR = BLANK)         *
      ********************************************************
      * TABLE 4 (TB4)                                        *
      *    (62% LABOR SHARE/38% NONLABOR SHARE)              *
      *    (REDUCED UPDATE                                   *
      *    (QUALITY NOT = 1 WAGE INDEX <=1 EHR = BLANK)      *
      ********************************************************
      * TABLE 5 (TB5) RATES WITH E.H.R. LOWER MARKET BASKET  *
      * NATIONAL (69.6% LABOR SHARE/30.4% NONLABOR SHARE)    *
      * PUERTO RICO  (63.2% LABOR SHARE/36.8% NONLABOR SHARE)*
      *    (FULL    UPDATE                                   *
      *    (QUALITY = 1 WAGE INDEX > 1 EHR = Y)              *
      ********************************************************
      * TABLE 6 RATES WITH E.H.R. LOWER MARKET BASKET        *
      * NATIONAL (69.6% LABOR SHARE/30.4% NONLABOR SHARE)    *
      * PUERTO RICO  (63.2% LABOR SHARE/36.8% NONLABOR SHARE)*
      *    (REDUCED UPDATE                                   *
      *    (QUALITY NOT = 1 WAGE INDEX > 1 EHR = Y)          *
      ********************************************************
      * TABLE 7  RATES WITH E.H.R. LOWER MARKET BASKET       *
      *    (62% LABOR SHARE/38% NONLABOR SHARE)              *
      *    (FULL UPDATE                                      *
      *    (QUALITY = 1 WAGE INDEX <= 1 EHR = Y)             *
      ********************************************************
      * TABLE 8 RATES WITH E.H.R. LOWER MARKET BASKET        *
      *    (62% LABOR SHARE/38% NONLABOR SHARE)              *
      *    (REDUCED UPDATE                                   *
      *    (QUALITY NOT = 1 WAGE INDEX <=1 EHR = Y)          *
      ********************************************************
       01  TB1-RATE-TABLE.
           02  TB1-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB1-NAT    PIC X(30) VALUE
                   ' 0407157 188974 0407157 188974'.
               05  TB1-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB1-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB1-RATE-TAB REDEFINES TB1-RATE-WORK.
               05  TB1-RATE-PERIOD         OCCURS 1.
                   10  TB1-RATE-EFF-DATE   PIC X(08).
                   10  TB1-REG-NAT         OCCURS 3.
                       15  TB1-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB1-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB1-REG-NLABOR  PIC 9(04)V9(02).
       01  TB2-RATE-TABLE.
           02  TB2-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB2-NAT    PIC X(30) VALUE
                   ' 0404771 187867 0404771 187867'.
               05  TB2-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB2-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB2-RATE-TAB REDEFINES TB2-RATE-WORK.
               05  TB2-RATE-PERIOD         OCCURS 1.
                   10  TB2-RATE-EFF-DATE   PIC X(08).
                   10  TB2-REG-NAT         OCCURS 3.
                       15  TB2-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB2-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB2-REG-NLABOR  PIC 9(04)V9(02).
       01  TB3-RATE-TABLE.
           02  TB3-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB3-NAT    PIC X(30) VALUE
                   ' 0369601 226530 0369601 226530'.
               05  TB3-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB3-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB3-RATE-TAB REDEFINES TB3-RATE-WORK.
               05  TB3-RATE-PERIOD         OCCURS 1.
                   10  TB3-RATE-EFF-DATE   PIC X(08).
                   10  TB3-REG-NAT         OCCURS 3.
                       15  TB3-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB3-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB3-REG-NLABOR  PIC 9(04)V9(02).
       01  TB4-RATE-TABLE.
           02  TB4-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB4-NAT    PIC X(30) VALUE
                   ' 0367436 225202 0367436 225202'.
               05  TB4-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB4-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB4-RATE-TAB REDEFINES TB4-RATE-WORK.
               05  TB4-RATE-PERIOD         OCCURS 1.
                   10  TB4-RATE-EFF-DATE   PIC X(08).
                   10  TB4-REG-NAT         OCCURS 3.
                       15  TB4-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB4-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB4-REG-NLABOR  PIC 9(04)V9(02).
       01  TB5-RATE-TABLE.
           02  TB5-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB5-NAT    PIC X(30) VALUE
                   ' 0400000 185652 0400000 185652'.
               05  TB5-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB5-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB5-RATE-TAB REDEFINES TB5-RATE-WORK.
               05  TB5-RATE-PERIOD         OCCURS 1.
                   10  TB5-RATE-EFF-DATE   PIC X(08).
                   10  TB5-REG-NAT         OCCURS 3.
                       15  TB5-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB5-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB5-REG-NLABOR  PIC 9(04)V9(02).
       01  TB6-RATE-TABLE.
           02  TB6-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB6-NAT    PIC X(30) VALUE
                   ' 0397614 184545 0397614 184545'.
               05  TB6-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB6-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB6-RATE-TAB REDEFINES TB6-RATE-WORK.
               05  TB6-RATE-PERIOD         OCCURS 1.
                   10  TB6-RATE-EFF-DATE   PIC X(08).
                   10  TB6-REG-NAT         OCCURS 3.
                       15  TB6-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB6-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB6-REG-NLABOR  PIC 9(04)V9(02).
       01  TB7-RATE-TABLE.
           02  TB7-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB7-NAT    PIC X(30) VALUE
                   ' 0363104 222548 0363104 222548'.
               05  TB7-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB7-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB7-RATE-TAB REDEFINES TB7-RATE-WORK.
               05  TB7-RATE-PERIOD         OCCURS 1.
                   10  TB7-RATE-EFF-DATE   PIC X(08).
                   10  TB7-REG-NAT         OCCURS 3.
                       15  TB7-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB7-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB7-REG-NLABOR  PIC 9(04)V9(02).
       01  TB8-RATE-TABLE.
           02  TB8-RATE-WORK.
               05  FILLER PIC X(08) VALUE '20201001'.
               05  TB8-NAT    PIC X(30) VALUE
                   ' 0360939 221220 0360939 221220'.
               05  TB8-PR     PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
               05  TB8-NATPR  PIC X(30) VALUE
                   ' 0000000 000000 0000000 000000'.
           02  TB8-RATE-TAB REDEFINES TB8-RATE-WORK.
               05  TB8-RATE-PERIOD         OCCURS 1.
                   10  TB8-RATE-EFF-DATE   PIC X(08).
                   10  TB8-REG-NAT         OCCURS 3.
                       15  TB8-LARGE-OTHER OCCURS 2.
                           20  FILLER          PIC X(01).
                           20  TB8-REG-LABOR   PIC 9(05)V9(02).
                           20  FILLER          PIC X(01).
                           20  TB8-REG-NLABOR  PIC 9(04)V9(02).
