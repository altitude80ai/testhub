      *****************************************************
      * NEW TECHNOLOGY VARIABLES TO DETERMINE ELIGIBILITY *
      * FOR ADD-ON PAYMENTS DURING FISCAL YEAR (FY) 2021  *
      * REVISED 08/19/2020                                *
      *****************************************************
       01  WK-NEW-TECH-VARIABLES.
          05  WK-PROC-NEW-TECH       PIC X(07).
             88  PROC-ANDEXXA
                   VALUE 'XW03372' 'XW04372'.
             88  PROC-AZEDRA
                   VALUE 'XW033S5' 'XW043S5'.
             88  PROC-BALVERSA
                   VALUE 'XW0DXL5'.
             88  PROC-BAROSTIM1
                   VALUE '0JH60MZ'.
             88  PROC-BAROSTIM2
                   VALUE '03HK3MZ' '03HL3MZ'.
             88  PROC-CABLIVI
                   VALUE 'XW013W5' 'XW033W5' 'XW043W5'.
             88  PROC-CONTACT
                   VALUE '4A03X5D'.
             88  PROC-ELUVIA
                   VALUE 'X27H385' 'X27H395' 'X27H3B5' 'X27H3C5'
                         'X27J385' 'X27J395' 'X27J3B5' 'X27J3C5'
                         'X27K385' 'X27K395' 'X27K3B5' 'X27K3C5'
                         'X27L385' 'X27L395' 'X27L3B5' 'X27L3C5'.
             88  PROC-ELZONRIS
                   VALUE 'XW033Q5' 'XW043Q5'.
             88  PROC-FETROJA
                   VALUE 'XW033A6' 'XW043A6'.
             88  PROC-HEMOSPRAY
                   VALUE 'XW0G886' 'XW0H886'.
             88  PROC-IMFINZI
                   VALUE 'XW03336' 'XW04336'.
             88  PROC-ISLET
                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
                         '3E0J8U1'.
             88  PROC-JAKAFI
                   VALUE 'XW0DXT5'.
             88  PROC-NUZYRA
                   VALUE 'XW033B6' 'XW043B6'.
             88  PROC-OPTIMIZER
                   VALUE '0JH60AZ' '0JH63AZ' '0JH80AZ' '0JH83AZ'.
             88  PROC-PLAZO
                   VALUE 'XW033G4' 'XW043G4'.
             88  PROC-RECARBIO
                   VALUE 'XW033U5' 'XW043U5'.
             88  PROC-SOLIRIS
                   VALUE 'XW033C6' 'XW043C6'.
             88  PROC-SPINEJACK
                   VALUE 'XNU0356' 'XNU4356'.
             88  PROC-SPRAVATO
                   VALUE 'XW097M5'.
             88  PROC-T2
                   VALUE 'XXE5XM5'.
             88  PROC-TECENTRIQ
                   VALUE 'XW033D6' 'XW043D6'.
             88  PROC-XENLETA
                   VALUE 'XW03366' 'XW04366' 'XW0DX66'.
             88  PROC-XOSPATA
                   VALUE 'XW0DXV5'.
             88  PROC-ZERBAXA
                   VALUE 'XW03396' 'XW04396'.

          05  WK-DIAG-NEW-TECH       PIC X(07).
             88  DIAG-ISLET
                   VALUE 'Z006   '.

          05  WK-NEW-TECH-FLAGS.
             10  PROC-ANDEXXA-FLAG       PIC X(01).
             10  PROC-AZEDRA-FLAG        PIC X(01).
             10  PROC-BALVERSA-FLAG      PIC X(01).
             10  PROC-BAROSTIM1-FLAG     PIC X(01).
             10  PROC-BAROSTIM2-FLAG     PIC X(01).
             10  PROC-CABLIVI-FLAG       PIC X(01).
             10  PROC-CONTACT-FLAG       PIC X(01).
             10  PROC-ELUVIA-FLAG        PIC X(01).
             10  PROC-ELZONRIS-FLAG      PIC X(01).
             10  PROC-FETROJA-FLAG       PIC X(01).
             10  PROC-HEMOSPRAY-FLAG     PIC X(01).
             10  PROC-IMFINZI-FLAG       PIC X(01).
             10  PROC-ISLET-FLAG         PIC X(01).
             10  PROC-JAKAFI-FLAG        PIC X(01).
             10  PROC-NUZYRA-FLAG        PIC X(01).
             10  PROC-OPTIMIZER-FLAG     PIC X(01).
             10  PROC-PLAZO-FLAG         PIC X(01).
             10  PROC-RECARBIO-FLAG      PIC X(01).
             10  PROC-SOLIRIS-FLAG       PIC X(01).
             10  PROC-SPINEJACK-FLAG     PIC X(01).
             10  PROC-SPRAVATO-FLAG      PIC X(01).
             10  PROC-T2-FLAG            PIC X(01).
             10  PROC-TECENTRIQ-FLAG     PIC X(01).
             10  PROC-XENLETA-FLAG       PIC X(01).
             10  PROC-XOSPATA-FLAG       PIC X(01).
             10  PROC-ZERBAXA-FLAG       PIC X(01).
             10  DIAG-ISLET-FLAG         PIC X(01).
