      ***************************************************************
      *  FUNCTION: RETRIEVES INFORMATION ABOUT SEVERAL ORCHESTRAS   *
      *            WITH MUTUAL FUNDS ALONG WITH THE MUTUAL FUNDS    *
      *            THAT THEY USE AND TRANSACTION REQUESTS FOR THEM. *
      *            IF THE TRANSACTION REQUESTS ARE VALID, THE       *
      *            PROGRAM PROCESSES THEM AND OUTPUTS THE RESULTS,  *
      *            PRINTING A LIST OF TOTALS AT THE END.            *
      *                                                             *
      *            ALSO OUTPUTS A LIST OF ORCHESTRAS WITH LOW       *
      *            ENDOWMENT AMOUNTS.                               *
      *                                                             *
      *  INPUT:    ORCHFILE --                                      *
      *            SEQUENTIAL FILE CONTAINING DATA FOR EACH         *
      *            INDIVIDUAL ORCHESTRA.                            *
      *            CONTAINS: NAME, BALANCE,                         *
      *            CAPITAL GAINS PERCENT NUMBER, USUFRUCTUARY LIMIT,*
      *            NUMBER OF THE FUND THAT IT USES.                 *
      *                                                             *
      *            FUNDFILE --                                      *
      *            SEQUENTIAL FILE CONTAINING DATA ON EACH OF THE   *
      *            AVAILIBLE MUTUAL FUNDS, ORDERED ASCENDINGLY BY   *
      *            FUND NUMBER.                                     *
      *            CONTAINS: FUND NUMBER, FUND NAME,                *
      *            FUND SHARE PRICE, CAPITAL GAINS PERCENTS.        *
      *                                                             *
      *            TRANFILE --                                      *
      *            SEQUENTIAL FILE CONTAINING TRANSACTION REQUESTS  *
      *            FOR THE SEVERAL ORCHESTRAS.                      *
      *            CONTAINS: ORCHESTRA NAME, TRANSACTION TYPE FLAG, *
      *            ENDOWMENT CHANGE AMOUNT.                         *
      *                                                             *
      *  OUTPUT:   RPTFILE --                                       *
      *            OUTPUT TABLE CONTAINING INFORMATION ABOUT EACH   *
      *            TRANSACTION ALONG WITH TOTALS. ALSO CONTAINS A   *
      *            LIST OF ORCHESTRAS WITH ENDOWMENTS LESS THAN $1M.*
      *            TRANSACTION INFORMATION INCLUDES: ORCHESTRA NAME,*
      *            FUND NAME AND NUMBER,                            *
      *            INITIAL SHARE AND ENDOWMENT AMOUNT,              *
      *            TRANSACTION TYPE, SHARE / ENDOWMENT CHANGE AMT,  *
      *            NEW SHARE / ENDOWMENT AMOUNT.                    *
      *            TRANSACTION TOTALS INCLUDE:                      *
      *            # TRANSACTIONS PROCESSED,                        *
      *            INITIAL SHARES/ENDOWMENTS,                       *
      *            SHARE/ENDOWMENT CHANGES, NEW SHARES/ENDOWMENTS   *
      *            LOW ENDOWMENT REPORT INCLUDES:                   *
      *            ORCHESTRA NAME, FUND #, FUND NAME, SHARE AMOUNT, *
      *            ENDOWMENT AMOUNT                                 *
      *                                                             *
      *  IN/OUTPUT: LEFILE --                                       *
      *            FILE THAT ACTS AS TEMPORARY STORAGE FOR          *
      *            ORCHESTRAS WITH LOW ENDOWMENTS.                  *
      *                                                             *
      *  ENTRY CONDITIONS: NONE                                     *
      *                                                             *
      *  EXIT CONDITIONS: NONE                                      *
      *                                                             *
      *  NOTES:    NONE                                             *
      ***************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.   ENDOWMT5.
       AUTHOR.       MITCHELL TRAFTON.
       DATE-WRITTEN. 11/30/2020.
       DATE-COMPILED.


       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT TRANS-FILE ASSIGN TO TRANFILE.
           SELECT LOWEND-FILE ASSIGN TO LEFILE.
           SELECT REPORT-FILE ASSIGN TO RPTFILE.


       DATA DIVISION.

       FILE SECTION.

       FD  TRANS-FILE
           RECORDING MODE IS F.

           COPY TRANREC.
           05                          PIC X(23).

       FD  LOWEND-FILE
           RECORDING MODE IS F.

           COPY LEREC.

       FD  REPORT-FILE
           RECORDING MODE IS F.

       01  REPORT-LINE                 PIC X(132).

       WORKING-STORAGE SECTION.

      *
      * TABLES FROM COPY LIB
      *
           COPY ORCHTBL.

       01  FUNDS-TBL.
           05 TBL-FUND-CTR             PIC S9(3) BINARY SYNC VALUE 0.
           05 TBL-FUND-NTRY            OCCURS 1 TO 999
                                       DEPENDING ON TBL-FUND-CTR
                                       ASCENDING KEY TBL-FUND-NBR
                                       INDEXED BY FUND-NDX.
              COPY FUNDSTBL.


      *
      * COUNTERS/FLAGS/MISC.
      *

       01  FLAGS.
           05 EOF-FLAG                 PIC X VALUE 'N'.
           05 NOT-FOUND-FLAG           PIC X VALUE 'N'.

       01  CURRENT-DATE-FIELDS.
           05  CURRENT-DATE.
               10  CURRENT-YEAR        PIC 9(4).
               10  CURRENT-MONTH       PIC 9(2).
               10  CURRENT-DAY         PIC 9(2).
           05  CURRENT-TIME.
               10  CURRENT-HOUR        PIC 9(2).
               10  CURRENT-MINUTE      PIC 9(2).
               10  CURRENT-SECOND      PIC 9(2).
               10  CURRENT-MS          PIC 9(2).
           05  DIFF-FROM-GMT           PIC S9(4).

       01  COUNTERS.
           05 TRAN-CTR                 PIC 999 PACKED-DECIMAL VALUE 0.
           05 LINE-CTR                 PIC 99 BINARY SYNC VALUE 10.
           05 PAGE-CTR                 PIC 99 PACKED-DECIMAL VALUE 0.

       01  SUBPROGRAM                  PIC 9(8).

      *
      *    TRANSACTION VARS
      *

       01  ENDOWMENT-TOALS.
           05 TOT-ENDOWMT-AMT          PIC 9(9)V99 PACKED-DECIMAL
                                            VALUE 0.
           05 TOT-ENDOWMT-CHG-AMT      PIC S9(9)V99 PACKED-DECIMAL
                                            VALUE 0.
           05 TOT-NEW-ENDOWMT-AMT      PIC 9(9)V99 PACKED-DECIMAL
                                            VALUE 0.

       01  SHARE-TOTALS.
           05 TOT-SHARE-AMT            PIC 9(13)V9(4) PACKED-DECIMAL
                                            VALUE 0.
           05 TOT-SHARE-CHG-AMT        PIC S9(13)V9(4) PACKED-DECIMAL
                                            VALUE 0.
           05 TOT-NEW-SHARE-AMT        PIC 9(13)V9(4) PACKED-DECIMAL
                                            VALUE 0.

       01  ENDOWMENT-DATA-INDIV.
           05  ENDOWMT-CHG-AMT         PIC S9(9)V99 PACKED-DECIMAL.
           05  NEW-ENDOWMT-AMT         PIC 9(9)V99 PACKED-DECIMAL.

       01  SHARE-DATA-INDIV.
           05 SHARE-AMT                PIC 9(11)V9(4) PACKED-DECIMAL
                                            VALUE 0.
           05 SHARE-CHG-AMT            PIC S9(11)V9(4) PACKED-DECIMAL
                                            VALUE 0.
           05 NEW-SHARE-AMT            PIC 9(11)V9(4) PACKED-DECIMAL
                                            VALUE 0.

       01  FUND-SHR-PRC                PIC S9(3)V99 PACKED-DECIMAL.

       01  PAGE-HEADER-1.
           05 CURRENT-MONTH            PIC 99.
           05                          PIC X VALUE '/'.
           05 CURRENT-DAY              PIC 99.
           05                          PIC X VALUE '/'.
           05 CURRENT-YEAR             PIC 9999.
           05                          PIC X(38) VALUE SPACES.
           05                          PIC X(36) VALUE
                                'ORCHESTRAL INVESTMENTS COMPANY, INC.'.
           05                          PIC X(40).
           05                          PIC X(6) VALUE 'PAGE: '.
           05 OUT-PAGE-CTR             PIC Z9.

       01  PAGE-HEADER-2.
           05 CURRENT-HOUR             PIC 99.
           05                          PIC X VALUE ':'.
           05 CURRENT-MINUTE           PIC 99.
           05                          PIC X(45) VALUE SPACES.
           05 HEADER-TITLE             PIC X(31) VALUE
                                     'MONTHLY INVESTMENT TRANSACTIONS'.
           05                          PIC X(52) VALUE SPACES.

       01  ENDOWMT-COL-HDR-1.
           05                          PIC X(54) VALUE SPACES.
           05                          PIC X(29) VALUE
                                       'ENDOWMENT SHARE   TRANSACTION'.
           05                          PIC X(49) VALUE
                   '       TRANSACTION CHANGE     NEW ENDOWMENT SHARE'.

       01  ENDOWMT-COL-HDR-2.
           05                          PIC X(15) VALUE 'ORCHESTRA NAME'.
           05                          PIC X(38) VALUE SPACES.
           05                          PIC X(30) VALUE
                                      'AMOUNT AND VALUE          TYPE'.
           05                          PIC X(49) VALUE
                   '   SHARE AMOUNT AND VALUE        AMOUNT AND VALUE'.

       01  ENDOWMT-COL-HDR-3.
           05                          PIC X(47) VALUE
                     '-----------------------------------------------'.
           05                          PIC X(36) VALUE
                                '   -------------------  ------------'.
           05                          PIC X(49) VALUE
                   '   ----------------------     -------------------'.

       01  ENDOWMT-COL-HDR-TTL.
           05                          PIC X(47) VALUE SPACES.
           05                          PIC X(37) VALUE
                               '----------------------               '.
           05                          PIC X(48) VALUE
                    '------------------------  ----------------------'.

       01  OUTPUT-ENDOWOMT-LINE-1.
           05 OUT-ORCH-NME             PIC X(45).
           05                          PIC X(5) VALUE SPACES.
           05 OUT-SHARE-AMT            PIC ZZ,ZZZ,ZZZ,ZZ9.9999.
           05                          PIC XX VALUE SPACES.
           05 OUT-TRAN-TYPE            PIC X(12).
           05                          PIC X(4) VALUE SPACES.
           05 NEG-PAREN-OPEN-1         PIC X VALUE SPACES.
           05 OUT-SHARE-CHG-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.9999.
           05 NEG-PAREN-CLOSE-1        PIC X VALUE SPACES.
           05                          PIC X(5) VALUE SPACES.
           05 OUT-NEW-SHARE-AMT        PIC ZZ,ZZZ,ZZZ,ZZ9.9999.

       01  OUTPUT-ENDOWMT-LINE-1-ERR.
           05 OUT-ORCH-NME-ERR         PIC X(45).
           05                          PIC X(5) VALUE SPACES.
           05 OUT-SHARE-AMT-ERR        PIC ZZ,ZZZ,ZZZ,ZZ9.9999.
           05                          PIC XX VALUE SPACES.
           05 OUT-TRAN-TYPE-ERR        PIC X(26).
           05                          PIC X(11) VALUE '   (0.0000)'.
           05                          PIC X(8) VALUE SPACES.
           05 OUT-NEW-SHARE-AMT-ERR    PIC ZZ,ZZZ,ZZZ,ZZ9.9999.

       01  OUTPUT-ENDOWMT-LINE-2.
           05                          PIC X(19) VALUE
                                                 'FUND NUMBER/NAME: '.
           05 OUT-FUND-NUMBER          PIC ZZ9.
           05                          PIC X VALUE '/'.
           05 OUT-FUND-NAME            PIC X(25).
           05                          PIC X(7) VALUE SPACES.
           05 OUT-ENDOWMT-AMT          PIC $$$,$$$,$$9.99.
           05                          PIC X(23) VALUE SPACES.
           05 NEG-PAREN-OPEN-2         PIC X VALUE SPACES.
           05 OUT-ENDOWMT-CHG-AMT      PIC $$$,$$$,$$9.99.
           05 NEG-PAREN-CLOSE-2        PIC X VALUE SPACES.
           05                          PIC X(10) VALUE SPACES.
           05 OUT-NEW-ENDOWMT-AMT      PIC $$$,$$$,$$9.99.

       01  OUTPUT-ENDOWMT-NF-LINE.
           05 OUT-NF-ORCHESTRA-NME     PIC X(45).
           05                          PIC X(7) VALUE SPACES.
           05                          PIC X(40) VALUE
                            'UNKNOWN ORCHESTRA  DEPOSIT NOT PROCESSED'.
           05                          PIC X(40) VALUE SPACES.

       01  OUTPUT-FUND-NF-LINE-1.
           05 OUT-FUND-NF-ORCH-NME     PIC X(45).
           05                          PIC X(26) VALUE SPACES.
           05                          PIC X(13) VALUE 'NOT PROCESSED'.
           05                          PIC X(48) VALUE SPACES.

       01  OUTPUT-FUND-NF-LINE-2.
           05                          PIC X(19) VALUE
                                                 'FUND NUMBER/NAME: '.
           05 OUT-FUND-NF-NBR          PIC ZZ9.
           05                          PIC X VALUE '/'.
           05                          PIC X(25) VALUE
                                          '** FUND NBR NOT FOUND ***'.
           05                          PIC X(74) VALUE SPACES.

       01  ENDOWMT-TTLS-HEADER.
           05                          PIC X(47) VALUE SPACES.
           05                          PIC X(37) VALUE
                               '----------------------               '.
           05                          PIC X(48) VALUE
                    '------------------------  ----------------------'.

       01  OUTPUT-ENDOWMT-TTLS-LN-1.
           05                          PIC X(14) VALUE 'TRANSACTIONS: '.
           05 OUT-TRAN-CTR             PIC ZZ9.
           05                          PIC X(15) VALUE SPACES.
           05                          PIC X(15) VALUE
                                                     'SHARE TOTALS:'.
           05 OUT-TOT-SHARE-AMT        PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05                          PIC X(16) VALUE SPACES.
           05 OUT-TOT-SHARE-CHG-AMT    PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05                          PIC XXX VALUE SPACES.
           05 OUT-TOT-NEW-SHARE-AMT    PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.9999.

       01  OUTPUT-ENDOWMT-TTLS-LN-2.
           05                          PIC X(31) VALUE SPACES.
           05                          PIC X(21) VALUE
                                                'DOLLAR TOTALS:       '.
           05 OUT-TOT-ENDOWMT-AMT      PIC $$,$$$,$$$,$$9.99.
           05                          PIC X(22) VALUE SPACES.
           05 OUT-TOT-ENDOWMT-CHG-AMT  PIC $$,$$$,$$$,$$9.99.
           05                          PIC X(8) VALUE SPACES.
           05 OUT-TOT-NEW-ENDOWMT-AMT  PIC $$,$$$,$$$,$$9.99.

      *
      *    LOW ENDOWMENT VARS
      *

       01  OUTPUT-LE-COL-HDR-1.
           05                          PIC X(14) VALUE 'ORCHESTRA NAME'.
           05                          PIC X(35) VALUE SPACES.
           05                          PIC X(24) VALUE
                                            'FUND NUMBER    FUND NAME'.
           05                          PIC X(20) VALUE SPACES.
           05                          PIC X(39) VALUE
                             'CURRENT SHARE AMOUNT      CURRENT VALUE'.

       01  OUTPUT-LE-COL-HDR-2.
           05                          PIC X(45) VALUE
                       '---------------------------------------------'.
           05                          PIC X(19) VALUE
                                                 '    -----------    '.
           05                          PIC X(29) VALUE
                                       '-------------------------    '.
           05                          PIC X(39) VALUE
                             '--------------------    ---------------'.

       01  OUTPUT-LE-LINE.
           05 OUT-LE-ORCH-NME          PIC X(45).
           05                          PIC X(12) VALUE SPACES.
           05 OUT-LE-FUND-NUM          PIC 999.
           05                          PIC X(4) VALUE SPACES.
           05 OUT-LE-FUND-NME          PIC X(25).
           05                          PIC X(5) VALUE SPACES.
           05 OUT-LE-SHARE-AMT         PIC ZZ,ZZZ,ZZZ,ZZ9.9999.
           05                          PIC X(5) VALUE SPACES.
           05 OUT-LE-ENDOWMT-AMT       PIC $$$,$$$,$$9.99.


       PROCEDURE DIVISION.

       0000-MAIN.

      ****************************************************************
      *    CONTROLS THE FLOW OF THE PROGRAM BY PROCESSING RELEVANT   *
      *    FILES AND CALLING SUBROUTINES TO HANDLE THEIR DATA        *
      ****************************************************************

           PERFORM 0050-GET-DATE.

           OPEN INPUT TRANS-FILE
                OUTPUT LOWEND-FILE
                OUTPUT REPORT-FILE.

           PERFORM 0100-RETRIEVE-ORCHESTRAS.

           MOVE 'N' TO EOF-FLAG.

           PERFORM 0150-RETRIEVE-FUNDS.

           MOVE 'N' TO EOF-FLAG.

           READ TRANS-FILE
              AT END MOVE 'Y' TO EOF-FLAG.

           PERFORM 0200-PROCESS-TRANSACTION
              UNTIL EOF-FLAG = 'Y'.

           PERFORM 0900-PRINT-TOTALS.

           CLOSE LOWEND-FILE.

           OPEN INPUT LOWEND-FILE.

           MOVE 'N' TO EOF-FLAG.

           READ LOWEND-FILE
              AT END MOVE 'Y' TO EOF-FLAG.

           MOVE 16 TO LINE-CTR.

           MOVE 0 TO PAGE-CTR.

           MOVE '     LOW INVESTMENT REPORT' TO HEADER-TITLE.

           PERFORM 1000-PRINT-LE-RECORDS
              UNTIL EOF-FLAG = 'Y'.

           CLOSE TRANS-FILE
                 LOWEND-FILE
                 REPORT-FILE.

           GOBACK.

       0000-EXIT. EXIT.

       0050-GET-DATE.

      **************************************************************
      *    RETRIEVES CURRENT DATE USING CURRENT-DATE FUNCTION      *
      *    AND MOVES THAT DATA TO THE HEADDERS THAT NEED IT        *
      **************************************************************

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-FIELDS.

           MOVE CORR CURRENT-DATE TO PAGE-HEADER-1.

           MOVE CORR CURRENT-TIME TO PAGE-HEADER-2.

       0050-EXIT. EXIT.

       0100-RETRIEVE-ORCHESTRAS.

      **************************************************************
      *    RETRIEVES DATA FROM THE ORCHESTRA-FILE AND SAVES IT     *
      *    INTO THE ORCHS-TBL.                                     *
      **************************************************************

           MOVE 'BLDORCH' TO SUBPROGRAM.

           CALL SUBPROGRAM USING ORCHS-TBL.

       0100-EXIT. EXIT.

       0150-RETRIEVE-FUNDS.

      **************************************************************
      *    RETRIEVES DATA FROM THE FUND-FILE AND SAVES IT          *
      *    INTO THE FUNDS-TBL.                                     *
      **************************************************************

           CALL 'BLDFUND' USING FUNDS-TBL.

       0150-EXIT. EXIT.

       0200-PROCESS-TRANSACTION.

      **************************************************************
      *    READS A TRANSACTION REQUEST FROM THE TRANS FILE, AND    *
      *    IF THE REQUEST'S ORCHESTRA NAME MATCHES ONE IN THE      *
      *    ORCHESTRA TABLE, MOVES TO PROCESS THE TRANSACTION.      *
      *    IF NOT, AN ERROR LINE IS WRITTEN TO THE REPORT.         *
      *                                                            *
      *    ALSO PRINTS A PAGE HEADER EVERY 10 LINES.               *
      **************************************************************

           IF LINE-CTR = 10
              PERFORM 0300-PRINT-PAGE-HEADER
              MOVE 0 TO LINE-CTR
           END-IF.

           ADD 1 TO LINE-CTR.


           SET ORCH-NDX TO 1.

           SEARCH TBL-ORCH-NTRY
              AT END
                 PERFORM 0450-PROCESS-ORCHESTRA-NOT-FOUND
              WHEN TBL-ORCH-NME (ORCH-NDX) = IN-ORCH-NME OF TRAN-RECORD
                 PERFORM 0400-PROCESS-ORCHESTRA-FOUND
           END-SEARCH.

           READ TRANS-FILE
              AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       0200-EXIT. EXIT.

       0300-PRINT-PAGE-HEADER.

      **************************************************************
      *    PRINTS A PAGE HEADER FOR THE REGULAR TRANSACTION REPORT.*
      **************************************************************

           ADD 1 TO PAGE-CTR.

           MOVE PAGE-CTR TO OUT-PAGE-CTR.

           MOVE PAGE-HEADER-1 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER PAGE.

           MOVE PAGE-HEADER-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

           MOVE ENDOWMT-COL-HDR-1 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.

           MOVE ENDOWMT-COL-HDR-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

           MOVE ENDOWMT-COL-HDR-3 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

       0300-EXIT. EXIT.

       0400-PROCESS-ORCHESTRA-FOUND.

      **************************************************************
      *    CHECKS IF THE FUND NUMBER OF THE TRANSACTION BELONGS    *
      *    TO ONE IN THE FUND LIST, MOVING TO PROCESS THE          *
      *    TRANSACTION IF SO.                                      *
      *    IF NOT, PRINTS AN ERROR LINE TO THE REPORT.             *
      **************************************************************

           SEARCH ALL TBL-FUND-NTRY
              AT END PERFORM 0520-FUND-NOT-FOUND
              WHEN TBL-FUND-NBR(FUND-NDX) = TBL-INV-FUND-NBR(ORCH-NDX)
                 PERFORM 0510-FUND-FOUND
           END-SEARCH.

       0400-EXIT. EXIT.

       0450-PROCESS-ORCHESTRA-NOT-FOUND.

      **************************************************************
      *    PRINTS AN APPROPRIATE ERROR LINE FOR AN ORCHESTRA       *
      *    THAT IS NOT ON RECORD TO THE REPORT.                    *
      **************************************************************

           MOVE IN-ORCH-NME OF TRAN-RECORD TO OUT-NF-ORCHESTRA-NME.

           MOVE OUTPUT-ENDOWMT-NF-LINE TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.

       0450-EXIT. EXIT.

       0510-FUND-FOUND.

      **************************************************************
      *    DOES THE FOLLOWING:                                     *
      *    - INCREMENTS TRANSACTION COUNTER.                       *
      *    - CALCULATES THE INITIAL SHARE AMOUNT FOR THE CURRENT   *
      *      TRANSACTION AND INCREMENTS ITS CORRESPONDING TOTAL    *
      *      FIELD.                                                *
      *    - CALLS THE APPROPRIATE ROUTINE TO PROCESS THE REQUESTED*
      *      TRANSACTION.                                          *
      *    - CALL THE APPROPRIATE ROUTINE IF THE NEW ENDOWMENT     *
      *      AMOUNT IS LOW.                                        *
      **************************************************************

           MOVE 'CALCSHRS' TO SUBPROGRAM.

           ADD 1 TO TRAN-CTR.

           IF NOT IN-TRAN-TYPE = 'C'
              MOVE IN-ENDOWMT-CHG-AMT TO ENDOWMT-CHG-AMT
           END-IF.

           CALL SUBPROGRAM USING TBL-ENDOWMT-AMT(ORCH-NDX)
                                 TBL-FUND-SHR-PRC(FUND-NDX)
                                 SHARE-AMT.

           COMPUTE TOT-SHARE-AMT ROUNDED =
              SHARE-AMT + TOT-SHARE-AMT.


           IF IN-TRAN-TYPE = 'D'
              PERFORM 0600-DEPOSIT-TRANSACTION
           END-IF.

           IF IN-TRAN-TYPE = 'W'
              PERFORM 0610-WITHDRAWL-TRANSACTION
           END-IF.

           IF IN-TRAN-TYPE = 'C'
              PERFORM 0620-CGI-TRANSACTION
           END-IF.

           IF IN-TRAN-TYPE = 'E'
              PERFORM 0630-EXG-OUT-TRANSACTION
           END-IF.

           IF IN-TRAN-TYPE = 'T'
              PERFORM 0640-TRANS-OUT-TRANSACTION
           END-IF.


           IF NEW-ENDOWMT-AMT < 1000000
              PERFORM 0800-PROCESS-LE-RECORD
           END-IF.

       0510-EXIT. EXIT.

       0520-FUND-NOT-FOUND.

      **************************************************************
      *    PRINTS AN APPROPRIATE ERROR LINE FOR A FUND             *
      *    THAT IS NOT ON RECORD TO THE REPORT.                    *
      **************************************************************

           MOVE IN-ORCH-NAME TO OUT-FUND-NF-ORCH-NME.

           MOVE TBL-INV-FUND-NBR(ORCH-NDX) TO OUT-FUND-NF-NBR.

           MOVE OUTPUT-FUND-NF-LINE-1 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.

           MOVE OUTPUT-FUND-NF-LINE-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

       0520-EXIT. EXIT.

       0600-DEPOSIT-TRANSACTION.

      **************************************************************
      *    PROCESSES A DEPOSIT TRANSACTION WHILE INCREMENTING      *
      *    THE RELEVANT TOTALS FIELDS, CALLING THE APPROPRIATE     *
      *    PRINT METHOD WHEN FINISHED.                             *
      **************************************************************

           CALL SUBPROGRAM USING ENDOWMT-CHG-AMT
                                 TBL-FUND-SHR-PRC(FUND-NDX)
                                 SHARE-CHG-AMT.

           COMPUTE NEW-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) + ENDOWMT-CHG-AMT.

           CALL SUBPROGRAM USING NEW-ENDOWMT-AMT
                                 TBL-FUND-SHR-PRC(FUND-NDX)
                                 NEW-SHARE-AMT.

           COMPUTE TOT-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-ENDOWMT-AMT.

           COMPUTE TOT-ENDOWMT-CHG-AMT ROUNDED =
              ENDOWMT-CHG-AMT + TOT-ENDOWMT-CHG-AMT.

           COMPUTE TOT-SHARE-CHG-AMT ROUNDED =
              TOT-SHARE-CHG-AMT + SHARE-CHG-AMT.

           COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
              NEW-ENDOWMT-AMT + TOT-NEW-ENDOWMT-AMT.

           COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
              NEW-SHARE-AMT + TOT-NEW-SHARE-AMT.

           MOVE 'DEPOSIT' TO OUT-TRAN-TYPE.

           PERFORM 0700-PRINT-ENDOWMENT-LINE.

       0600-EXIT. EXIT.

       0610-WITHDRAWL-TRANSACTION.

      **************************************************************
      *    PROCESSES A WITHDRAWL TRANSACTION REQUEST.              *
      *    IF WITHDRAWL DOES NOT GO BELOW THE USUFRUCTUARY LIMIT   *
      *    FOR THE ORCHESTRA, PROCESS AS NORMAL AND CALL THE       *
      *    REGULAR PRINT METHOD.                                   *
      *    IF NOT, DOESN'T PERFORM TRANSACTION AND CALLS THE ERROR *
      *    PRINT METHOD.                                           *
      *    THE RELEVANT TOTALS FIELDS WILL BE INCREMENTED          *
      *    APPROPRIATELY.                                          *
      **************************************************************

           COMPUTE NEW-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) - ENDOWMT-CHG-AMT.

           COMPUTE TOT-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-ENDOWMT-AMT.

           IF NEW-ENDOWMT-AMT < TBL-ENDOWMT-USUF-LMT(ORCH-NDX)
               MOVE 'WITHDRAWAL NOT PROCESSED' TO OUT-TRAN-TYPE-ERR
               PERFORM 0750-PRINT-ERROR-ENDOWMENT-LINE
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-NEW-ENDOWMT-AMT
               MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO NEW-ENDOWMT-AMT
               MOVE SHARE-AMT TO NEW-SHARE-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
           ELSE
               SUBTRACT ENDOWMT-CHG-AMT FROM TOT-ENDOWMT-CHG-AMT
               CALL SUBPROGRAM USING NEW-ENDOWMT-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     NEW-SHARE-AMT
               CALL SUBPROGRAM USING ENDOWMT-CHG-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     SHARE-CHG-AMT
               COMPUTE TOT-SHARE-CHG-AMT ROUNDED =
                    TOT-SHARE-CHG-AMT - SHARE-CHG-AMT
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                    NEW-ENDOWMT-AMT + TOT-NEW-ENDOWMT-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
               MOVE 'WITHDRAWAL' TO OUT-TRAN-TYPE
               MOVE '(' TO NEG-PAREN-OPEN-1
               MOVE ')' TO NEG-PAREN-CLOSE-1
               MOVE '(' TO NEG-PAREN-OPEN-2
               MOVE ')' TO NEG-PAREN-CLOSE-2
               PERFORM 0700-PRINT-ENDOWMENT-LINE
               MOVE ' ' TO NEG-PAREN-OPEN-1
               MOVE ' ' TO NEG-PAREN-CLOSE-1
               MOVE ' ' TO NEG-PAREN-OPEN-2
               MOVE ' ' TO NEG-PAREN-CLOSE-2
           END-IF.

       0610-EXIT. EXIT.

       0620-CGI-TRANSACTION.

      **************************************************************
      *    PROCESSES A CAPITAL GAINS INCREASE TRANSACTION,         *
      *    USING THE APPROPRIATE PERCENTAGE RATE WHILE INCREMENTING*
      *    THE RELEVANT TOTALS FIELDS, CALLING THE APPROPRIATE     *
      *    PRINT METHOD WHEN FINISHED.                             *
      **************************************************************

           IF TBL-CAP-GAINS-PCT-FLG(ORCH-NDX) = 1
               COMPUTE ENDOWMT-CHG-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) *
                   TBL-FUND-CAP-GAINS-PCT(FUND-NDX, 1)
           END-IF.

           IF TBL-CAP-GAINS-PCT-FLG(ORCH-NDX) = 2
               COMPUTE ENDOWMT-CHG-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) *
                   TBL-FUND-CAP-GAINS-PCT(FUND-NDX, 2)
           END-IF.

           IF TBL-CAP-GAINS-PCT-FLG(ORCH-NDX) = 3
               COMPUTE ENDOWMT-CHG-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) *
                   TBL-FUND-CAP-GAINS-PCT(FUND-NDX, 3)
           END-IF.

           IF TBL-CAP-GAINS-PCT-FLG(ORCH-NDX) = 4
               COMPUTE ENDOWMT-CHG-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) *
                   TBL-FUND-CAP-GAINS-PCT(FUND-NDX, 4)
           END-IF.

           CALL SUBPROGRAM USING ENDOWMT-CHG-AMT
                                 TBL-FUND-SHR-PRC(FUND-NDX)
                                 SHARE-CHG-AMT.

           COMPUTE NEW-ENDOWMT-AMT ROUNDED =
               TBL-ENDOWMT-AMT(ORCH-NDX) + ENDOWMT-CHG-AMT.

           CALL SUBPROGRAM USING NEW-ENDOWMT-AMT
                                 TBL-FUND-SHR-PRC(FUND-NDX)
                                 NEW-SHARE-AMT.

           COMPUTE TOT-ENDOWMT-AMT ROUNDED =
               TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-ENDOWMT-AMT.

           COMPUTE TOT-ENDOWMT-CHG-AMT ROUNDED =
               ENDOWMT-CHG-AMT + TOT-ENDOWMT-CHG-AMT.

           COMPUTE TOT-SHARE-CHG-AMT ROUNDED =
               SHARE-CHG-AMT + TOT-SHARE-CHG-AMT.

           COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
               NEW-ENDOWMT-AMT + TOT-NEW-ENDOWMT-AMT.

           COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
               NEW-SHARE-AMT + TOT-NEW-SHARE-AMT.

           MOVE 'REINVESTMENT' TO OUT-TRAN-TYPE.

           PERFORM 0700-PRINT-ENDOWMENT-LINE.

       0620-EXIT. EXIT.

       0630-EXG-OUT-TRANSACTION.

      **************************************************************
      *    PROCESSES AN EXCHANGE OUT TRANSACTION REQUEST.          *
      *    IF THE CHANGE AMOUNT DOES NOT GO BELOW THE USUFRUCTUARY *
      *    LIMIT FOR THE ORCHESTRA, PROCESS AS NORMAL AND CALL THE *
      *    REGULAR PRINT METHOD.                                   *
      *    IF NOT, DOESN'T PERFORM TRANSACTION AND CALLS THE ERROR *
      *    PRINT METHOD.                                           *
      *    THE RELEVANT TOTALS FIELDS WILL BE INCREMENTED          *
      *    APPROPRIATELY.                                          *
      **************************************************************

           COMPUTE NEW-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) - ENDOWMT-CHG-AMT.

           COMPUTE TOT-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-ENDOWMT-AMT.

           IF NEW-ENDOWMT-AMT < TBL-ENDOWMT-USUF-LMT(ORCH-NDX)
               MOVE 'EXCHANGE OUT NOT PROCESSED' TO OUT-TRAN-TYPE-ERR
               PERFORM 0750-PRINT-ERROR-ENDOWMENT-LINE
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-NEW-ENDOWMT-AMT
               MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO NEW-ENDOWMT-AMT
               MOVE SHARE-AMT TO NEW-SHARE-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
           ELSE
               SUBTRACT ENDOWMT-CHG-AMT FROM TOT-ENDOWMT-CHG-AMT
               CALL SUBPROGRAM USING NEW-ENDOWMT-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     NEW-SHARE-AMT
               CALL SUBPROGRAM USING ENDOWMT-CHG-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     SHARE-CHG-AMT
               COMPUTE TOT-SHARE-CHG-AMT ROUNDED =
                    TOT-SHARE-CHG-AMT - SHARE-CHG-AMT
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                    NEW-ENDOWMT-AMT + TOT-NEW-ENDOWMT-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
               MOVE 'EXCHANGE OUT' TO OUT-TRAN-TYPE
               MOVE '(' TO NEG-PAREN-OPEN-1
               MOVE ')' TO NEG-PAREN-CLOSE-1
               MOVE '(' TO NEG-PAREN-OPEN-2
               MOVE ')' TO NEG-PAREN-CLOSE-2
               PERFORM 0700-PRINT-ENDOWMENT-LINE
               MOVE ' ' TO NEG-PAREN-OPEN-1
               MOVE ' ' TO NEG-PAREN-CLOSE-1
               MOVE ' ' TO NEG-PAREN-OPEN-2
               MOVE ' ' TO NEG-PAREN-CLOSE-2
           END-IF.

       0630-EXIT. EXIT.

       0640-TRANS-OUT-TRANSACTION.

      **************************************************************
      *    PROCESSES A TRANSFER OUT TRANSACTION REQUEST.           *
      *    IF THE CHANGE AMOUNT DOES NOT GO BELOW THE USUFRUCTUARY *
      *    LIMIT FOR THE ORCHESTRA, PROCESS AS NORMAL AND CALL THE *
      *    REGULAR PRINT METHOD.                                   *
      *    IF NOT, DOESN'T PERFORM TRANSACTION AND CALLS THE ERROR *
      *    PRINT METHOD.                                           *
      *    THE RELEVANT TOTALS FIELDS WILL BE INCREMENTED          *
      *    APPROPRIATELY.                                          *
      **************************************************************

           COMPUTE NEW-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) - ENDOWMT-CHG-AMT.

           COMPUTE TOT-ENDOWMT-AMT ROUNDED =
              TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-ENDOWMT-AMT.

           IF NEW-ENDOWMT-AMT < TBL-ENDOWMT-USUF-LMT(ORCH-NDX)
               MOVE 'TRANSFER OUT NOT PROCESSED' TO OUT-TRAN-TYPE-ERR
               PERFORM 0750-PRINT-ERROR-ENDOWMENT-LINE
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                   TBL-ENDOWMT-AMT(ORCH-NDX) + TOT-NEW-ENDOWMT-AMT
               MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO NEW-ENDOWMT-AMT
               MOVE SHARE-AMT TO NEW-SHARE-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
           ELSE
               SUBTRACT ENDOWMT-CHG-AMT FROM TOT-ENDOWMT-CHG-AMT
               COMPUTE NEW-SHARE-AMT ROUNDED =
                    NEW-ENDOWMT-AMT / TBL-FUND-SHR-PRC(FUND-NDX)
               CALL SUBPROGRAM USING NEW-ENDOWMT-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     NEW-SHARE-AMT
               CALL SUBPROGRAM USING ENDOWMT-CHG-AMT
                                     TBL-FUND-SHR-PRC(FUND-NDX)
                                     SHARE-CHG-AMT.
               COMPUTE TOT-SHARE-CHG-AMT ROUNDED =
                    TOT-SHARE-CHG-AMT - SHARE-CHG-AMT
               COMPUTE TOT-NEW-ENDOWMT-AMT ROUNDED =
                    NEW-ENDOWMT-AMT + TOT-NEW-ENDOWMT-AMT
               COMPUTE TOT-NEW-SHARE-AMT ROUNDED =
                    NEW-SHARE-AMT + TOT-NEW-SHARE-AMT
               MOVE 'TRANSFER OUT' TO OUT-TRAN-TYPE
               MOVE '(' TO NEG-PAREN-OPEN-1
               MOVE ')' TO NEG-PAREN-CLOSE-1
               MOVE '(' TO NEG-PAREN-OPEN-2
               MOVE ')' TO NEG-PAREN-CLOSE-2
               PERFORM 0700-PRINT-ENDOWMENT-LINE
               MOVE ' ' TO NEG-PAREN-OPEN-1
               MOVE ' ' TO NEG-PAREN-CLOSE-1
               MOVE ' ' TO NEG-PAREN-OPEN-2
               MOVE ' ' TO NEG-PAREN-CLOSE-2
           END-IF.

       0640-EXIT. EXIT.

       0700-PRINT-ENDOWMENT-LINE.

      **************************************************************
      *    PRINTS THE REPORT LINES FOR A SUCCESSFUL TRANSACTION.   *
      **************************************************************

           MOVE TBL-ORCH-NME(ORCH-NDX) TO OUT-ORCH-NME.

           MOVE SHARE-AMT TO OUT-SHARE-AMT.

           MOVE SHARE-CHG-AMT TO OUT-SHARE-CHG-AMT.

           MOVE NEW-SHARE-AMT TO OUT-NEW-SHARE-AMT.

           MOVE TBL-INV-FUND-NBR(ORCH-NDX) TO OUT-FUND-NUMBER.

           MOVE TBL-FUND-NME(FUND-NDX) TO OUT-FUND-NAME.

           MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO OUT-ENDOWMT-AMT.

           MOVE ENDOWMT-CHG-AMT TO OUT-ENDOWMT-CHG-AMT.

           MOVE NEW-ENDOWMT-AMT TO OUT-NEW-ENDOWMT-AMT.

           MOVE OUTPUT-ENDOWOMT-LINE-1 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.

           MOVE OUTPUT-ENDOWMT-LINE-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

       0700-EXIT. EXIT.

       0750-PRINT-ERROR-ENDOWMENT-LINE.

      **************************************************************
      *    PRINTS THE REPORT LINES FOR AN UNCUCCESSFUL TRANSACTION.*
      **************************************************************

           MOVE TBL-ORCH-NME(ORCH-NDX) TO OUT-ORCH-NME-ERR.

           MOVE SHARE-AMT TO OUT-SHARE-AMT-ERR.

           MOVE SHARE-AMT TO OUT-NEW-SHARE-AMT-ERR.

           MOVE TBL-INV-FUND-NBR(ORCH-NDX) TO OUT-FUND-NUMBER.

           MOVE TBL-FUND-NME(FUND-NDX) TO OUT-FUND-NAME.

           MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO OUT-ENDOWMT-AMT.

           MOVE 0 TO OUT-ENDOWMT-CHG-AMT.

           MOVE TBL-ENDOWMT-AMT(ORCH-NDX) TO OUT-NEW-ENDOWMT-AMT.

           MOVE OUTPUT-ENDOWMT-LINE-1-ERR TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.

           MOVE OUTPUT-ENDOWMT-LINE-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

       0750-EXIT. EXIT.

       0800-PROCESS-LE-RECORD.

      **************************************************************
      *    MOVES THE CURRENT ORCHESTRA BEING PROCESSED INTO THE    *
      *    LOW ENDOWMENT FILE.                                     *
      **************************************************************

           MOVE TBL-ORCH-NME(ORCH-NDX) TO LE-ORCH-NME.

           MOVE TBL-INV-FUND-NBR(ORCH-NDX) TO LE-FUND-NBR.

           MOVE NEW-ENDOWMT-AMT TO LE-ENDOWMT-AMT.

           WRITE LOW-ENDOWMT-RECORD.

       0800-EXIT. EXIT.

       0900-PRINT-TOTALS.

      **************************************************************
      *    PRINTS THE TOTALS LINES FOR THE REPORT.                 *
      **************************************************************

           MOVE TRAN-CTR TO OUT-TRAN-CTR.

           MOVE TOT-SHARE-AMT TO OUT-TOT-SHARE-AMT.

           MOVE TOT-SHARE-CHG-AMT TO OUT-TOT-SHARE-CHG-AMT.

           MOVE TOT-NEW-SHARE-AMT TO OUT-TOT-NEW-SHARE-AMT.

           MOVE TOT-ENDOWMT-AMT TO OUT-TOT-ENDOWMT-AMT.

           MOVE TOT-ENDOWMT-CHG-AMT TO OUT-TOT-ENDOWMT-CHG-AMT.

           MOVE TOT-NEW-ENDOWMT-AMT TO OUT-TOT-NEW-ENDOWMT-AMT.

           MOVE ENDOWMT-TTLS-HEADER TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

           MOVE OUTPUT-ENDOWMT-TTLS-LN-1 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

           MOVE OUTPUT-ENDOWMT-TTLS-LN-2 TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 1.

       0900-EXIT. EXIT.

       1000-PRINT-LE-RECORDS.

      **************************************************************
      *    DOES THE FOLLOWING:                                     *
      *    - PRINTS A PAGE HEADER EVERY 16 LINES.                  *
      *    - CALCULATES THE SHARE AMOUNT FOR THE CURRENT LOW-      *
      *      ENDOWMENT RECORD BEING PROCESSED.                     *
      *    - PRINTS A LOW ENDOWMENT RECORD.                        *
      **************************************************************

           MOVE 'CALCSHRS' TO SUBPROGRAM.

           IF LINE-CTR = 16
              ADD 1 TO PAGE-CTR
              MOVE PAGE-CTR TO OUT-PAGE-CTR
              MOVE PAGE-HEADER-1 TO REPORT-LINE
              WRITE REPORT-LINE AFTER PAGE
              MOVE PAGE-HEADER-2 TO REPORT-LINE
              WRITE REPORT-LINE AFTER 1
              MOVE OUTPUT-LE-COL-HDR-1 TO REPORT-LINE
              WRITE REPORT-LINE AFTER 2
              MOVE OUTPUT-LE-COL-HDR-2 TO REPORT-LINE
              WRITE REPORT-LINE AFTER 1
              MOVE 0 TO LINE-CTR
           END-IF.

           ADD 1 TO LINE-CTR.


           SEARCH ALL TBL-FUND-NTRY
              WHEN TBL-FUND-NBR(FUND-NDX) = LE-FUND-NBR
                 PERFORM 1050-GET-LE-FUND-INFO
           END-SEARCH.

           CALL SUBPROGRAM USING LE-ENDOWMT-AMT
                                 FUND-SHR-PRC
                                 SHARE-AMT.


           MOVE LE-ORCH-NME TO OUT-LE-ORCH-NME.

           MOVE LE-FUND-NBR TO OUT-LE-FUND-NUM.

           MOVE SHARE-AMT TO OUT-LE-SHARE-AMT.

           MOVE LE-ENDOWMT-AMT TO OUT-LE-ENDOWMT-AMT.

           MOVE OUTPUT-LE-LINE TO REPORT-LINE.

           WRITE REPORT-LINE AFTER 2.


           READ LOWEND-FILE
              AT END MOVE 'Y' TO EOF-FLAG.

       1000-EXIT. EXIT.

       1050-GET-LE-FUND-INFO.

      **************************************************************
      *    RETRIEVES THE FUND INFORMATION TO THE CURRENT           *
      *    LOW ENDOWMENT RECORD BEING PROCESSED.                   *
      **************************************************************

           MOVE TBL-FUND-NME(FUND-NDX) TO OUT-LE-FUND-NME.

           MOVE TBL-FUND-SHR-PRC(FUND-NDX) TO FUND-SHR-PRC.

       1050-EXIT. EXIT.