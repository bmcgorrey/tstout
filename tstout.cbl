      ******************************************************
      *  Copyright 1991-2017, Digital Systems Group, Inc.  *
      *                                                    *
      *  Digital Systems Group, Inc.                       *
      *  400 Horsham Rd                                    *
      *  Suite 120                                         *
      *  Horsham, PA 19044                                 *
      *                                                    *
      *                                                    *
      *              Restricted Rights Legend              *
      *                                                    *
      *  Use, duplication or disclosure is subject to      *
      *  restrictions stated in Contract N62269-91-D-0331  *
      *  with Digital Systems Group, Inc.                  *
      *                                                    *
      ******************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTOUT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MIPS.
       OBJECT-COMPUTER. MIPS.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT XOUT-FILE ASSIGN TO "DBPATH/TSTOUT.rpt".
       
       DATA DIVISION.
       
       FILE SECTION.
       FD  XOUT-FILE.
       01 XOUT-0001-Z-00003.
          02 NAME-0002-Z-00004 PIC X(1).
          02 NULL-TERM-Z-00005 PIC X(1).
       
       WORKING-STORAGE SECTION.
       01 COUNT-Z-00008 PIC S9(8) VALUE 1.
       01 FILLER PIC X(50) VALUE 
           "(C) DIGITAL SYSTEMS GROUP, INC. 1991-2017 IPL ".
       01 FILLER PIC X(30) VALUE "Mon Apr 19 13:43:02 2021".
       01 FILLER PIC X(10) VALUE "v3.0.0g ".
       01 FILLER PIC X(30) VALUE "Wed Sep 26 16:29:21 EDT 2018".
       01 DATE-Z-00014 PIC X(8) VALUE SPACES.
       01 TIME-Z-00015 PIC X(6) VALUE ZERO.
       01 CURRENT-DATE-Z-00016 PIC X(8) VALUE SPACES.
       01 CURRENT-TIME-Z-00017 PIC X(6) VALUE ZERO.
       01 DATE-STAT-Z-00018 PIC S9(10) USAGE COMP VALUE ZERO.
       01 COMMAND-Z-00019.
          02 COMMAND-KEY-Z-00020 PIC X(20) VALUE SPACES.
          02 PANEL-VAR-Z-00021 PIC X(7) VALUE SPACES.
          02 TABLE-NAME-Z-00022 PIC X(7) VALUE SPACES.
          02 TABLE-INDEX-Z-00023 PIC X(4) VALUE SPACES.
          02 COMMAND-PARAMS-Z-00024 PIC X(5132) VALUE SPACES.
       01 PAGE-MODE-Z-00025 PIC S9(9) USAGE COMP-5 VALUE 2.
       01 PAGE-INFO-Z-00026.
          02 PAGE-NUM-Z-00027 PIC S9(9) USAGE COMP-5 VALUE ZERO.
          02 LINE-NUM-Z-00028 PIC S9(9) USAGE COMP-5 VALUE ZERO.
          02 PAGECHK-Z-00029 PIC S9(9) USAGE COMP-5 VALUE ZERO.
       01 PARAMS-Z-00030.
          02 PARAM1-Z-00031 PIC S9(9) USAGE COMP-5 VALUE ZERO.
          02 PARAM2-Z-00032 PIC S9(9) USAGE COMP-5 VALUE ZERO.
          02 PARAM3-Z-00033 PIC S9(10) USAGE COMP-3 VALUE ZERO.
          02 PARAM4-Z-00034 PIC X(30) VALUE SPACES.
       01 REC-NUM-Z-00035 PIC S9(8) VALUE ZERO.
       01 THIS-ANY-Z-00036 PIC S9(10) USAGE COMP VALUE ZERO.
       01 DATABASE-Z-00037 PIC X(10) VALUE SPACES.
       01 I-Z-00038 PIC S9(10) USAGE COMP VALUE ZERO.
       01 J-Z-00039 PIC S9(10) USAGE COMP VALUE ZERO.
       01 K-Z-00040 PIC S9(10) USAGE COMP VALUE ZERO.
       01 SUB-SYSTEM-Z-00041 PIC X(2) VALUE SPACES.
       01 TP-INPROGRESS-Z-00042 PIC S9(10) USAGE COMP VALUE ZERO.
       01 CCI-STATUS-Z-00043 PIC X(2) USAGE COMP-X VALUE ZERO.
       01 PBLK-Z-00044 PIC X(36) VALUE ZERO.
       01 PBLK-Z-00045 REDEFINES PBLK-Z-00044 OCCURS 9 PIC S9(9)
            USAGE COMP-5.
       01 VBLK-Z-00046 PIC X(36) VALUE ZERO.
       01 VBLK-Z-00047 REDEFINES VBLK-Z-00046 OCCURS 9 USAGE 
           BINARY-LONG.
       01 PROGRAM-ID-Z-00048 PIC X(10) VALUE "TSTOUT".
       01 XOUT-Z-00049.
          02 FILLER PIC X(4) VALUE "XOUT".
          02 STAT-Z-00051.
             03 STAT1-Z-00052 PIC X(1) VALUE SPACES.
             03 STAT2-Z-00053 PIC X(1) VALUE SPACES.
          02 FILLER PIC X(2) VALUE SPACES.
          02 FILLER PIC X(1) VALUE "F".
          02 FILLER PIC X(1) VALUE "F".
          02 FILLER PIC X(10) VALUE "TSTOUT.rpt".
          02 FILLER PIC X(10) VALUE SPACES.
          02 FILLER PIC X(10) VALUE SPACES.
          02 FILLER PIC X(10) VALUE SPACES.
          02 FILLER PIC X(4) VALUE SPACES.
          02 FMODE-Z-00062 PIC X(1) VALUE "2".
             88 READ-MODE-Z-00063 VALUE "1" THRU "2".
             88 INPUT-MODE-Z-00064 VALUE "1".
             88 IO-MODE-Z-00065 VALUE "2".
             88 WRITE-MODE-Z-00066 VALUE "3" THRU "4".
             88 OUTPUT-MODE-Z-00067 VALUE "3".
             88 EXTEND-MODE-Z-00068 VALUE "4".
          02 LOCK-MODE-Z-00069 PIC X(1) VALUE "0".
             88 LOCK-PROCESSING-Z-00070 VALUE "1".
          02 TRANSACTION-FILE-Z-00071 PIC X(1) VALUE "0".
          02 TRANSACTION-MODE-Z-00072 PIC X(1) VALUE "0".
             88 TRANSACTION-PROCESSING-Z-00073 VALUE "1".
       01 COUNT-Z-00077 PIC S9(8) VALUE 1.
       
       PROCEDURE DIVISION.
       
       PARA-0001-0002 SECTION .
       
        CALL "REMARK" USING  BY CONTENT 
           "Start:  TSTOUT(tstout - Mon Apr 19 13:43:02 2021)."
            & X"00"
        MOVE 8 TO PBLK-Z-00045 (2)
        MOVE 1 TO PBLK-Z-00045 (1)
        CALL "DATEMM" USING PBLK-Z-00044, DATE-Z-00014
        ACCEPT TIME-Z-00015 FROM  TIME 
        ACCEPT COMMAND-Z-00019 FROM  COMMAND-LINE 
        CONTINUE 
        PERFORM PARA-0002-0002
        ROLLBACK 
        DISPLAY "TSTOUT"
        PERFORM PARA-0002-0003
        CALL "REMARK" USING  BY CONTENT "Finish: TSTOUT(tstout)."
            & X"00"
        STOP RUN.
       
       EXITPARA.
       EXIT.
       
       PARA-0002-0001 SECTION .
       
        GO TO EXITPARA.
       
       EXITPARA.
       EXIT.
       
       PARA-0002-0002 SECTION .
       
        CALL "INITX" USING  BY REFERENCE XOUT-FILE, BY REFERENCE 
           XOUT-Z-00049
        IF (STAT-Z-00051 = "30")
           PERFORM PARA-0002-0003
           CALL "REMARK" USING  BY CONTENT "Finish: TSTOUT(tstout)."
               & X"00"
           MOVE 35 TO PBLK-Z-00045 (2)
           MOVE 2 TO PBLK-Z-00045 (3)
           MOVE 2 TO PBLK-Z-00045 (1)
           CALL "ABORT" USING PBLK-Z-00044, 
              " ABORT - Accessing XOUT. (tstout 2)", STAT-Z-00051
        END-IF .
       
       EXITPARA.
       EXIT.
       
       PARA-0002-0003 SECTION .
       
        CALL "ENDWIN"
       
        ROLLBACK 
       
        CALL "CLOSR" USING  BY REFERENCE XOUT-FILE, BY REFERENCE 
           XOUT-Z-00049.
       
       EXITPARA.
       EXIT.
       
       PARA-0001-0001 SECTION .
       
        GO TO EXITPARA.
       
       EXITPARA.
       EXIT.
       
