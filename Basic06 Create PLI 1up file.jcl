//BASIC06  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A ,TYPRUN=SCAN
//S1  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=C
//SYSUT2   DD DSN=HERCEL.BASIC1UP.PLI,DISP=OLD
//SYSIN DD *
./  ADD NAME=BASENV
1       /*************** START BASENV  V1.0.0 *****************/
0BASIC:PROCEDURE(EXEC_PARM) OPTIONS(MAIN);
     DECLARE EXEC_PARM  CHAR(100) VARYING;
 /********************************************************************
 *                                                                   *
 *   BASENV - THIS IS THE MODULE TO ESTABLISH THE BASIC ENVIRONMENT  *
 *            THIS ONE SETS UP FOR THE BATCH1UP OPTION.              *
 *                                                                   *
 ********************************************************************/

 /********************************************************************
 *                                                                   *
 *   DEFINE MAX SIZES FOR GLOBAL TABLES USED IN THIS PROGRAM         *
 *                                                                   *
 ********************************************************************/

     %DECLARE $DATA_STACK        FIXED;
     %DECLARE $MAX_LINES         FIXED;
     %DECLARE $MAX_SYM           FIXED;
     %DECLARE $MAX_PCODE         FIXED;
     %DECLARE $MAX_EXECS         FIXED;
     %DECLARE $DEFAULT_DSN       CHAR;
     %$DATA_STACK=500;       /* MAX NUMBER OF DATA NUMBERS OR STRING*/
     %$MAX_LINES=500;        /* MAX NUMBER OF LINE IN BASIC PGM     */
     %$MAX_SYM=200;          /* MAX NUMBER DATA ELEMENTS IN SYM TBL */
     %$MAX_PCODE=500;        /* MAX NUMBER OF PCODES                */
     %$MAX_EXECS=5000;       /* MAX PCODES BEFORE ABORTED AS LOOPED */
     %$DEFAULT_DSN='''HERCEL.BASICLIB.BAS''';

 /********************************************************************
 *                                                                   *
 *                      ENVIRONMENT SETUP                            *
 *                                                                   *
 *    TWO OPTIONAL ITEMS CAN BE PASSED FROM THE EXEC STATEMENT.      *
 *     THE FIRST IS AN ALTERNATE DSN FOR THE SAVE LIBRARY OTHER      *
 *     THAN THE DEFAULT.                                             *
 *     THE SECOND IS THE DESIRE LIMIT ON HOW MANY PCODES ARE         *
 *     ALLOWED.  IF ZERO, NO LIMIT IS IMPOSED.                       *
 *     IF YOU WISH TO SET A LIMIT, A COMMA MUST BE CODED.            *
 *                                                                   *
 *     VALID EXAMPLES:                                               *
 *     PARM='HERC01.STUFF,100000' OR PARM=',0'                       *
 *     PARM='HERC01.STUFF'                                           *
 *                                                                   *
 ********************************************************************/

     DECLARE PAGE_TITLE          CHAR(20) STATIC
                                 INITIAL('BASIC/360   V3.2.0 ');
     DECLARE ENV_PTR             POINTER;

     DECLARE DEFAULT_DSN         CHAR(44) INITIAL((44)' ');
     DECLARE DEFAULT_LIB         CHAR(44) INITIAL(
                                 $DEFAULT_DSN);
     DECLARE SAVE_MEMBER_NAME    CHAR(8)  INITIAL((8)' ');
     DECLARE NEW_LIB             CHAR(100) VARYING;
     DECLARE ENFORCE_MAX_EXECS   BIT(1) ALIGNED INITIAL('1'B);
     DECLARE MAX_EXECS           FIXED BINARY(31)
                                 ALIGNED INITIAL($MAX_EXECS);
     DECLARE INPUT_BUFFER        CHAR(80) VARYING
                                 INITIAL('');
     DECLARE KEYINPT             RECORD INPUT FILE ENV(CONSECUTIVE);
     DECLARE KEYINPT_OPEN        BIT(1) ALIGNED INITIAL('0'B);

     DEFAULT_LIB = $DEFAULT_DSN;

     IF LENGTH(EXEC_PARM) > 0 THEN
     DO;
         COMMA=INDEX(EXEC_PARM,',');
         IF COMMA = LENGTH(EXEC_PARM) &
            COMMA = 1 THEN               /* PARM IS ',' */
             GO TO ENV_EXIT;
         IF COMMA > 0 THEN
         DO;
             ON CONVERSION
             BEGIN;
                PUT SKIP EDIT('*** INVALID EXEC PARM ***') (A);
                STOP;
             END;
             IF COMMA < LENGTH(EXEC_PARM) THEN
                 MAX_EXECS=SUBSTR(EXEC_PARM,COMMA+1);
             ON CONVERSION SYSTEM;
         END;
         IF COMMA > 1 THEN
             NEW_LIB=SUBSTR(EXEC_PARM,1,COMMA-1);
         ELSE
             IF COMMA = 0 THEN
                 NEW_LIB=EXEC_PARM;
             ELSE
                 NEW_LIB=DEFAULT_LIB;
         DEFAULT_LIB=NEW_LIB;
         ENFORCE_MAX_EXEC=(MAX_EXECS>0);
         PUT SKIP EDIT('*** EXEC_PARM PASSED=',EXEC_PARM) (A,A)
                      ('MAX_EXECS CHANGED TO ',MAX_EXECS) (SKIP,A,F(10))
                      ('DEFAULT DSN=',DEFAULT_LIB) (SKIP,A,A);
 ENV_EXIT:
     END;
        /*************** END BASENV  V1.0.0 *****************/
./  ADD NAME=BASINP
 /******************** START BASINP.PLI V1.0.0 **********************/
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:                                                          *
 ********************************************************************/

 INPUT_RTN:PROC(VALUE);
     DECLARE VALUE       CHAR(80) VARYING;
     DECLARE CARD        CHAR(80),
             CARD_CHR(80) DEFINED CARD CHAR(1);

     IF INPUT_FILE_OPEN THEN ;
     ELSE
     DO;
        OPEN FILE(KEYINPT);
        KEYINPT_OPEN=TRUE;
     END;
     IF EXECUTION_DEBUG THEN
        PUT SKIP DATA(INPUT_FILE_OPEN);
     ON ENDFILE(KEYINPT)
     BEGIN;
        IF LENGTH(PRINT_LINE) > 0 THEN
            CALL DISPLAY_PRINT_LINE;
        PRINT_LINE='BASINP:END OF FILE DETECTED';
        CALL DISPLAY_PRINT_LINE;
        STOP;
     END;

     IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(INPUT_BUFFER);
     IF LENGTH(INPUT_BUFFER) > 0 THEN /* GET MORE DATA ON FROM CARD*/
        GO TO GET_NEXT_VALUE;
 GET_INPUT:
     READ FILE(KEYINPT) INTO(CARD);
     IF EXECUTION_DEBUG THEN
         PUT SKIP EDIT('BASINP:INPUT CARD=',CARD) (A,A);
     DO I=STMT_LEFT TO STMT_RIGHT
        WHILE (CARD_CHR(I)=' ');
     END;
     IF I > STMT_RIGHT THEN               /* IGNORE BLANK CARD */
        GOTO GET_INPUT;
     INPUT_BUFFER=SUBSTR(CARD, I);   /* TRIM OF LEADING SPACES */
 GET_NEXT_VALUE:
     DO J=1 TO LENGTH(INPUT_BUFFER)  /* IGNORE LEADING SPACES */
        WHILE (SUBSTR(INPUT_BUFFER,J,1)=' ');
     END;
     IF J > LENGTH(INPUT_BUFFER) THEN /* IGNORE IF ALL BLANKS */
        GOTO GET_INPUT;
     IF J > 1 THEN                  /* IGNORE LEADING SPACES */
        INPUT_BUFFER=SUBSTR(INPUT_BUFFER,J);

 /*********************************************************************
 *                                                                    *
 *   IF THE VALUE ENCOUNTERED IS A STRING, FIND THE END OF THE        *
 *   STRING.  IF THERE IS NO END OF STRING ON THE CARD, ASSUME ONE    *
 *   IS PRESENT, END THE STRING AND INDICATE THE BUFFER IS EMPTY.     *
 *                                                                    *
 *   IF AN END OF STRING IS FOUND, REMOVE THE STRING FROM THE BUFFER  *
 *   AND THEN SEARCH FOR THE NEXT NON-BLANK.  IF IT IS A COMMA, GET   *
 *   RID OF IT.  THIS AVOIDS ENDING A VALUE WITH 2 DELIMITERS.        *
 *                                                                    *
 *********************************************************************/
     IF SUBSTR(INPUT_BUFFER,1,1)='"' THEN
     DO;
        DO I=2 TO LENGTH(INPUT_BUFFER)
           WHILE(SUBSTR(INPUT_BUFFER,I,1)^='"');
        END;
        IF I > LENGTH(INPUT_BUFFER) THEN
        DO;
            VALUE=SUBSTR(INPUT_BUFFER,2);
            INPUT_BUFFER='';
        END;
        ELSE
        DO;
            VALUE=SUBSTR(INPUT_BUFFER,2,I-2);
            INPUT_BUFFER=SUBSTR(INPUT_BUFFER,I+1);
            DO I=1 TO LENGTH(INPUT_BUFFER)
               WHILE(SUBSTR(INPUT_BUFFER,I,1)=' ');
            END;
            IF I <= LENGTH(INPUT_BUFFER) THEN
                INPUT_BUFFER=SUBSTR(INPUT_BUFFER,I);
            IF SUBSTR(INPUT_BUFFER,1,1)=',' THEN
               INPUT_BUFFER=SUBSTR(INPUT_BUFFER,2);
        END;
        GO TO INPUT_RTN_EXIT;
     END;
 /*********************************************************************
 *                                                                    *
 *   IF THE VALUE ENCOUNTERED IS NOT A STRING, FIND THE NEXT COMMA.   *
 *   IF THERE IS NO COMMA ON THE CARD, ASSUME ONE IS PRESENT.         *
 *   IS PRESENT, END THE STRING AND INDICATE THE BUFFER IS EMPTY.     *
 *                                                                    *
 *   IF TWO COMMAS ARE FOUND WITH NOTHING OR JUST SPACES BETWEEN      *
 *   THEM, A NULL VALUE WILL BE RETURNED.                             *
 *                                                                    *
 *********************************************************************/
     DO I=1 TO LENGTH(INPUT_BUFFER)     /* LOOK FOR DELIMITER  */
         WHILE(SUBSTR(INPUT_BUFFER,I,1)^=',' &
               SUBSTR(INPUT_BUFFER,I,1)^=' ');
     END;
     IF I > LENGTH(INPUT_BUFFER) THEN /* IF NO DELIMITER, TAKE   */
     DO;                                      /* ALL AS VALUE  */
         VALUE=INPUT_BUFFER;
         INPUT_BUFFER='';
     END;
     ELSE
     DO;                           /* ELSE TAKE SELECTED CHARS */
         VALUE=SUBSTR(INPUT_BUFFER,1,I-1);
         INPUT_BUFFER=SUBSTR(INPUT_BUFFER,I+1);
     END;
 INPUT_RTN_EXIT:
    IF EXECUTION_DEBUG THEN
       PUT SKIP EDIT('BASINP:INPUT VALUE=''',VALUE,'''') (A,A,A);
 END INPUT_RTN;
 /******************** END BASINP.PLI V1.0.0 **********************/
./  ADD NAME=BASPRT
 /******************** START BASPRT.PLI V1.0.0 **********************/
 DISPLAY_PRINT_LINE:PROC;

 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:                                                          *
 ********************************************************************/

    PUT SKIP EDIT(PRINT_LINE) (A);
    PRINT_LINE='';

 END DISPLAY_PRINT_LINE;

 DISPLAY_PAGE_EJECT:PROC;

 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:                                                          *
 ********************************************************************/

    PUT PAGE;

 END DISPLAY_PAGE_EJECT;
 /******************** END BASPRT.PLI V1.0.0 **********************/
./  ADD NAME=BASRDR
1/****************  START BASRDR  V1.0.0  *****************************
 *                   FOR STANDALONE BATCH                             *
 *                                                                    *
 *  THIS IS THE MAIN DRIVING LOOP FOR BASIC.                          *
 *                                                                    *
 *********************************************************************/

     OPEN FILE(SYSIN) INPUT;

     ON ENDFILE(SYSIN)
        EOF_SYSIN,EOP_SYSIN=TRUE;

     READ FILE(SYSIN) INTO(STMT_BUFF);
                                  /*  THIS PRIMES THE INPUT PROCESS */

     DO WHILE(EOF_SYSIN=FALSE);
        CALL INITIALIZE;
        CALL LOAD_SOURCE;
        IF SC_MAX>0 THEN
        DO;
           BASIC_RENUM_DONE=FALSE;
           CALL COMPILE;
           IF BASIC_RENUM & (ERROR_COUNT=0) THEN
           DO;
              CALL RENUM;
              IF ERROR_COUNT=0 THEN
              DO;
                 CALL INITIALIZE;
                 BASIC_RENUM_DONE=TRUE;
                 CALL COMPILE;
              END;
           END;
           IF ERROR_COUNT=0 & SAVE_CODE THEN
              CALL SAVE_CODE_ROUTINE;
           IF ERROR_COUNT=0 & EXECUTE_CODE THEN
           DO;
              CALL EXECUTE;
              CALL TERMINATE;
           END;
        END;
     END;
     CLOSE FILE(SYSIN);
     RETURN;
1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 LOAD_SOURCE:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC READS THE INPUT FILE AND LOADS THE BASIC PROGRAMS     *
 *   INTO THE SOURCE_CODE STRUCTURE.    OPTION CARDS START WITH      *
 *   AN * IS COL 1.  THEY ARE LOADED INTO THE SOURCE CODE TABLE SO   *
 *   COMPILE CAN PROCESS THEM.  OPTIONS DO NOT GENERATE ANY PCODES.  *
 *                                                                   *
 * NESTING:LOAD_SOURCE                                               *
 ********************************************************************/

     ON ENDFILE(SYSIN)
        EOF_SYSIN,EOP_SYSIN=TRUE;

     EOP_SYSIN=FALSE;
     TABLE_PRINT=FALSE;
     TABLE_DUMP=FALSE;
     STACK_PRINT_DEBUG=FALSE;
     ICODE_PRINT=FALSE;
     SUBTR_PRINT=FALSE;
     EXECUTION_DEBUG=FALSE;
     BASIC_RENUM=FALSE;
     PGM_PAGE_NUM=0;
     SC_CUR,SC_MAX=0;

     DO WHILE((EOF_SYSIN=FALSE)&(EOP_SYSIN=FALSE));
        SC_MAX=SC_MAX+1;
        IF SC_MAX>HBOUND(SOURCE_LINE,1) THEN
        DO;
           PUT SKIP LIST
             ('**** FATAL ERROR - PROGRAM TO BIG ***');
           STOP;
        END;
        SOURCE_LINE(SC_MAX)=STMT_BUFF;
        SOURCE_APPENDED(SC_MAX)=FALSE;
        READ FILE(SYSIN) INTO(STMT_BUFF);
     END;

 END LOAD_SOURCE;
 /****************  END BASRDR  V1.0.0  *****************************/
./  ENDUP
