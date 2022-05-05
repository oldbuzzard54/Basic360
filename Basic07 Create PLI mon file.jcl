//BASIC07  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A, ,TYPRUN=SCAN
//   USER=HERC01,PASSWORD=CUL8TR
//S1  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=HERC01.BASICMON.PLI,DISP=OLD
//SYSIN DD *
./  ADD NAME=BASENV
1       /************* START BASENV  V1.0.1 MON *****************/
0BASIC:PROCEDURE(EXEC_PARM) OPTIONS(MAIN);
     DECLARE EXEC_PARM  CHAR(100) VARYING;
 /********************************************************************
 *                                                                   *
 *   BASENV - THIS IS THE MODULE TO ESTABLISH THE BASIC ENVIRONMENT  *
 *            THIS ONE SETS UP FOR THE BASICMON OPTION.              *
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
 *     PARM='HERC01.STUFF'                                           *
 *                                                                   *
 ********************************************************************/

     DECLARE PAGE_TITLE          CHAR(20) STATIC
                                 INITIAL('BASIC/360 V3.3.0 MON');
     DECLARE ENV_PTR             POINTER;

     DECLARE DEFAULT_DSN         CHAR(44) INITIAL((44)' ');
     DECLARE DEFAULT_LIB         CHAR(44) INITIAL(
                                 $DEFAULT_DSN);
     DECLARE SAVE_MEMBER_NAME    CHAR(8)  INITIAL((8)' ');
     DECLARE NEW_LIB             CHAR(100) VARYING;
     DECLARE ENFORCE_MAX_EXECS   BIT(1) ALIGNED INITIAL('1'B);
     DECLARE MAX_EXECS           FIXED BINARY(31)
                                 ALIGNED INITIAL($MAX_EXECS);

     DEFAULT_LIB = $DEFAULT_DSN;

     IF LENGTH(EXEC_PARM) > 0 THEN
     DO;
         NEW_LIB=EXEC_PARM;
         DEFAULT_LIB=NEW_LIB;
         PUT SKIP EDIT('*** EXEC_PARM=',EXEC_PARM) (A,A)
                      ('DEFAULT DSN=',NEW_LIB) (SKIP,A,A);
     END;
        /*************** END BASENV  V1.0.0 *****************/
./  ADD NAME=BASINP
 /******************** START BASINP.PLI V1.0.0 **********************/
 /********************************************************************
 *                                                                   *
 *                                                                   *
 ********************************************************************/

 INPUT_RTN:PROC(VALUE);
     DECLARE VALUE       CHAR(80) VARYING;

     IF LENGTH(PRINT_LINE) > 0 THEN
         CALL DISPLAY_PRINT_LINE;
     PRINT_LINE='BASINP:INPUT STATMENT NOT SUPPORTED';
     CALL DISPLAY_PRINT_LINE;
     ABNORMAL_STOP=TRUE;

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
 *                   FOR BASICMON BATCH                               *
 *                                                                    *
 *  THIS IS THE MAIN DRIVING LOOP FOR BASIC.  IF THERE IS A ++        *
 *  LINE AS FIRST LINE IN SYSIN, IT IS ASSUMED BASIC IS RUNNING IN    *
 *  MONITOR MODE.  IF NO ++ LINE IS FOUND, IT IS 1 UP MODE.           *
 *                                                                    *
 *********************************************************************/
     READ FILE(SYSIN) INTO(STMT_BUFF);
                                  /*  THIS PRIMES THE INPUT PROCESS */

     DO WHILE(EOF_SYSIN=FALSE);
        CALL INITIALIZE;
        CALL MONITOR;
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
     RETURN;   /* TO MVS */

1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 MONITOR:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC READS THE INPUT FILE AND LOADS THE BASIC PROGRAMS     *
 *   INTO THE SOURCE_CODE STRUCTURE FOR MONITOR MODE.  MONITOR       *
 *   CONTROL STATEMENTS START WITH ++.  OPTION CARDS START WITH      *
 *   AN * IS COL 1.  THEY ARE LOADED INTO THE SOURCE CODE TABLE SO   *
 *   COMPILE CAN PROCESS THEM.  OPTIONS DO NOT GENERATE ANY PCODES.  *
 *                                                                   *
 * NESTING:MONITOR                                                   *
 ********************************************************************/

     ON ENDFILE(SYSIN)
        EOF_SYSIN,EOP_SYSIN=TRUE;
     MONITOR_STMT=(80)' ';

     IF SUBSTR(STMT_BUFF,1,2)='++' THEN
     DO;
        MONITOR_STMT=STMT_BUFF;
        EOP_SYSIN=FALSE;
        TABLE_PRINT=FALSE;
        TABLE_DUMP=FALSE;
        STACK_PRINT_DEBUG=FALSE;
        ICODE_PRINT=FALSE;
        SUBTR_PRINT=FALSE;
        EXECUTION_DEBUG=FALSE;
        IF (SUBSTR(STMT_BUFF,1,8)='++RENUM ') THEN
        DO;
           BASIC_RENUM=TRUE;
           RENUM_START,RENUM_STEP=10;
        END;
        ELSE
           BASIC_RENUM=FALSE;
        PGM_PAGE_NUM=0;
        SC_CUR,SC_MAX=0;
        READ FILE(SYSIN) INTO(STMT_BUFF);
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
           IF SUBSTR(STMT_BUFF,1,2)='++' THEN
           DO;
             EOP_SYSIN=TRUE;
           END;
        END;
     END;
     ELSE  /*  NO MONITOR CNTL - TREAT AS STRAIGHT BATCH */
     DO WHILE(EOF_SYSIN=FALSE);
        SC_MAX=SC_MAX+1;
        IF SC_MAX>HBOUND(SOURCE_LINE,1) THEN
        DO;
            PUT SKIP LIST('**** FATAL ERROR - PROGRAM TO BIG ***');
            STOP;
        END;
        SOURCE_LINE(SC_MAX)=STMT_BUFF;
        SOURCE_APPENDED(SC_MAX)=FALSE;
        READ FILE(SYSIN) INTO(STMT_BUFF);
     END;

 END MONITOR;
 /****************  END BASRDR  V1.0.0  *****************************/
./  ENDUP
