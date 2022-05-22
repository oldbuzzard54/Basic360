//BASIC04  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A, ,TYPRUN=SCAN
//   USER=HERC01,PASSWORD=CUL8TR
//S1  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=*
//SYSUT2   DD DSN=HERC01.BASIC.PLI,DISP=OLD
//SYSIN DD *
./  ADD NAME=BASCORE
             /****** BASIC/360  V3.3.1 ** 05/21/2022 *********/
 /********************************************************************
 *                                                                   *
 *   SOUTH HAMMOND INSTITUTE OF TECHNOLOGY  BASIC/360   FALL 1974    *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 *   IMPLEMENT A BASIC COMPILER/INTERPRETER FOR THE IBM/360          *
 *   USING THE ORIGINAL DARTMOUTH SPECS FOR BASIC.  THE PRIMARY      *
 *   INTENT IS TO CREATE A BASIC COMPILER/INTERPRETER FOR BEGINNING  *
 *   STUDENTS TO LEARN THE BASIC LANGUAGE INSTEAD OF GOTRAN ON THE   *
 *   SOON TO BE RETIRED 1620.                                        *
 *                                                                   *
 *   THE TARGET ENVIRONMENT IS A 32K IBM/360 MOD 30 RUNNING          *
 *   DOS/360 AND PL/I(D) COMPILER.                                   *
 *                                                                   *
 *   STUDENTS MAY NOT BE COMPUTER MAJORS AND MOST PROGRAMS WOULD BE  *
 *   SMALL, A SIMPLE MONITOR MONITOR WAS IMPLEMENTED SO THE LAB AID  *
 *   OR INSTRUCTOR COULD ACTUALLY SUBMIT ALL THE BASIC PROGRAMS AS   *
 *   ONE JOB.                                                        *
 *                                                                   *
 *   THIS PACKAGE IS BEING DESIGNED TO HAVE MODULAR SOURCE CODE      *
 *   SINCE IT ENVISIONED THAT THIS PRODUCT WILL BE IMPLEMENTED       *
 *   IN SEVERAL DIFFERENT ENVIRONMENTS                               *
 *      1) SIMPLE BATCH - 1 BASIC PROGRAM AT A TIME                  *
 *      2) MONITOR BATCH - MULTIPLE BASIC PROGRAMS CAN BE EXECUTED   *
 *                         PER RUN.                                  *
 *      3) ONLINE (WISH) - BASIC PROGRAM CAN BE ENTERED, EDITED AND  *
 *                         EXECUTED ON LINE.                         *
 *                                                                   *
 ********************************************************************/
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V3.3.1                                                *
 *                                                                   *
 *   V3.3.1 CHANGE LOG                                               *
 *  -- FIXES:                                                        *
 *   - FIXED BUG THAT REJECTED ZERO SUBSCRIPTS                       *
 *   - FIXED BUG THAT CAUSE STACK ERRORS                             *
 *                                                                   *
 ********************************************************************/
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V3.3                                                  *
 *                                                                   *
 *   V3.3 CHANGE LOG                                                 *
 *  -- FIXES:                                                        *
 *   - FIXED BUG COMPILING 1ST PRINT STATEMENT.                      *
 *   - FIXED SEP_CHAR USED IN A STRING TERMINATES STATEMENT          *
 *   - FIXED BUG IN PRINT STATEMENT WHERE CODE COMPILED BUT          *
 *     THE VALUE IS NOT PRINTED.                                     *
 *   - FIXED BUG IN BALANCE_STMT TO CORRECTLY COUNT PARENS           *
 *  --ENHANCEMENTS:                                                  *
 *   - CODE CLEAN UP                                                 *
 *   - ADDED EXP AND LOG LIBRARY FUNCTION                            *
 *   - MODIFIED KEYWORD SCAN TO STOP WHEN KEYWORD FOUND RATHER THAN  *
 *     DEPENDING ON A SPACE AS A DELIMITER.                          *
 *   - IMPLEMENTED IMPLIED LET IF AN = IS FOUND DURING KEYWORD SCAN  *
 *   - INTRODUCED THE $DEBUG MACRO                                   *
 *  -- KNOWN BUGS:                                                   *
 *   - RENUM DOES NOT WORK WITH THE SEPCHAR CHARACTER.               *
 *   - UNINARY "-" NOT WORKING.  CAUSES CONVERSION                   *
 *        WORKAROUNC IS TO CODE 0-Z RATHER THEN -Z                   *
 *        MORE TO COME....                                           *
 ********************************************************************/
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V3.2                                                  *
 *                                                                   *
 *   V3.2 CHANGE LOG                                                 *
 *  --ENHANCEMENTS:                                                  *
 *   - SOURCE CODE REVISIONS TO ENABLE INCLUSION INTO ENVIRONMENTS   *
 *     OTHER THAN BATCH,  KEY SUBROUTINES WERE MOVED INTO INCLUDES   *
 *     NEW INCLUDES ARE:                                             *
 *        BASENV  -  ENVIRONMENTAL SUPPORT (BATCH VS INTERACTIVE)    *
 *        BASRDR  -  LOADS THE SOURCE_CODE TABLE                     *
 *        BASPRT  -  HANDLES ALL PRINTED OUTPUT EXCEPT FOR           *
 *                   DEBUGGING AND ERROR HANDLING, WHICH GO TO       *
 *                   SYSPRINT.                                       *
 *        BASINP  -  HANDLE THE INPUT STATEMENT EXECUTION            *
 *                                                                   *
 *   - SUPPORT FOR CHANGING THE PCODE INSTRUCTION EXECUTION          *
 *     LIMITS.  FOR BATCH1UP, IF THE MAX IS SET TO ZERO, NO LIMIT    *
 *     WILL BE ENFORCED.                                             *
 *   - FINISHED INPUT STATEMENT WITHOUT PROMPTS.  SEE BASINP.PLI     *
 *        FOR MORE DETAILS.                                          *
 *   - ADDED *SEPCHAR TO ALLOW CHANGE THE MULTI STATEMENT SEPERATOR  *
 *  -- KNOWN BUGS:                                                   *
 *   - RENUM DOES NOT WORK WITH THE SEPCHAR CHARACTER.               *
 *   - UNINARY "-" NOT WORKING.  CAUSES CONVERSION                   *
 *        WORKAROUNC IS TO CODE 0-Z RATHER THEN -Z                   *
 *        MORE TO COME....                                           *
 *                                                                   *
 *********************************************************************
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V3.1                                                  *
 *                                                                   *
 *   V3.1 CHANGE LOG                                                 *
 *  --ENHANCEMENTS:                                                  *
 *   - ADDED SUPPORT FOR MULTIPLE STATEMENT PER LINE.  STATEMENTS    *
 *     CAN BE SEPARATED BY THE \ CHARACTER.  EXAMPLE:                *
 *        10 PRINT "HELLO"\PRINT "AGAIN"\PRINT "!"                   *
 *     THE \ CHARACTER CANNOT BE USED IN LITTERALS OR REM STMTS      *
 *     TO END A REM STATEMENT.                                       *
 *   - ADDED THE LS1 AND LS2 PSEUDO OP CODES,  CHANGED LDR TO        *
 *     TO SKP TO SUPPORT THE EJECT KEYWORD.                          *
 *   - DIM STMT MODIFIED TO HANDLE 1 OR 2 SUBSCRIPTS                 *
 *   - SUPPORT FOR 2 DIMENSION ARRAYS ADDED.                         *
 *   - ADDED SOME COMMENTS TO CODE AND DID SOME CODE CLEANUP.        *
 *   - ADDED EJECT STATEMENT TO SKIP TO A NEW PAGE.                  *
 *   - FIXED BUGS FOUND WHILE TESTING DOUBLE SUBSRIPTS               *
 *  -- KNOWN BUGS:                                                   *
 *   - RENUM DOES NOT WORK WITH THE \ CHARACTER.                     *
 *                                                                   *
 *********************************************************************
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V3.0                                                  *
 *                                                                   *
 *   V3.0 CHANGE LOG                                                 *
 *  -- FIXES:                                                        *
 *   - FIXED EXPRESSION PARSE WHEN LAST ITEM WAS SOMETIMES DROPPED   *
 *   - REPLACED STREAM INPUT WITH RECORD INPUT FOR SYSIN             *
 *   - CHANGED FOR NEXT FROM A DO..UNTIL TO A DO..WHILE.  MAKES      *
 *     THIS BASIC HANDLING OF FOR NEXT STANDARD.                     *
 *   - ADDED THE STRINGRANGE PREFIX TO THE EXECUTE PROC.  THIS       *
 *     ELIMINATED SOME S0C4 PROBLEMS EVEN THOUGH NO STRING           *
 *     RANGES ARE BEING VIOLATED.                                    *
 *  --ENHANCEMENTS:                                                  *
 *   - ADDED RANDOMIZE STATEMENT                                     *
 *   - ENHANCED RND FUNCTION TO ALLOW BASIC PGM TO SET THE SEED.     *
 *     THIS IS EQUIVALENT TO THE RANDOMIZE STATEMENT.                *
 *   - ADDED SAVE, APPEND AND LIB OPTIONS.                           *
 *   - ADDED LIST/NOLIST OPTIONS                                     *
 *   - ADDED CODE TO ALLOW DEFAULT LIBRARY DSN TO BE PASSED FROM     *
 *     THE EXEC CARD PARM.                                           *
 *   - IMPLEMENTED PRINT USING                                       *
 *   - RENAMED FLUSH_BUFFER TO DISPLAY_PRINT_LINE AND CHANGED BOTH   *
 *     COMPILE AND EXECUTE PROCS TO USE IT.  DEBUGGING CODE NOT      *
 *     CONVERTED.                                                    *
 *                                                                   *
 *********************************************************************
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V2.2                                                  *
 *                                                                   *
 *   V2.2 CHANGE LOG                                                 *
 *  -- FIXES:                                                        *
 *   - FIXED LINE OVERFLOW REPORTED BY MARCUS LOEW                   *
 *   - COSMETIC FIXES TO LISTING                                     *
 *  --ENHANCEMENTS:                                                  *
 *   - ADDED INR FUNCTION - IT IS INT WITH ROUNDING                  *
 *   - ADDED STRING COMPARE TO IF STATEMENT                          *
 *   - ADDED STOP STATEMENT TO BE USED FOR ABNORMAL ENDING           *
 *   - ADDED SUBSCRIPT CHECKING TO PREVENT PROTECTION EXCEPTIONS     *
 *                                                                   *
 *********************************************************************
 /********************************************************************
 *                                                                   *
 *   BASIC/360 V2.1                                                  *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 *   THIS PROJECT WAS STARTED AS A CLASS PROJECT A WHILE BACK. IN    *
 *   TYPICAL IT STYLE, IT WAS SHELVED UNTIL WE HAD TIME TO WORK ON   *
 *   IT AGAIN.  IT IS ONLY 42 YEARS LATE.                            *
 *                                                                   *
 *   I FOUND IT IM MY ARCHIVES AND SCANNED IT.  WITH A LITTLE        *
 *   WORK, BASIC/360 LIVES (OR HAS BEEN RESURECTED - DEPENDS ON HOW  *
 *   YOU WANT TO LOOK AT IT).                                        *
 *                                                                   *
 *   V1.0 WORKED BUT I DID SOME TESTING ON IT AND FOUND A FEW BUGS   *
 *   IN THE CODE.  THEY WERE FIXED.                                  *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 *   V2.1 CHANGE LOG                                                 *
 *  -- FIXES:                                                        *
 *   - CORRECTED TYPOS.                                              *
 *  --ENHANCEMENTS:                                                  *
 *   - CLEANED UP CODE FOR IMPLEMENTING BASIC LIBRARY FUNCTIONS      *
 *   - ADDED RND FUNCTION TO THE BASIC LIBRARY FUNCTIONS             *
 *   - CONSOLDATED THE STRING_STACK INTO SYMBOL_TABLE TO PREPARE     *
 *     FOR SUPPORTING STRING VARIABLES.                              *
 *   - REVISING CODE TO USE THE SELECT....ENDSELECT MACROS           *
 *   - REVISING CREATION OF PC_OPCODE TABLE TO USE MACROS            *
 *   - REVISED SYNTAX ERROR MESSAGES WITH MORE DETAIL                *
 *   - ADDED SUPPORT TO PCODE INTERPRETER TO ABOUT ILLEGAL MIXED     *
 *     MODE (I.E. MIXING NUMERIC AND STRINGS TOGETHER IN A LINE)     *
 *   - STRING VARIABLES ADDED.                                       *
 *   - STRING CONSTANTS IN LET STATEMENTS ADDED.                     *
 *   - SUPPORT FOR STRINGS IN READ AND DATA STATEMENTS.              *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 *   V2.0 CHANGE LOG                                                 *
 *  -- ID CHANGE.  THERE WAS NO SUCH PLACE AS SOUTH HAMMOND          *
 *                 INSTITUTE OF TECHNOLOGY.  IT WAS REALLY PURDUE    *
 *                 UNIVERSITY CALUMET.  THE ACRONYM WAS A JOKE       *
 *                 ORIGINALLY BUT NOW IS NOT IN GOOD TASTE.          *
 *  -- FIXES:                                                        *
 *   - IF A PRINT STATEMENT FOLLOWS AN IF STATEMENT, THE COMPILER    *
 *     ABORTS WITH A PROTECTION EXCEPTION.                           *
 *   - PRINTING VALUES => 1.0E+6 RESULTS IN BAD OUTPUT               *
 *   - DEFAULT PRINT COLUMN WIDTHS WERE CHANGED FROM 12 TO 14        *
 *   - DIVISION BY ZERO CAUSES JOB TO ABORT                          *
 *   - CODE ADDED TO ABORT THE BASIC PROGRAM NOT THE JOB             *
 *   - FIXED CODE GENERATION FOR DIM ACCESS.                         *
 *  --ENHANCEMENTS:                                                  *
 *   - CHANGED CODE TO UTILIZE PL/I(F) FEATURES.                     *
 *   - MISC CODE CLEANUP AND COMMENTS ADDED.                         *
 *   - PC_FORMAT WAS ADDED TO THE VALID OPCODE TABLE TO IDENTIFY     *
 *     WHAT WAS IN THE PC_OBJECT FIELD.  PRINT_PCODES WAS ALSO       *
 *     MODIFIED TO USED THE FORMAT CODES INSTEAD OF THE PNEMONICS    *
 *   - DEF FUNCTIONS HAVE BEEN IMPLEMENTED.                          *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 *   WISH LIST (OR STUFF PUT OFF UNTIL LATER)                        *
 *   - CHANGE FOR..NEXT TO A DO..WHILE CONSTRUCT                     *
 *   - TSO ENVIRONMENT IMPLEMENTATION.                               *
 *   - RELAX KEYWORD DETECTION SO THINGS LIKE PRINT" OR LET ASSUMED  *
 *     IF NO KEYWORD IS PRESENT.                                     *
 *                                                                   *
 ********************************************************************/
1%INCLUDE FIX2STR;
 %INCLUDE SELECT;
 %INCLUDE GENPC;
 %INCLUDE GENSYM;
 %INCLUDE $DEBUG;
 %$DEBUG_TR='OFF';
 /*#INCLUDE ..\BASICTEST\BASICTEST1UP\BASENV.PLI*/ %INCLUDE BASENV;

 /********************************************************************
 *                                                                   *
 *                         GLOBAL VARIABLES                          *
 *                                                                   *
 ********************************************************************/

     DECLARE PAGE_NUM            FIXED DECIMAL(5,0) INITIAL(0);
     DECLARE PGM_PAGE_NUM        FIXED DECIMAL(5,0) INITIAL(0);
     DECLARE SYSIN               RECORD FILE;
     DECLARE EOF_SYSIN           BIT(1) ALIGNED INITIAL('0'B);
     DECLARE EOP_SYSIN           BIT(1) ALIGNED;
     DECLARE QUOTE_1             CHAR(1) INITIAL('"'),
             QUOTE_2             CHAR(2) INITIAL('""');
     DECLARE ERROR_COUNT         FIXED DECIMAL(5,0) INITIAL(0);

     DECLARE 1 STMT_IN,
               2 STMT            CHAR(80),
              (2 STMT_LEFT,
               2 STMT_RIGHT,
               2 STMT_CH)        FIXED BINARY ALIGNED;
     DECLARE STMT_BUFF           CHAR(80);
     DECLARE LAST_LINE_NUM       FIXED DECIMAL(5,0);
     DECLARE REF_LINE_NUM        FIXED DECIMAL(5,0);
     DECLARE WORD                CHAR(10);
     DECLARE RUN_DATE            CHAR(10);
     DECLARE BASIC_RENUM         BIT(1) ALIGNED INITIAL('0'B);
     DECLARE BASIC_RENUM_DONE    BIT(1) ALIGNED INITIAL('0'B);
     DECLARE PRINT_USING_MODE    BIT(1) ALIGNED INITIAL('0'B);
     DECLARE EXECUTE_CODE        BIT(1) ALIGNED INITIAL('0'B);
     DECLARE SAVE_CODE           BIT(1) ALIGNED INITIAL('0'B);
     DECLARE ABNORMAL_STOP       BIT(1) ALIGNED INIT('0'B);
     DECLARE MONITOR_STMT        CHAR(80);

     DECLARE (RENUM_START,RENUM_STEP)
                                 FIXED DECIMAL(5,0);

     DECLARE 1 BASALO_PARM   STATIC,
               2  BLOCK_NAME              CHAR(8) INITIAL('BASIC'),
               2  FUNCTION_CODE           CHAR(4) INITIAL((4)' '),
               2  SVC99_RETURN,
                  3  SVC99_RETURN_CODE    FIXED BINARY(15),
                  3  SVC99_INFO_CODE      FIXED BINARY(15),
               2  DD_NAME                 CHAR(8)  INITIAL('BASICLIB'),
               2  DS_NAME                 CHAR(44) INITIAL((44)' '),
               2  MEMBER_NAME             CHAR(8)  INITIAL((8)' ');
 /********************************************************************
 *                                                                   *
 *                         GLOBAL DEBUGING                           *
 *                                                                   *
 ********************************************************************/

     DECLARE STACK_PRINT_DEBUG   BIT(1) ALIGNED INITIAL('0'B);
     DECLARE EXECUTION_DEBUG     BIT(1) ALIGNED INITIAL('0'B);
     DECLARE TABLE_PRINT         BIT(1) ALIGNED INITIAL('0'B);
     DECLARE TABLE_DUMP          BIT(1) ALIGNED INITIAL('0'B);
     DECLARE ICODE_PRINT         BIT(1) ALIGNED INITIAL('0'B);
     DECLARE SUBTR_PRINT         BIT(1) ALIGNED INITIAL('0'B);

1/********************************************************************
 *                                                                   *
 *                         GLOBAL CONSTANTS                          *
 *                                                                   *
 *  THESE CONSTANTS ARE USED IN TWO OR MORE OF THE MAJOR MODULES     *
 *  OF THE COMPILER/INTERPRETER.                                     *
 *                                                                   *
 ********************************************************************/

     DECLARE TRUE                BIT(1) ALIGNED STATIC INITIAL('1'B);
     DECLARE FALSE               BIT(1) ALIGNED STATIC INITIAL('0'B);
     DECLARE ZERO_FBA            FIXED BINARY ALIGNED STATIC
                                                        INITIAL(0);
     DECLARE ONE_FBA             FIXED BINARY ALIGNED STATIC
                                                        INITIAL(1);
     DECLARE A_ZERO              CHAR(10) STATIC INITIAL('0');
     DECLARE A_ONE               CHAR(10) STATIC INITIAL('1');
     DECLARE SEP_CHAR_DEFAULT    CHAR(1) INITIAL('\') STATIC;
     DECLARE SEP_CHAR            CHAR(1) INITIAL('\');
     DECLARE VALID_VAR_CHARS     CHAR(37) STATIC
        INITIAL(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');

     DECLARE 1 KEY_WORD_AREA     STATIC,
               2 KW_DATA         CHAR(10) INITIAL('DATA'),
               2 KW_DEF          CHAR(10) INITIAL('DEF'),
               2 KW_DIM          CHAR(10) INITIAL('DIM'),
               2 KW_END          CHAR(10) INITIAL('END'),
               2 KW_FOR          CHAR(10) INITIAL('FOR'),
               2 KW_GOSUB        CHAR(10) INITIAL('GOSUB'),
               2 KW_GOTO         CHAR(10) INITIAL('GOTO'),
               2 KW_IF           CHAR(10) INITIAL('IF'),
               2 KW_LET          CHAR(10) INITIAL('LET'),
               2 KW_NEXT         CHAR(10) INITIAL('NEXT'),
               2 KW_PRINT        CHAR(10) INITIAL('PRINT'),
               2 KW_READ         CHAR(10) INITIAL('READ'),
               2 KW_REM          CHAR(10) INITIAL('REM'),
               2 KW_RETURN       CHAR(10) INITIAL('RETURN'),
               2 KW_RESTORE      CHAR(10) INITIAL('RESTORE'),
               2 KW_STOP         CHAR(10) INITIAL('STOP'),
               2 KW_RANDOMIZE    CHAR(10) INITIAL('RANDOMIZE'),
               2 KW_INPUT        CHAR(10) INITIAL('INPUT    '),
               2 KW_EJECT        CHAR(10) INITIAL('EJECT    '),

             1 KEY_WORDS(19)     DEFINED KEY_WORD_AREA
                                 CHAR(10);

     DECLARE 1 SS_CONSTANTS      STATIC ALIGNED,
               2  SS_UNKNWN      FIXED BINARY INITIAL(0),
               2  SS_UNKNWM_DESC CHAR(8)      INITIAL('UNKNOWN '),
               2  SS_CONST       FIXED BINARY INITIAL(1),
               2  SS_CONST_DESC  CHAR(8)      INITIAL('CONST   '),
               2  SS_FUNC        FIXED BINARY INITIAL(2),
               2  SS_FUNC_DESC   CHAR(8)      INITIAL('FUNCTION'),
               2  SS_VAR         FIXED BINARY INITIAL(3),
               2  SS_VAR_DESC    CHAR(8)      INITIAL('VAR     '),
               2  SS_DIM_VAR     FIXED BINARY INITIAL(4),
               2  SS_DIM_DESC    CHAR(8)      INITIAL('DIM     '),
               2  SS_DEF_VAR     FIXED BINARY INITIAL(5),
               2  SS_DEF_DESC    CHAR(8)      INITIAL('DEF     '),
               2  SS_STRCON      FIXED BINARY INITIAL(6),
               2  SS_STRCON_DESC CHAR(8)      INITIAL('STRCON  '),
               2  SS_STRVAR      FIXED BINARY INITIAL(7),
               2  SS_STRVAR_DESC CHAR(8)      INITIAL('STRVAR  '),
               2  SS_STRDIM      FIXED BINARY INITIAL(8),
               2  SS_STRDIM_DESC CHAR(8)      INITIAL('STRDIM  ');

     DECLARE 1 SS_CON_TABLE      BASED (SS_CON_TABLE_PTR),
               2  SS_TAB(0:8),
                 3 SS_CODE        FIXED BINARY,
                 3 SS_DESC        CHAR(8);
1/********************************************************************
 *                                                                   *
 *                        PSEUDO OPCODES DEFINITION                  *
 *                                                                   *
 *  THE PC_FORMAT CODES DESCRIBE THE TYPE OF ARGUMENT EACH PSEUDO    *
 *  EXPECTS.  MOSTLY USED TO CORRECTLY PRINT THE PCODES.             *
 *      PC_FORMAT_   INDICATES                                       *
 *      ----------   -------------------------------------------     *
 *          0        VARIABLE LOCATED IN THE SYMBOL_TABLE            *
 *          1        OBJECT IS A LINE NUMBER DEFINITION              *
 *          2        OBJECT IS A LINE NUMBER IN THE LINE_STACK       *
 *          3        OBJECT IS A STRING                              *
 *          4        OBJECT IS NOT USED                              *
 *          5        OBJECT IS AN OFFSET IN THE PC_TABLE             *
 *                                                                   *
 *   THE GENPC MACRO DEFINES A P-CODE.  GENPC IS GIVEN 4 PARMS:      *
 *     1)  THE MNEMONIC FOR THE P-CODE                               *
 *     2)  THE NUMERIC P-CODE                                        *
 *     3)  THE P-CODE FORMAT AS DEFINED IN PC-FORMAT                 *
 *     4)  TYPE CHECKING ENFORCEMENT - TWO BINARY DIGITS THAT        *
 *         INDICATE IF NUMBER VS STRING TESTS ARE TO BE MADE.        *
 *         00=NO CHECKING                                            *
 *         01=OPERAND MUST BE NUMERIC                                *
 *         10=OPERAND MUST BE STRING                                 *
 *         11=OPERAND MUST TYPE MUST MATCH ACCUM TYPE                *
 *                                                                   *
 ********************************************************************/


     DECLARE 1 MISC_CODE_DEF     STATIC ALIGNED,
               2  PC_FORMAT_0    FIXED BINARY INITIAL(0),
               2  PC_FORMAT_1    FIXED BINARY INITIAL(1),
               2  PC_FORMAT_2    FIXED BINARY INITIAL(2),
               2  PC_FORMAT_3    FIXED BINARY INITIAL(3),
               2  PC_FORMAT_4    FIXED BINARY INITIAL(4),
               2  PC_FORMAT_5    FIXED BINARY INITIAL(5),
               2  PCT_LFEED      FIXED BINARY INITIAL(0),
               2  PCT_TAB        FIXED BINARY INITIAL(1),
               2  PCT_NOTAB      FIXED BINARY INITIAL(2),
               2  EXP_RCVR       FIXED BINARY INITIAL(0),
               2  EXP_CALC       FIXED BINARY INITIAL(1),
               2  EXP_FN_CALC    FIXED BINARY INITIAL(2);

     DECLARE 1 PC_CONSTANTS      STATIC ALIGNED,
               GENPC(SLN,00,1,00)
               GENPC(LDA,01,0,11)
               GENPC(STA,02,0,11)
               GENPC(EXP,03,0,01)
               GENPC(ADD,04,0,01)
               GENPC(SUB,05,0,01)
               GENPC(MUL,06,0,01)
               GENPC(DIV,07,0,01)
               GENPC(RDV,08,0,11)
               GENPC(PRV,09,0,11)
               GENPC(PCT,10,5,00)
               GENPC(FNC,11,0,00)
               GENPC(END,12,4,00)
               GENPC(B  ,13,2,00)
               GENPC(BAL,14,2,00)
               GENPC(RET,15,0,00)
               GENPC(PRS,16,3,00)
               GENPC(LCA,17,0,11)
               GENPC(LCB,18,0,11)
               GENPC(BEQ,19,2,00)
               GENPC(BNE,20,2,00)
               GENPC(BGT,21,2,00)
               GENPC(BLT,22,2,00)
               GENPC(BGE,23,2,00)
               GENPC(BLE,24,2,00)
               GENPC(FSU,25,0,00)
               GENPC(FIX,26,0,00)
               GENPC(FUL,27,0,00)
               GENPC(FST,28,0,00)
               GENPC(FNX,29,0,00)
               GENPC(PTB,30,0,00)
               GENPC(RST,31,4,00)
               GENPC(DSL,32,0,00)
               GENPC(SKP,33,0,00)
               GENPC(STR,34,0,00)
               GENPC(JMP,35,5,00)
               GENPC(CFN,36,0,00)
               GENPC(RFN,37,0,00)
               GENPC(STP,38,0,00)
               GENPC(PUS,39,4,00)
               GENPC(PUE,40,4,00)
               GENPC(RAN,41,4,00)
               GENPC(LS1,42,0,00)
               GENPC(LS2,43,0,00)
               GENPC(INP,44,0,11)

             1 PC_CON_TABLE      BASED (PC_CON_TABLE_PTR),
               2  PC_OPTAB(0:GENPC_CTR),
                 3 PC_OP_CODE     FIXED BINARY,
                 3 PC_MNEMONIC    CHAR(4),
                 3 PC_FORMAT      FIXED BINARY,
                 3 PC_ALLOW       BIT(2) ALIGNED;
1/********************************************************************
 *                                                                   *
 *                     GLOBAL OBJECT STRUCTURES                      *
 *                                                                   *
 *  THESE ITEMS ARE USED TO EXECUTE THE BASIC PROGRAM.  THE COMPILE  *
 *  PHASE STORES THE DATA IN THESE OBJECTS AND THE EXECUTION PHASE   *
 *  EXECUTES THEM.                                                   *
 *                                                                   *
 *  PRINT_AREA IS USED TO SEND OUTPUT TO IT'S DESTINATION.  ALL      *
 *                OUTPUT DESTINED FOR USED SHOULD BE PUT IN HERE     *
 *                AND A CALL TO DISPLAY_PRINT_LINE ISSUED.           *
 *  DATA_STACK IS USED TO STORE NUMBERS FROM DATA STATEMENTS.        *
 *                COMPILE STACKS THEM UP AND EXECUTE UNSTACKS THEM   *
 *                NUMBERS EACH TIME A READ IS EXECUTED.              *
 *                                                                   *
 *  LINE_STACK IS USED TO STORE THE BASIC LINE NUMBERS AND THE       *
 *                OFFSET TO WHERE IN P_CODE THE STATEMENT STARTS.    *
 *                THESE ARE USED TO FIND WHERE GOTO AND GOSUBS       *
 *                TRANSFER CONTROL TO IN P_CODE_STACK.               *
 *                                                                   *
 *  P_CODE_STACK IS USED TO STORE THE EXECUTABLE P_CODES GENERATED   *
 *                DURING THE COMPILE PROCESS ARE THEN EXECUTED.      *
 *                                                                   *
 *  SYMBOL_TABLE IS USED TO STORE THE ALL OF THE NUMERIC DATA        *
 *                VARIABLE, CONSTANTS, DIM VARIABLES AND FUNCTIONS.  *
 *                THIS TABLE IS POPULATED DURING COMPILE TIME WITH   *
 *                VARIABLES NAMES, CONSTANTS AND DIMS NAMES.         *
 *                DURING EXECUTION, THE VALUES FOR ALL VARIABLES     *
 *                STORED AND RETRIEVED FROM THIS TABLE BY THE        *
 *                EXECUTION PHASE.  NUMERIC CONSTANTS ARE RETRIEVED  *
 *                FROM THIS TABLE DURING EXECUTION.                  *
 *                FUNCTIONS ARE INCLUDED IN THIS TABLE AS WELL.      *
 *                STRING_TABLE AND SYMBOL TABLE WERE MERGED. IT      *
 *                IS USED TO STORE THE ALL OF THE STRING DATA.       *
 *                THIS TABLE IS POPULATED DURING COMPILE TIME WITH   *
 *                STRING CONSTANTS AND SPACE RESERVED FOR STRING     *
 *                VARIABLES.                                         *
 *                DURING EXECUTION, THE VALUES FOR THE CONSTANTS     *
 *                AND VARIABLES ARE RETRIEVED FROM THIS TABLE.       *
 *                                                                   *
 *  SOURCE_TABLE  IS USED TO STORE THE SOURCE CODE TO BE COMPILED.   *
 *                EACH OF THE ENVIRONMENTS LOADS THE BASIC PROGRAM   *
 *                AND THEM PASSES IT TO THE COMPILER.                *
 *                                                                   *
 *  DEF_FUNCTIONS IS USED TO STORE THE NAMES OF THE USER DEFINED     *
 *                FUNCTIONS.                                         *
 *                                                                   *
 ********************************************************************/

     DECLARE 1 PRINT_AREA,
               2  PRINT_LINE     CHAR(120) VARYING;
     DECLARE 1 DATA_STACK        ALIGNED,
               2  (DS_CUR,
                   DS_MAX)       FIXED BINARY,
               2   DS_TABLE($DATA_STACK),
                  3 DS_STR        FIXED BINARY,
                  3 DS_ITEM       FLOAT BINARY;
     DECLARE 1 LINE_STACK        ALIGNED,
               2  (LS_CUR,
                   LS_MAX)       FIXED BINARY,
               2   LS_NUM($MAX_LINES),
                  3  LS_LINE     FIXED DECIMAL(5,0),
                  3  LS_OFFSET   FIXED BINARY,
                  3  LS_SOURCE   FIXED BINARY;
     DECLARE 1 SOURCE_CODE       ALIGNED,
               2  (SC_CUR,
                   SC_MAX)       FIXED BINARY,
               2  SOURCE_AREA($MAX_LINES),
                  3  SOURCE_LINE CHAR(80),
                  3  SOURCE_APPENDED BIT(1) ALIGNED;
     DECLARE 1 P_CODE_STACK      ALIGNED,
               2  (PC_CUR,
                   PC_MAX)       FIXED BINARY,
               2   PC_NUM($MAX_PCODE),
                  3  (PC_OPCODE,
                      PC_OBJECT) FIXED BINARY;
     DECLARE 1 SYMBOL_STACK      ALIGNED,
               2  (SS_CUR,
                   SS_MAX_FNC,
                   SS_MAX)       FIXED BINARY,
               2  SYMBOL_AREA($MAX_SYM),
                  3  SYMBOL       CHAR(10),
                  3  SYM_TYPE     FIXED BINARY,
                  3  SYM_VALUE    FLOAT DECIMAL,
                  3  SYM_DIM_MAX  FIXED BINARY,
                  3  SYM_DIM2_MAX FIXED BINARY,
                  3  STRING_VAL   CHAR(80) VARYING;
     DECLARE 1 DEF_FUNCTIONS      ALIGNED,
               2  (DF_CUR,
                   DF_MAX)       FIXED BINARY,
               2  DEF_FUNC_AREA(10),
                  3  DF_NAME     CHAR(10),
                  3 (DF_OFFSET,
                  3  DF_RETURN)  FIXED BINARY;

1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

     RUN_DATE=DATE;   /* DATE IS IN YYMMDD FORMAT */
     RUN_DATE=SUBSTR(RUN_DATE,3,2)|| '/' || SUBSTR(RUN_DATE,5,2) ||
              '/20' || SUBSTR(RUN_DATE,1,2);

 /*********************************************************************
 *                                                                    *
 *  THESE TWO POINTERS MUST BE SET.  THE BASE STRUCTURES ARE MIXED    *
 *  WITH BINARY AND CHARACTER DATA AND PL/I DOES NOT ALLOW DEFINED    *
 *  STRUCTURES LIKE THIS.  SO THEY WERE MADE INTO BASED TABLES.       *
 *                                                                    *
 *********************************************************************/
     PC_CON_TABLE_PTR = ADDR(PC_CONSTANTS);
     SS_CON_TABLE_PTR = ADDR(SS_CONSTANTS);

 /*#INCLUDE ..\..\BASICTEST\BASICTEST1UP\BASRDR.PLI*/ %INCLUDE BASRDR;
1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 INITIALIZE:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC INITIALIZES ALL OF THE GLOBAL DATA ELEMENTS AND       *
 *   STRUCTURES FOR THE COMPILATION AND EXECUTION OF THE BASIC       *
 *   PROGRAM.                                                        *
 *                                                                   *
 * NESTING:INITIALIZE                                                *
 ********************************************************************/

     STMT_LEFT=1;
     STMT_RIGHT=72;
     LAST_LINE_NUM=-1;

     DS_CUR,DS_MAX=0;
     LS_CUR,LS_MAX=0;
     PC_CUR,PC_MAX=0;
     DF_CUR,DF_MAX=0;
     ERROR_COUNT=0;

     PRINT_LINE='';
     PRINT_USING_MODE=FALSE;
     DEFAULT_DSN=DEFAULT_LIB;
     SEP_CHAR=SEP_CHAR_DEFAULT;

     GENSYM(RESULTS,SS_VAR,0.0,*)

   /**********************************************************
   *                                                         *
   *  THESE ARE THE BUILTIN FUNCTIONS.  IF YOU ADD MORE      *
   *  BE SURE TO ADD THEM TO THE FUNCTION INTERPRETER IN     *
   *  EXECUTE   PSUDEO OP CODE FNC                           *
   *                                                         *
   **********************************************************/

 /********************************************************************
 *                                                                   *
 * IMPORTANT NOTE - IF ANY CHANGES ARE MADE TO LIBRARY FUNCTIONS,    *
 * THE PCODE FNC SHOULD MATCH THE CHANGES IN THE INITIALIZE PROC     *
 *                                                                   *
 ********************************************************************/

     GENSYM(SQR,SS_FUNC,0.0,*)
     GENSYM(ABS,SS_FUNC,0.0,*)
     GENSYM(TAB,SS_FUNC,0.0,*)
     GENSYM(INT,SS_FUNC,0.0,*)
     GENSYM(COS,SS_FUNC,0.0,*)
     GENSYM(SIN,SS_FUNC,0.0,*)
     GENSYM(TAN,SS_FUNC,0.0,*)
     GENSYM(RND,SS_FUNC,0.0,*)
     GENSYM(INR,SS_FUNC,0.0,*)
     GENSYM(EXP,SS_FUNC,0.0,*)
     GENSYM(LOG,SS_FUNC,0.0,*)

     SS_CUR,SS_MAX,SS_MAX_FNC = GENSYM_CTR;

 END INITIALIZE;

1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 RENUM:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC RENUMBERS THE SOURCE PROGRAM.  IT IS ASSUMED THAT     *
 *   THE BASIC PROGRAM COMILED WITH NO ERRORS AND THE CONTENTS OF    *
 *   THE GLOBAL TABLES ARE INTACT.  THE SOURCE_CODE TABLE WILL BE    *
 *   UPDATED WITH THE RENUMBERED PROGRAM.                            *
 *                                                                   *
 *   A "DECK" OF THE RENUMBERED PROGRAM WILL BE WRITTEN TO THE       *
 *   RENUMFL                                                         *
 *                                                                   *
 *   NOTE - APPENDED SOURCE WILL NOT BE RENUMBERED OR SAVED TO       *
 *          RENUMFL.                                                 *
 *                                                                   *
 * NESTING:NONE                                                      *
 ********************************************************************/

     DECLARE LINE_WORK           CHAR(80);
     DECLARE LINE_SUB            FIXED BINARY ALIGNED;
     DECLARE NEW_SC_MAX          FIXED BINARY ALIGNED;
     DECLARE A_BLANK             FIXED BINARY ALIGNED;
     DECLARE FIRST_CHAR          FIXED BINARY ALIGNED;
     DECLARE FIRST_DIGIT         FIXED BINARY ALIGNED;
     DECLARE SEP_CHAR_IDX        FIXED BINARY ALIGNED;
     DECLARE (I,LAST_CHAR)       FIXED BINARY ALIGNED;
     DECLARE OLD_LINE_NUM        FIXED DECIMAL(5,0);
     DECLARE NEW_LINE_NUM($MAX_LINES)
                                 FIXED DECIMAL(5,0);
     DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;
     DECLARE EDIT_LINE_NUM       PIC 'ZZZZ9';
     DECLARE RENUMFL             STREAM OUTPUT FILE;

     DO LINE_SUB = 1 TO LS_MAX;
        NEW_LINE_NUM(LINE_SUB)=RENUM_START;
        RENUM_START=RENUM_START+RENUM_STEP;
     END;

     PUT FILE(RENUMFL) EDIT('++BASIC') (SKIP,A);
     NEW_SC_MAX=SC_MAX;
     DO LINE_SUB = 1 TO SC_MAX WHILE(SC_MAX=NEW_SC_MAX);
        IF SOURCE_APPENDED(LINE_SUB) THEN
            NEW_SC_MAX=LINE_SUB-1;
     END;
     SC_MAX=NEW_SC_MAX;
     DO LINE_SUB = 1 TO SC_MAX;
         LINE_WORK = SOURCE_LINE(LINE_SUB);
         SEP_CHAR_IDX=INDEX(LINE_WORK,SEP_CHAR);
         IF SEP_CHAR_IDX > 0 THEN
         DO;
            PUT SKIP EDIT('**** CANNOT RENUM PROGRAM WITH ',
                          'MULTIPLE STATEMENTS PER LINE ****',
                          '**** RENUM CANCELLED ****')
                         (A,A,SKIP,A);
            ERROR_COUNT=1;
            RETURN;
         END;
         IF SUBSTR(LINE_WORK,1,1)='*' THEN;
         ELSE
         DO;
            A_BLANK = INDEX(LINE_WORK,' ');  /* FIND FIRST SPACE */
            IF A_BLANK < 2 THEN
            DO;
               PUT SKIP LIST('**** RENUM FATAL ERROR 1 ****');
               STOP;
            END;
            OLD_LINE_NUM = SUBSTR(LINE_WORK, 1, A_BLANK-1);
            CONTINUE_SCAN=TRUE;
            DO I=1 TO LS_MAX WHILE(CONTINUE_SCAN);
               IF OLD_LINE_NUM=LS_LINE(I) THEN
               DO;
                   EDIT_LINE_NUM = NEW_LINE_NUM(I);
                   CALL TRIM_EDIT_NUM;
                   LINE_WORK=SUBSTR(EDIT_LINE_NUM,FIRST_DIGIT) ||
                             SUBSTR(LINE_WORK,A_BLANK);
                   CONTINUE_SCAN=FALSE;
               END;
            END;
            IF CONTINUE_SCAN THEN
            DO;
               PUT SKIP LIST('**** RENUM FATAL ERROR 2 ****');
               STOP;
            END;

            CONTINUE_SCAN=TRUE;
            A_BLANK = INDEX(LINE_WORK,' ');  /* FIND FIRST SPACE */
            IF A_BLANK = 0 THEN
            DO;
               PUT SKIP LIST('**** RENUM FATAL ERROR 3 ****');
               STOP;
            END;

            DO I=A_BLANK+1 TO STMT_RIGHT WHILE(CONTINUE_SCAN);
               IF SUBSTR(LINE_WORK,I,1)=' ' THEN;
               ELSE CONTINUE_SCAN=FALSE;
            END;

            I=I-1;
            IF SUBSTR(LINE_WORK,I,3)='IF ' |
               SUBSTR(LINE_WORK,I,2)='GO'   THEN
            DO;
               CONTINUE_SCAN=TRUE;
               I=STMT_RIGHT;
               DO WHILE(CONTINUE_SCAN);
                  IF SUBSTR(LINE_WORK,I,1)=' ' THEN
                     I=I-1;
                  ELSE
                     CONTINUE_SCAN=FALSE;
               END;
               CONTINUE_SCAN=TRUE;
               LAST_CHAR=I;
               DO WHILE(CONTINUE_SCAN);
                  IF SUBSTR(LINE_WORK,I,1)=' ' THEN
                     CONTINUE_SCAN=FALSE;
                  ELSE
                     I=I-1;
               END;
               FIRST_CHAR=I;
               OLD_LINE_NUM = SUBSTR(LINE_WORK,I+1,LAST_CHAR-I);
               CONTINUE_SCAN=TRUE;
               DO I=1 TO LS_MAX WHILE(CONTINUE_SCAN);
                  IF OLD_LINE_NUM=LS_LINE(I) THEN
                  DO;
                      IF SOURCE_APPENDED(LS_SOURCE(I)) THEN;
                      ELSE
                      DO;
                         EDIT_LINE_NUM = NEW_LINE_NUM(I);
                         CALL TRIM_EDIT_NUM;
                         LINE_WORK=SUBSTR(LINE_WORK,1,FIRST_CHAR)
                                || SUBSTR(EDIT_LINE_NUM,FIRST_DIGIT);
                      END;
                      CONTINUE_SCAN=FALSE;
                  END;
               END;
            END;
            SOURCE_LINE(LINE_SUB)=LINE_WORK;
         END;
         IF SOURCE_APPENDED(LINE_SUB) = FALSE THEN
            PUT FILE(RENUMFL) EDIT(SOURCE_LINE(LINE_SUB)) (SKIP,A);
     END;
     BASIC_RENUM=FALSE;

 TRIM_EDIT_NUM:PROC;
     SELECT (TRUE)
        WHEN (NEW_LINE_NUM(I)<10)
           FIRST_DIGIT = 5;
        WHEN (NEW_LINE_NUM(I)<100)
           FIRST_DIGIT = 4;
        WHEN (NEW_LINE_NUM(I)<1000)
           FIRST_DIGIT = 3;
        WHEN (NEW_LINE_NUM(I)<10000)
           FIRST_DIGIT = 2;
        OTHERWISE
           FIRST_DIGIT = 1;
     ENDSELECT;
 END TRIM_EDIT_NUM;

 END RENUM;
1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 COMPILE:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC DRIVES THE COMPILE PROCESS FOR THE BASIC PROGRAM      *
 *                                                                   *
 * NESTING:NONE                                                      *
 ********************************************************************/

     DECLARE LAST_PCODE_PRINTED        FIXED BINARY ALIGNED INITIAL(0);
     DECLARE (SCAN_START,SCAN_END)     FIXED BINARY ALIGNED;
     DECLARE SEP_CHAR_IDX              FIXED BINARY ALIGNED;
     DECLARE SEP_CHAR_IDX_2            FIXED BINARY ALIGNED;
     DECLARE TERMINATE_SCAN            BIT(1) ALIGNED;
     DECLARE ALL_PROCESSED             BIT(1) ALIGNED;
     DECLARE MORE_STMTS                BIT(1) ALIGNED;
     DECLARE LIST_SOURCE_CODE          BIT(1) ALIGNED;
     DECLARE (FUNC_NAME,FUNC_ARG)      CHAR(10);
     DECLARE (TMP_CNT,STR_CNT)         PICTURE '99';
     DECLARE RESULT_SYMBOL             CHAR(10);
     DECLARE RESULT_OFFSET             FIXED BINARY ALIGNED;
     DECLARE (TEMP_LEFT,TEMP_RIGHT)    FIXED BINARY ALIGNED;
     DECLARE LAST_NON_BLANK            FIXED BINARY ALIGNED;

     ON ENDPAGE(SYSPRINT)
     BEGIN;
        IF PAGE_NUM > 0 THEN PUT PAGE;
        PAGE_NUM=PAGE_NUM+1;
        PGM_PAGE_NUM=PGM_PAGE_NUM+1;
        PUT EDIT (PAGE_TITLE,'DATE ',RUN_DATE,
                  'PAGE ',PGM_PAGE_NUM)
                 (COLUMN(60),A,COLUMN(93),A,A,COLUMN(110),
                   A,F(5,0));
        PUT SKIP(2) EDIT(MONITOR_STMT) (A)
                    EDIT('OFFSET') (SKIP(2),A);
        PUT SKIP;
     END;

     SIGNAL ENDPAGE(SYSPRINT);

     LIST_SOURCE_CODE=TRUE;
     EXECUTE_CODE=TRUE;
     SAVE_CODE=FALSE;
     STR_CNT = 0;
     SC_CUR=0;
     $DEBUG(OFF)
     DO WHILE(SC_CUR<SC_MAX);
         SC_CUR=SC_CUR+1;
         STMT=SOURCE_LINE(SC_CUR);
         IF LIST_SOURCE_CODE THEN
         DO;
            PUT STRING(PRINT_LINE)
                     EDIT(PC_MAX+1,STMT) (P'999999',X(19),A);
            CALL DISPLAY_PRINT_LINE;
         END;
         IF SUBSTR(STMT,1,1)='*' THEN      /* DONT COMPILE OPTIONS */
            CALL PROCESS_OPTS;
         ELSE
         DO;
            STMT_CH=STMT_LEFT;
            TERMINATE_SCAN=FALSE;
            CALL GET_STMT_NUM(TRUE);
            LS_MAX=LS_MAX+1;
            CALL ADD_PCODE(PC_OPCODE_SLN,LS_MAX);
            LS_LINE(LS_MAX)=LAST_LINE_NUM;
            LS_OFFSET(LS_MAX)=PC_MAX;
            LS_SOURCE(LS_MAX)=SC_CUR;
            STMT_RIGHT=72;   /*  RESET DEFAULT  */

  /*   MULTIPLE KEYWORDS PER STATEMENT ARE SEPERATED BY SEP_CHAR */
  /*   MUST IGNORE SEP_CHAR IF IT IS IN A STRING                 */

            SEP_CHAR_IDX=INDEX_SEP_CHAR(ONE_FBA);

            $DEBUG(DATA,SEP_CHAR_IDX,SEP_CHAR)
            IF SEP_CHAR_IDX = 0 THEN
            DO;
               CALL GET_KEYWORD;
               CALL PROCESS_KEYWORD;
               IF ICODE_PRINT THEN
                  CALL PRINT_PCODES;
            END;
            ELSE
            DO;
               LAST_NON_BLANK=72;
               DO WHILE(SUBSTR(STMT,LAST_NON_BLANK,1)=' '
                        & LAST_NON_BLANK > 1);
                  LAST_NON_BLANK=LAST_NON_BLANK-1;
               END;
               $DEBUG(DATA,LAST_NON_BLANK)
               STMT_RIGHT=SEP_CHAR_IDX-1;
               TEMP_LEFT=STMT_CH;
               TEMP_RIGHT=SEP_CHAR_IDX-1;
               MORE_STMTS=TRUE;
               ALL_PROCESSED=FALSE;
               DO WHILE(MORE_STMTS);
                  $DEBUG(LIST,'MORE STMTS')
                  $DEBUG(DATA,STMT_CH,STMT_RIGHT,SEP_CHAR_IDX)
                  $DEBUG(DATA,TEMP_LEFT,TEMP_RIGHT,ALL_PROCESSED)
                  STMT_CH=TEMP_LEFT;
                  STMT_RIGHT=TEMP_RIGHT;
                  $DEBUG(LIST,'OLD CODE IS ',
                           SUBSTR(STMT,STMT_CH,STMT_RIGHT-STMT_CH+1))
                  $DEBUG(LIST,'NEW CODE IS ',
                           SUBSTR(STMT,TEMP_LEFT,
                                  TEMP_RIGHT-TEMP_LEFT+1))                                             I
                  IF ALL_PROCESSED THEN
                      MORE_STMTS=FALSE;
                  ELSE
                  DO;
                     CALL GET_KEYWORD;
                     CALL PROCESS_KEYWORD;
                     IF ICODE_PRINT THEN
                        CALL PRINT_PCODES;
                     TEMP_LEFT=TEMP_RIGHT+2;
                     STMT_CH=SEP_CHAR_IDX+1;
                     STMT_RIGHT=72;
                     SEP_CHAR_IDX_2=INDEX_SEP_CHAR(TEMP_LEFT);
                     $DEBUG(LIST,'SCANNING FOR SEP_CHAR_IDX_2')
                     $DEBUG(DATA,TEMP_LEFT,SEP_CHAR_IDX_2,SEP_CHAR)
                     IF SEP_CHAR_IDX_2 = 0 THEN
                     DO;
                        ALL_PROCESSED=TRUE;
                        STMT_RIGHT=72;
                        SEP_CHAR_IDX=STMT_CH+1;
                        TEMP_LEFT=TEMP_RIGHT+2; /* SKIP SEP CHAR */
                        TEMP_RIGHT=LAST_NON_BLANK;
                        $DEBUG(LIST,'ALL PROCESSED')
                        $DEBUG(DATA,TEMP_LEFT,TEMP_RIGHT)
                     END;
                     ELSE
                     DO;
                        STMT_RIGHT=SEP_CHAR_IDX_2-1;
                        SEP_CHAR_IDX=SEP_CHAR_IDX_2+SEP_CHAR_IDX;
                        $DEBUG(LIST,'MORE TO PROCESS')
                        TEMP_LEFT=TEMP_RIGHT+2;  /* SKIP SEP_CHAR */
                        TEMP_RIGHT=SEP_CHAR_IDX_2-1;
                        $DEBUG(DATA,TEMP_LEFT,TEMP_RIGHT,SEP_CHAR_IDX_2)
                     END;
                     $DEBUG(LIST,'END OF LOOP')
                     $DEBUG(DATA,TEMP_LEFT,TEMP_RIGHT)
                     $DEBUG(DATA,STMT_RIGHT,SEP_CHAR_IDX,STMT_CH)
                  END;
               END; /* OF DO WHILE */
            END; /* OF MULTI STMTS */
         END;
     END;

     ON ENDPAGE(SYSPRINT)
     BEGIN;
        IF PAGE_NUM > 0 THEN PUT PAGE;
        PAGE_NUM=PAGE_NUM+1;
        PGM_PAGE_NUM=PGM_PAGE_NUM+1;
        PUT EDIT (PAGE_TITLE,'DATE ',RUN_DATE,
                  'PAGE ',PGM_PAGE_NUM)
                 (COLUMN(60),A,COLUMN(93),A,A,COLUMN(110),
                   A,F(5,0));
        PUT SKIP(2);
     END;
     $DEBUG(OFF)
     IF TABLE_PRINT THEN;
     ELSE
        GO TO END_OF_COMP;

     DECLARE I                  FIXED BINARY ALIGNED;

     CALL PRINT_SYMBOLS;

     PUT SKIP(2) LIST('DEF NAME','OFFSET');
     DO I=1 TO DF_MAX;
       PUT SKIP LIST(DF_NAME(I),DF_OFFSET(I));
     END;
     PUT SKIP LIST('END OF DEF NAME TABLE');

     PUT SKIP(2) LIST('DATA STACK ITEM','VALUE');
     DO I=1 TO DS_MAX;
        IF DS_STR(I) > 0 THEN
           PUT SKIP LIST(I,QUOTE_1 || STRING_VAL(DS_STR(I)) || QUOTE_1);
        ELSE
           PUT SKIP LIST(I,DS_ITEM(I));
     END;
     PUT SKIP LIST('END OF DATA STACK');

     PUT SKIP(2) EDIT('OFFSET','LINE  OP   OBJECT      FORMAT')
                      (A,X(7),A);
     CALL PRINT_PCODES;
     PUT SKIP LIST('END OF PCODE TABLE');

 END_OF_COMP:

     CALL DISPLAY_PRINT_LINE;
     IF BASIC_RENUM_DONE THEN
         BASIC_RENUM=FALSE;

     IF ERROR_COUNT=0 THEN
     DO;
        PRINT_LINE='NO ERRORS FOUND';
        IF BASIC_RENUM THEN
            PRINT_LINE=PRINT_LINE||' - RENUMBERING PROGRAM';
     END;
     ELSE
     DO;
        PUT STRING(PRINT_LINE)
              EDIT(ERROR_COUNT,' ERRORS FOUND') (F(5),A);
        IF BASIC_RENUM THEN
            PRINT_LINE=PRINT_LINE||' - RENUMBERING BYPASSED';
     END;
     PRINT_LINE='**** END OF COMPILATION **** '||PRINT_LINE;
     CALL DISPLAY_PRINT_LINE;
     PUT STRING(PRINT_LINE) EDIT
            ('****',PC_MAX,' INSTRUCTIONS GENERATED,',
                    SS_MAX,' SYMBOLS DEFINED',
                    DS_MAX,' DATA ITEMS DEFINED ****')
            (A,F(6),A,F(6),A,F(6),A);
     CALL DISPLAY_PRINT_LINE;

 INDEX_SEP_CHAR:PROC(STMT_POS) RETURNS(FIXED BINARY);
 /********************************************************************
 *                                                                   *
 *   THIS ROUTINE SCANS THE SOURCE STATEMENT FOR SEP_CHAR CHARACTER  *
 *                                                                   *
 *   A SIMPLE INDEX WILL DETECT A SEP_CHAR INSIDE OF A STRING.       *
 *   THEREFORE, A SCAN FOR NOT LOOKING FOR SEP_CHAR WITHIN A STRING  *
 *   MUST BE PERFORMED.                                              *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
     DECLARE STMT_POS                  FIXED BINARY ALIGNED;
     DECLARE IDX,I                     FIXED BINARY ALIGNED;
     DECLARE IN_STR                    BIT(1) ALIGNED;
     DECLARE CH                        CHAR(1);
     $DEBUG(OFF)
     $DEBUG(LIST,'IN INDEX,STMT_POS=',STMT_POS)
     IF STMT_POS < 2 THEN
        IDX=INDEX(STMT,SEP_CHAR);
     ELSE
        IDX=INDEX(SUBSTR(STMT,STMT_POS),SEP_CHAR);
     $DEBUG(DATA,IDX)
     IF IDX=0 THEN
        RETURN(IDX);
     I=STMT_POS;
     IN_STR=FALSE;
     IDX=0;
     DO WHILE(I<=72         & IDX=0);
        CH=SUBSTR(STMT,I,1);
        $DEBUG(DATA,I,CH)
        IF IN_STR THEN
        DO;
           IF CH=QUOTE_1 THEN
              IN_STR=FALSE;
        END;
        ELSE
        DO;
           IF CH=QUOTE_1 THEN
              IN_STR=TRUE;
           ELSE
              IF CH=SEP_CHAR THEN
                IDX=I;
        END;
        I=I+1;
     END;
     $DEBUG(DATA,IDX,CH)
     $DEBUG(OFF)
     RETURN(IDX);
 END INDEX_SEP_CHAR;

 PROCESS_OPTS:PROC;
 /********************************************************************
 *                                                                   *
 *   OPTIONS STATEMENTS ARE MIXED IN WITH THE SOURCE PROGRAM.  THEY  *
 *   HAVE A "*" IN COLUMN 1 AND ARE TREATED AS A COMMENT BY THE      *
 *   COMPILER.                                                       *
 *                                                                   *
 *   THESE ARE OPTIONS FOR SAVING AND RETRIEVING BASIC SOURCE CODE   *
 *   FROM A PARTITIONED DATASET.  NAME ENDS AT FIRST SPACE AFTER     *
 *   THE = CHARACTER.  NAME MUST BE LETTERS AND DIGITS.  MEMBERS     *
 *   ARE 1-8 CHARACTERS, DSN IS 44 OR LESS CHARACTERS.               *
 *                                                                   *
 *   *APPEND=XXXXXXXX  COPY MEMBER XXXXXXXX FROM LIBRARY PDS         *
 *   *SAVE=XXXXXXXX    SAVE CURRENT PROGRAM IN MEMORY TO XXXXXXXX    *
 *                     IN LIBRARY PDS.                               *
 *   *LIB=XXXXXXX      DATASET NAME OF THE LIBRARY FOR THE BASIC     *
 *                     PROGRAM ONLY.  TO CHANGE DEFAULT FOR ENTIRE   *
 *                     JOB, PASS DSN AS EXEC CARD PARM               *
 *                                                                   *
 *   *NOEXEC   OPTION TO SUPRESS EXECUTION EVEN IF NO ERRORS         *
 *             DETECTED DURING COMPILATON                            *
 *                                                                   *
 *   *LIST     ALLOW LISTING OF BASIC SOURCE FROM NOW ON             *
 *   *NOLIST   SUPPRESS LISTING OF BASIC SOURCE FROM NOW ON          *
 *                                                                   *
 *   *RENUM      RENUM PROGRAM STARTING AT 10 BY 10                  *
 *   *RENUM10    RENUM PROGRAM STARTING AT 10 BY 10                  *
 *   *RENUM100   RENUM PROGRAM STARTING AT 100 BY 10                 *
 *   *RENUM1000  RENUM PROGRAM STARTING AT 1000 BY 10                *
 *   *RENUM10000 RENUM PROGRAM STARTING AT 10000 BY 10               *
 *                                                                   *
 *   THESE ARE BASICLY DEBUGGING TOOLS BUILT IN AND WILL NOT         *
 *   NORMALLY BE USED.  THEY OPTIONS ARE:                            *
 *   *TABLE    PRINT ALL OBJECT TABLES AT THE END OF COMPILATION     *
 *   *DUMP     PRINT ALL OBJECT TABLES AT THE END OF EXECUTION       *
 *   *STACK    PRINT THE PARSING STACK DEBUGGING TRACING.  THIS      *
 *             OPTION STARTS AS SOON AS THE STATEMENT IS PROCESSED   *
 *             IT REMAINS IN EFFECT UNTIL THE END OF PROGRAM OR      *
 *             A *NOSTACK IS PROCESSED.                              *
 *   *SUBTR    TRACE SUBSCRIPT PROCESSING.  REMAINS IN EFFECT        *
 *             UNTIL THE END OF THE PROGRAM OR A *NOSUBTR            *
 *             IS PROCESSED.                                         *
 *   *ICODE    PRINTS THE PCODE OBJECT CODE AS IT IS GENERATED.      *
 *             OPTION STARTS AS SOON AS THE STATEMENT IS PROCESSED   *
 *             IT REMAINS IN EFFECT UNTIL THE END OF PROGRAM OR      *
 *             A *NOICODE IS PROCESSED.                              *
 *   *TRACE    PRINT DEBUGGING INFORMATION WHILE THE BASIC PROGRAM   *
 *             IS EXECUTING.                                         *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

     SELECT(TRUE)
     WHEN(SUBSTR(STMT,1,4)='*REM')
     WHEN(SUBSTR(STMT,1,8)='*APPEND=')
         CALL PROCESS_APPEND;
     WHEN(SUBSTR(STMT,1,6)='*SAVE=')
         CALL PROCESS_SAVE;
     WHEN(SUBSTR(STMT,1,5)='*LIB=')
     BEGIN;
         DECLARE VALID_DSN_CHARS    CHAR(47)  STATIC INITIAL(
             'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.');
         DECLARE SPACE_INDEX         FIXED BINARY ALIGNED;
         DECLARE A_ONE               FIXED BINARY ALIGNED INITIAL(1);
         DECLARE A_SEVEN             FIXED BINARY ALIGNED INITIAL(7);
         DECLARE TEMP_DSN            CHAR(44) VARYING;
         SPACE_INDEX = INDEX(STMT,' ');
         IF SPACE_INDEX < 7 |
            SPACE_INDEX >= 50 THEN
         DO;
            CALL PRINT_ERR(A_SEVEN,'*** LIB NAME INVALID ***');
            RETURN;
         END;
         ELSE
         DO;
            TEMP_DSN=SUBSTR(STMT,6,SPACE_INDEX-6);
            IF VERIFY(TEMP_DSN,VALID_DSN_CHARS) > 0 THEN
            DO;
               CALL PRINT_ERR(A_SEVEN,'*** INVALID LIB NAME ***');
               RETURN;
            END;
         DEFAULT_DSN=TEMP_DSN;
         END;
     END;
     WHEN(SUBSTR(STMT,1,7)='*RENUM ')
         BASIC_RENUM=TRUE;
         RENUM_START,RENUM_STEP=10;
     WHEN(SUBSTR(STMT,1,10)='*RENUM100 ')
         BASIC_RENUM=TRUE;
         RENUM_START=100;
         RENUM_STEP=10;
     WHEN(SUBSTR(STMT,1,11)='*RENUM1000 ')
         BASIC_RENUM=TRUE;
         RENUM_START=1000;
         RENUM_STEP=10;
     WHEN(SUBSTR(STMT,1,12)='*RENUM10000 ')
         BASIC_RENUM=TRUE;
         RENUM_START=10000;
         RENUM_STEP=10;
     WHEN(SUBSTR(STMT,1,6)='*LIST ')
        LIST_SOURCE_CODE=TRUE;
     WHEN(SUBSTR(STMT,1,8)='*NOLIST ')
        LIST_SOURCE_CODE=FALSE;
     WHEN(SUBSTR(STMT,1,8)='*NOEXEC ')
        EXECUTE_CODE=FALSE;
     WHEN(SUBSTR(STMT,1,7)='*TABLE ')
        TABLE_PRINT=TRUE;
     WHEN(SUBSTR(STMT,1,9)='*NOTABLE ')
        TABLE_PRINT=FALSE;
     WHEN(SUBSTR(STMT,1,6)='*DUMP ')
        TABLE_DUMP=TRUE;
     WHEN(SUBSTR(STMT,1,7)='*STACK ')
        STACK_PRINT_DEBUG=TRUE;
     WHEN(SUBSTR(STMT,1,9)='*NOSTACK ')
        STACK_PRINT_DEBUG=FALSE;
     WHEN(SUBSTR(STMT,1,7)='*ICODE ')
        ICODE_PRINT=TRUE;
     WHEN(SUBSTR(STMT,1,9)='*NOICODE ')
        ICODE_PRINT=FALSE;
     WHEN(SUBSTR(STMT,1,7)='*TRACE ')
        EXECUTION_DEBUG=TRUE;
     WHEN(SUBSTR(STMT,1,9)='*NOTRACE ')
        EXECUTION_DEBUG=FALSE;
     WHEN(SUBSTR(STMT,1,7)='*SUBTR ')
        SUBTR_PRINT=TRUE;
     WHEN(SUBSTR(STMT,1,9)='*NOSUBTR ')
        SUBTR_PRINT=FALSE;
     WHEN(SUBSTR(STMT,1,4)='*PAT')
         CALL PROCESS_PATCH;
     WHEN(SUBSTR(STMT,1,9)='*SEPCHAR ')
         SEP_CHAR = SUBSTR(STMT,10,1);
         IF SEP_CHAR=' ' THEN
           CALL PRINT_ERR(A_ONE,'*** SEPCHAR CANNOT BE BLANK ***');
     OTHERWISE   /*  JUST IGNORE INVALID OPTIONS */
        CALL PRINT_ERR(A_ONE,'*** INVALID OPTION ***');
     ENDSELECT;
     RETURN;

 PROCESS_PATCH:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:PROCESS_OPTS                                              *
 ********************************************************************/

     DECLARE (PAT_OFF,PAT_NEW_OP,PAT_NEW_OB)  FIXED BINARY ALIGNED;

     PAT_OFF=SUBSTR(STMT,5,6);
     PAT_NEW_OP=SUBSTR(STMT,11,2);
     PAT_NEW_OB=SUBSTR(STMT,13,6);
     PUT SKIP DATA(PC_MAX,PAT_OFF);
     PUT SKIP DATA(PAT_NEW_OP,PAT_NEW_OB);
     PUT SKIP LIST(PC_OPCODE(PAT_OFF),PC_OBJECT(PAT_OFF));
     IF PAT_OFF > PC_MAX THEN
         CALL PRINT_ERR(A_ONE,'*** PAT_OFF > PC_MAX ***');
     ELSE
     DO;
        PC_OPCODE(PAT_OFF)=PAT_NEW_OP;
        PC_OBJECT(PAT_OFF)=PAT_NEW_OB;
        TABLE_PRINT=TRUE;
        LAST_PCODE_PRINTED=0;
        TABLE_DUMP=TRUE;
     END;

 END PROCESS_PATCH;

 PROCESS_APPEND:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:PROCESS_OPTS                                              *
 ********************************************************************/

 %INCLUDE GETPDSPA;

     DECLARE VALID_MEM_CHARS    CHAR(46)  STATIC INITIAL(
             'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');
     DECLARE SPACE_INDEX         FIXED BINARY ALIGNED;
     DECLARE A_ONE               FIXED BINARY ALIGNED INITIAL(1);
     DECLARE A_SEVEN             FIXED BINARY ALIGNED INITIAL(7);
     DECLARE TEMP_DSN            CHAR(44) VARYING;
     DECLARE MEM                 CHAR(8);

     SPACE_INDEX = INDEX(STMT,' ');
     IF SPACE_INDEX < 9 |
        SPACE_INDEX >= 18 THEN
     DO;
        CALL PRINT_ERR(A_SEVEN,'*** PROGRAM INVALID ***');
        RETURN;
     END;
     ELSE
     DO;
        TEMP_DSN=SUBSTR(STMT,9,SPACE_INDEX-9);
        IF VERIFY(TEMP_DSN,VALID_MEM_CHARS) > 0 THEN
        DO;
           CALL PRINT_ERR
                (A_SEVEN,'*** INVALID PROGRAM NAME ***');
           RETURN;
        END;
        MEM=(8)' ';  /*TEMP_DSN;*/
        CALL DYNALLOC('ALLO',DEFAULT_DSN,MEM);
        IF SVC99_RETURN_CODE > 0 THEN
        DO;
           CALL ANALYZE_DYNALLOC_RC;
           CALL PRINT_ERR(A_SEVEN,
                     '**** CANNOT ALLOCATE THE LIBRARY ***');
           RETURN;
        END;
        PDSGET_REQUEST = PDSGET_REQUEST_OPEN;
        PDSGET_MEMBER  = DD_NAME;
        PDSGET_RECORD80 = (80)' ';
        PDSGET_RETURN_CODE = 0;
        CALL GETPDSP(PDSGET_REQUEST_1,  PDSGET_MEMBER_1,
                     PDSGET_RECORD80_1, PDSGET_RETURN_CODE_1);
        PDSGET_REQUEST = PDSGET_REQUEST_LOCATE;
        MEM  = TEMP_DSN;
        PDSGET_MEMBER  = MEM;
        CALL GETPDSP(PDSGET_REQUEST_1,  PDSGET_MEMBER_1,
                     PDSGET_RECORD80_1, PDSGET_RETURN_CODE_1);
        IF PDSGET_RETURN_CODE > 0 THEN
        DO;
           IF PDSGET_RETURN_CODE = 4 THEN
              CALL PRINT_ERR(A_SEVEN,'**** NOT IN LIBRARY ****');
           ELSE
              CALL PRINT_ERR(A_SEVEN,
                     '**** LIBRARY CANNOT BE ACCESSED ****');
        END;
        ELSE   /*  APPEND THE MEMBER TO THE SOURCE TABLE */
        DO;
           PDSGET_RECORD80='*REM APPENDED CODE';
           DO WHILE(PDSGET_RETURN_CODE=0);
              IF PDSGET_RETURN_CODE = 0 THEN
              DO;
                 SC_MAX=SC_MAX+1;
                 IF SC_MAX>HBOUND(SOURCE_LINE,1) THEN
                 DO;
                    PUT SKIP LIST
                     ('**** FATAL ERROR - PROGRAM TO BIG ***');
                    STOP;
                 END;
                 SOURCE_LINE(SC_MAX)=PDSGET_RECORD80;
                 SOURCE_APPENDED(SC_MAX)=TRUE;
              END;
              PDSGET_REQUEST = PDSGET_REQUEST_READ;
              CALL GETPDSP(PDSGET_REQUEST_1, PDSGET_MEMBER_1,
                        PDSGET_RECORD80_1, PDSGET_RETURN_CODE_1);
           END;
        END;
        PDSGET_REQUEST = PDSGET_REQUEST_CLOSE;
        CALL GETPDSP(PDSGET_REQUEST_1,  PDSGET_MEMBER_1,
                     PDSGET_RECORD80_1, PDSGET_RETURN_CODE_1);
     END;
 END PROCESS_APPEND;

 PROCESS_SAVE:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:PROCESS_OPTS                                              *
 ********************************************************************/

     DECLARE VALID_MEM_CHARS    CHAR(46)  STATIC INITIAL(
             'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789');
     DECLARE SPACE_INDEX         FIXED BINARY ALIGNED;
     DECLARE A_ONE               FIXED BINARY ALIGNED INITIAL(1);
     DECLARE A_SEVEN             FIXED BINARY ALIGNED INITIAL(7);
     DECLARE TEMP_DSN            CHAR(44) VARYING;
     DECLARE MEM                 CHAR(8);

     SPACE_INDEX = INDEX(STMT,' ');
     IF SPACE_INDEX <= 7 |
        SPACE_INDEX > 15 THEN
     DO;
        CALL PRINT_ERR(A_SEVEN,'*** PROGRAM INVALID ***');
        RETURN;
     END;
     ELSE
     DO;
        TEMP_DSN=SUBSTR(STMT,7,SPACE_INDEX-7);
        IF VERIFY(TEMP_DSN,VALID_MEM_CHARS) > 0 THEN
        DO;
           CALL PRINT_ERR
                (A_SEVEN,'*** INVALID PROGRAM NAME ***');
           RETURN;
        END;
        MEM=TEMP_DSN;
        SAVE_CODE=TRUE;
        SAVE_MEMBER_NAME=MEM;
     END;
 END PROCESS_SAVE;

 END PROCESS_OPTS;

 PRINT_PCODES:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

     IF LAST_PCODE_PRINTED < PC_MAX THEN
     DO;
       DO I=LAST_PCODE_PRINTED+1 TO PC_MAX;
          SELECT(PC_FORMAT(PC_OPCODE(I)))
          WHEN(PC_FORMAT_0)
              PUT SKIP EDIT(I,   PC_MNEMONIC(PC_OPCODE(I)),
                                 SYMBOL(PC_OBJECT(I)))
                                (P'999999',X(13),A,X(2),A)
                                ('0') (COL(38),A);
          WHEN(PC_FORMAT_1)
             PUT SKIP EDIT(I,    LS_LINE(PC_OBJECT(I)),
                                 PC_MNEMONIC(PC_OPCODE(I)))
                                (P'999999',X(3),A,X(2),A)
                                ('1') (COL(38),A);
          WHEN(PC_FORMAT_2)
              PUT SKIP EDIT(I,   PC_MNEMONIC(PC_OPCODE(I)),
                                 PC_OBJECT(I))
                                (P'999999',X(13),A,X(2),A)
                                ('2') (COL(38),A);
          WHEN(PC_FORMAT_3)
              PUT SKIP EDIT(I,   PC_MNEMONIC(PC_OPCODE(I)),
                                 STRING_VAL(PC_OBJECT(I)))
                                (P'999999',X(13),A,X(2),A)
                                ('3') (COL(38),A);
          WHEN(PC_FORMAT_4)
              PUT SKIP EDIT(I,   PC_MNEMONIC(PC_OPCODE(I)),
                                 ' ')
                                (P'999999',X(13),A,X(2),A)
                                ('4') (COL(38),A);
          WHEN(PC_FORMAT_5)
              PUT SKIP EDIT(I,   PC_MNEMONIC(PC_OPCODE(I)),
                                 PC_OBJECT(I))
                                (P'999999',X(13),A,X(2),F(5))
                                ('5') (COL(38),A);
          OTHERWISE
             PUT EDIT('**** FATAL ERROR IN COMPILER *****',
                      '**** INVALID VALUE FOR PC_OPCODE ',
                      PC_MNEMONIC(PC_OPCODE(I)))
                      (SKIP(2),A,SKIP,A,A);
             STOP;
          ENDSELECT;
        END;
        LAST_PCODE_PRINTED = PC_MAX;
     END;

 END PRINT_PCODES;

 SKIP_BLANKS:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE CONTINUE_SCAN       BIT(1)    ALIGNED;
    DECLARE I                   FIXED BIN ALIGNED;

    CONTINUE_SCAN=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(CONTINUE_SCAN);
       IF SUBSTR(STMT,I,1)=' ' THEN ;
       ELSE
       DO;
          STMT_CH=I;
          CONTINUE_SCAN=FALSE;
       END;
    END;
    IF CONTINUE_SCAN THEN      /* NO NON BLANK FOUND */
       STMT_CH=STMT_RIGHT+1;

 END SKIP_BLANKS;

 PRINT_ERR:PROC(I,MSG);
 /********************************************************************
 *                                                                   *
 *   PRINTS ALL ERROR MESSAGE FOR THE COMPILE PHASE                  *
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE MSG                 CHAR(*);
    PUT STRING(PRINT_LINE)
             EDIT('*****                   ','^')
                 (A,X(I),A);
    CALL DISPLAY_PRINT_LINE;
    PUT STRING(PRINT_LINE) EDIT(MSG) (X(11),A);
    CALL DISPLAY_PRINT_LINE;
    ERROR_COUNT=ERROR_COUNT+1;
    TERMINATE_SCAN=TRUE;
 END PRINT_ERR;

 LOOKUP_SYMBOL_TABLE:PROC(V) RETURNS(FIXED BINARY);
 /********************************************************************
 *                                                                   *
 *  LOOKS UP SYMBOLS IN THE SYMBOL TABLE.  IF NOT FOUND, IT ADDS     *
 *  IT AND DETERMINE WHAT TYPE OF SYMBOL IT IS.                      *
 *  IF A ZERO IS RETURNED, THERE WAS AN ERROR.  OTHERWISE THE        *
 *  SUBSCRIPT OF THE ITEM "V" IS RETURNED.                           *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

    DECLARE V                    CHAR(10),
            OPT                  FIXED BINARY ALIGNED;
    DECLARE I                    FIXED BINARY ALIGNED;
    DECLARE STR_IND              FIXED BINARY ALIGNED;

    ON CONVERSION
    BEGIN;
       $DEBUG(OFF)
       $DEBUG(LIST,'CONVERSION RAISED FOR V',V);
       $DEBUG(OFF)
       CONTINUE_SCAN=FALSE;
       ONCHAR='0';
    END;

    DO I=1 TO SS_MAX;
       IF SYMBOL(I)=V THEN
       DO;
          IF SYM_TYPE(I)=SS_DIM_VAR &
             WORD=KW_DIM THEN
                CALL PRINT_ERR(I,'CANNOT REDIM VARIABLE');
          RETURN(I);
       END;
    END; /* OF DO */

    IF SS_MAX=HBOUND(SYMBOL,1) THEN
    DO;
       CALL PRINT_ERR(10,'SYMBOL TABLE OVERFLOW');
       RETURN(0);
    END;

    SS_MAX=SS_MAX+1;
    SYMBOL(SS_MAX)=V;
    SYM_DIM_MAX(SS_MAX)=0;
    SYM_DIM2_MAX(SS_MAX)=0;
    STRING_VAL(SS_MAX)='*';
    $DEBUG(OFF)
    $DEBUG(LIST,'V=',V,LENGTH(V))
    $DEBUG(OFF)
    IF SUBSTR(V,1,1) >= 'A' & SUBSTR(V,1,1) <= 'Z' THEN
    DO;
       STR_IND=VERIFY(V,VALID_VAR_CHARS);
       IF STR_IND > 0 THEN
       DO;
          IF SUBSTR(V,STR_IND,1)='$' THEN
          DO;
             SYM_VALUE(SS_MAX)=0.0;
             IF SUBSTR(V,1,4)='STR$' THEN
                SYM_TYPE(SS_MAX)=SS_STRCON;
             ELSE
                IF WORD=KW_DIM THEN
                   SYM_TYPE(SS_MAX)=SS_STRDIM;
                ELSE
                   SYM_TYPE(SS_MAX)=SS_STRVAR;
          END;
          ELSE
          DO;
             CALL PRINT_ERR(I,'INVALID VARIABLE NAME');
             RETURN(0);
          END;
       END;
       ELSE
       DO;
          SYM_VALUE(SS_MAX)=0.0;
          IF WORD=KW_DIM THEN
              SYM_TYPE(SS_MAX)=SS_DIM_VAR;
          ELSE
              SYM_TYPE(SS_MAX)=SS_VAR;
       END;
    END;
    ELSE
    DO;
       IF VERIFY(V,'0123456789+-.E ') > 0 THEN
       DO;
          CALL PRINT_ERR(I,'INVALID CONSTANT '||V);
          RETURN(0);
       END;
       ELSE
       DO;
          SYM_VALUE(SS_MAX)=V;
          SYM_TYPE(SS_MAX)=SS_CONST;
       END;
    END;
    RETURN(SS_MAX);

 END LOOKUP_SYMBOL_TABLE;

 ADD_PCODE:PROC(PCODE,OFFSET);
 /********************************************************************
 *                                                                   *
 *   ADDS CODES TO THE PSEUDO MACHINE CODE TABLE                     *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

    DECLARE PCODE        FIXED BINARY ALIGNED,
            OFFSET       FIXED BINARY ALIGNED;

    IF PC_MAX+1>HBOUND(PC_OPCODE,1) THEN
    DO;
       PUT SKIP(2) EDIT('**** FATAL ERROR-PCODE TABLE OVERFLOW *****')
                       (A);
       STOP;
    END;

    PC_MAX=PC_MAX+1;
    PC_OPCODE(PC_MAX)=PCODE;
    PC_OBJECT(PC_MAX)=OFFSET;

    IF STACK_PRINT_DEBUG | SUBTR_PRINT THEN
       PUT SKIP LIST('****ADD PCODE ', PC_MNEMONIC(PCODE));
          /*****     SYMBOL(OFFSET));                  *DEL*/

 END ADD_PCODE;

 GET_STMT_NUM:PROC(DEFINITION);
 /********************************************************************
 *                                                                   *
 *   EXTRACT THE STATMENT NUMBER.  MAKE SURE IT IS NUMERIC AND IN    *
 *   SEQUENCE.  IF OK, ADD IT TO THE LINE STACK                      *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE DEFINITION          BIT(1) ALIGNED;
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE LN                  CHAR(6) VARYING;
    DECLARE LINE_NUM            FIXED DECIMAL(5,0);

    LN='';
    TMP_CNT=0;
    CONTINUE_SCAN=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(CONTINUE_SCAN);
       CH=SUBSTR(STMT,I,1);
       IF CH=' ' THEN CONTINUE_SCAN=FALSE;
       ELSE
          IF CH < '0' | CH >'9' THEN
          DO;
             CONTINUE_SCAN=FALSE;
             CALL PRINT_ERR(I,'INVALID LINE NUMBER');
          END;
          ELSE
          DO;
             LN=LN||CH;
             IF LENGTH(LN)>5 THEN
             DO;
                CONTINUE_SCAN=FALSE;
                CALL PRINT_ERR(I,'LINE NUMBER TOO LONG');
             END;
          END;
    END;

    IF DEFINITION=TRUE THEN
    DO;
       LINE_NUM=LN;
       IF LINE_NUM=LAST_LINE_NUM THEN
       DO;
          CONTINUE_SCAN=FALSE;
          CALL PRINT_ERR(STMT_CH,'DUPLICATE LINE NUMBER');
       END;
       ELSE
          IF LINE_NUM<LAST_LINE_NUM THEN
          DO;
             CONTINUE_SCAN=FALSE;
             CALL PRINT_ERR(STMT_CH,'LINE NUMBER OUT OF SEQUENCE');
          END;
          ELSE
          DO;
             IF LS_MAX=HBOUND(LS_LINE,1) THEN
                CALL PRINT_ERR(STMT_CH,'TOO MANY LINE NUMBERS');
          END;
       LAST_LINE_NUM=LINE_NUM;
    END;
    ELSE
       REF_LINE_NUM=LN;

    STMT_CH=I;

 END GET_STMT_NUM;

 GET_KEYWORD:PROC;
 /********************************************************************
 *                                                                   *
 *   EXTRACT THE STATMENT KEYWORD AND VALIDATE IT                    *
 *                                                                   *
 *   KEYWORDS SCAN IS ENDED WHEN A COMPLETE KEYWORD IS DETECTED.  A  *
 *   SPACE CAN END A KEYWORD AS WELL.                                *
 *                                                                   *
 *   SPECIAL CASE EXISTS WHEN THE KEYWORD LET IS OMITTED.  IN THIS   *
 *   CASE, AN '=' ENDS THE SEARCH AND THE LET ASSUMED.               *
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE (I,J)               FIXED BINARY ALIGNED;
    DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;
    DECLARE CONTINUE_LOOKUP     BIT(1) ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE KW                  CHAR(11) VARYING;

    CALL SKIP_BLANKS;
    IF STMT_CH=STMT_RIGHT THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'BLANK LINE?');
       RETURN;
    END;

    $DEBUG(OFF)
    KW='';
    CONTINUE_SCAN=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(CONTINUE_SCAN);
       CH=SUBSTR(STMT,I,1);
       IF CH=' ' THEN
       DO;
          IF LENGTH(KW)=2 THEN
          DO;
             IF KW='GO' THEN ;
             ELSE
                CONTINUE_SCAN=FALSE;
          END;
          ELSE
             CONTINUE_SCAN=FALSE;
          DO J=I+1 TO STMT_RIGHT WHILE(CH=' ');
             CH=SUBSTR(STMT,J,1);
          END;
          IF CH='=' THEN           /* IMPLIED LET? */
          DO;
             KW='LET';
             WORD=KW;
             $DEBUG(LIST,WORD,' ASSUMED LET')
             RETURN;
          END;
       END;
       ELSE
       DO;
          IF CH='=' THEN           /* IMPLIED LET? */
          DO;
             KW='LET';
             WORD=KW;
             RETURN;
          END;
          KW=KW||CH;
          IF LENGTH(KW)>9 THEN
          DO;
             CONTINUE_SCAN=FALSE;
             CALL PRINT_ERR(I,'KEYWORD TOO LONG');
          END;
          ELSE
          DO;
             WORD=KW;
             CONTINUE_LOOKUP=TRUE;
             DO J=1 TO HBOUND(KEY_WORDS,1) WHILE(CONTINUE_LOOKUP);
               IF WORD=KEY_WORDS(J) THEN
                  CONTINUE_LOOKUP=FALSE;
             END;
             IF CONTINUE_LOOKUP THEN ;    /* NO KEYWORD FOUND */
             ELSE
             DO;
                $DEBUG(DATA,WORD,CONTINUE_LOOKUP,I)
                CONTINUE_SCAN=FALSE;
             END;
          END;
       END;
    END;
 /* SHORT_CIRCUIT: */
    WORD=KW;
    $DEBUG(DATA,WORD,KW)
    CONTINUE_SCAN=TRUE;
    DO J=1 TO HBOUND(KEY_WORDS,1) WHILE(CONTINUE_SCAN);
       IF WORD=KEY_WORDS(J) THEN
           CONTINUE_SCAN=FALSE;
    END;
    IF CONTINUE_SCAN THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'INVALID KEYWORD');
    END;

    STMT_CH=I;
    $DEBUG(OFF)
 END GET_KEYWORD;

 PROCESS_KEYWORD:PROC;
 /********************************************************************
 *                                                                   *
 *   SYNTAX CHECK AND COMPILE STATEMENTS                             *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE ERR_PTR             FIXED BINARY ALIGNED;
    DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;

    SELECT(WORD)
    WHEN(KW_REM)             /*  REMARKS - NOTHING TO DO! */
    WHEN(KW_END)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
           CALL ADD_PCODE(PC_OPCODE_END,ZERO);
       ELSE
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING BLANKS AFTER END');
    WHEN(KW_EJECT)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
           CALL ADD_PCODE(PC_OPCODE_SKP,ZERO);
       ELSE
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING BLANKS AFTER EJECT');
    WHEN(KW_STOP)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
           CALL ADD_PCODE(PC_OPCODE_STP,ZERO);
       ELSE
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING BLANKS AFTER STOP');
    WHEN(KW_RETURN)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
           CALL ADD_PCODE(PC_OPCODE_RET,ZERO);
       ELSE
           CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING BLANKS AFTER RETURN');
    WHEN(KW_GOTO)
        CALL SKIP_BLANKS;
        IF STMT_CH>STMT_RIGHT THEN
           CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING LINE NUMBER AFTER GOTO');
        ELSE
           CALL PROCESS_GOTO;
    WHEN(KW_GOSUB)
        CALL SKIP_BLANKS;
        IF STMT_CH>STMT_RIGHT THEN
           CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING LINE NUMBER AFTER GOSUB');
        ELSE
           CALL PROCESS_GOSUB;
    WHEN(KW_DATA)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING DATA ELEMENTS');
       ELSE
          CALL EXTRACT_DATA;
    WHEN(KW_LET)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING LET STATEMENT');
       ELSE
          CALL PROCESS_LET;
    WHEN(KW_DEF)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
                         'INVALID SYNTAX - EXPECTING FUNCTION');
       ELSE
          CALL PROCESS_DEF;
    WHEN(KW_READ)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING VARIABLE(S) AFTER READ');
       ELSE
          CALL PROCESS_READ_INPUT(PC_OPCODE_RDV);
    WHEN(KW_PRINT)
       CALL SKIP_BLANKS;
       CALL PROCESS_PRINT;
    WHEN(KW_IF)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - EXPECTING COMPARISON FOR IF');
       ELSE
          CALL PROCESS_IF;
    WHEN(KW_FOR)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
               'INVALID SYNTAX - INCOMPLETE FOR STATEMENT');
       ELSE
          CALL PROCESS_FOR;
    WHEN(KW_NEXT)
       ERR_PTR=STMT_CH;
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(ERR_PTR,
               'INVALID SYNTAX - EXPECTING VARIABLE AFTER NEXT');
       ELSE
          CALL PROCESS_NEXT;
    WHEN(KW_RESTORE)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL ADD_PCODE(PC_OPCODE_RST,ZERO);
       ELSE
          CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING BLANKS AFTER RESTORE');
    WHEN(KW_DIM)
       ERR_PTR=STMT_CH;
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(ERR_PTR,
               'INVALID SYNTAX - EXPECTING DIM VARIABLE');
       ELSE
          CALL PROCESS_DIM;
    WHEN(KW_RANDOMIZE)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL ADD_PCODE(PC_OPCODE_RAN,ZERO);
       ELSE
          CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - EXPECTING BLANKS AFTER RANDOMIZE');
    WHEN(KW_INPUT)
       CALL SKIP_BLANKS;
       IF STMT_CH>STMT_RIGHT THEN
          CALL PRINT_ERR(STMT_CH,
                'INVALID SYNTAX - NOT EXPECTING BLANKS AFTER INPUT');
      ELSE
          CALL PROCESS_READ_INPUT(PC_OPCODE_INP);
    OTHERWISE
       CALL PRINT_ERR(STMT_CH,
                            '***INVALID KEYWORD '|| WORD ||'****');
    ENDSELECT

 END PROCESS_KEYWORD;

 PROCESS_GOTO:PROC;
 /********************************************************************
 *                                                                   *
 *   EXTRACT AND VERIFY LINE NUMBER FROM THE GOTO STATEMENT.         *
 *   IF THE NUMBER IS CLEAN, ADD A PC B  TO THE CODE                 *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

    CALL GET_STMT_NUM(FALSE);
    CALL SKIP_BLANKS;
    IF STMT_CH>STMT_RIGHT THEN
    DO;
       I=REF_LINE_NUM;
       CALL ADD_PCODE(PC_OPCODE_B,I);
    END;
    ELSE
       CALL PRINT_ERR(STMT_CH,
           'INVALID SYNTAX - EXPECTING BLANKS AFTER LINE');

 END PROCESS_GOTO;

 PROCESS_GOSUB:PROC;
 /********************************************************************
 *                                                                   *
 *   EXTRACT AND VERIFY LINE NUMBER FROM THE GOSUB STATEMENT.        *
 *   IF THE NUMBER IS CLEAN, ADD A PC BAL TO THE CODE                *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

    CALL GET_STMT_NUM(FALSE);
    CALL SKIP_BLANKS;
    IF STMT_CH>STMT_RIGHT THEN
    DO;
       I=REF_LINE_NUM;
       CALL ADD_PCODE(PC_OPCODE_BAL,I);
    END;
    ELSE
       CALL PRINT_ERR(STMT_CH,
           'INVALID SYNTAX - EXPECTING BLANKS AFTER LINE');

 END PROCESS_GOSUB;

 EXTRACT_DATA:PROC;
 /********************************************************************
 *                                                                   *
 *   EXTRACT AND VERIFY NUMBERS FROM THE DATA STATEMENTS.            *
 *   IF THE NUMBER IS CLEAN, ADD IT TO THE DATA STACK                *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE VAL                 CHAR(80) VARYING;
    DECLARE HOLD_VAL            CHAR(80) VARYING;
    DECLARE NUM_VAL             FLOAT BINARY;
    DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;
    DECLARE IN_STR              BIT(1) ALIGNED;

    ON CONVERSION
    BEGIN;
       CONTINUE_SCAN=FALSE;
       ONCHAR='0';
    END;
    CONTINUE_SCAN=TRUE;
    IN_STR=FALSE;
    VAL='';
    DO I=STMT_CH TO STMT_RIGHT;
       CH=SUBSTR(STMT,I,1);
       SELECT(TRUE)
       WHEN(IN_STR)
          VAL=VAL||CH;
          IF CH=QUOTE_1 THEN
             IN_STR=FALSE;
       WHEN(CH=QUOTE_1)
          VAL=VAL||CH;
          IN_STR=TRUE;
       WHEN(CH=' ')
       WHEN(CH=',')
          CALL EXTRACT_DATA_ITEM;
          CONTINUE_SCAN=TRUE;
          VAL='';
       OTHERWISE
          VAL=VAL||CH;
       ENDSELECT
    END;
    CALL EXTRACT_DATA_ITEM;
 EXTRACT_DATA_ITEM:PROC;
    DECLARE TMP_VAR              CHAR(10);
    DECLARE OFFSET               FIXED BINARY ALIGNED;
    OFFSET=0;
    NUM_VAL=0.0;
    HOLD_VAL=VAL;
    IF SUBSTR(VAL,1,1)=QUOTE_1 THEN
    DO;
       IF SUBSTR(VAL,LENGTH(VAL),1)=QUOTE_1 THEN
       DO;
          TMP_VAR='STR$'||STR_CNT;
          STR_CNT=STR_CNT+1;
          OFFSET=LOOKUP_SYMBOL_TABLE(TMP_VAR);
          IF STACK_PRINT_DEBUG THEN
             PUT DATA(VAL,TMP_VAR,OFFSET);
          IF LENGTH(VAL)>2 THEN
             STRING_VAL(OFFSET)=SUBSTR(VAL,2,LENGTH(VAL)-2);
          ELSE
             STRING_VAL(OFFSET)='';
       END;
       ELSE
       DO;
          CONTINUE_SCAN=FALSE;
       END;
    END;
    ELSE
    DO;
       IF VERIFY(V,'0123456789+-.E ') > 0 THEN
          CONTINUE_SCAN=FALSE;
       ELSE
          NUM_VAL=VAL;
    END;
    IF CONTINUE_SCAN=FALSE THEN
        CALL PRINT_ERR(I,'ILLEGAL CONSTANT IN DATA STATEMENT '
                                             || HOLD_VAL);
    ELSE
    DO;
        IF DS_MAX=HBOUND(DS_ITEM,1) THEN
           CALL PRINT_ERR(I,'DATA STACK FULL');
        ELSE
        DO;
           DS_MAX=DS_MAX+1;
           DS_STR(DS_MAX)=OFFSET;
           DS_ITEM(DS_MAX)=NUM_VAL;
        END;
    END;
 END EXTRACT_DATA_ITEM;
 END EXTRACT_DATA;

 PROCESS_READ_INPUT:PROC(RI_OPCODE);
 /********************************************************************
 *                                                                   *
 *   PROCESS READ AND INPUT                                          *
 *   SINCE THE SYNTAX FOR READ AND INPUT ARE THE SAME, COMMON CODE   *
 *   IS USED TO COMPILE THE TWO STATEMENTS.                          *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE RI_OPCODE           FIXED BINARY ALIGNED;
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE PAREN_CTR           FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE VAR                 CHAR(10);
    DECLARE LEFT_SIDE           CHAR(80) VARYING;
    DECLARE NO_COMMA            BIT(1) ALIGNED;
    DECLARE IN_SUB              BIT(1) ALIGNED;

  /*  EXTRACT THE RECEIVING FIELDS */

    LEFT_SIDE='';
    NO_COMMA=TRUE;
    IN_SUB=FALSE;
    PAREN_CTR=0;
    DO I=STMT_CH TO STMT_RIGHT;
      CH=SUBSTR(STMT,I,1);
      IF CH='(' THEN
      DO;
         PAREN_CTR=PAREN_CTR+1;
         LEFT_SIDE=LEFT_SIDE||CH;
      END;
      ELSE
      IF CH=')' THEN
      DO;
         PAREN_CTR=PAREN_CTR-1;
         LEFT_SIDE=LEFT_SIDE||CH;
      END;
      ELSE
      IF CH=',' & PAREN_CTR=0 THEN
      DO;
         NO_COMMA=FALSE;
         CALL PROCESS_READ_VAR;
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            LEFT_SIDE=LEFT_SIDE||CH;
    END;
    IF LENGTH(LEFT_SIDE)>0 THEN
        CALL PROCESS_READ_VAR;

 PROCESS_READ_VAR:PROC;

     /** IF LENGTH(LEFT_SIDE)>10 THEN
             CALL PRINT_ERR(STMT_CH,'VARIABLE TOO LONG');
         ELSE
     **/ DO;
             CALL BALANCE_STMT(LEFT_SIDE);
             IF TERMINATE_SCAN THEN RETURN;
             LEFT_SIDE='('||LEFT_SIDE||')';
             CALL PARSE_EXP(LEFT_SIDE,EXP_CALC);
             IF PC_MAX > 2 THEN
             DO;
                IF PC_OPCODE(PC_MAX-2)=PC_OPCODE_LDA THEN
                DO;
                   PC_MAX=PC_MAX-2;      /* DELETE 2 NOISE LDA/STA */
                   PC_OPCODE(PC_MAX)=RI_OPCODE;
                   GO TO EXIT_PROCESS_READ_VAR;
                END;
                IF PC_OPCODE(PC_MAX-2)=PC_OPCODE_DSL THEN
                DO;
                   PC_MAX=PC_MAX-1;      /* DELETE 1 NOISE LDA/STA */
                   PC_OPCODE(PC_MAX)=RI_OPCODE;
                   GO TO EXIT_PROCESS_READ_VAR;
                END;
                IF PC_OPCODE(PC_MAX-1)=PC_OPCODE_DSL THEN
                DO;
                   PC_OPCODE(PC_MAX)=RI_OPCODE;
                   GO TO EXIT_PROCESS_READ_VAR;
                END;
             END;
             IF PC_MAX > 0 THEN  /*  ERROR IF A TMP IS FOUND */
             DO;
             IF PC_OPCODE(PC_MAX)=PC_OPCODE_STA THEN
                IF SUBSTR(SYMBOL(PC_OBJECT(PC_MAX)),1,3)='TMP' THEN
                   CALL PRINT_ERR(STMT_CH,'EXPRESSION NOT ALLOWED');
             IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
                IF SUBSTR(SYMBOL(PC_OBJECT(PC_MAX)),1,3)='TMP' THEN
                   CALL PRINT_ERR(STMT_CH,'EXPRESSION NOT ALLOWED');
                ELSE
                   PC_OPCODE(PC_MAX)=RI_OPCODE;
             END;
         END;
 EXIT_PROCESS_READ_VAR:
         LEFT_SIDE='';

 END PROCESS_READ_VAR;
 END PROCESS_READ_INPUT;

 PROCESS_PRINT:PROC;

 /********************************************************************
 *                                                                   *
 *   PROCESS PRINT                                                   *
 *                                                                   *
 *   PARSE THE STATEMENT INTO OBJECTS - NUMERIC EXPRESSION OR        *
 *   STRING LITTERALS.                                               *
 *   ALSO DECIDES IF A LINE FEED SHOULD BE ISSUED OR NOT BASED ON    *
 *   ON DANGLING COMMA                                               *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE (I,LAST_NB)         FIXED BINARY ALIGNED;
    DECLARE PAREN_CTR           FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE VAR                 CHAR(10);
    DECLARE LEFT_SIDE           CHAR(80) VARYING;
    DECLARE NO_COMMA            BIT(1) ALIGNED;
    DECLARE IN_STR              BIT(1) ALIGNED;
    DECLARE IN_SUB              BIT(1) ALIGNED;

  /*  EXTRACT THE PRINT OBJECT */

    LEFT_SIDE='';
    LAST_NB=0;
    NO_COMMA=TRUE;
    DANGLE=FALSE;
    IN_SUB,IN_STR=FALSE;
    PAREN_CTR=0;

    IF STMT_CH>STMT_RIGHT THEN     /* PRINT A LINE FEED FOR */
    DO;                            /* KEYWORD PRINT ONLY */
       I=1;
       CALL ADD_PCODE(PC_OPCODE_PCT,PCT_LFEED);
       RETURN;
    END;

    IF SUBSTR(STMT,STMT_CH,6)='USING ' THEN
    DO;
       CALL PROCESS_PRINT_USING;
    /* RETURN; */
    END;

    I=STMT_CH;
    $DEBUG(OFF)
    DO WHILE(I <= STMT_RIGHT);
      CH=SUBSTR(STMT,I,1);
      $DEBUG(DATA,CH,IN_STR,LEFT_SIDE)
      IF IN_STR THEN
      DO;
         IF CH=QUOTE_1 THEN
            IN_STR=FALSE;
         LEFT_SIDE=LEFT_SIDE||CH;
      END;
      ELSE
      DO;
         IF CH=QUOTE_1 THEN
         DO;
            IN_STR=TRUE;
            LEFT_SIDE=LEFT_SIDE||CH;
         END;
         IF CH='(' THEN
         DO;
            PAREN_CTR=PAREN_CTR+1;
       /*   LEFT_SIDE=LEFT_SIDE||CH;   */
         END;
         ELSE
            IF CH=')' THEN
            DO;
               PAREN_CTR=PAREN_CTR-1;
        /*     LEFT_SIDE=LEFT_SIDE||CH;  */
            END;
      END;

      IF IN_STR THEN;
      ELSE
      DO;
         IF CH=' ' THEN ;
         ELSE LAST_NB=I;
         IF PAREN_CTR=0 & (CH=',' | CH=';') THEN
         DO;
            NO_COMMA=FALSE;
            IF SUBSTR(LEFT_SIDE,1,1)=QUOTE_1 THEN
                CALL PROCESS_PRINT_STR;
            ELSE
                CALL PROCESS_PRINT_VAR;
            IF CH=',' THEN
                CALL ADD_PCODE(PC_OPCODE_PCT,PCT_TAB);
            ELSE
                CALL ADD_PCODE(PC_OPCODE_PCT,PCT_NOTAB);
         END;
         ELSE
            IF CH=' ' | CH=QUOTE_1 THEN;
            ELSE
               LEFT_SIDE=LEFT_SIDE||CH;
      END;
    I=I+1;
    END;
    $DEBUG(DATA,LEFT_SIDE)
    $DEBUG(OFF)
    IF LENGTH(LEFT_SIDE)>0 THEN
        IF SUBSTR(LEFT_SIDE,1,1)=QUOTE_1 THEN
            CALL PROCESS_PRINT_STR;
        ELSE
            CALL PROCESS_PRINT_VAR;

    IF LAST_NB > 0 THEN
    DO;
       IF SUBSTR(STMT,LAST_NB,1)=',' |
          SUBSTR(STMT,LAST_NB,1)=';' THEN;
       ELSE
          CALL ADD_PCODE(PC_OPCODE_PCT,PCT_LFEED);
    END;
    ELSE
          CALL PRINT_ERR(STMT_CH,'INVALID PRINT SYNTAX');
    IF PRINT_USING_MODE THEN
    DO;
       CALL ADD_PCODE(PC_OPCODE_PUE,0);
       PRINT_USING_MODE=FALSE;
    END;

 PROCESS_PRINT_USING:PROC;

    PRINT_USING_MODE=TRUE;
    CALL ADD_PCODE(PC_OPCODE_PUS,0);
    STMT_CH=STMT_CH+5;
    CALL SKIP_BLANKS;
    IF STMT_CH>STMT_RIGHT THEN     /* USING MUST BE FOLLOWED */
    DO;                            /* BY A STRING AND A VAR  */
       CALL PRINT_ERR(STMT_CH,'A STRING MUST FOLLOW USING');
       RETURN;
    END;

 END PROCESS_PRINT_USING;

 PROCESS_PRINT_VAR:PROC;
    DECLARE TAB_TEST  BIT(1) ALIGNED;
    CALL BALANCE_STMT(LEFT_SIDE);
    IF TERMINATE_SCAN THEN
        RETURN;

    LEFT_SIDE='('||LEFT_SIDE||')';
    CALL PARSE_EXP(LEFT_SIDE,EXP_CALC);
    $DEBUG(OFF)
    IF PC_MAX > 0 THEN
    DO;
       $DEBUG(LIST,'***ENTER PRINT_VAR',PC_MAX);
       IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
       DO;
          IF PC_OPCODE(PC_MAX-2)=PC_OPCODE_DSL THEN
          DO;
             PC_MAX=PC_MAX-1;
             PC_OPCODE(PC_MAX)=PC_OPCODE_PRV;
          END;
          ELSE
          IF PC_MAX > 2 &
             PC_FORMAT(PC_OPCODE(PC_MAX-2))=PC_FORMAT_0 THEN
          DO;
             TAB_TEST=(PC_MAX > 2);
             IF TAB_TEST THEN
                TAB_TEST=(PC_OBJECT(PC_MAX-2) > 0);
             IF TAB_TEST THEN
                TAB_TEST=(SYMBOL(PC_OBJECT(PC_MAX-2))='TAB       ');
             IF TAB_TEST THEN
                  PC_OPCODE(PC_MAX)=PC_OPCODE_PTB;
             ELSE
                  PC_OPCODE(PC_MAX)=PC_OPCODE_PRV;
          END;
          ELSE
               PC_OPCODE(PC_MAX)=PC_OPCODE_PRV;
       END;
       IF PC_OPCODE(PC_MAX)=PC_OPCODE_STA THEN
       DO;
          $DEBUG(LIST,'****STA OPTION')
          $DEBUG(DATA,PC_MAX)
          $DEBUG(DATA,SYMBOL(PC_OBJECT(PC_MAX)),
                      SYMBOL(PC_OBJECT(PC_MAX-2)),
                      SYMBOL(PC_OBJECT(PC_MAX-1)))
          $DEBUG(DATA,PC_OPCODE(PC_MAX-1),PC_OPCODE_DSL);
          IF PC_OPCODE(PC_MAX-1)=PC_OPCODE_DSL  &
             PC_OBJECT(PC_MAX-1)=PC_OBJECT(PC_MAX) THEN
          DO;
             $DEBUG(LIST,'**** CHANGES STA TO PRV');
             PC_OPCODE(PC_MAX)=PC_OPCODE_PRV;
          END;
          ELSE
          DO;
             IF PC_MAX > 2 THEN
             DO;
         /***   IF PC_FORMAT(PC_OBJECT(PC_MAX-2))=PC_FORMAT_0 THEN
                DO; ***/
                   IF SYMBOL(PC_OBJECT(PC_MAX-1))='TAB       ' THEN;
                   ELSE
                   DO;
                      $DEBUG(LIST,'**** ADDS PRV');
                      CALL ADD_PCODE(PC_OPCODE_PRV,PC_OBJECT(PC_MAX));
                   END;
         /****  END;
                ELSE
                DO;
                     $DEBUG(LIST,'**** UNSURE PRV') ;
                      CALL ADD_PCODE(PC_OPCODE_PRV,PC_OBJECT(PC_MAX));
                END; ***/
             END;
             ELSE
             DO;
                $DEBUG(LIST,'**** LAST RESORT ADDS PRV',PC_MAX);
                CALL ADD_PCODE(PC_OPCODE_PRV,PC_OBJECT(PC_MAX));
             END;
          END;
       END;
       $DEBUG(LIST,'***EXIT PRINT_VAR',PC_MAX);
    END;
    $DEBUG(OFF)

    LEFT_SIDE='';

 END PROCESS_PRINT_VAR;

 PROCESS_PRINT_STR:PROC;

    DECLARE  (I,TICS)             FIXED BINARY ALIGNED;
    TICS=0;
    DO I = 1 TO LENGTH(LEFT_SIDE);
        IF SUBSTR(LEFT_SIDE,I,1)=QUOTE_1 THEN
            TICS=TICS+1;
    END;
    IF MOD(TICS,2)=1 THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'UNBALANCED STRING');
       RETURN;
    END;

    IF SS_MAX>=HBOUND(STRING_VAL,1) THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'STRING CONSTANT TABLE FULL');
       RETURN;
    END;
                   /*  STRIP OFF THE QUOTE MARKS AND REDUCE
                       DOUBLE QUOTES TO 1 QUOTE BEFORE SAVING */

    LEFT_SIDE=SUBSTR(LEFT_SIDE,2,LENGTH(LEFT_SIDE)-2);
    I = 1;
    DO WHILE(I<LENGTH(LEFT_SIDE));
       IF SUBSTR(LEFT_SIDE,I,2)=QUOTE_2 THEN
          LEFT_SIDE=SUBSTR(LEFT_SIDE,1,I)||SUBSTR(LEFT_SIDE,I+2);
       I=I+1;
    END;
    SS_CUR,SS_MAX=SS_MAX+1;
    SYMBOL(SS_CUR)='_PRS';
    STRING_VAL(SS_CUR)=LEFT_SIDE;
    SYM_TYPE(SS_CUR)=SS_STRCON;
    CALL ADD_PCODE(PC_OPCODE_PRS,SS_CUR);
    LEFT_SIDE='';

 END PROCESS_PRINT_STR;

 END PROCESS_PRINT;

 PROCESS_IF:PROC;
 /********************************************************************
 *                                                                   *
 *   PROCESS IF                                                     *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE (LEFT_SIDE,RIGHT_SIDE)
                                CHAR(80) VARYING;
    DECLARE NO_OPER             BIT(1) ALIGNED;
    DECLARE OPER                CHAR(2);
    DECLARE THEN_WORD           CHAR(4);

  /*  EXTRACT THE LEFT FIELD */

    LEFT_SIDE='';
    NO_OPER=TRUE;
    OPER='..';
    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      IF CH='=' |
         CH='<' |
         CH='>' THEN
      DO;
         NO_OPER=FALSE;
         OPER=CH;
         RIGHT_SIDE=SUBSTR(STMT,I+1);
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            LEFT_SIDE=LEFT_SIDE||CH;
    END;

    STMT_CH=I;
    CH=SUBSTR(STMT,I,1);
    IF (OPER='= ' & CH='>') | (OPER='> ' & CH='=') THEN
    DO;
       OPER='>=';
       STMT_CH=I+1;
    END;
    ELSE
    IF (OPER='= ' & CH='<') | (OPER='< ' & CH='=') THEN
    DO;
       OPER='<=';
       STMT_CH=I+1;
    END;
    ELSE
    IF (OPER='< ' & CH='>') THEN
    DO;
       OPER='<>';
       STMT_CH=I+1;
    END;
    ELSE
    IF OPER='= ' | OPER='< ' | OPER='> ' THEN;
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'NO COMPARISON OPERATOR FOUND');
       RETURN;
    END;

  /*  EXTRACT THE RIGHT FIELD */

    RIGHT_SIDE='';
    NO_OPER=TRUE;
    DO I=STMT_CH TO STMT_RIGHT-3 WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      THEN_WORD=SUBSTR(STMT,I,4);
      IF THEN_WORD='THEN' THEN
      DO;
         NO_OPER=FALSE;
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            RIGHT_SIDE=RIGHT_SIDE||CH;
    END;

    IF NO_OPER=TRUE THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'THEN NOT FOUND');
       RETURN;
    END;

    STMT_CH=I+4;

    CALL SKIP_BLANKS;
    IF STMT_CH>STMT_RIGHT THEN
      CALL PRINT_ERR(STMT_CH,
                       'INVALID SYNTAX - EXPECTING LINE NUMBER');
    ELSE
    DO;
       CALL GET_STMT_NUM(FALSE);

       CALL SKIP_BLANKS;

       CALL BALANCE_STMT(LEFT_SIDE);
       IF TERMINATE_SCAN THEN
          RETURN;
       LEFT_SIDE='('||LEFT_SIDE||')';
       CALL PARSE_EXP(LEFT_SIDE,EXP_CALC);
       IF PC_MAX > 0 THEN
       DO;
          CALL IF_PATCH(PC_OPCODE_LCA);
       END;
       CALL BALANCE_STMT(RIGHT_SIDE);
       IF TERMINATE_SCAN THEN RETURN;
       LEFT_SIDE='('||RIGHT_SIDE||')';
       CALL PARSE_EXP(LEFT_SIDE,EXP_CALC);
       IF PC_MAX > 0 THEN  /*  ERROR IF A TMP IS FOUND */
       DO;
          CALL IF_PATCH(PC_OPCODE_LCB);
       END;
       I=REF_LINE_NUM;
       SELECT(OPER)
          WHEN('= ')
             CALL ADD_PCODE(PC_OPCODE_BEQ,I);
          WHEN('<>')
             CALL ADD_PCODE(PC_OPCODE_BNE,I);
          WHEN('< ')
             CALL ADD_PCODE(PC_OPCODE_BLT,I);
          WHEN('> ')
             CALL ADD_PCODE(PC_OPCODE_BGT,I);
          WHEN('<=')
             CALL ADD_PCODE(PC_OPCODE_BLE,I);
          WHEN('>=')
             CALL ADD_PCODE(PC_OPCODE_BGE,I);
       OTHERWISE
          TERMINATE_SCAN=TRUE;
          PUT SKIP EDIT('**** INTERNAL COMPILER ERROR IF-01')
                         (A);
       ENDSELECT

    END;

 IF_PATCH:PROC(LCX);
     DECLARE LCX  FIXED BINARY ALIGNED;

          IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA &
             PC_OPCODE(PC_MAX-1)=PC_OPCODE_STA &
             PC_OPCODE(PC_MAX-2)=PC_OPCODE_DSL THEN
          DO;
             PC_OPCODE(PC_MAX-1)=LCX;
             PC_MAX=PC_MAX-1;
          END;
          ELSE
          IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA |
             PC_OPCODE(PC_MAX-1)=PC_OPCODE_DSL THEN
             PC_OPCODE(PC_MAX)=LCX;
 END IF_PATCH;

 END PROCESS_IF;

 PROCESS_FOR:PROC;
 /********************************************************************
 *                                                                   *
 *   PROCESS FOR                                                     *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE OFFSET              FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE (LEFT_SIDE,START_VAL,TO_VAL,STEP_VAL)
                                CHAR(80) VARYING;
    DECLARE CTL_VAR             CHAR(10);
    DECLARE NO_OPER             BIT(1) ALIGNED;
    DECLARE STEP_WORD           CHAR(4);
    DECLARE TO_WORD             CHAR(2);

  /*  EXTRACT THE CONTROL VARIABLE */

    LEFT_SIDE,START_VAL,TO_VAL,STEP_VAL='';
    NO_OPER=TRUE;

    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      IF CH='=' THEN
      DO;
         NO_OPER=FALSE;
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            LEFT_SIDE=LEFT_SIDE||CH;
    END;

    STMT_CH=I;

  /*  EXTRACT THE STARTING VALUE */

    NO_OPER=TRUE;
    DO I=STMT_CH TO STMT_RIGHT-1 WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      TO_WORD=SUBSTR(STMT,I,2);
      IF TO_WORD='TO' THEN
      DO;
         NO_OPER=FALSE;
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            START_VAL=START_VAL||CH;
    END;

    IF NO_OPER=TRUE THEN
    DO;
       CALL PRINT_ERR(STMT_CH,'TO NOT FOUND');
       RETURN;
    END;

    STMT_CH=I+2;

  /*  EXTRACT THE TO VALUE */

    NO_OPER=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      STEP_WORD=SUBSTR(STMT,I,4);
      IF STEP_WORD='STEP' THEN
      DO;
         NO_OPER=FALSE;
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            TO_VAL=TO_VAL||CH;
    END;

    IF NO_OPER=FALSE THEN
    DO;
       NO_OPER=TRUE;
       STMT_CH=I+4;
       DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
         CH=SUBSTR(STMT,I,1);
         IF CH=' ' THEN;
         ELSE
            STEP_VAL=STEP_VAL||CH;
       END;
    END;
    ELSE
       STEP_VAL='1';

    IF STMT_CH>STMT_RIGHT THEN
    DO;
      CALL PRINT_ERR(STMT_CH,
                       'INVALID SYNTAX - EXPECTING BLANKS');
      RETURN;
    END;

    CTL_VAR=LEFT_SIDE;
    OFFSET=LOOKUP_SYMBOL_TABLE(CTL_VAR);
    IF SYM_TYPE(OFFSET)=SS_VAR THEN
        CALL ADD_PCODE(PC_OPCODE_FSU,OFFSET);
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'SIMPLE VARIABLE EXPECTED NOT '||
                              CTL_VAR);
       RETURN;
    END;

    CALL BALANCE_STMT(START_VAL);
    IF TERMINATE_SCAN THEN RETURN;
    START_VAL='('||START_VAL||')';
    CALL PARSE_EXP(START_VAL,EXP_CALC);

    IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
       PC_OPCODE(PC_MAX)=PC_OPCODE_FIX;
    ELSE
       CALL ADD_PCODE(PC_OPCODE_FIX,OFFSET);

    CALL BALANCE_STMT(TO_VAL);
    IF TERMINATE_SCAN THEN RETURN;
    TO_VAL='('||TO_VAL||')';
    CALL PARSE_EXP(TO_VAL,EXP_CALC);

    IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
       PC_OPCODE(PC_MAX)=PC_OPCODE_FUL;
    ELSE
       CALL ADD_PCODE(PC_OPCODE_FUL,OFFSET);

    CALL BALANCE_STMT(STEP_VAL);
    IF TERMINATE_SCAN THEN RETURN;
    STEP_VAL='('||STEP_VAL||')';
    CALL PARSE_EXP(STEP_VAL,EXP_CALC);

    IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
       PC_OPCODE(PC_MAX)=PC_OPCODE_FST;
    ELSE
       CALL ADD_PCODE(PC_OPCODE_FST,OFFSET);

 END PROCESS_FOR;

 PROCESS_NEXT:PROC;
 /********************************************************************
 *                                                                   *
 *   PROCESS NEXT                                                    *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE ERR_PTR             FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE (LEFT_SIDE,START_VAL,TO_VAL,STEP_VAL)
                                CHAR(80) VARYING;
    DECLARE NO_OPER             BIT(1) ALIGNED;
    DECLARE OFFSET              FIXED BINARY ALIGNED;
    DECLARE CTL_VAR             CHAR(10);

  /*  EXTRACT THE CONTROL VARIABLE */

    LEFT_SIDE='';
    NO_OPER=TRUE;

    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      IF CH=' ' THEN
         NO_OPER=FALSE;
      ELSE
         LEFT_SIDE=LEFT_SIDE||CH;
    END;

    ERR_PTR=STMT_CH;
    STMT_CH=I;

    CALL SKIP_BLANKS;
    IF STMT_CH>STMT_RIGHT THEN;
    ELSE
    DO;
      CALL PRINT_ERR(ERR_PTR,
                       'INVALID SYNTAX - EXPECTING BLANKS');
      RETURN;
    END;
    CTL_VAR=LEFT_SIDE;
    OFFSET=LOOKUP_SYMBOL_TABLE(CTL_VAR);
    IF SYM_TYPE(OFFSET)=SS_VAR THEN
        CALL ADD_PCODE(PC_OPCODE_FNX,OFFSET);
    ELSE
        CALL PRINT_ERR(ERR_PTR,'SIMPLE VARIABLE EXPECTED');

 END PROCESS_NEXT;

 PROCESS_LET:PROC;
 /********************************************************************
 *                                                                   *
 *   PROCESS LET                                                     *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE NO_BLANK_FOUND      FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE (LEFT_SIDE,RIGHT_SIDE)
                                CHAR(80) VARYING;

    CH=SUBSTR(STMT,STMT_CH,1);
    IF CH<'A' | CH>'Z' THEN
       CALL PRINT_ERR(STMT_CH,'EXPECTING VARIABLE');
    IF TERMINATE_SCAN THEN RETURN;

  /*  EXTRACT THE RECEIVING FIELD */

    LEFT_SIDE='';
    NO_EQUAL=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_EQUAL);
      CH=SUBSTR(STMT,I,1);
      IF CH='=' THEN
      DO;
         NO_EQUAL=FALSE;
         RIGHT_SIDE=SUBSTR(STMT,I+1,STMT_RIGHT-I);
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            LEFT_SIDE=LEFT_SIDE||CH;
    END;
    NO_BLANK_FOUND=0;
    DO I=1 TO LENGTH(RIGHT_SIDE) WHILE(NO_BLANK_FOUND=0);
       IF (SUBSTR(RIGHT_SIDE,I,1)=' ') THEN;
       ELSE
          NO_BLANK_FOUND = I;
    END;
    IF NO_BLANK_FOUND > 1 THEN
        RIGHT_SIDE = SUBSTR(RIGHT_SIDE,NO_BLANK_FOUND);
    NO_BLANK_FOUND=0;
    DO I=LENGTH(RIGHT_SIDE) TO 1 BY -1 WHILE(NO_BLANK_FOUND=0);
       IF (SUBSTR(RIGHT_SIDE,I,1)=' ') THEN;
       ELSE
          NO_BLANK_FOUND = I;
    END;
    IF NO_BLANK_FOUND > 0 THEN
        RIGHT_SIDE = SUBSTR(RIGHT_SIDE,1,NO_BLANK_FOUND);
    $DEBUG(OFF)
    $DEBUG(DATA,LEFT_SIDE,RIGHT_SIDE,NO_BLANK_FOUND);
    $DEBUG(OFF)
    CALL BALANCE_STMT(LEFT_SIDE);
    IF TERMINATE_SCAN THEN RETURN;
    CALL BALANCE_STMT(RIGHT_SIDE);
    IF TERMINATE_SCAN THEN RETURN;

    RIGHT_SIDE='('||RIGHT_SIDE||')';

    CALL PARSE_EXP(RIGHT_SIDE,EXP_CALC);

    IF PC_MAX > 0 THEN        /*  CHANGE A STA TMPXX TO STA RESULT */
    DO;
   /***IF PC_OPCODE(PC_MAX)=PC_OPCODE_LDA THEN
    *      IF SUBSTR(SYMBOL(PC_OBJECT(PC_MAX)),1,3)='TMP' THEN
    **        PC_OPCODE(PC_MAX)=PC_OPCODE_STA;                *DEL*/
    END;

    LEFT_SIDE='('||LEFT_SIDE||')';
    CALL PARSE_EXP(LEFT_SIDE,EXP_RCVR);

 END PROCESS_LET;
 PROCESS_DEF:PROC;
 /********************************************************************
 *                                                                   *
 *   PROCESS DEF                                                     *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE JMP_OFFSET          FIXED BINARY ALIGNED;
    DECLARE (OFFSET,OFFSET2)    FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE CH2                 CHAR(2);
    DECLARE (LEFT_SIDE,RIGHT_SIDE,FUNC_TEMP)
                                CHAR(80) VARYING;
    DECLARE TEMP_NAME           CHAR(10);
    DECLARE NO_EQUAL            BIT(1) ALIGNED;

    TMP_CNT=50;

    CH2=SUBSTR(STMT,STMT_CH,2);
    IF CH2='FN' THEN;
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'DEF MUST START WITH FN');
       RETURN;
    END;

  /*  EXTRACT THE FUNCTION NAME   */

    LEFT_SIDE='';
    NO_EQUAL=TRUE;
    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_EQUAL);
      CH=SUBSTR(STMT,I,1);
      IF CH='=' THEN
      DO;
         NO_EQUAL=FALSE;
         RIGHT_SIDE=SUBSTR(STMT,I+1);
      END;
      ELSE
         IF CH=' ' THEN;
         ELSE
            LEFT_SIDE=LEFT_SIDE||CH;
    END;

    CALL BALANCE_STMT(LEFT_SIDE);
    IF TERMINATE_SCAN THEN RETURN;

    I=INDEX(LEFT_SIDE,'(');
    IF I>0 THEN
    DO;
       FUNC_TEMP=SUBSTR(LEFT_SIDE,1,I-1);
       FUNC_NAME=FUNC_TEMP;
       LEFT_SIDE=SUBSTR(LEFT_SIDE,I+1);
       IF VERIFY(FUNC_NAME,VALID_VAR_CHARS) > 0 THEN
       DO;
          CALL PRINT_ERR(I,'INVALID FUNCTION NAME');
          RETURN;
       END;
    END;
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'DEF SYNTAX ERROR');
       RETURN;
    END;
    IF SUBSTR(LEFT_SIDE,LENGTH(LEFT_SIDE),1)=')' THEN
    DO;
       FUNC_ARG=SUBSTR(LEFT_SIDE,1,LENGTH(LEFT_SIDE)-1);
       IF VERIFY(FUNC_ARG,VALID_VAR_CHARS) > 0 THEN
       DO;
          CALL PRINT_ERR(I,'INVALID FUNCTION ARGUMENT');
          RETURN;
       END;
       FUNC_TEMP=FUNC_TEMP||' '||FUNC_ARG;
    END;
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'DEF ARGUMENT ERROR');
       RETURN;
    END;

    OFFSET=LOOKUP_SYMBOL_TABLE(FUNC_NAME);
    IF OFFSET=SS_MAX THEN
       IF SYM_TYPE(OFFSET)=SS_VAR THEN
       DO;
           SYM_TYPE(OFFSET)=SS_DEF_VAR;
           CALL ADD_PCODE(PC_OPCODE_JMP,ZERO);
           JMP_OFFSET=PC_MAX;
           TEMP_NAME=FUNC_TEMP;
           OFFSET2=LOOKUP_SYMBOL_TABLE(FUNC_TEMP);
           CALL ADD_PCODE(PC_OPCODE_STA,OFFSET2);
           IF OFFSET+1=OFFSET2 THEN;
           ELSE
           DO;
              CALL PRINT_ERR(ERR_PTR,'FUNC/ARG NOT CONTIG');
              RETURN;
           END;
           IF DF_MAX>=HBOUND(DF_NAME,1) THEN
           DO;
              CALL PRINT_ERR(ERR_PTR,'TOO MANY DEF');
              RETURN;
           END;
           DF_MAX=DF_MAX+1;
           DF_NAME(DF_MAX)=FUNC_NAME;
           DF_OFFSET(DF_MAX)=PC_MAX;
           DF_RETURN(DF_MAX)=0;
       END;
       ELSE
           CALL PRINT_ERR(ERR_PTR,'DEF SYMBOL NOT FOUND');
    ELSE
        CALL PRINT_ERR(ERR_PTR,'DEF SYMBOL REDEFINED');

    CALL BALANCE_STMT(RIGHT_SIDE);
    IF TERMINATE_SCAN THEN RETURN;

    IF SUBSTR(RIGHT_SIDE,1,1)='(' THEN;
    ELSE RIGHT_SIDE='('||RIGHT_SIDE||')';

    CALL PARSE_EXP(RIGHT_SIDE,EXP_FN_CALC);

    CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);

    IF PC_MAX > 0 THEN        /*  CHANGE A STA TMPXX TO STA RESULT */
    DO;
       IF PC_OPCODE(PC_MAX)=PC_OPCODE_STA THEN
           IF SUBSTR(SYMBOL(PC_OBJECT(PC_MAX)),1,3)='TMP' THEN
              PC_OBJECT(PC_MAX)=1 ;
    END;
    CALL ADD_PCODE(PC_OPCODE_RFN,OFFSET);
    PC_OBJECT(JMP_OFFSET)=PC_MAX+1;

 END PROCESS_DEF;

 BALANCE_STMT:PROC(EXP);
 /********************************************************************
 *                                                                   *
 *   CHECK FOR BALANCE PARENS AND QUOTES                             *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE EXP                 CHAR(*) VARYING;
    DECLARE I                   FIXED BINARY ALIGNED;
    DECLARE (PARENS,QUOTES)     FIXED BINARY ALIGNED;

    PARENS=0;
    QUOTES=0;
    DO I=1 TO LENGTH(EXP);
       IF SUBSTR(EXP,I,1)='(' THEN PARENS=PARENS+1;
       ELSE
          IF SUBSTR(EXP,I,1)=')' THEN
          DO;
             PARENS=PARENS-1;
             IF PARENS < 0 THEN
             DO;
                CALL PRINT_ERR(STMT_CH,'INVALID USE OF PARENS');
                RETURN;
             END;
          END;
       IF SUBSTR(EXP,I,1)=QUOTE_1 THEN QUOTES=QUOTES+1;
    END;

    IF PARENS=0 THEN;
    ELSE
       CALL PRINT_ERR(STMT_CH,'UNBALANCED PARENS');
    IF MOD(QUOTES,2)=1 THEN   /* IF QUOTES IS ODD, ERROR */
       CALL PRINT_ERR(STMT_CH,'UNBALANCED QUOTES');

 END BALANCE_STMT;

 PARSE_EXP:PROC(EXP,EXP_TYPE);
 /********************************************************************
 *                                                                   *
 * THIS PROC TAKES A NUMERICAL EXPRESSION AND GENERATES THE PCODE TO *
 * COMPLEMENT THE EXPRESSION.  THERE ARE 2 TYPES OF EXPRESSIONS -    *
 * CALCULATING AND STORAGE.  FOR EXAMPLE, X=Y+1   X IS A STORAGE     *
 * EXPRESSION AND Y+1 IS A CALCULATING EXPRESSION.  A STORAGE        *
 * EXPRESSION CAN CONTAIN EXPRESSIONS IN SPITE OF THE FACT A SINGLE  *
 * RESULT WILL BE STORED THERE.  I.E.  X(Y+2)=B*W                    *
 *                                                                   *
 *********************************************************************
 * NESTING:COMPILE                                                   *
 ********************************************************************/
    DECLARE EXP                 CHAR(*) VARYING;
    DECLARE (I,J,EXP_TYPE)      FIXED BINARY ALIGNED;
    DECLARE EXPR                BIT(1) ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE V                   CHAR(10) VARYING;
    DECLARE FN_TMP              CHAR(10);
    DECLARE (LAST_LP,FIRST_RP,OFFSET,
             RP,BREAKER,
             MAX_BREAKER)       FIXED BINARY ALIGNED;
    DECLARE NO_PARENS           BIT(1) ALIGNED;
    DECLARE CONTINUE_SCAN       BIT(1) ALIGNED;

    DECLARE 1 STACK,
              2  STACK_MAX      FIXED BINARY ALIGNED,
              2  STACK_CUR      FIXED BINARY ALIGNED,
              2  ITEMS(50),
                 3  WORD        CHAR(10),
                 3  OP          CHAR(1);

    STACK_MAX,STACK_CUR=0;
    WORD(*)=(10)' ';
    OP(*)=' ';
    CALL POPULATE_STACK;
    IF EXP_TYPE=EXP_FN_CALC THEN
    DO;
       I=INDEX(FUNC_NAME,' ');
       J=INDEX(FUNC_ARG,' ');
       FN_TMP=SUBSTR(FUNC_NAME,1,I)||SUBSTR(FUNC_ARG,1,J)||(6)' ';
       IF STACK_PRINT_DEBUG THEN
          PUT SKIP DATA(FN_TMP);
       DO I=1 TO STACK_MAX;
          IF WORD(I)=FUNC_ARG THEN
             WORD(I)=FN_TMP;
          IF STACK_PRINT_DEBUG THEN
             PUT SKIP DATA(ITEMS(I));
       END;
    END;

    EXPR=(EXP_TYPE=EXP_CALC | EXP_TYPE=EXP_FN_CALC);

    BREAKER=0;
    MAX_BREAKER=STACK_MAX;
    CONTINUE_SCAN=TRUE;

       /*  FIND A PAIR OF PARENS TO PROCESS  */

    DO WHILE(CONTINUE_SCAN & BREAKER <= MAX_BREAKER);
       I=0;
       NO_PARENS=TRUE;
       LAST_LP,FIRST_RP=0;
       DO WHILE(NO_PARENS & I<= STACK_MAX);
          I=I+1;
          IF OP(I)='(' THEN LAST_LP=I;
          ELSE
          DO;
             IF OP(I)=')' THEN
             DO;
                FIRST_RP=I;
                NO_PARENS=FALSE;
             END;
          END;
       END; /* DO WHILE(NO_PARENS....  */
       IF NO_PARENS THEN
       DO;
          LAST_LP=1;
          RP=STACK_MAX;
       END;
       ELSE
          RP=FIRST_RP;
       $DEBUG(OFF)
       $DEBUG(DATA,LAST_LP,RP,FIRST_RP)
       $DEBUG(DATA,STACK_MAX)
       $DEBUG(OFF)
       CALL SIMPLIFY_SUB_STACK(LAST_LP,RP);
   /** IF STACK_MAX > 1 THEN
          CONTINUE_SCAN = TRUE;
       ELSE
          CONTINUE_SCAN = FALSE;
   **/ IF STACK_MAX < 2 | TERMINATE_SCAN THEN
          CONTINUE_SCAN = FALSE;
       ELSE
          CONTINUE_SCAN = TRUE;
       BREAKER=BREAKER+1;
    END; /* DO WHILE(CONTINUE_SCAN.....  */


    IF BREAKER>MAX_BREAKER THEN
    DO;
        PUT SKIP LIST('BREAKER>MAX');
    END;

    IF STACK_MAX = 1 THEN
    DO;
       RESULT_SYMBOL = WORD(1);
       RESULT_OFFSET = LOOKUP_SYMBOL_TABLE(RESULT_SYMBOL);
    END;
    ELSE
    DO;
       RESULT_SYMBOL = (10)' ';
       RESULT_OFFSET = ZERO_FBA;
    END;
    IF STACK_MAX < 3 THEN  /*  SIMPLE VARIABLE? */
    DO;
      OFFSET=LOOKUP_SYMBOL_TABLE(WORD(1));
      DO I=1 TO SS_MAX;
        IF WORD(1)=SYMBOL(I) THEN
        DO;
          IF EXPR THEN
          DO;
      /***  IF SYM_TYPE(I)=SS_FUNC THEN
      *       RC=PR_FUNC;
      *     ELSE
      *       IF SYM_TYPE(I)=SS_VAR THEN
      *          RC=PR_VAR;
      *       ELSE
      *           IF SYM_TYPE(I)=SS_DIM_VAR THEN
      *              RC=PR_SUB_VAR;
      *           ELSE
      *               IF SYM_TYPE(I)=SS_CONST THEN
      *                  RC=PR_VAR;
      *               ELSE
      *                   CALL PRINT_ERR(STMT_CH,'VARIABLE EXPECTED');
      */  END;
          ELSE
          DO;
            IF SYM_TYPE(I)=SS_VAR THEN
            DO;
              IF PC_OPCODE(PC_MAX)=PC_OPCODE_STA THEN
                IF SUBSTR(SYMBOL(PC_OBJECT(PC_MAX)),1,3)='TMP' THEN
                  PC_OBJECT(PC_MAX)=I;
                ELSE
                  /* CALL PRINT_ERR(STMT_CH,
                                 'SYNTAX ERROR RESULT EXPECTED') */;
              ELSE
                 CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);
            END;
            ELSE
              IF SYM_TYPE(I)=SS_STRCON  |
                 SYM_TYPE(I)=SS_STRVAR  |
                 SYM_TYPE(I)=SS_STRDIM  |                /*PAT 02*/
                 SYM_TYPE(I)=SS_DIM_VAR THEN ;
              ELSE
                CALL PRINT_ERR(STMT_CH,'A VARIABLE IS EXPECTED HERE');
          END;
          RETURN;
        END;
       END; /* DO LOOP */
       CALL PRINT_ERR(STMT_CH,'VARIABLE ' || WORD(1) || ' UNDEFINED?');
    END;
    ELSE
       IF TERMINATE_SCAN THEN;
       ELSE
       CALL PRINT_ERR(STMT_CH,'BIG TIME SYNTAX ERROR IN EXPRESSION');

    IF STACK_PRINT_DEBUG THEN
    DO;
        PUT SKIP LIST('****DONE WITH PARSE');
        PUT SKIP DATA(RESULT_SYMBOL,RESULT_OFFSET);
        CALL DEBUG_PRINT_STACK;
    END;

 POPULATE_STACK:PROC;
 /********************************************************************
 *                                                                   *
 *  BREAK EXP INTO WORDS AT THE SPECIAL CHARACTERS                   *
 *  TO SUPPORT STRINGS, ANY STRING CONSTANTS WILL BE EXTRACTED AND   *
 *  ADDED TO SYMBOL TABLE USING A GENERATED NAME.  THIS NAME WILL    *
 *  BE SUBSTITUTED IN THE STACK FOR THE STRING.                      *
 *                                                                   *
 *********************************************************************
 * NESTING:COMPILE - PARSE_EXP                                       *
 ********************************************************************/
    DECLARE IN_STR         BIT(1) ALIGNED;
    DECLARE STR_WORK       CHAR(80) VARYING;
    DECLARE TMP_VAR        CHAR(10);
    DECLARE OFFSET         FIXED BINARY ALIGNED;
    IN_STR = FALSE;

    STR_WORK='';
    V='';
    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('****POPULATE STACK ENTRY  EXP=',
                     '''' || EXP || '''');
    END;
    DO I=1 TO LENGTH(EXP);
       CH=SUBSTR(EXP,I,1);
       IF IN_STR THEN
       DO;
          IF CH=QUOTE_1 THEN
          DO;
             IN_STR=FALSE;
             TMP_VAR='STR$'||STR_CNT;
             STR_CNT=STR_CNT+1;
             V=TMP_VAR;
             OFFSET=LOOKUP_SYMBOL_TABLE(TMP_VAR);
             IF STACK_PRINT_DEBUG THEN
                PUT DATA(STR_WORK,V,TMP_VAR,OFFSET);
             STRING_VAL(OFFSET)=STR_WORK;
          END;
          ELSE
             STR_WORK=STR_WORK||CH;
       END;
       ELSE
       DO;
          IF CH=QUOTE_1 THEN
          DO;
              IN_STR=TRUE;
          END;
          ELSE
          IF CH='(' | CH=')' | CH='+' | CH='-' | CH='*' | CH='/' |
             CH='^' | CH=',' THEN
          DO;
              STACK_MAX=STACK_MAX+1;
              WORD(STACK_MAX)=V;
              OP(STACK_MAX)=CH;
              V='';
          END;
          ELSE
              IF CH=' ' THEN ;
              ELSE
                 V=V||CH;
       END;
    END;

    IF STACK_MAX=0 THEN   /*  EXP IS A SIMPLE VAR OR CONSTANT */
    DO;
       STACK_MAX=1;
       WORD(1)=V;
       OP(1)=' ';
    END;
    ELSE
    DO I=1 TO STACK_MAX;
       IF WORD(I)=(10)' ' THEN
          IF OP(I)= '(' THEN;
          ELSE
             IF I>1 THEN
                IF OP(I-1)=')' | OP(I)='-' THEN;
                ELSE
                   CALL PRINT_ERR(STMT_CH,'SYNTAX ERROR');
    END;

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('****POPULTATE STACK EXIT');
       CALL DEBUG_PRINT_STACK;
    END;

 END POPULATE_STACK;

 SIMPLIFY_SUB_STACK:PROC(LP,RP);
 /********************************************************************
 *                                                                   *
 *                                                                   *
 *   PROCESS OPERATORE LOCATED BETWEEN THE LEFT PAREN (LP) AND       *
 *   RIGHT PAREN (RP).  FIRST CHECK STACK FOR A SIMPLE QUANTITY      *
 *   (I.E. (X) ) OR VAIABLE IN THE STACK.  NO NEED TO SIMPLFY THESE  *
 *                                                                   *
 *                                                                   *
 *********************************************************************
 * NESTING:COMPILE - PARSE_EXP                                       *
 ********************************************************************/
    DECLARE (LP,RP)                 FIXED BINARY ALIGNED;
    DECLARE (I,J,K,HJ,HK,OFFSET)    FIXED BINARY ALIGNED;
    DECLARE OFFSET2                 FIXED BINARY ALIGNED;
    DECLARE  TMP_VAR                CHAR(5);
    DECLARE  SYM_DESC               CHAR(8);

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('****SIMPLIFY_SUB_STACK ENTRY FROM',LP,'TO ',RP);
       CALL DEBUG_PRINT_STACK;
    END;

    IF STACK_MAX = 1 THEN
        GO TO SIMPLIFY_SUB_STACK_EXIT;

         /*  CHECK FOR SIMPLE VARIABLE QUANTITY, DIM VAR OR FUNC */

    IF STACK_MAX = 2 THEN
    DO;
       IF OP(LP)='(' & OP(LP+1)=')' THEN
       DO;
          IF WORD(LP)=(10)' ' THEN
             IF WORD(LP+1)=(10)' ' THEN   /* EMPTY PARENS ERROR */
             DO;
                 CALL PRINT_ERR(STMT_CH,'EMPTY PARENS ERROR');
                 RETURN;
             END;
             ELSE                    /*  SIMPLE QUANTITY  */
        /*** DO;
        **      WORD(LP)=WORD(LP+1);
        **      OP(LP)=' ';
        **      STACK_MAX=STACK_MAX-1;
        **      GO TO SIMPLIFY_SUB_STACK_EXIT;
        **** END;  *****/;
          ELSE
          DO;                         /* COULD BE X(Y) OR FNC(Y)  */
             IF STACK_PRINT_DEBUG THEN
             DO;
                PUT SKIP LIST('FOUND DIM VAR OR FUNC');
             END;
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(LP));
             IF SYM_TYPE(OFFSET)=SS_DIM_VAR |
                SYM_TYPE(OFFSET)=SS_STRDIM  |
                SYM_TYPE(OFFSET)=SS_FUNC       THEN;
             ELSE
             DO;
                 CALL PRINT_ERR(STMT_CH,
                    'EXPECTING DIM VARIABLE OR FUNCTION CALL');
                 RETURN;
             END;
          END;
       END;
    END;

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('****SIMPLIFY_SUB_STACK BEFORE ADJ',LP,'TO ',RP);
       CALL DEBUG_PRINT_STACK;
    END;
     /*   ADJUST POINTERS TO IGNORE LP AND RP   */

    IF OP(LP)= '(' THEN
    DO;
       HJ=LP+1;
       HK=RP-1;
    END;
    ELSE
    DO;
       HJ=LP;
       HK=RP;
    END;

    J=HJ;
    K=HK;
    IF K<J THEN
    DO;
       IF STACK_PRINT_DEBUG THEN
       DO;
          PUT SKIP(2) LIST('****PROCESS_OPERATORS BYPASSED BEFORE',J,K);
          CALL DEBUG_PRINT_STACK;
          PUT SKIP DATA(K,J);
          PUT SKIP DATA(WORD(K),OP(K),OP(J));
       END;
       IF WORD(K)=(10)' ' THEN
       DO;
          IF STACK_PRINT_DEBUG THEN
              PUT SKIP LIST('WORD IS BLANK');
          IF OP(K)='(' & OP(J)=')' THEN
          DO;
             OFFSET2=LOOKUP_SYMBOL_TABLE(WORD(J));
          END;
          ELSE
          DO;
             CALL PRINT_ERR(K,'**** STACK ERROR ****');
             GOTO SIMPLIFY_SUB_STACK_EXIT;
          END;
       END;
       ELSE
       DO;
          OFFSET2=LOOKUP_SYMBOL_TABLE(WORD(K));
          IF OP(K)='(' & SYM_TYPE(OFFSET2)=SS_VAR THEN
          DO;
             CALL PRINT_ERR(K,'**** ' || SYMBOL(OFFSET2) ||
                              ' NOT A DIM OR FN ****');
             GOTO SIMPLIFY_SUB_STACK_EXIT;
          END;
       END;
       IF STACK_PRINT_DEBUG THEN
       DO;
          PUT SKIP(2) LIST('****PROCESS_OPERATORS BYPASSED AFTER',J,K);
          CALL DEBUG_PRINT_STACK;
          PUT SKIP DATA(K,SYMBOL(OFFSET2),SYM_TYPE(OFFSET2));
          PUT SKIP DATA(WORD(K),OP(K));
       END;
    END;
    ELSE
    DO;
       CALL PROCESS_OPERATORS('^','^',PC_OPCODE_EXP,PC_OPCODE_EXP);
       J=HJ;
       CALL PROCESS_OPERATORS('*','/',PC_OPCODE_MUL,PC_OPCODE_DIV);
       J=HJ;
       CALL PROCESS_OPERATORS('+','-',PC_OPCODE_ADD,PC_OPCODE_SUB);
       J=HJ;
       CALL PROCESS_OPERATORS(',',',',PC_OPCODE_LS1,PC_OPCODE_LS2);
       IF TERMINATE_SCAN THEN
          GOTO SIMPLIFY_SUB_STACK_EXIT;
    END;


    IF WORD(LP)=(10)' ' THEN          /* QUANTITY - GET RID OF () */
    DO;
       IF STACK_MAX=1 THEN;
       ELSE
       DO;
          IF STACK_PRINT_DEBUG THEN
          DO;
             PUT SKIP LIST('SIMPLIFY_SUB_STACK BEFORE UPDATE',
                           LP,RP);
             CALL DEBUG_PRINT_STACK;
          END;
          IF STACK_MAX=2 THEN
          DO;
             WORD(1)=WORD(2);
             OP(1)=' ';
             STACK_MAX=1;
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(1));
             IF EXPR THEN
                 CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);
             ELSE
                 CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);
          END;
          ELSE
          DO;
             WORD(LP)=WORD(LP+1);
             IF LP+2 <= STACK_MAX THEN
                OP(LP)=OP(LP+2);
             ELSE
                OP(LP)=' ';
             DO I=LP+1 TO STACK_MAX;
                ITEMS(I)=ITEMS(I+2);
             END;
             IF LP+1=STACK_MAX THEN
                STACK_MAX=STACK_MAX-1;
             ELSE
                STACK_MAX=STACK_MAX-2;
             K=K-1;
             J=LP;
          END;
          IF STACK_PRINT_DEBUG THEN
          DO;
             PUT SKIP LIST('SIMPLIFY_SUB_STACK AFTER UPDATE',
                           LP,RP);
             CALL DEBUG_PRINT_STACK;
          END;
       END;
    END;
    ELSE    /*  COULD BE A FUNCTION OR DIM VARIABLE */
    DO;
      OFFSET=LOOKUP_SYMBOL_TABLE(WORD(LP));
      IF STACK_PRINT_DEBUG THEN
      DO;
         SYM_DESC=SS_DESC(SYM_TYPE(OFFSET));
         PUT SKIP LIST('SIMPLIFY TYPE=',SYM_DESC);
      END;
      IF SYM_TYPE(OFFSET)=SS_FUNC |
         SYM_TYPE(OFFSET)=SS_DEF_VAR THEN
         CALL PROCESS_FUNCTION(OFFSET);
      ELSE
         IF SYM_TYPE(OFFSET)=SS_DIM_VAR |
            SYM_TYPE(OFFSET)=SS_STRDIM  THEN
            CALL PROCESS_SUBSCRIPT(OFFSET);
         ELSE
            IF SYM_TYPE(OFFSET)=SS_VAR   |
               SYM_TYPE(OFFSET)=SS_CONST THEN;
            ELSE
               CALL PRINT_ERR(STMT_CH,'EXPECTING DIM');
    END;

 SIMPLIFY_SUB_STACK_EXIT:
    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('****SIMPLIFY_SUB_STACK EXIT');
       CALL DEBUG_PRINT_STACK;
    END;


 PROCESS_OPERATORS:PROC(OP1,OP2,PC1,PC2);
 /********************************************************************
 *                                                                   *
 * THIS PROC SCANS FOR OP1 AND OP2 WITHIN ROWS J AND K OF THE STACK  *
 * PC1 AND PC2 ARE THE OPCODES FOR OP1 AND OP2 RESPECTIVELY          *
 * THE VARIABLES J AND K ARE GLOBAL TO SIMPLYFY_SUB_STACK            *
 * THEY CONTAIN THE FIRST AND LAST ROWS FOR THIS PROC TO PROCESS     *
 *                                                                   *
 *********************************************************************
 * NESTING:COMPILE - PARSE_EXP - SIMPLYFY_SUB_STACK                  *
 ********************************************************************/
    DECLARE (OP1,OP2)                CHAR(1),
             TEMP_WORD               CHAR(10) INITIAL((10)' '),
            (PC1,PC2)                FIXED BINARY ALIGNED;
    DECLARE (OFFSET,OFFSET2,OFFSET3,OFFSET4)
                                     FIXED BINARY ALIGNED;

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP(2) LIST('PROCESS_OPERATORS ENTRY',J,K,OP1,OP2);
       CALL DEBUG_PRINT_STACK;
    END;

    DO WHILE(J<=K);

       IF OP1=',' & OP(J)=',' THEN
       DO;
          IF STACK_PRINT_DEBUG THEN
             PUT SKIP LIST('**** VALIDATING SUBSCRIPTS ****');
          OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J-1));
          IF (SYM_TYPE(OFFSET)=SS_DIM_VAR |
              SYM_TYPE(OFFSET)=SS_STR_DIM) &
             ((EXP_TYPE=EXP_FN_CALC & SYM_DIM_MAX(OFFSET) > 1) |
              SYM_DIM_MAX(OFFSET) = 1) THEN
             DO;
                CALL PRINT_ERR(STMT_CH,
                          'TWO SUBSCRIPTS NOT ALLOWED IN DEF');
                GO TO EXIT_PROCESS_OPERATORS;
             END;
          TMP_VAR='TMP'||TMP_CNT;
          TMP_CNT=TMP_CNT+1;
          TEMP_WORD=TMP_VAR;
          OFFSET2=LOOKUP_SYMBOL_TABLE(WORD(J));
          OFFSET3=LOOKUP_SYMBOL_TABLE(WORD(J+1));
          OFFSET4=LOOKUP_SYMBOL_TABLE(TEMP_WORD);
          CALL ADD_PCODE(PC_OPCODE_LS1,OFFSET2);
          CALL ADD_PCODE(PC_OPCODE_LS2,OFFSET3);
          CALL ADD_PCODE(PC_OPCODE_DSL,OFFSET);
          CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);
          WORD(J-1)=TEMP_WORD;
          OP(J-1)=OP(J+2);
          WORD(J)=TMP_VAR;
       /* OP(J)=OP(J+1);    */
          IF STACK_PRINT_DEBUG THEN
          DO;
             PUT SKIP DATA(WORD(J),WORD(J+1),OFFSET);
             PUT SKIP DATA(OP(J),OP(J+1));
             CALL DEBUG_PRINT_STACK;
          END;
          DO I=J+3 TO STACK_MAX;
             ITEMS(I-3)=ITEMS(I);
          END;
          STACK_MAX=STACK_MAX-3;
          K=K-1;
          J=LP;
          GO TO EXIT_PROCESS_OPERATORS;
       END;

       IF OP(J)=OP1 | OP(J)=OP2 THEN
       DO;
          IF STACK_PRINT_DEBUG THEN
          DO;
             PUT SKIP LIST('**** STARTING *',OP1,OP2);
             DO I=1 TO STACK_MAX;
               PUT SKIP LIST(I,WORD(I),OP(I));
             END;
             PUT SKIP DATA(J);
          END;
          IF WORD(J-1)=(10)' ' THEN
             OFFSET=1;     /* ASSUME SYMBOL IS 'RESULT'  */
          ELSE
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J-1));
          IF SYM_TYPE(OFFSET)=SS_DIM_VAR |
             SYM_TYPE(OFFSET)=SS_STR_DIM THEN
          DO;
             IF STACK_PRINT_DEBUG THEN
                PUT SKIP LIST('**** A SUB ***');
             IF SYM_DIM_MAX(OFFSET)=1 & OP(J)=',' THEN
             DO;
                CALL PRINT_ERR(STMT_CH,'TWO SUBSCRIPTS NOT ALLOWED');
                RETURN;
             END;
             TMP_VAR='TMP'||TMP_CNT;
             TMP_CNT=TMP_CNT+1;

             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J));
             CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J+1));
             IF OP(J)=OP1 THEN
                CALL ADD_PCODE(PC1,OFFSET);
             ELSE
                CALL ADD_PCODE(PC2,OFFSET);

             WORD(J)=TMP_VAR;
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J));
             CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);

             OP(J)=OP(J+1);
             DO I=J+1 TO STACK_MAX;
                ITEMS(I)=ITEMS(I+1);
             END;
             STACK_MAX=STACK_MAX-1;
             K=K-1;
             J=LP;
             IF STACK_PRINT_DEBUG THEN
             DO;
                PUT SKIP LIST('PROCESS_OPERATORS UPDATE',J,K);
                CALL DEBUG_PRINT_STACK;
             END;
          END;
          ELSE
          DO;
             IF STACK_PRINT_DEBUG THEN
             DO;
                PUT SKIP LIST('**** NOT A SUB*',OP1,OP2);
                CALL DEBUG_PRINT_STACK;
                PUT SKIP DATA(J,SYMBOL(OFFSET),SYM_TYPE(OFFSET));
                PUT SKIP DATA(WORD(J),OP(J));
             END;
             TMP_VAR='TMP'||TMP_CNT;
             TMP_CNT=TMP_CNT+1;

             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J));
             CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);

             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J+1));
             IF OP(J)=OP1 THEN
                CALL ADD_PCODE(PC1,OFFSET);
             ELSE
                CALL ADD_PCODE(PC2,OFFSET);

             IF STACK_PRINT_DEBUG THEN
                PUT SKIP DATA(WORD(J), OFFSET);
             WORD(J)=TMP_VAR;
             OFFSET=LOOKUP_SYMBOL_TABLE(WORD(J));
             CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);
             IF STACK_PRINT_DEBUG THEN
                 PUT SKIP LIST('PROCESS_OPERATOR CLOBBERING ',
                               WORD(J-1));
        /*   WORD(J-1)=' ';  */
             IF STACK_PRINT_DEBUG THEN
             DO;
                PUT SKIP DATA(WORD(J),WORD(J+1),OFFSET);
                PUT SKIP DATA(OP(J),OP(J+1));
             END;
             OP(J)=OP(J+1);
             DO I=J+1 TO STACK_MAX;
                ITEMS(I)=ITEMS(I+1);
             END;
             STACK_MAX=STACK_MAX-1;
             K=K-1;
             J=LP;
             IF STACK_PRINT_DEBUG THEN
             DO;
                PUT SKIP LIST('PROCESS_OPERATORS UPDATE',J,K);
                CALL DEBUG_PRINT_STACK;
             END;
          END;
       END;
       J=J+1;
    END; /* DO WHILE */

 EXIT_PROCESS_OPERATORS:
    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('PROCESS_OPERATORS EXIT',J,K);
       CALL DEBUG_PRINT_STACK;
    END;

 END PROCESS_OPERATORS;

 PROCESS_FUNCTION:PROC(OFFSET);
 /********************************************************************
 *                                                                   *
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE - PARSE_EXP - SIMPLYFY_SUB_STACK                  *
 ********************************************************************/
    DECLARE (OFFSET,OFFSET2,OFFSET3,I)       FIXED BINARY ALIGNED;
    DECLARE TEMP_SYM                         CHAR(10) INITIAL((10)' ');

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('PROCESS_FUNCTION ENTRY');
       CALL DEBUG_PRINT_STACK;
       PUT SKIP LIST('LP=',LP);
    END;

    OFFSET2=LOOKUP_SYMBOL_TABLE(WORD(LP+1));
    IF SYM_TYPE(OFFSET2)=SS_VAR |
       SYM_TYPE(OFFSET2)=SS_CONST THEN
    DO;
       TMP_VAR='TMP'||TMP_CNT;
       TMP_CNT=TMP_CNT+1;
       TEMP_SYM=TMP_VAR;
       OFFSET3=LOOKUP_SYMBOL_TABLE(TEMP_SYM);

       IF SYM_TYPE(OFFSET)=SS_FUNC THEN
       DO;
          CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET2);
          CALL ADD_PCODE(PC_OPCODE_FNC,OFFSET);
          CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3);
       END;
       ELSE
       IF SYM_TYPE(OFFSET)=SS_DEF_VAR THEN
       DO;
          CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET2);
          CALL ADD_PCODE(PC_OPCODE_CFN,OFFSET);
          CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3);
       END;
       ELSE
          CALL PRINT_ERR(STMT_CH,'UNKNOWN FUNCTION DETECTED');
   /* CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3); */
       /**************************************************************
       *                                                             *
       *  ADJUST THE STACK TO REPLACE THE FUNC REF WITH THE TEMP VAR *
       *  SPECIAL CASE WHERE STACK REDUCES TO 1 ITEM. OTHERWISE      *
       *  IF NO OTHER OPERATORS FOLLOW, PUSH UP 2 ITEMS,  IF OTHER   *
       *  OPERATORS FOLLOW, PUSH UP 1 ITEM ONLY.                     *
       *                                                             *
       **************************************************************/
       IF STACK_MAX = 4 &
          WORD(1) = (10)' ' & OP(1) = '(' & OP(2) = '(' &
                              OP(3) = ')' & OP(4) = ')'  THEN
       DO;
          WORD(1)=TMP_VAR;
          OP(1)=' ';
          STACK_MAX=1;
       END;
       ELSE
       IF STACK_MAX < 5 THEN
       DO;
          WORD(LP)=TMP_VAR;
          OP(LP)=')';
          DO I=LP+2 TO STACK_MAX;
            ITEMS(I-1)=ITEMS(I);
          END;
          STACK_MAX=STACK_MAX-2;
       END;
       ELSE
       DO;
          WORD(LP)=TMP_VAR;
          OP(LP)=OP(LP+2);
          DO I=LP+3 TO STACK_MAX;
            ITEMS(I-2)=ITEMS(I);
          END;
          STACK_MAX=STACK_MAX-2;
       END;
    END;
    ELSE
       CALL PRINT_ERR(STMT_CH,'INVALID FUNCTION ARGUMENT');

    IF STACK_PRINT_DEBUG THEN
    DO;
       PUT SKIP LIST('PROCESS_FUNCTION EXIT');
       CALL DEBUG_PRINT_STACK;
    END;

 END PROCESS_FUNCTION;

 PROCESS_SUBSCRIPT:PROC(OFFSET);
 /********************************************************************
 *                                                                   *
 *   SUBSCRIPT COMPILING HAS 2 MAJOR PARTS - ARRAYS WITH 1 OR 2      *
 *   SUBSRIPTS.  EACH IS HANDLED IN A DIFFERENT WAY.  DUAL SUB-      *
 *   SCRIPTS ARE PROCESSED USING THE ',' AS AN OPERATOR.  THESE      *
 *   ARE COMPILED IN 'PROCESS_OPERATOR' PROC.  SINGLE SUBSCRIPTS     *
 *   ARE PROCESSED IN THIS PROC.                                     *
 *                                                                   *
 *   ONE DIMENSION ARRAYS ARE TREATED INTERNALY AS TWO DIMENSION     *
 *   TABLES.  FOR EXAMPLE, 10 DIM X(10)  IS PROCESSED INTERNALY      *
 *   AT IF IT WERE 10 DIM X(1,10).                                   *
 *                                                                   *
 *   ARRAY ELEMENTS CAN BE USED TO RETRIEVE DATA OR STORE DATA.      *
 *   THE ACTUAL PROCESS FOR LOCATING THE ELEMENT IN MEMORY IS THE    *
 *   SAME.  FOR RETRIEVING DATA, THE DATA IS LOADED INTO THE         *
 *   ACCUMULATOR AFTER OFFSET DETERMINATION.  FOR STORING DATA,      *
 *   THE ACCUMULATOR MUST BE LOADED BEFORE THE OFFSET DETERMINATION  *
 *   AND THEN STORED FROM THE ACCUMULATOR AFTER OFFSET CALCULATION.  *
 *   EXAMPLE OF PCODE GENERATED:                                     *
 *              10 LET X=Y(Z)             10 LET X(Y)=Z              *
 *                                                                   *
 *                                           LDA  Z                  *
 *                 LS1  1                    LS1  1                  *
 *                 LS2  Z                    LS2  Y                  *
 *                 DSL  Y                    DSL  X                  *
 *                 LDA  Y                    STA  X                  *
 *                 STA  X                                            *
 *                                                                   *
 *   UPON ENTRY TO THIS PROC, OFFSET POINTS TO THE SYMBOL TABLE FOR  *
 *   THE ARRAY.  LP POINTS TO THE BEGINING OF THE SUB_STACK. ONLY    *
 *   SUB_STACK ITEMS LP AND LP+1 ARE INVOLVED.                       *
 *                                                                   *
 *                                                                   *
 *********************************************************************
 *                                                                   *
 * NESTING:COMPILE - PARSE_EXP - SIMPLYFY_SUB_STACK                  *
 *                                                                   *
 ********************************************************************/
    DECLARE (OFFSET,OFFSET2,OFFSET3,OFFSET4,OFFSET5,I)
                                             FIXED BINARY ALIGNED;
    DECLARE TEMP_SYM                         CHAR(10) INITIAL((10)' ');
    DECLARE TMP_VAR                          CHAR(10) INITIAL((10)' ');
    DECLARE SUB_SUB                          BIT(1) ALIGNED;

    OFFSET2=LOOKUP_SYMBOL_TABLE(WORD(LP+1));

    IF STACK_PRINT_DEBUG | SUBTR_PRINT THEN
    DO;
       PUT SKIP LIST('****PROCESS_SUBSCRIPT ENTRY');
       CALL DEBUG_PRINT_STACK;
       PUT SKIP LIST('**** LP=',LP);
       PUT SKIP LIST('**** OP(LP)=',OP(LP));
       PUT SKIP LIST('**** OP(LP+1)=',OP(LP+1));
    END;

    IF SYM_TYPE(OFFSET2)=SS_VAR |
       SYM_TYPE(OFFSET2)=SS_DIM_VAR |
       SYM_TYPE(OFFSET2)=SS_CONST THEN
    DO;
       IF SYM_TYPE(OFFSET)=SS_DIM_VAR |
          SYM_TYPE(OFFSET)=SS_STRDIM  THEN
       DO;
          IF SYM_TYPE(OFFSET)=SS_DIM_VAR THEN
             TMP_VAR='TMP'||TMP_CNT;
          ELSE
             TMP_VAR='TMP'||TMP_CNT||'$';
          TMP_CNT=TMP_CNT+1;
          TEMP_SYM=TMP_VAR;
          OFFSET3=LOOKUP_SYMBOL_TABLE(TEMP_SYM);

          IF WORD(LP-1)=(10)' ' THEN
             SUB_SUB=FALSE;
          ELSE
          DO;
             IF SUBTR_PRINT THEN
                PUT SKIP LIST('****POSSIBLE SUBSCRIPTED SUB');
             OFFSET5=LOOKUP_SYMBOL_TABLE(WORD(LP-1));
             SUB_SUB = ((SYM_TYPE(OFFSET5)=SS_DIM_VAR  |
                        SYM_TYPE(OFFSET5)=SS_STRDIM)  &
                        OP(LP-1)='(')
                       |
                        OP(LP-1)=',';
             IF SUBTR_PRINT THEN
             DO;
                IF SUB_SUB THEN
                   PUT SKIP LIST('****IS SUBSCRIPTED SUB');
                ELSE
                   PUT SKIP LIST('****NOT SUBSCRIPTED SUB');
                CALL DEBUG_PRINT_STACK;
             END;
          END;

          IF SYM_DIM_MAX(OFFSET)=0 THEN
          DO;
             IF SUBTR_PRINT THEN
                PUT SKIP LIST('****PROCESS_SUB - 1 DIM');
             OFFSET4=LOOKUP_SYMBOL_TABLE(A_ONE);
             IF SUBTR_PRINT THEN
                PUT SKIP DATA(EXPR,SUB_SUB);
             IF EXPR THEN
                IF SUB_SUB THEN
                DO;
                   CALL GENERATE_1_SUB;
                   CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);
                   CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3);
                END;
                ELSE
                DO;
                   CALL GENERATE_1_SUB;
                   CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);
                   CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3);
                END;
             ELSE
                IF SUB_SUB THEN
                DO;
                   CALL GENERATE_1_SUB;
                   CALL ADD_PCODE(PC_OPCODE_LDA,OFFSET);
                   CALL ADD_PCODE(PC_OPCODE_STA,OFFSET3);
                END;
                ELSE
                DO;
                   CALL GENERATE_1_SUB;
                   CALL ADD_PCODE(PC_OPCODE_STA,OFFSET);
                END;
             IF SUBTR_PRINT THEN
                PUT SKIP LIST('****END PROCESS_SUB - 1 DIM');
          END;
          ELSE
          DO;
             CALL PRINT_ERR(STMT_CH,'2 SUBSCRIPTS REQUIRED FOR '
                           || WORD(LP));
          END;
       /**************************************************************
       *                                                             *
       *  ADJUST THE STACK TO REPLACE THE SUB  REF WITH THE TEMP VAR *
       *  IF NO OTHER OPERATORS FOLLOW, PUSH UP 2 ITEMS,  IF OTHER   *
       *  OPERATORS FOLLOW, PUSH UP 1 ITEM ONLY.                     *
       *                                                             *
       **************************************************************/

          IF STACK_PRINT_DEBUG | SUBTR_PRINT THEN
          DO;
             PUT SKIP LIST('****PROCESS_SUBSCRIPT BEFORE COMPRESS');
             CALL DEBUG_PRINT_STACK;
          END;
          IF STACK_MAX < 5 THEN
          DO;
             WORD(LP)=TMP_VAR;
             OP(LP)=')';
             DO I=LP+2 TO STACK_MAX;
               ITEMS(I-1)=ITEMS(I);
             END;
             STACK_MAX=STACK_MAX-2;
          END;
          ELSE
          DO;
             WORD(LP)=TMP_VAR;
             OP(LP)=OP(LP+2);
             DO I=LP+3 TO STACK_MAX;
               ITEMS(I-2)=ITEMS(I);
             END;
             STACK_MAX=STACK_MAX-2;
          END;
       END;
       ELSE
          CALL PRINT_ERR(STMT_CH,'UNKNOWN SUBSCRIPT DETECTED');
    END;
    ELSE
       CALL PRINT_ERR(STMT_CH,'INVALID SUBSCRIPT');

    IF STACK_PRINT_DEBUG | SUBTR_PRINT THEN
    DO;
       PUT SKIP LIST('****PROCESS_SUBSCRIPT EXIT');
       IF STACK_MAX = 0 THEN
          PUT SKIP DATA(STACK_MAX);
       ELSE
   /*  DO I=1 TO STACK_MAX;
             PUT SKIP LIST(I,WORD(I),OP(I));
       END;                                   *DEL*/
       CALL DEBUG_PRINT_STACK;
    END;

 GENERATE_1_SUB:PROC;

             CALL ADD_PCODE(PC_OPCODE_LS1,OFFSET4);
             CALL ADD_PCODE(PC_OPCODE_LS2,OFFSET2);
             CALL ADD_PCODE(PC_OPCODE_DSL,OFFSET);

 END GENERATE_1_SUB;

 END PROCESS_SUBSCRIPT;

 END SIMPLIFY_SUB_STACK;

 DEBUG_PRINT_STACK:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 *                                                                   *
 * NESTING:COMPILE - PARSE_EXP                                       *
 ********************************************************************/
    DECLARE I                       FIXED BINARY ALIGNED;

    DO I=1 TO STACK_MAX;
          PUT SKIP LIST(I,WORD(I),OP(I));
    END;
 END DEBUG_PRINT_STACK;

 END PARSE_EXP;

 PROCESS_DIM:PROC;
 /********************************************************************
 *                                                                   *
 * PROCESS_DIM                                                       *
 *                                                                   *
 * NESTING:COMPILE                                                   *
 ********************************************************************/

    DECLARE (I,J,NUM_OCCURS)    FIXED BINARY ALIGNED;
    DECLARE NUM_OCCURS2         FIXED BINARY ALIGNED;
    DECLARE NUM_OCCURS_T        FIXED BINARY ALIGNED;
    DECLARE ERR_PTR             FIXED BINARY ALIGNED;
    DECLARE CH                  CHAR(1);
    DECLARE (LEFT_SIDE,RIGHT_SIDE)
                                CHAR(80) VARYING;
    DECLARE (NO_OPER,DIM2_FND)  BIT(1) ALIGNED;
    DECLARE OFFSET              FIXED BINARY ALIGNED;
    DECLARE DIM_VAR             CHAR(10);

  /*  EXTRACT THE DIM VARIABLE */

 NEXT_DIM:
    LEFT_SIDE,RIGHT_SIDE='';
    NO_OPER=TRUE;

    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      IF CH='(' | CH=' ' THEN
         NO_OPER=FALSE;
      ELSE
         LEFT_SIDE=LEFT_SIDE||CH;
    END;

    DIM_VAR=LEFT_SIDE;
    OFFSET=LOOKUP_SYMBOL_TABLE(DIM_VAR);

    IF SYM_TYPE(OFFSET)=SS_DIM_VAR |
       SYM_TYPE(OFFSET)=SS_STRDIM  THEN;
    ELSE
    DO;
       CALL PRINT_ERR(STMT_CH,'VARIABLE NOT ALLOWED');
       RETURN;
    END;

   /*  EXTRACT THE OCCURANCES - NUMBERS ONLY  */

    STMT_CH=I;
    CALL SKIP_BLANKS;

    LEFT_SIDE='';
    NO_OPER=TRUE;
    DIM2_FND=FALSE;

    DO I=STMT_CH TO STMT_RIGHT WHILE(NO_OPER);
      CH=SUBSTR(STMT,I,1);
      IF CH=')'  THEN
         NO_OPER=FALSE;
      ELSE
         IF CH=' ' THEN ;
         ELSE
            IF (CH>='0' & CH<='9') | CH=',' THEN
            DO;
               IF DIM2_FND THEN
                  RIGHT_SIDE=RIGHT_SIDE||CH;
               ELSE
               IF CH=',' THEN
                  DIM2_FND=TRUE;   /* IGNORE THE , */
               ELSE
                  LEFT_SIDE=LEFT_SIDE||CH;
            END;
            ELSE
            DO;
               CALL PRINT_ERR(STMT_CH,'EXPECTING NUMBER');
               RETURN;
            END;

    END;

    IF DIM2_FND THEN
       IF VERIFY(RIGHT_SIDE,'0123456789') > 0 THEN
       DO;
          CALL PRINT_ERR(STMT_CH,'EXPECTING 1 OR 2 NUMBERS');
          RETURN;
       END;

    IF NO_OPER THEN         /* ENSURE THERE IS A ) */
    DO;
        CALL PRINT_ERR(STMT_CH,'UNEXPECTED END OF STATEMENT');
        RETURN;
   END;
   STMT_CH=I;

 /*PUT SKIP DATA(LEFT_SIDE,RIGHT_SIDE);                      *DEL*/

   NUM_OCCURS=LEFT_SIDE;
   IF DIM2_FND THEN
      NUM_OCCURS2=RIGHT_SIDE;
   ELSE
   DO;
      NUM_OCCURS2=NUM_OCCURS;
      NUM_OCCURS=0;
   END;
   $DEBUG(OFF)
   $DEBUG(DATA,NUM_OCCURS,NUM_OCCURS2)
   NUM_OCCURS_T=(NUM_OCCURS+1)*MAX(NUM_OCCURS2+1,1);
   $DEBUG(DATA,NUM_OCCURS_T)
   $DEBUG(OFF)
   IF OFFSET=SS_MAX &
      (SYM_TYPE(OFFSET)=SS_DIM_VAR |
       SYM_TYPE(OFFSET)=SS_STRDIM) THEN
   DO;
       IF SS_MAX+NUM_OCCURS_T > HBOUND(SYMBOL,1) THEN
       DO;
          CALL PRINT_ERR(STMT_CH,'DIM OCCURS TOO LARGE');
          RETURN;
       END;
       SYM_DIM_MAX(SS_MAX)=NUM_OCCURS;
       SYM_DIM2_MAX(SS_MAX)=NUM_OCCURS2;
       DO I=0 TO NUM_OCCURS;
       DO J=0 TO NUM_OCCURS2;
          SS_MAX=SS_MAX+1;
          SYMBOL(SS_MAX)=SUBSTR(DIM_VAR,1,9)||'+';
          SYM_TYPE(SS_MAX)=SYM_TYPE(OFFSET);
          SYM_VALUE(SS_MAX)=0.0;
          SYM_DIM_MAX(SS_MAX)=I;
          SYM_DIM2_MAX(SS_MAX)=J;
          STRING_VAL(SS_MAX)='*';
       END;
       END;
   END;
   ELSE
   DO;
       CALL PRINT_ERR(STMT_CH,'DUPLICATE DIM VARIABLE');
       RETURN;
   END;

   CALL SKIP_BLANKS;
   IF STMT_CH>STMT_RIGHT THEN;
   ELSE
   DO;
      CH=SUBSTR(STMT,STMT_CH,1);
      IF CH=',' THEN
      DO;
         STMT_CH=STMT_CH+1;
         GO TO NEXT_DIM;
      END;
      ELSE
      DO;
         CALL PRINT_ERR(STMT_CH,'COMMA EXPECTED');
         RETURN;
      END;

 END PROCESS_DIM;

 END COMPILE;
1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 (STRINGRANGE):
 EXECUTE:PROC;

 /********************************************************************
 *                                                                   *
 *   THIS PROC DRIVES THE EXECUTION OF THE PSEUDO MACHINE CODE.      *
 *   ERROR TRAPPING FOR THE BASIC PROGRAM AS WELL AS LIMITATIONS     *
 *   ON EXECUTION TO PREVENT RUN AWAY PROGRAMS.                      *
 *                                                                   *
 *   ACCUM      IS THE PSEUDO COMPUTER ACCUMULATOR                   *
 *   CUR_LN     IS CURRENT LINE NUMBER OF THE BASIC PGM EXECUTING    *
 *   P_CTR      IS THE COUNTER OF PCODES EXECUTED                    *
 *   P_CTR_MAX  IS THE MAXIMUM VALUE P_CTR CAN HAVE.  ONCE THIS      *
 *              VALUE IS REACHED, THE BASIC PRORAM IS ABORTED.       *
 *   P_PTR      IS THE CURRENT PC_OPCODE TO BE/CURRENTLY EXECUTING   *
 *   P_PTR_SUB  IS THE CURRENT PC_OPCODE MODIFIER FOR TYPE CHECKS    *
 *                                                                   *
 *   GOSUB_STACK IS USED TO IMPLEMENT THE GOSUB AND RETURN STMTS     *
 *   FOR_STACK   IS USED TO IMPLEMENT FOR NEXT LOOPS                 *
 *                                                                   *
 *   THERE IS A SPECIAL REGISTER CALLED DSL_REG (DIM SUBSCRIPT       *
 *   LOCATOR) THAT IS USED TO IMPLEMENT SUBSCRIPTS.  THE DSL_REG IS  *
 *   SET BY THE DSL PSUEDO INSTRUCTION USING THE CONTENTS OF LS1/2.  *
 *   PRIOR TO THE EXECUTION OF THE NEXT PSEDUO INSTRUCTION AFTER THE *
 *   DSL IS EXECUTED, THE VALUE OF THE DSL_REG WILL BE ADDED TO THE  *
 *   OFFSET (PC_OFFSET) TO EFFECTIVLY IMPLEMENT THE SUBSCRIPT. THE   *
 *   DSL_REG IS THEN SET TO ZERO. THE CONTENTS OF THE PC_OFFSET      *
 *   FOR THE DSL IS NOT USED NOW.                                    *
 *                                                                   *
 *********************************************************************
 * NESTING:EXECUTE                                                   *
 ********************************************************************/

     DECLARE PC_INST(0:GENPC_CTR)      LABEL;
     DECLARE LIB_FNC(2:GENSYM_CTR)     LABEL;
     DECLARE (P_PTR,P_PTR_SUB)         FIXED BINARY ALIGNED;
     DECLARE (P_CTR,P_CTR_MAX)         FIXED BINARY(31) ALIGNED;
     DECLARE (DSL_REG,OFFSET_VAL)      FIXED BINARY ALIGNED;
     DECLARE (LS1_REG,LS2_REG)         FIXED BINARY ALIGNED;
     DECLARE CUR_LN                    FIXED BINARY ALIGNED;
     DECLARE CUR_DF                    FIXED BINARY ALIGNED;
     DECLARE (I,L,TAB_POS,TAB_AMT,PRINT_TAB_AMT,PRINT_LAST_PCT)
                                       FIXED BINARY ALIGNED;
     DECLARE (ACCUM,REGISTER,COMP_A,COMP_B,DUMMY,ACCUM_STACK)
                                       FLOAT DECIMAL;
     DECLARE ACCUM_STACK_USED          BIT(1) ALIGNED INIT('0'B);
     DECLARE (ACCUM_TYPE,ACCUM_STR,
              COMP_RESULT,
              COMP_A_TYPE,COMP_A_STR,
              COMP_B_TYPE,COMP_B_STR)  FIXED BINARY ALIGNED;
     DECLARE (COMP_RESULT_LT           INITIAL(1),
              COMP_RESULT_EQ           INITIAL(2),
              COMP_RESULT_GT           INITIAL(3))
                                       STATIC FIXED BINARY ALIGNED;
     DECLARE 1 GOSUB_STACK       ALIGNED,
               2 (GS_CURM,
                  GS_MAX)        FIXED BINARY,
               2  GOSUB_AREA(25),
                  3  GS_LINE     FIXED BINARY,
                  3  GS_PTR      FIXED BINARY;
      DECLARE 1 FOR_STACK        ALIGNED,
                2 (FS_CUR,
                   FS_MAX)       FIXED BINARY,
                2  FS_AREA(10),
                   3  FS_CTL_VAR FIXED BINARY,
                  (3  FS_START,
                   3  FS_LIMIT,
                   3  FS_STEP)   FLOAT DECIMAL,
                   3  FS_INST    FIXED BINARY;

      DECLARE INPUT_VALUE        CHAR(80) VARYING INITIAL('');
      DECLARE PRINT_WORK               PICTURE '(6)-9.V(6)9',
              PRINT_WORK_CHAR(14) DEFINED PRINT_WORK
                                 CHAR(1);
      DECLARE PRINT_E_FORMAT           CHAR(14);
      DECLARE PRINT_ZERO               CHAR(2) VARYING INITIAL(' 0');

      DECLARE PRINT_FIELD        CHAR(14) VARYING;

      DECLARE 1 PRINT_USING_AREA   ALIGNED,
                2  PU_CUR_CH       FIXED BINARY,
                2  PU_TEXT         CHAR(80) VARYING;

      CALL DISPLAY_PRINT_LINE;

      ACCUM=-1.0;
      ACCUM=RND(ACCUM);    /* RANDOMIZE */
      ACCUM_STACK=ACCUM;
      ACCUM_STACK_USED=FALSE;
      ABNORMAL_STOP=FALSE;

      P_CTR,P_PTR = 0;
      P_CTR_MAX = MAX_EXECS;
      ACCUM,REGISTER=0.0;
      ACCUM_TYPE=0;
      COMP_A,COMP_B=0.0;
      COMP_A_TYPE,COMP_B_TYPE,COMP_A_STR,COMP_B_STR=0;
      GS_CUR,GS_MAX=0;
      FS_CUR,FS_MAX=0;
      DSL_REG=0;
      LS1_REG,LS2_REG=0;
      CUR_DEF=0;

      PRINT_LINE='';
      PRINT_TAB_AMT=0;
      PRINT_LAST_PCT=0;

      ON ERROR
      BEGIN;
          PUT SKIP LIST('FATAL BASIC INTERPRETER ERROR');
          PUT SKIP DATA(P_CTR,P_PTR);
          PUT SKIP DATA(PC_OPCODE(P_PTR),PC_OBJECT(P_PTR));
          PUT SKIP DATA(DSL_REG,OFFSET_VAL);
          PUT SKIP DATA(ACCUM,REGISTER);
          PUT SKIP DATA(COMP_A,COMP_B,COMP_A_TYPE,COMP_B_TYPE);
          PUT SKIP DATA(GS_CUR,GS_MAX);
          PUT SKIP DATA(FS_CUR,FS_MAX);
          TABLE_DUMP=TRUE;
          CALL TERMINATE;
          STOP;
      END;
 P_CODE_NEXT:
      P_PTR=P_PTR+1;
 P_CODE_JUMP:
      IF ABNORMAL_STOP THEN
          RETURN;

      P_CTR=P_CTR+1;
      IF ENFORCE_MAX_EXECS THEN
      DO;
          IF P_CTR>P_CTR_MAX THEN
          DO;
             CALL PRINT_ERR('**** PROGRAM ABORTED AFTER EXECUTING ' ||
                              P_CTR_MAX ||' INSTRUCTIONS ****');
             RETURN;
          END;
      END;
      IF P_PTR>PC_MAX THEN
      DO;
         CALL PRINT_ERR('**** PROGRAM RUN AWAY DETECTED ****');
         RETURN;
      END;

 /*********************************************************************
 *                                                                    *
 *  OFFSET ADJUSTMENT FOR SUBSCRIPTS TAKES PLACE HERE                 *
 *                                                                    *
 *********************************************************************/

      OFFSET_VAL=PC_OBJECT(P_PTR)+DSL_REG;
      IF (EXECUTION_DEBUG | SUBTR_PRINT) & (DSL_REG > 0) THEN
      DO;
         PUT SKIP LIST('****START ADJUST FOR SUBSCRIPT');
         PUT SKIP DATA(P_PTR,DSL_REG,PC_OBJECT(P_PTR),OFFSET_VAL);
         PUT SKIP LIST('****END ADJUST FOR SUBSCRIPT');
      END;
      DSL_REG=0;

 /*********************************************************************
 *                                                                    *
 *  ENFORCE OBJECT TYPING IF OPCODE REQUIRES IT                       *
 *                                                                    *
 *********************************************************************/
      SELECT(PC_ALLOW(PC_OPCODE(P_PTR)))
      WHEN('00'B)
         P_PTR_SUB = 0;
      WHEN('01'B)
         IF SYM_TYPE(PC_OBJECT(P_PTR)) < SS_STRCON THEN
            P_PTR_SUB = 1;
         ELSE
         DO;
            CALL PRINT_ERR('**** STRING NOT ALLOWED ****');
            RETURN;
         END;
      WHEN('10'B)
         IF SYM_TYPE(PC_OBJECT(P_PTR)) >= SS_STRCON THEN
             P_PTR_SUB = 2;
         ELSE
         DO;
            CALL PRINT_ERR('**** NUMBER NOT ALLOWED ****');
            RETURN;
         END;
      WHEN('11'B)
         IF SYM_TYPE(PC_OBJECT(P_PTR)) < SS_STRCON THEN
            P_PTR_SUB = 1;
         ELSE
            P_PTR_SUB = 2;
      OTHERWISE
         CALL PRINT_ERR
              ('*** FATAL ERROR - TYPE ENFORCEMENT FAILED ***');
         SIGNAL ERROR;
      ENDSELECT


      GO TO PC_INST(PC_OPCODE(P_PTR));

 /*     PCODE SLN - SET LINE NUMBER    */

 PC_INST(0):
      CUR_LN=LS_LINE(PC_OBJECT(P_PTR));
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(CUR_LN);
      GO TO P_CODE_NEXT;

 /*     PCODE LDA - LOAD ACCUMULATOR    */

 PC_INST(1):
      ACCUM=SYM_VALUE(OFFSET_VAL);
      ACCUM_TYPE=SYM_TYPE(OFFSET_VAL);
      ACCUM_STR=OFFSET_VAL;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM,
                       SYMBOL(OFFSET_VAL),SYM_TYPE(OFFSET_VAL));
      GO TO P_CODE_NEXT;

 /*     PCODE SKP - PAGE EJECT          */

 PC_INST(33):
      IF LENGTH(PRINT_LINE) > 0 THEN
          CALL DISPLAY_PRINT_LINE;
      PUT PAGE;
      GO TO P_CODE_NEXT;

 /*     PCODE STA - STORE ACCUMULATOR    */

 PC_INST(2):
      IF ACCUM_TYPE >= SS_STRCON THEN
         IF SYM_TYPE(OFFSET_VAL) = SS_STRVAR |
            SYM_TYPE(OFFSET_VAL) = SS_STRDIM THEN
             STRING_VAL(OFFSET_VAL)=STRING_VAL(ACCUM_STR);
         ELSE
         DO;
            CALL PRINT_ERR('**** STRING CANNOT BE STORED ' ||
                           'IN A NUMERIC VARIABLE ****');
            RETURN;
         END;
      ELSE
         IF SYM_TYPE(OFFSET_VAL) < SS_STRCON THEN
           SYM_VALUE(OFFSET_VAL)=ACCUM;
         ELSE
            DO;
               CALL PRINT_ERR('**** NUMBER CANNOT BE STORED ' ||
                              'IN A STRING VARIABLE ****');
               RETURN;
            END;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM,SYMBOL_AREA(OFFSET_VAL),ACCUM_STR);
                    /* SYMBOL(OFFSET_VAL),SYM_TYPE(OFFSET_VAL)*/
      GO TO P_CODE_NEXT;

 /*     PCODE STR - STORE REGISTER       */

 PC_INST(34):
      SYM_VALUE(OFFSET_VAL)=REGISTER;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(REGISTER,SYMBOL(OFFSET_VAL));
      GO TO P_CODE_NEXT;

 /*     PCODE EXP - RAISE ACCUMULATOR    */

 PC_INST(3):
      ACCUM=ACCUM**SYM_VALUE(OFFSET_VAL);
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM,SYMBOL(OFFSET_VAL));
      GO TO P_CODE_NEXT;

 /*     PCODE ADD - ADD TO ACCUMULATOR    */

 PC_INST(4):
      ACCUM=ACCUM+SYM_VALUE(OFFSET_VAL);
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM);
      GO TO P_CODE_NEXT;

 /*     PCODE SUB - SUBTRACT FROM ACCUMULATOR    */

 PC_INST(5):
      ACCUM=ACCUM-SYM_VALUE(OFFSET_VAL);
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM);
      GO TO P_CODE_NEXT;

 /*     PCODE MUL - MULTIPLY ACCUMULATOR    */

 PC_INST(6):
      ACCUM=ACCUM*SYM_VALUE(OFFSET_VAL);
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM);
      GO TO P_CODE_NEXT;


 /*     PCODE DIV - DIVIDE ACCUMULATOR    */

 PC_INST(7):
      IF SYM_VALUE(OFFSET_VAL)=0.0 THEN
      DO;
         CALL PRINT_ERR('**** DIVISION BY ZERO DETECTED ****');
         RETURN;
      END;
      ACCUM=ACCUM/SYM_VALUE(OFFSET_VAL);
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(ACCUM);
      GO TO P_CODE_NEXT;

 /*     PCODE RDV - READ VARIABLE    */

 PC_INST(8):
      DS_CUR=DS_CUR+1;
      IF DS_CUR > DS_MAX THEN
      DO;
         CALL PRINT_ERR('*** NO DATA FOR ' ||
                        SYMBOL(OFFSET_VAL));
         RETURN;
      END;
      IF P_PTR_SUB = 1 THEN
         SYM_VALUE(OFFSET_VAL) = DS_ITEM(DS_CUR);
      ELSE
         STRING_VAL(OFFSET_VAL) = STRING_VAL(DS_STR(DS_CUR));
      GO TO P_CODE_NEXT;

 /*     PCODE PRV - PRINT VARIABLE    */

 PC_INST(9):

      IF P_PTR_SUB = 2 THEN     /*  IF ARGUMENT IS A STRING, GO TO */
          GO TO PC_INST(16);    /*     PRS TO PRINT IT             */

      IF PRINT_USING_MODE THEN
         CALL EXEC_PRINT_USING;
      ELSE
      DO;
         PRINT_FIELD=FORMAT_NUMBER(SYM_VALUE(OFFSET_VAL));
         CALL PRINT_BUFFER(PRINT_FIELD);
      END;
      GO TO P_CODE_NEXT;

 /*     PCODE PRS - PRINT STRING   */

 PC_INST(16):

      IF PRINT_USING_MODE THEN
      DO;
         CALL EXEC_PRINT_USING;
         GO TO P_CODE_NEXT;
      END;

      CALL PRINT_BUFFER(STRING_VAL(OFFSET_VAL));

      GO TO P_CODE_NEXT;

 /*     PCODE PCT - PRINT CONTROL      */

 PC_INST(10):
      IF PRINT_USING_MODE THEN
         GO TO P_CODE_NEXT;

      SELECT(PC_OBJECT(P_PTR))
      WHEN(PCT_LFEED)
          CALL DISPLAY_PRINT_LINE;
          PRINT_TAB_AMT=0;
          PRINT_LAST_PCT=0;
      WHEN(PCT_TAB)
          PRINT_LAST_PCT=PCT_TAB;
      WHEN(PCT_NOTAB)
          PRINT_LAST_PCT=PCT_NOTAB;
      OTHERWISE
      DO;
         CALL PRINT_ERR
            ('**** UNDEFINED PRINT CONTROL VALUE ****');
         RETURN;
      END;
      ENDSELECT
      GO TO P_CODE_NEXT;

 /*     PCODE FNC - FUNCTION CALL  */

 /********************************************************************
 *                                                                   *
 * IMPORTANT NOTE - IF ANY CHANGES ARE MADE TO LIBRARY FUNCTIONS,    *
 * THIS PCODE FNC SHOULD MATCH THE CHANGES IN THE INITIALIZE PROC    *
 *                                                                   *
 ********************************************************************/

 PC_INST(11):
      IF PC_OBJECT(P_PTR) < LBOUND(LIB_FNC,1) |
         PC_OBJECT(P_PTR) > HBOUND(LIB_FNC,1) THEN
      DO;
          PUT SKIP(2) LIST('**** FATAL ERROR - UNDEFINED FUNCTION ',
                           SYMBOL(PC_OBJECT(P_PTR)),' ****');
          SIGNAL ERROR;
      END;
      IF EXECUTION_DEBUG THEN
         PUT SKIP LIST('FNC',SYMBOL(PC_OBJECT(P_PTR)),
                             SYM_VALUE(PC_OBJECT(P_PTR)),
                             ACCUM);
      GO TO LIB_FNC(PC_OBJECT(P_PTR));
 LIB_FNC(2):
      IF ACCUM < 0.0 THEN
      DO;
        CALL PRINT_ERR
            ('**** NEGATIVE VALUE IN SQR FUNCTION ****');
        RETURN;
      END;
      ACCUM=SQRT(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(3):
      ACCUM=ABS(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(4):
      IF PRINT_USING_MODE THEN
      DO;
        CALL PRINT_ERR('**** TAB() INVALID IN PRINT USING ****');
        RETURN;
      END;
      PRINT_TAB_AMT=ACCUM;
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      IF PRINT_TAB_AMT<1 | PRINT_TAB_AMT>120 THEN
      DO;
        CALL PRINT_ERR
            ('**** INVALID VALUE IN TAB FUNCTION ****');
        RETURN;
      END;
      GO TO END_FNC;
 LIB_FNC(5):
      ACCUM=TRUNC(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(6):
      ACCUM=COS(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(7):
      ACCUM=SIN(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(8):
      ACCUM=TAN(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(9):
      ACCUM=RND(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(10):
      IF ACCUM>=0.0 THEN
         ACCUM=TRUNC(ACCUM+0.5);
      ELSE
         ACCUM=TRUNC(ACCUM-0.5);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(11):
      ACCUM=EXP(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
      GO TO END_FNC;
 LIB_FNC(12):
      ACCUM=LOG(ACCUM);
      SYM_VALUE(PC_OBJECT(P_PTR))=ACCUM;
   /* GO TO END_FNC; */
 END_FNC:
      IF EXECUTION_DEBUG THEN
         PUT SKIP LIST('FNC',SYMBOL(PC_OBJECT(P_PTR)),
                             SYM_VALUE(PC_OBJECT(P_PTR)),
                             ACCUM);
      GO TO P_CODE_NEXT;


 /*     PCODE END - END OF EXECUTION  */

 PC_INST(12):
      CALL DISPLAY_PRINT_LINE;
      CALL DISPLAY_PRINT_LINE; /* PRINT BLANK LINE */
      PUT STRING(PRINT_LINE)
                  EDIT('**** PROGRAM EXECUTION COMPLETE - ',
                          P_CTR, ' INSTRUCTIONS EXECUTED ****')
                          (A,F(8),A);
      CALL DISPLAY_PRINT_LINE;
      RETURN;

 /*     PCODE B - BRANCH   */

 PC_INST(13):
 COMMON_BRANCH:

      DO LS_CUR=1 TO LS_MAX;
          IF LS_LINE(LS_CUR)=PC_OBJECT(P_PTR) THEN
          DO;
            P_PTR=LS_OFFSET(LS_CUR);
            GOTO P_CODE_JUMP;
          END;
      END;
      CALL PRINT_ERR('**** LINE ' || PC_OBJECT(P_PTR) ||
                          ' NOT FOUND');
      RETURN;

 /*     PCODE BAL - BRANCH AND LINK    */

 PC_INST(14):

      IF EXECUTION_DEBUG THEN
         PUT SKIP LIST('BRANCH AND LINK TO',PC_OBJECT(P_PTR));
      IF GS_MAX >= HBOUND(GS_LINE,1) THEN
      DO;
         CALL PRINT_ERR('**** TOO MANY ACTIVE GOSUBS ****');
         RETURN;
      END;
      IF GS_MAX > 0 THEN
      DO;
         DO GS_CUR=1 TO GS_MAX;
            IF GS_LINE(GS_CUR)=CUR_LN THEN
            DO;
               CALL PRINT_ERR('**** RECURSIVE GOSUB ****');
               RETURN;
            END;
         END;
      END;
      DO LS_CUR=1 TO LS_MAX;
          IF LS_LINE(LS_CUR)=PC_OBJECT(P_PTR) THEN
          DO;
            GS_MAX=GS_MAX+1;
            GS_LINE(GS_MAX)=CUR_LN;
            GS_PTR(GS_MAX)=P_PTR;
            P_PTR=LS_OFFSET(LS_CUR);
            GOTO P_CODE_JUMP;
          END;
      END;
      CALL PRINT_ERR('**** LINE '||PC_OBJECT(P_PTR)||' NOT FOUND ****');
      RETURN;

 /*     PCODE RET - RETURN TO LINK    */

 PC_INST(15):

      IF GS_MAX=0 THEN
      DO;
         CALL PRINT_ERR('**** RETURN WITHOUT A GOSUB ****');
         RETURN;
      END;
      P_PTR=GS_PTR(GS_MAX);
      GS_MAX=GS_MAX-1;

      GO TO P_CODE_NEXT;

 /*     PCODE PRS - PRINT STRING   */

 /*  PC_INST(16):   MOVED TO FOLLOW PRV CODE 9 */

 /*     PCODE LCA - LOAD COMPARATOR A   */

 PC_INST(17):
      COMP_A=SYM_VALUE(OFFSET_VAL);
      COMP_A_TYPE=SYM_TYPE(OFFSET_VAL);
      COMP_A_STR=OFFSET_VAL;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(P_PTR_SUB,COMP_A,COMP_A_TYPE,COMP_A_STR);
      GO TO P_CODE_NEXT;

 /*     PCODE LCB - LOAD COMPARATOR B   */

 PC_INST(18):

      COMP_B=SYM_VALUE(OFFSET_VAL);
      COMP_B_TYPE=SYM_TYPE(OFFSET_VAL);
      COMP_B_STR=OFFSET_VAL;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(P_PTR_SUB,COMP_B,COMP_B_TYPE,COMP_B_STR);
      GO TO P_CODE_NEXT;

 /*     PCODE BEQ - BRANCH IF A=B   */

 PC_INST(19):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_EQ THEN
         GO TO COMMON_BRANCH;
      ELSE
         GO TO P_CODE_NEXT;

 /*     PCODE BNE - BRANCH IF A<>B   */

 PC_INST(20):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_EQ THEN
         GO TO P_CODE_NEXT;
      ELSE
         GO TO COMMON_BRANCH;


 /*     PCODE BGT - BRANCH IF A>B   */

 PC_INST(21):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_GT THEN
         GO TO COMMON_BRANCH;
      ELSE
         GO TO P_CODE_NEXT;


 /*     PCODE BLT - BRANCH IF A<B   */

 PC_INST(22):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_LT THEN
         GO TO COMMON_BRANCH;
      ELSE
         GO TO P_CODE_NEXT;


 /*     PCODE BGE - BRANCH IF A>=B   */

 PC_INST(23):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_GT |
         COMP_RESULT=COMP_RESULT_EQ THEN
         GO TO COMMON_BRANCH;
      ELSE
         GO TO P_CODE_NEXT;


 /*     PCODE BLE - BRANCH IF A<=B   */

 PC_INST(24):

      CALL COMPARE_RTN;
      IF COMP_RESULT=COMP_RESULT_LT |
         COMP_RESULT=COMP_RESULT_EQ THEN
         GO TO COMMON_BRANCH;
      ELSE
         GO TO P_CODE_NEXT;

      GO TO P_CODE_NEXT;

 /*     PCODE FSU - FOR NEXT SETUP   */

 PC_INST(25):

      IF FS_MAX = 0 THEN   /* SKIP IF NO ACTIVE FORS */
         FS_CUR,FS_MAX=1;
      ELSE
      DO;
         FS_CUR=1;
         DO WHILE(FS_CUR<=FS_MAX);
           IF FS_CTL_VAR(FS_CUR)=PC_OBJECT(P_PTR) THEN /* FOUND IT */
              GO TO RECYCLE_FOR;
           ELSE
              FS_CUR=FS_CUR+1;
         END;
         IF FS_MAX=HBOUND(FS_CTL_VAR,1) THEN
         DO;
            CALL PRINT_ERR('**** TOO MANY FOR NEXT LOOPS ****');
            RETURN;
         END;
         FS_MAX=FS_MAX+1;
         FS_CUR=FS_MAX;
      END;
 RECYCLE_FOR:
      FS_CTL_VAR(FS_CUR)=PC_OBJECT(P_PTR);
      FS_START(FS_CUR),FS_LIMIT(FS_CUR),FS_STEP(FS_CUR)=0;
      FS_INST(FS_CUR)=0;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(FS_AREA(FS_CUR));
      GO TO P_CODE_NEXT;

 PC_INST(26):
      FS_START(FS_CUR)=SYM_VALUE(OFFSET_VAL);
      GO TO P_CODE_NEXT;
  PC_INST(27):
      FS_LIMIT(FS_CUR)=SYM_VALUE(OFFSET_VAL);
      GO TO P_CODE_NEXT;
 PC_INST(28):
      FS_STEP(FS_CUR)=SYM_VALUE(OFFSET_VAL);
      SYM_VALUE(FS_CTL_VAR(FS_CUR))=FS_START(FS_CUR);
      FS_INST(FS_CUR)=P_PTR;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(FS_AREA(FS_CUR));
      IF (FS_STEP(FS_CUR) > 0.0 &
          FS_START(FS_CUR) > FS_LIMIT(FS_CUR)) |
         (FS_STEP(FS_CUR) < 0.0 &
          FS_START(FS_CUR) < FS_LIMIT(FS_CUR)) THEN
      DO;
          /******************************************************
          *                                                     *
          *   FOR LOOP COMPLETION DETECTED BEFORE START.  NOW   *
          *   SKIP TO THE CORRESPONDING NEXT STATEMENT.         *
          *                                                     *
          ******************************************************/
 SEEK_NEXT:
          P_PTR=P_PTR+1;
          IF P_PTR > PC_MAX THEN
          DO;
             CALL PRINT_ERR('**** FOR WITHOUT NEXT ****');
             RETURN;
          END;
          IF PC_OPCODE(P_PTR)=PC_OPCODE_FNX &
             FS_CTL_VAR(FS_CUR)=PC_OBJECT(P_PTR) THEN
          DO;
             GO TO PC_INST(29);
          END;
          ELSE
             GO TO SEEK_NEXT;
      END;
      GO TO P_CODE_NEXT;

 PC_INST(29):
      IF FS_MAX = 0 THEN   /* ERROR IF NO ACTIVE FORS */
      DO;
         CALL PRINT_ERR('**** NEXT WITH NO FOR ****');
         RETURN;
      END;
      FS_CUR=1;
      DO WHILE(FS_CUR<=FS_MAX);
         IF FS_CTL_VAR(FS_CUR)=PC_OBJECT(P_PTR) THEN /* FOUND IT */
            GO TO FOUND_FOR;
         ELSE
            FS_CUR=FS_CUR+1;
      END;
      CALL PRINT_ERR('**** NEXT WITH NO FOR ****');
      RETURN;
 FOUND_FOR:
      IF FS_STEP(FS_CUR)=0.0 THEN
      DO;
         CALL PRINT_ERR('**** ENDLESS LOOP DETECTED - STEP IS 0 ****');
         RETURN;
      END;
      FS_START(FS_CUR)=FS_START(FS_CUR)+FS_STEP(FS_CUR);
      SYM_VALUE(FS_CTL_VAR(FS_CUR))=FS_START(FS_CUR);
      IF FS_STEP(FS_CUR)>0.0 THEN
      DO;
         IF FS_START(FS_CUR)>FS_LIMIT(FS_CUR) THEN   /* LOOP DONE? */
            CALL COMPRESS_FS;
         ELSE
            P_PTR=FS_INST(FS_CUR);
      END;
      ELSE
      DO;
         IF FS_START(FS_CUR)<FS_LIMIT(FS_CUR) THEN   /* LOOP DONE? */
            CALL COMPRESS_FS;
         ELSE
            P_PTR=FS_INST(FS_CUR);
      END;
      GO TO P_CODE_NEXT;

 /*     PCODE PTB - PRINT TAB */

 PC_INST(30):

      GO TO P_CODE_NEXT;

 /*     PCODE RST - RESTORE DATA */

 PC_INST(31):

      DS_CUR=0;

      GO TO P_CODE_NEXT;

 /*     PCODE JMP - JUMP   */

 PC_INST(35):

      P_PTR=PC_OBJECT(P_PTR);
      GO TO P_CODE_JUMP;

 /*     PCODE CFN - CALL A DEF FUNCTION   */

 PC_INST(36):
      DO I=1 TO DF_MAX;
         IF DF_NAME(I)=SYMBOL(PC_OBJECT(P_PTR)) THEN
         DO;
            DF_RETURN(I)=P_PTR;
            P_PTR=DF_OFFSET(I);
            CUR_DEF=I;
            GO TO P_CODE_JUMP;
         END;
      END;
      CALL PRINT_ERR('**** USER FUNCTION NOT FOUND ****');
      RETURN;

 /*     PCODE RFN - RETURN FROM FUNCTION */

 PC_INST(37):

      P_PTR=DF_RETURN(CUR_DEF);
      CUR_DEF=0;
      GO TO P_CODE_NEXT;

 /*     PCODE STP - STOP EXECUTION  */

 PC_INST(38):

      CALL PRINT_ERR('**** STOP STATEMENT EXECUTED ****');
      RETURN;

 /*     PCODE PUS - PRINT USING START  */

 PC_INST(39):

      PRINT_USING_MODE=FALSE;
      IF PC_OPCODE(P_PTR+1)=PC_OPCODE_PRS THEN
          PRINT_USING_MODE=TRUE;
      ELSE
         IF PC_OPCODE(P_PTR+1)=PC_OPCODE_PRV THEN
         DO;
             IF SYM_TYPE(PC_OBJECT(P_PTR+1)) >= SS_STRCON THEN
                PRINT_USING_MODE=TRUE;
             ELSE
             DO;
                CALL PRINT_ERR
                  ('**** A STRING VARIABLE MUST FOLLOW USING ****');
                RETURN;
             END;
         END;
         ELSE
         DO;
            CALL PRINT_ERR('**** A STRING MUST FOLLOW USING ****');
            RETURN;
         END;

      PU_CUR_CH=1;
      PU_TEXT=STRING_VAL(PC_OBJECT(P_PTR+1));
      IF LENGTH(PU_TEXT) < 2 THEN
      DO;
         CALL PRINT_ERR('**** PRINT USING STRING TOO SHORT ****');
         RETURN;
      END;
      P_PTR=P_PTR+2;         /* SKIP NEXT PRV/PRS PCODE */
      GO TO P_CODE_JUMP;

 /*     PCODE PUE - PRINT USING END  */

 PC_INST(40):

      CALL PRINT_BUFFER(PU_TEXT);
      IF PC_OPCODE(P_PTR-1)=PC_OPCODE_PCT THEN
      DO;
         SELECT (PC_OBJECT(P_PTR-1))
         WHEN (PCT_LFEED)
            CALL DISPLAY_PRINT_LINE;
            PRINT_TAB_AMT=0;
            PRINT_LAST_PCT=0;
         WHEN (PCT_TAB)
            PRINT_LAST_PCT=PCT_TAB;
         WHEN (PCT_NOTAB)
            PRINT_LAST_PCT=PCT_NOTAB;
         OTHERWISE
         ENDSELECT;
      END;
      PRINT_USING_MODE=FALSE;
      GO TO P_CODE_NEXT;

 /*     PCODE RAN - RANDOMIZE */

 PC_INST(41):

      DUMMY=RND(-1.0);

      GO TO P_CODE_NEXT;
1/*********************************************************************
 *                                                                    *
 *  SUBSCRIPT HANDLING ROUTINES START HERE                            *
 *                                                                    *
 *  BASIC/360 VERSION 3.1 REVISED HOW EXECUTION OF SUBSCRIPTS IS      *
 *  IMPLEMENTED.  TWO NEW REGISTERS WERE ADDED TO THE VIRTUAL         *
 *  MACHINE - LS1 AND LS2.  LS_ MEANS LOAD SUBSCRIPT_.                *
 *                                                                    *
 *  SUBSCRIPTS ARE HANDLED LIKE THIS:                                 *
 *  IT IS ASSUMMED THE ALL DIMS HAVE 2 SUBS EVEN IF THE DIM HAS ONE.  *
 *       EX:  DIM X(10) IS INTERNALLY TREATED AS DIM X(1,10).         *
 *            HOWEVER, THE COMPILER WILL NOT ALL 2 SUBS FOR THIS X.   *
 *  DURING COMPILATION, A 3 INSTRUCTION SEQUENCE IS GENERATED FOR     *
 *  ALL SUBSCRIPT REFERNECE.                                          *
 *    EX; DIM(10,10)                          DIM(10)                 *
 *                                                                    *
 *        LS1 SUB1                            LS1 1                   *
 *        LS2 SUB2                            LS2 SUB1                *
 *        DSL X                               DSL X                   *
 *                                                                    *
 *  THE DSL (DIM SUBSCRIPT LOCATOR) USES THE CONTENT OF THE LS1 AND   *
 *  LS2 REGISTERS TO LOCATE THE CORRECT OCCURANCE OF X.               *
 *  DSL ALSO CHECKS THAT THE OBJECT X IS A DIM AND THAT THE VALUES    *
 *  IN LS1 AND LS2 REGISTERS ARE WITHIN THE VALUES FROM THE DIM.      *
 *  AFTER THE SUBSCRIPT(S) ARE VERIFIED AND THE OFFSET INTO THE       *
 *  OBJECT IS DETERMINE, A DSL OFFSET REGISTER IS LOADED.  THE        *
 *  VALUE OF THE DSL REGISTER IS THEN ADDED TO THE OBJECT OF THE      *
 *  *NEXT* INSTRUCTION.  THIS HAPPENS NEAR THE LABEL P_CODE_JUMP.     *
 *  THE LS1 AND LS2 REGISTERS ARE THEN SET TO ZERO.                   *
 *                                                                    *
 *********************************************************************/

 /*     PCODE DSL - DIM SUBSCRIPT LOCATOR */

 PC_INST(32):
 /*********************************************************************
 *                                                                    *
 *  CHECK FOR VALID SUBSCRIPT(S).  IF INVALID, ABORT BASIC PROGRAM    *
 *                                                                    *
 *********************************************************************/
      IF EXECUTION_DEBUG THEN
      DO;
         PUT SKIP LIST('**** ENTER DSL CODE ****');
         PUT SKIP DATA(LS1_REG,LS2_REG,SYMBOL_AREA(PC_OBJECT(P_PTR)));
      END;
      IF SYM_TYPE(PC_OBJECT(P_PTR)) = SS_DIM_VAR |
         SYM_TYPE(PC_OBJECT(P_PTR)) = SS_STRDIM  THEN ;
      ELSE
      DO;
          CALL PRINT_ERR('**** SUBSCRIPT DETECTED ON NON DIM ' ||
                          SYMBOL(PC_OBJECT(P_PTR)));
          RETURN;
      END;
 /*******************************************************************
 *                                                                  *
 *  SPECIAL NOTE - SINGLE SUBSCRIPTED ITEMS ARE TREATED LIKE ITEMS  *
 *     THAT HAVE A TWO SUBSCRIPTS.  EXAMPLE  A(10) IS INTERNALLY    *
 *     PROCESSED AS A(0,10).  THEREFORE, ANY DIM VARIABLE THAT HAS  *
 *     A ZERO FOR THE FIRST, THE ONLY SUBSCRIPT MUST BE TREATED AS  *
 *     THE SECOND SUBSCRIPT.                                        *
 *                                                                  *
 *******************************************************************/
      IF SYM_DIM_MAX(PC_OBJECT(P_PTR)) = 0 THEN  /* SINGLE SUB? */
      DO;
         IF LS2_REG < ZERO_FBA |
            LS2_REG > SYM_DIM2_MAX(PC_OBJECT(P_PTR)) THEN
         DO;
             CALL PRINT_ERR('**** SUBSCRIPT OUT OF RANGE FOR ' ||
                            SYMBOL(PC_OBJECT(P_PTR)) );
             RETURN;
         END;
         REGISTER = LS2_REG + 1;
      END;
      ELSE
      DO;
         IF LS1_REG < ZERO_FBA |
            LS1_REG > SYM_DIM_MAX(PC_OBJECT(P_PTR)) THEN
         DO;
             CALL PRINT_ERR('**** SUBSCRIPT 1 OUT OF RANGE FOR ' ||
                            SYMBOL(PC_OBJECT(P_PTR)) );
             RETURN;
         END;
         IF LS2_REG < ZERO_FBA |
            LS2_REG > SYM_DIM2_MAX(PC_OBJECT(P_PTR)) THEN
         DO;
             CALL PRINT_ERR('**** SUBSCRIPT 2 OUT OF RANGE FOR ' ||
                            SYMBOL(PC_OBJECT(P_PTR)) );
             RETURN;
         END;
         REGISTER = LS1_REG * (SYM_DIM2_MAX(PC_OBJECT(P_PTR)) + 1) +
                    LS2_REG + 1;
      END;


      DSL_REG=REGISTER;
      IF EXECUTION_DEBUG THEN
      DO;
         PUT SKIP DATA(LS1_REG,LS2_REG);
         PUT SKIP DATA(DSL_REG,REGISTER);
      END;
      $DEBUG(OFF)
      $DEBUG(DATA,LS1_REG,LS2_REG)
      $DEBUG(DATA,DSL_REG,REGISTER)
      $DEBUG(OFF)
      GO TO P_CODE_NEXT;

 /*     PCODE LS1 - LOAD SUBSCRIPT REGISTER 1  */

 PC_INST(42):

      LS1_REG=SYM_VALUE(PC_OBJECT(P_PTR));
      LS2_REG=0;

      GO TO P_CODE_NEXT;

 /*     PCODE LS2 - LOAD SUBSCRIPT REGISTER 2  */

 PC_INST(43):

      LS2_REG=SYM_VALUE(PC_OBJECT(P_PTR));

      GO TO P_CODE_NEXT;


 /*     PCODE INP - INPUT WITHOUT PROMPT       */

 PC_INST(44):

      IF LENGTH(PRINT_LINE) > 0 THEN
          CALL DISPLAY_PRINT_LINE;
      CALL INPUT_RTN(INPUT_VALUE);
      PRINT_LINE=INPUT_VALUE || ' ENTERED VIA INPUT';
      CALL DISPLAY_PRINT_LINE;

      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(INPUT_VALUE,P_PTR_SUB);

      IF P_PTR_SUB = 1 THEN                   /* NUMERIC VALUE */
      BEGIN;
         ON CONVERSION
         BEGIN;
            CALL PRINT_ERR('**** INPUT VALUE = ' || INPUT_VALUE ||
                ' FOUND - NUMERIC DATA IS EXPECTED. ****');
            ABNORMAL_STOP=TRUE;
            GO TO P_CODE_NEXT;
         END;
         SYM_VALUE(OFFSET_VAL) = INPUT_VALUE;
      END;
      ELSE                                    /* STRING VALUE */
         STRING_VAL(OFFSET_VAL) = INPUT_VALUE;

      GO TO P_CODE_NEXT;

 COMPARE_RTN:PROC;
 /********************************************************************
 *                                                                   *
 *    THIS ROUTINE DOES ALL THE COMPARES AND SETS A LT, EQ, GT IND.  *
 *                                                                   *
 *    NUMBERIC ITEMS WILL BE COMPARED AND THE LT,EQ, OR GT INDICATOR *
 *         SET.                                                      *
 *    STRINGS WILL BE COMPARED.  STRINGS OF UNEQUAL LENGTH WILL BE   *
 *         PADDED WITH SPACES SO THE LENGTHS WILL BE EQUAL.          *
 *         A RESULT WILL BE SET TO LT, EQ, OR GT.                    *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/

      COMP_RESULT=0;

      /*  COMPARE NUMERIC ITEMS  */

      IF COMP_A_TYPE < SS_STRCON & COMP_B_TYPE < SS_STRCON THEN
      DO;
         IF COMP_A < COMP_B THEN
            COMP_RESULT=COMP_RESULT_LT;
         ELSE
            IF COMP_A = COMP_B THEN
               COMP_RESULT=COMP_RESULT_EQ;
            ELSE
               COMP_RESULT=COMP_RESULT_GT;
      END;
      ELSE    /*  COMPARE STRING ITEMS  */
      DO;
         IF EXECUTION_DEBUG THEN
            PUT SKIP DATA(SS_STRCON,STRING_VAL(COMP_A_STR),
                                 STRING_VAL(COMP_B_STR));
         IF COMP_A_TYPE >= SS_STRCON & COMP_B_TYPE >= SS_STRCON THEN
         DO;
           IF LENGTH(STRING_VAL(COMP_A_STR)) =
              LENGTH(STRING_VAL(COMP_B_STR)) THEN
           DO;
              IF STRING_VAL(COMP_A_STR) <
                 STRING_VAL(COMP_B_STR) THEN
                    COMP_RESULT=COMP_RESULT_LT;
              ELSE
                 IF STRING_VAL(COMP_A_STR) =
                    STRING_VAL(COMP_B_STR) THEN
                       COMP_RESULT=COMP_RESULT_EQ;
                 ELSE
                       COMP_RESULT=COMP_RESULT_GT;
           END;
           ELSE
               CALL COMPARE_DIF_LEN;
         END;
         ELSE  /* OH OH - CANNOT MIX NUMBERS AND STRINGS */
         DO;
            CALL PRINT_ERR('**** NUMBERS AND STRINGS CANNOT ' ||
                              'BE COMPARED ****');
            RETURN;
         END;
      END;

 COMPARE_DIF_LEN:PROC;
      DECLARE (TEMP_A,TEMP_B)       CHAR(80) INITIAL((80)' ');
      TEMP_A=STRING_VAL(COMP_A_STR);
      TEMP_B=STRING_VAL(COMP_B_STR);
      IF TEMP_A < TEMP_B THEN
         COMP_RESULT = COMP_RESULT_LT;
      ELSE
         IF TEMP_A = TEMP_B THEN
            COMP_RESULT = COMP_RESULT_EQ;
         ELSE
            COMP_RESULT = COMP_RESULT_GT;
      IF EXECUTION_DEBUG THEN
         PUT SKIP DATA(COMP_RESULT,TEMP_A,TEMP_B);
 END COMPARE_DIF_LEN;
 END COMPARE_RTN;

      DECLARE FORMAT_NUMBER ENTRY(FLOAT DECIMAL)
                            RETURNS(CHAR(14) VARYING);
 FORMAT_NUMBER:PROC(A_NUMBER) RETURNS(CHAR(14) VARYING);
 /********************************************************************
 *                                                                   *
 *    THIS ROUTINE CONVERT THE INTERNAL FLOATING POINT TO CHARACTER  *
 *                                                                   *
 *    THIS ROUTINE IS CALLED BY PRINT_VAR AND THE STR FUNCTION TO    *
 *         DO THE CONVERSION.                                        *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/

      DECLARE A_NUMBER        FLOAT DECIMAL,
              PRINT_FIELD     CHAR(14) VARYING;

      IF ABS(A_NUMBER) >= 1.0E+6 |
         ABS(A_NUMBER) <= 1.0E-6 THEN
      DO;
         IF A_NUMBER = 0.0 THEN
             PRINT_FIELD=PRINT_ZERO;
         ELSE
         DO;
             PRINT_E_FORMAT=A_NUMBER;
             PRINT_FIELD=SUBSTR(PRINT_E_FORMAT,1,12);
         END;
      END;
      ELSE
      DO;
         PRINT_WORK=A_NUMBER;
         I=1;
         DO WHILE(PRINT_WORK_CHAR(I)=' ');
           I=I+1;
         END;
         IF PRINT_WORK_CHAR(I)='-' THEN;
         ELSE
           I=I-1;
         J=14;
         DO WHILE(PRINT_WORK_CHAR(J)='0');
           J=J-1;
         END;
         IF PRINT_WORK_CHAR(J)='.' THEN
           J=J-1;
         PRINT_FIELD=SUBSTR(PRINT_WORK,I,J-I+1);
      END;
      RETURN(PRINT_FIELD);

 END FORMAT_NUMBER;

 EXEC_PRINT_USING:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/

      DECLARE (PU_NUM,PU_STR,PU_NEXT,PU_LAST,PU_LEN,
               PU_DEC,PU_OFFSET     )      FIXED BINARY ALIGNED;
      DECLARE PU_NUMBER                    FLOAT DECIMAL ALIGNED;

      DECLARE PU_WORK                  CHAR(81),
              PU_WORK_CHAR(81)         DEFINED PU_WORK
                                       CHAR(1);
      DECLARE PU_STRING                CHAR(80) VARYING;

      PU_NUM=INDEX(PU_TEXT,'#');
      PU_STR=INDEX(PU_TEXT,'&');
      SELECT(TRUE)
      WHEN(PU_NUM = 0 & PU_STR = 0)
          CALL PRINT_ERR('**** INSUFFICIENT USING ITEMS ****');
          RETURN;
      WHEN(PU_NUM > 0 & PU_STR = 0)
          PU_NEXT = PU_NUM;
      WHEN(PU_NUM = 0 & PU_STR > 0)
          PU_NEXT = PU_STR;
      WHEN(PU_NUM < PU_STR)
          PU_NEXT = PU_NUM;
      WHEN(PU_NUM > PU_STR)
          PU_NEXT = PU_STR;
      OTHERWISE
          CALL PRINT_ERR('**** INTERNAL ERROR 1 IN PRINT USING ****');
          RETURN;
      ENDSELECT;


      IF SUBSTR(PU_TEXT,PU_NEXT,1)='#' THEN
      DO;
         ON STRINGRANGE
         BEGIN;
            CALL PRINT_ERR
            ('**** ERROR IN PRINT USING NUMERIC EDIT ROUTINE****');
            PUT SKIP DATA(PU_TEXT,PU_NEXT,PU_LAST,PU_LEN);
            PUT SKIP DATA(PU_DEC,PU_OFFSET);
         END;
         IF SYM_TYPE(PC_OBJECT(P_PTR)) >= SS_STRCON THEN
         DO;
            CALL PRINT_ERR('**** TYPE MISMATCH IN PRINT USING ****');
            RETURN;
         END;

         PU_NUMBER = SYM_VALUE(OFFSET_VAL);
         IF ABS(PU_NUMBER) >= 1.0E+6 THEN
         DO;
            CALL PRINT_ERR(
                 '**** PRINT USING VALUE IS TOO LARGE ****');
            RETURN;
         END;
         PRINT_WORK=PU_NUMBER;

         /****************************************************
         *                                                   *
         *   LOCATE THE EDIT SPECS IN THE STRING AND FIND    *
         *   THE DECIMAL POINT, IF ANY.  THEN MOVE COPY THE  *
         *   EDIT SPEC TO THE WORK AREA SO THE DECIMAL       *
         *   DECIMAL POINTS ALIGN WITH PRINT_WORK.           *
         *                                                   *
         ****************************************************/
         PU_LAST=INDEX(SUBSTR(PU_TEXT,PU_NEXT),' ');
         IF PU_LAST <= 0 THEN
            PU_LAST=LENGTH(PU_TEXT)+1;
         ELSE
            PU_LAST=PU_LAST+PU_NEXT-1;
         PU_LEN=PU_LAST-PU_NEXT;

         PU_DEC=INDEX(SUBSTR(PU_TEXT,PU_NEXT,PU_LEN),'.');
         IF PU_DEC = 0 THEN
         DO;
            PU_OFFSET = 41 - PU_LEN;     /* 41 IS MIDPOINT OF */
         END;                            /*  PU_WORK  */
         ELSE
         DO;
             PU_OFFSET = 41 - PU_DEC + 1;
         END;
         PU_WORK=(81)' ';

         SUBSTR(PU_WORK,PU_OFFSET,PU_LEN)=
              SUBSTR(PU_TEXT,PU_NEXT,PU_LEN);

       /*  NOW THE DECIMAL POINTS ARE ALIGNED  */
       /*  MOVE THE FRACTION DIGITS IF ANY     */
         IF PU_WORK_CHAR(41)='.' THEN
         DO;
            J=9;
            DO I=42 TO 81;
               IF PU_WORK_CHAR(I) = '#' THEN
               DO;
                  PU_WORK_CHAR(I) = PRINT_WORK_CHAR(J);
                  J=J+1;
               END;
            END;
         END;
       /*  MOVE THE WHOLE DIGITS - PROVIDE FOR FLOATING -  */
         J=7;
         DO I=40 TO 1 BY -1;
            IF PU_WORK_CHAR(I) = '#' THEN
            DO;
               PU_WORK_CHAR(I) = PRINT_WORK_CHAR(J);
               J=J-1;
            END;
            ELSE    /* EDIT FILL CHARACTER - SUPRESS IF SOURCE */
            DO;     /* IS BLANK                                */
               IF PRINT_WORK_CHAR(J) = ' ' THEN
                  PU_WORK_CHAR(I) = ' ';
               ELSE
                  IF PRINT_WORK_CHAR(J) = '-' THEN
                  DO;
                     PU_WORK_CHAR(I) = '-';
                     J=J-1;
                  END;
            END;
         END;

         SUBSTR(PU_TEXT,PU_NEXT,PU_LEN)=
             SUBSTR(PU_WORK,PU_OFFSET,PU_LEN);
      END;
      ELSE    /*  START P U FOR STRINGS  */
      DO;
         ON STRINGRANGE
         BEGIN;
            CALL PRINT_ERR
            ('**** ERROR IN PRINT USING STRING EDIT ROUTINE****');
            PUT SKIP DATA(PU_TEXT,PU_STRING,PU_NEXT,PU_LAST,PU_LEN);
         END;
         IF SYM_TYPE(PC_OBJECT(P_PTR)) < SS_STRCON THEN
         DO;
            CALL PRINT_ERR('**** TYPE MISMATCH IN PRINT USING ****');
            RETURN;
         END;
         PU_LAST=INDEX(SUBSTR(PU_TEXT,PU_NEXT),' ');
         IF PU_LAST <= 0 THEN
            PU_LAST=LENGTH(PU_TEXT)+1;
         ELSE
            PU_LAST=PU_LAST+PU_NEXT-1;
         PU_LEN=PU_LAST-PU_NEXT;
         PU_STRING=STRING_VAL(PC_OBJECT(P_PTR));

         IF PU_LEN = 1 THEN     /*  FOR 1 &, INSERT WHOLE STRING */
         DO;
            IF PU_NEXT = LENGTH(PU_TEXT) THEN
            PU_TEXT=SUBSTR(PU_TEXT,1,PU_NEXT-1) || PU_STRING;
            ELSE
            PU_TEXT=SUBSTR(PU_TEXT,1,PU_NEXT-1) || PU_STRING ||
                    SUBSTR(PU_TEXT,PU_NEXT+1);
         END;
         ELSE
         DO;
             SELECT(TRUE)
             WHEN(LENGTH(PU_STRING) = PU_LEN)
             WHEN(LENGTH(PU_STRING) < PU_LEN)
                PU_STRING=SUBSTR(PU_STRING || (80)' ',1,PU_LEN);
             WHEN(LENGTH(PU_STRING) > PU_LEN)
                PU_STRING=SUBSTR(PU_STRING,1,PU_LEN);
             OTHERWISE
                 CALL PRINT_ERR
                   ('**** INTERNAL ERROR 2 IN PRINT USING ****');
                 RETURN;
             ENDSELECT;
             SUBSTR(PU_TEXT,PU_NEXT,PU_LEN)=PU_STRING;
         END;
      END;
      PU_CUR_CH=PU_LAST+1;

 END EXEC_PRINT_USING;

 COMPRESS_FS:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/
      DECLARE (I,T,B) FIXED BINARY ALIGNED;

      IF FS_MAX<=1 THEN
      DO;
         FS_MAX,FS_CUR=0;
         RETURN;
      END;
      ELSE
         IF FS_CUR=FS_MAX THEN
         DO;
            FS_AREA(FS_MAX)=0;
            FS_MAX=FS_MAX-1;
            RETURN;
         END;

   /*  DETERMINE TOP AND BOTTOM ROWS IN FS_AREA TO MOVE  */

      IF FS_CUR=1 THEN
      DO;
         T=2;
         B=FS_MAX;
      END;
      ELSE
      DO;
         T=FS_CUR+1;
         B=FS_MAX;
      END;

      DO I=T TO B;
         FS_AREA(I-1)=FS_AREA(I);
      END;

      FS_MAX=FS_MAX-1;

 END COMPRESS_FS;


 PRINT_BUFFER:PROC(ITEM);
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/
    DECLARE ITEM                 CHAR(*) VARYING;
    DECLARE NEXT_TAB             FIXED BINARY ALIGNED;
    DECLARE BLANKS               CHAR(120) STATIC INITIAL((120)' ');
    DECLARE COL_WIDTH            STATIC FIXED BINARY ALIGNED
                                                  INITIAL(14);

    IF PRINT_TAB_AMT > 0 THEN            /*  THIS IMPLEMENTS TAB() */
    DO;
       IF LENGTH(PRINT_LINE) < PRINT_TAB_AMT THEN
          PRINT_LINE=PRINT_LINE ||
                  SUBSTR(BLANKS,1,PRINT_TAB_AMT-LENGTH(PRINT_LINE));
       ELSE
          IF LENGTH(PRINT_LINE) > PRINT_TAB_AMT THEN
          DO;
             CALL DISPLAY_PRINT_LINE;
             PRINT_LINE=SUBSTR(BLANKS,1,PRINT_TAB_AMT);
          END;
          ELSE;  /*  NO ACTION NEEDED ON = */

       IF LENGTH(PRINT_LINE)+LENGTH(ITEM) > 120 THEN
          CALL DISPLAY_PRINT_LINE;

       PRINT_LINE=PRINT_LINE || ITEM;

       PRINT_TAB_AMT = 0;
       PRINT_LAST_PCT = 0;    /*  TAB() OVERRIDES PCT */
    END;
    ELSE
    DO;
       IF LENGTH(PRINT_LINE)+LENGTH(ITEM) > 120 THEN
          CALL DISPLAY_PRINT_LINE;

       IF LENGTH(PRINT_LINE) > 0  & PRINT_LAST_PCT = PCT_TAB THEN
       DO;
          NEXT_TAB=LENGTH(PRINT_LINE)/COL_WIDTH;
          NEXT_TAB=(NEXT_TAB+1)*COL_WIDTH;
          NEXT_TAB=NEXT_TAB-LENGTH(PRINT_LINE);
          IF NEXT_TAB>0 THEN
          DO;
             IF LENGTH(PRINT_LINE)+NEXT_TAB > 120 THEN
                CALL DISPLAY_PRINT_LINE;
             ELSE
                PRINT_LINE=PRINT_LINE || SUBSTR(BLANKS,1,NEXT_TAB);
          END;
       END;

       PRINT_LINE=PRINT_LINE || ITEM;
    END;
 END PRINT_BUFFER;

 PRINT_ERR:PROC(MSG);
 /********************************************************************
 *                                                                   *
 *   PRINTS ALL ERROR MESSAGE FOR THE EXECUTION PHASE                *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/
    DECLARE MSG                 CHAR(*);
    DECLARE EDIT_WORK           CHAR(40) VARYING;
    CALL DISPLAY_PRINT_LINE;
    CALL DISPLAY_PRINT_LINE;
    PUT STRING(PRINT_LINE)
                EDIT('**** PROGRAM EXECUTION TERMINATED IN LINE',
                     CUR_LN) (A,F(6));
    IF TABLE_DUMP | TABLE_PRINT | ICODE_PRINT THEN
       PUT STRING(EDIT_WORK) EDIT(' @ OFFSET',P_PTR) (A,F(6));
    ELSE
       EDIT_WORK='';
    PRINT_LINE=PRINT_LINE || EDIT_WORK || ' ****';
    CALL DISPLAY_PRINT_LINE;
    PRINT_LINE=MSG;
    CALL DISPLAY_PRINT_LINE;
    ABNORMAL_STOP=TRUE;    /* FOR END OF PROGRAM EXECUTION */
 END PRINT_ERR;

 /********************************************************************
 *  RND FUNCTION                                                     *
 *                                                                   *
 * NESTING:EXECUTION                                                 *
 ********************************************************************/
 %INCLUDE RNDGEN;

 END EXECUTE;

1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 TERMINATE:PROC;

     DECLARE I                  FIXED BINARY ALIGNED;

     IF TABLE_DUMP THEN
        CALL PRINT_SYMBOLS;

 END TERMINATE;
1/*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/
 /*******************************************************************/

 PRINT_SYMBOLS:PROC;

     PUT SKIP(2) EDIT('OFFSET  SYMBOL         TYPE           OCCURS',
                      '     VALUE')
                     (A);
     DO I=1 TO SS_MAX;
       PUT SKIP EDIT(I,SYMBOL(I),SS_DESC(SYM_TYPE(I)),
                     SYM_DIM_MAX(I),
                     SYM_DIM2_MAX(I))
                    (F(6),X(2),A(15),A(11),F(5),F(5));
       IF SYM_TYPE(I) = SS_STRCON |
          SYM_TYPE(I) = SS_STRVAR |
          SYM_TYPE(I) = SS_UNKNWN |
          SYM_TYPE(I) = SS_STRDIM THEN
          PUT LIST(STRING_VAL(I));
       ELSE
          PUT LIST(SYM_VALUE(I));
     END;
     PUT SKIP LIST('END OF SYMBOL TABLE');

 END PRINT_SYMBOLS;

 SAVE_CODE_ROUTINE:PROC;
     PRINT_LINE =
           '*** SAVING CODE INTO AS ' || SAVE_MEMBER_NAME ||
           ' IN LIBRARY ' || DEFAULT_DSN;
     CALL DISPLAY_PRINT_LINE;
     CALL DYNALLOC('ALLO',DEFAULT_DSN,SAVE_MEMBER_NAME);
     IF SVC99_RETURN_CODE > 0 THEN
     DO;
        CALL ANALYZE_DYNALLOC_RC;
        RETURN;
     END;
     DECLARE I FIXED BINARY ALIGNED;
     DECLARE SAVEFL  STREAM OUTPUT FILE;

     OPEN FILE(SAVEFL) TITLE(DD_NAME);
     DO I=1 TO SC_MAX;
        IF SOURCE_APPENDED(I)  THEN;
        ELSE
           PUT FILE(SAVEFL) EDIT(SOURCE_LINE(I)) (A);
     END;
     CLOSE FILE(SAVEFL);

 END SAVE_CODE_ROUTINE;

 DYNALLOC:PROC(FUNC,DSN,MEM);

     DECLARE FUNC               CHAR(4),
             DSN                CHAR(44),
             MEM                CHAR(8);

        /* RETURN CODE 5896 IS DSN NOT FOUND */
        /* RETURN CODE 528  IS DSN IN USE    */
     IF FUNC=FUNCTION_CODE & DSN=DS_NAME & MEM=MEMBER_NAME THEN
        RETURN;         /* STUFF ALREADY ALLOCATED */
     IF FUNCTION_CODE = (4)' ' THEN;
     ELSE
     DO;
        FUNCTION_CODE='FREE';
        CALL BASALOP(BASALO_PARM);
     END;
     FUNCTION_CODE='ALLO';
     DS_NAME=DSN;
     MEMBER_NAME=MEM;
     CALL BASALOP(BASALO_PARM);
 END DYNALLOC;

 ANALYZE_DYNALLOC_RC:PROC;
 /********************************************************************
 *                                                                   *
 *                                                                   *
 * NESTING:                                                          *
 ********************************************************************/

        SELECT(SVC99_RETURN_CODE)
        WHEN(0)
           RETURN;
        WHEN(528)
           PRINT_LINE='**** LIBRARY IN USE BY ANOTHER USER ****';
        WHEN(5896)
           PRINT_LINE='**** LIBRARY NOT FOUND ****';
        OTHERWISE
           PRINT_LINE='**** LIBARY ALLOCATION PROBLEM RC=';
           PRINT_LINE=PRINT_LINE || SVC99_RETURN_CODE;
        ENDSELECT;
        CALL DISPLAY_PRINT_LINE;
    /** PUT STRING(PRINT_LINE) EDIT('**** DYNALLOC FAILED RC=',
                        SVC99_RETURN_CODE, ' ****') (A,F(6),A);  **/

 END ANALYZE_DYNALLOC_RC;

 /*#INCLUDE ..\..\BASICTEST\BASICTEST1UP\BASINP.PLI*/ %INCLUDE BASINP;

 /*#INCLUDE ..\..\BASICTEST\BASICTEST1UP\BASPRT.PLI*/ %INCLUDE BASPRT;

 END BASIC;
./  ENDUP
