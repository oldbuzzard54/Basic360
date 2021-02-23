//BASIC05  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A ,TYPRUN=SCAN
//S1  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=C
//SYSUT2   DD DSN=HERCEL.BASICINC.PLI,DISP=OLD
//SYSIN DD *
./  ADD NAME=FIX2STR
 /*********************************************************************

       THE FIX2STR MACRO - CONVERT A FIXED ITEM TO A STRING
                   FOR USE WITH PL/I (F).

 **********************************************************************/
 %DECLARE
     FIX2STR   ENTRY (FIXED) RETURNS (CHARACTER);
 %FIX2STR :
     PROCEDURE (N) RETURNS (CHARACTER);

       DECLARE  (N,ABSN)      FIXED,
                NSTR          CHARACTER;
       NSTR = N;
       IF N < 0 THEN
           ABSN = N * -1;
       ELSE
           ABSN = N;
       NSTR = ABSN;   /* CONVERSION RESULTS IN 99999999 FORMAT */
       IF ABSN < 10 THEN
             NSTR = SUBSTR(NSTR,8,1);
       ELSE
       IF ABSN < 100 THEN
             NSTR = SUBSTR(NSTR,7,2);
       ELSE
       IF ABSN < 1000 THEN
             NSTR = SUBSTR(NSTR,6,3);
       ELSE
       IF ABSN < 10000 THEN
             NSTR = SUBSTR(NSTR,5,4);
       ELSE
             NSTR = '/* FIX2STR - NUMBER TOO LARGE */';
       IF N<0 THEN
          NSTR = '-' || NSTR;
       RETURN(NSTR);
 %END;
./  ADD NAME=GENPC
 /*********************************************************************

   THIS MACRO GENERATES A SERIES OF ELEMENTS FOR CREATING A TABLE.

   THE THIS MACRO IS CUSTOMIZED TO CREATE THE PSEUDO CODE (PC)
   CODE TABLE FOR BASIC/360.

   TO USE, %INCLUDE THIS MEMBER IN YOUR PROGRAM AND COMPILE WITH
   THE MACRO OPTION. (NOMACRO IS THE DEFAULT).

   CODE:
       GENPC(MMM,PPP,F,A)

       WHERE
         MMM IS THE OPCODE
         PPP IS THE OPCODE
         F   IS THE CODE FOR ACCEPTABLE OPERANDS
         A   IS THE PERMISSIBLE TYPE OF OPERANDS AS BIT STRING.

 **********************************************************************/


 %DECLARE
     GENPC     ENTRY (CHARACTER,CHARACTER,CHARACTER,CHARACTER)
               RETURNS (CHARACTER);
 %DECLARE
     GENPC_CTR            FIXED;
     %GENPC_CTR = -1;
 %GENPC :
     PROCEDURE (M,P,F,A) RETURNS (CHARACTER);

       DECLARE  (M,P,F,A,L1,L2,L3,L4,L5) CHARACTER;
       DECLARE  GSPACES     CHARACTER;
       GSPACES = '
                 ';

       L1 = '/* GENPC(' || M || ',' || P || ',' || F || ',' || A ||
            ') */';
       L2 = '2  PC_OPCODE_' || M || ' FIXED BINARY ' ||
            'INITIAL(' || P || '),';
       L3 = '2  PC_MNCODE_' || M || ' CHAR(4)      ' ||
            'INITIAL(''' || M || '''),';
       L4 = '2  PC_FORMAT_' || M || ' FIXED BINARY ' ||
            'INITIAL(' || F || '),';
       L5 = '2  PC_ALLOWS_' || M || ' BIT(2)       ' ||
            'INITIAL(''' || A || '''B),';

       L1 = SUBSTR(L1||GSPACES,1,71);
       L2 = SUBSTR(L2||GSPACES,1,71);
       L3 = SUBSTR(L3||GSPACES,1,71);
       L4 = SUBSTR(L4||GSPACES,1,71);
       GENPC_CTR=GENPC_CTR+1;

       RETURN(L1||L2||L3||L4||L5);
 %END;
 /*********************************************************************/
./  ADD NAME=GENSYM
 /*********************************************************************

   THIS MACRO GENERATES A SERIES OF ELEMENTS FOR CREATING A TABLE.

   THE THIS MACRO IS CUSTOMIZED TO INITIALIZE THE SYMBOL STACK (SS)
   CODE TABLE FOR BASIC/360.

   TO USE, %INCLUDE THIS MEMBER IN YOUR PROGRAM AND COMPILE WITH
   THE MACRO OPTION. (NOMACRO IS THE DEFAULT).

   CODE:
       GENSYM(S,T,F)

       WHERE
         S   IS THE SYMBOL TO DEFINE
         T   IS THE SYMBOL TYPE
         VN  IS THE SYMBOL INITIAL NUMERIC VALUE
         VS  IS THE SYMBOL INITIAL STRING VALUE

 **********************************************************************/


 %DECLARE
     GENSYM    ENTRY (CHARACTER,CHARACTER,CHARACTER,CHARACTER)
               RETURNS (CHARACTER);
 %DECLARE
     GENSYM_CTR            FIXED;
     %GENSYM_CTR = 0;
 %GENSYM :
     PROCEDURE (S,T,VN,VS) RETURNS (CHARACTER);

       DECLARE  (S,T,VN,VS,L1,L2,L3,L4,L5) CHARACTER;
       DECLARE  GSPACES     CHARACTER;
       GSPACES = '
                 ';

       GENSYM_CTR=GENSYM_CTR+1;
       L1 = '/* GENSYM(' || S || ',' || T || ',' || VN || ','
                         || VS || ') */';
       L2 = 'SYMBOL    (' || GENSYM_CTR || ') = ''' || S || ''';';

       L3 = 'SYM_TYPE  (' || GENSYM_CTR || ') = ' || T || ';';

       L4 = 'SYM_VALUE (' || GENSYM_CTR || ') = ' || VN || ';';

       L5 = 'STRING_VAL(' || GENSYM_CTR || ') = ''' || VS || ''';';

       L1 = SUBSTR(L1||GSPACES,1,71);
       L2 = SUBSTR(L2||GSPACES,1,71);
       L3 = SUBSTR(L3||GSPACES,1,71);
       L4 = SUBSTR(L4||GSPACES,1,71);
       L5 = SUBSTR(L5||GSPACES,1,71);

       RETURN(L1||L2||L3||L4||L5);
 %END;
 /*********************************************************************/
./  ADD NAME=GETPDSPA
 /********************** GETPDSPA V1.1.0 ******************************/
 /*********************************************************************/
 /*                                                                   */
 /* NAME: GETPDSPA - PARM LIST DEFINITIONS FOR GETPDS V1.1.0          */
 /*                                                                   */
 /*********************************************************************/
 /*                                                                   */
 /*  (C) COPYRIGHT 2017 EDWARD G LISS   ALL RIGHTS RESERVED           */
 /*                                                                   */
 /*  THIS SOURCE CODE AS WELL AS ANY OBJECT CODE RESULTING FROM THIS  */
 /*  SOURCE CODE MAY BE DISTRIBUTED FREELY PROVIDED NO FEE IS CHARGED */
 /*  AND FOR NON-COMERCIAL PURPOSES.                                  */
 /*                                                                   */
 /*  FOR COMMERCIAL DISTRIBUTION RIGHTS, CONTACT THE COPYRIGHT OWNER. */
 /*                                                                   */
 /*********************************************************************/
 /*                                                                   */
 /* REVISION HISTORY                                                  */
 /* ------  --------------------------------------------------------  */
 /* V1.1.0  INITIAL VERSION.                                          */
 /*                                                                   */
 /*********************************************************************/

     DECLARE
        1  PDSGET_PARAMETERS   STATIC ALIGNED,
            2  PDSGET_REQUEST_OPEN   INITIAL(0)  FIXED BINARY(31),
            2  PDSGET_REQUEST_START  INITIAL(1)  FIXED BINARY(31),
            2  PDSGET_REQUEST_LOCATE INITIAL(4)  FIXED BINARY(31),
            2  PDSGET_REQUEST_NEXT   INITIAL(5)  FIXED BINARY(31),
            2  PDSGET_REQUEST_READ   INITIAL(8)  FIXED BINARY(31),
            2  PDSGET_REQUEST_CLOSE  INITIAL(12) FIXED BINARY(31);
     DECLARE
        1   PDSGET_REQUEST_1      STATIC,
            2  PDSGET_REQUEST     FIXED BINARY(31),
        1   PDSGET_MEMBER_1       STATIC,
            2  PDSGET_MEMBER      CHAR(8),     /* DDNAME FOR OPEN */
        1   PDSGET_RECORD80_1     STATIC,
            2  PDSGET_RECORD80    CHAR(80),
        1   PDSGET_RETURN_CODE_1  STATIC,
            2  PDSGET_RETURN_CODE FIXED BINARY(31);
 /*********************************************************************/
 /*                                                                   */
 /* PDSGET_RETURN_CODE DEFINITIONS                                    */
 /*       RETURN_CODE 0 = SUCCESSFUL COMPLETION OF REQUEST            */
 /*       RETURN_CODE 8 = SERIOUS ERROR.                              */
 /*       RETURN_CODE 4 = DEPENDS ON REQUEST CODE                     */
 /*            REQUEST_CODE    MEANING                                */
 /*                  0         PDS COULD NOT BE OPENED.               */
 /*                  1         PDS COULD NOT BE OPENED.               */
 /*                  4         MEMBER NOT FOUND.                      */
 /*                  5         END OF DIRECTORY.                      */
 /*                  8         END OF CURRENT MEMBER.                 */
 /*                                                                   */
 /*********************************************************************/
./  ADD NAME=RNDGEN
 /*********************************************************************
 *                                                                    *
 *   RND - RANDOM NUMBER GENERATOR                                    *
 *                                                                    *
 *   THIS FUNCTION GENERATES 'RANDOM' NUMBERS BETWEEN 0.0 AND 1.0     *
 *                                                                    *
 *   NOTE - THIS IS A SIMPLE RANDOM VALUE THAT IS NOT CONSIDERED      *
 *          STATISTICALY A RANDOM NUMBER.  A STATISTICALY RANDOM      *
 *          REQUIRES MULTIPLE RANDOM VALUES BE GENERATED, A MEAN BE   *
 *          DETERMINED AND STUFF BEYOND THE SCOPE OF THIS MODULE.     *
 *                                                                    *
 *   PASSING A SEED_VALUE OF ZERO RETAINS CURRENT SEED (IY)           *
 *   PASSING A SEED_VALUE >  ZERO, SETS CURRENT SEED TO SEED_VALUE    *
 *                                 FOR OPTIMAL RESULTS, SEED_VALUE    *
 *                                 SHOULD BE >= 1000.                 *
 *   PASSING A SEED_VALUE <  ZERO, SETS CURRENT SEED TO THE LAST 4    *
 *                                 DIGITS OF THE CURRENT TIME.        *
 *                                                                    *
 **********************************************************************
 *********************************************************************/
    DECLARE RND ENTRY(FLOAT DECIMAL) RETURNS(FLOAT DECIMAL);
 RND:PROC(SEED_VALUE) RETURNS(FLOAT DECIMAL);
    DECLARE SEED_VALUE  FLOAT DECIMAL;
    DECLARE YFL         FLOAT DECIMAL;
    DECLARE ZERO        FLOAT DECIMAL STATIC INITIAL(0.0);
    DECLARE BZERO       FIXED BINARY(31) STATIC INITIAL(0);
    DECLARE (IX,IY)     FIXED BINARY(31) STATIC;
    DECLARE RND_SEEDED  BIT(1) ALIGNED STATIC INITIAL('0'B);

    IF SEED_VALUE > ZERO THEN
       IY = SEED_VALUE;
    ELSE IF SEED_VALUE < ZERO | (RND_SEEDED = '0'B)  THEN
       IY = SUBSTR(TIME,4,6);    /* TIME IS HHMMSSTTT FORMAT */
    IF MOD(IY,2) = BZERO THEN
       IY = IY+1;
    IX = IY;
 (NOFIXEDOVERFLOW):
    IY = IX * 65539;
    IF IY < BZERO THEN
    DO;
 (NOFIXEDOVERFLOW):
       IY = IY + 2147483647 + 1;
    END;
    RND_SEEDED='1'B;
    YFL = IY;
    YFL = YFL * .4656613E-9;
    RETURN(YFL);
 END RND;
 /*******************************************************************/
./  ADD NAME=SELECT
 /*********************************************************************

       THE SELECT, WHEN, BREAK, OTHERWISE AND ENDSELECT MACROS
                   FOR USE WITH PL/I (F).

   THESE MACROS EMULATE THE PL/I SELECT STATEMENT WHICH WAS NOT
   IMPLEMENTED IN THE PL/I (F) COMPILER.

   TO USE, %INCLUDE THIS MEMBER IN YOUR PROGRAM AND COMPILE WITH
   THE MACRO OPTION. (NOMACRO IS THE DEFAULT).

   CODE THE SELECT:

       SELECT (A)                 SELECT (TRUE)
       WHEN(1)                    WHEN(A=1)
         CALL SUB1;                 CALL SUB1;
       WHEN(X)                    WHEN(A=X)
         CALL XRTN(X);              CALL XTRN(X);
         X=X+A;                     X=X+A;
       ENDSELECT                  OTHERWISE
                                    PUT SKIP LIST('ERROR');
                                  ENDSELECT

   NOTE - USING THE MACROS, THE ENDSELECT IS REQUIRED.

   THE ALTERNATE SELECT WHEN BREAK IS ALSO SUPPORTED.  THIS ONE IS
   POPULAR WITH C, C++, JAVA, ETC PROGRAMMERS.

   THERE IS A COMPILE TIME VARIABLE DEFINED SELECT_TYPE.  BY DEFAULT
   IT IS SET TO 0. THIS INDICATES UPON COMPLETION OF A WHEN BLOCK,
   THE STATEMENT AFTER THE ENDSELECT WILL BE THE NEXT EXECUTED.
   IF SELECT_TYPE IS SET TO 1, UPON COMPLETION OF A WHEN BLOCK, THE
   NEXT WHEN WILL BE EXECUTED.  A BREAK MACRO IN A WHEN BLOCK
   WILL CAUSE THE STATEMENT AFTER THE ENDSELECT TO BE THE NEXT
   STATEMENT EXECUTED.
   TO SWITCH TYPE, CODE A %SELECT_TYPE=1; OR %SELECT_TYPE=0;
 **********************************************************************/

 %DECLARE
     SELECT    ENTRY (CHARACTER) RETURNS (CHARACTER),
     WHEN      ENTRY (CHARACTER) RETURNS (CHARACTER),
     OTHERWISE ENTRY             RETURNS (CHARACTER),
     BREAK     ENTRY             RETURNS (CHARACTER),
     ENDSELECT ENTRY             RETURNS (CHARACTER);

 %DECLARE
      WHEN_CTR            FIXED,
      SELECT_CTR          FIXED,
      SELECT_TYPE         FIXED,
      SELECT_NAME         CHARACTER,
     (SELEXPR,SPACES)     CHARACTER;

   %WHEN_CTR = 0;
   %SELECT_CTR = 0;
   %SELECT_TYPE = 0;
   %SELECT_NAME = ' ';
   %SPACES = '
             ';
 /*********************************************************************/
 %SELECT :
     PROCEDURE (A) RETURNS (CHARACTER);

       DECLARE  (A,L1,L2) CHARACTER;

       IF WHEN_CTR > 0 THEN
           RETURN('/*** THE SELECT MACRO CANNOT BE NESTED ***/');

       IF SELECT_TYPE < 0 | SELECT_TYPE > 1 THEN
           RETURN('/*** SELECT_TYPE MUST BE 0 OR 1 ***/');

       SELECT_CTR = SELECT_CTR+1;
       SELECT_NAME = FIX2STR(SELECT_CTR);
       SELECT_NAME = ' ENDSELECT_MACRO' || SELECT_NAME;
       SELEXPR = A;
       WHEN_CTR = 1;

       L1 = '       /*';
       L2 = 'SELECT (' || SELEXPR || ')   */ DO;';

       L1 = SUBSTR(L1||SPACES,1,71);

       RETURN(L1||L2);
 %END;
 /*********************************************************************/
 %WHEN :
     PROCEDURE (A) RETURNS (CHARACTER);

       DECLARE  (A,L1,L2,L4,L5) CHARACTER;

       IF WHEN_CTR > 1 THEN
       DO;
          IF SELECT_TYPE = 0 THEN
             L1='             GO TO ' || SELECT_NAME || '; END; /*';
          ELSE
             L1='             END; /*';
          L2='WHEN (' || A || ')    */';
       END;
       ELSE
       IF WHEN_CTR = 1 THEN
       DO;
          L1='                 /*';
          L2='WHEN (' || A || ')    */';
       END;
       ELSE
          RETURN('/** WHEN WITHOUT A SELECT **/');

       WHEN_CTR = WHEN_CTR + 1;

       IF SELEXPR = 'TRUE' THEN
          L4='         IF (' || A || ') THEN';
       ELSE
          L4='         IF (' || SELEXPR || ')=(' || A || ') THEN';
       L5='                      DO;';

       L1 = SUBSTR(L1||SPACES,1,71);
       L2 = SUBSTR(L2||SPACES,1,71);
       L4 = SUBSTR(L4||SPACES,1,71);

       RETURN(L1||L2||L4||L5);
 %END ;
 /*********************************************************************/
 %BREAK :
     PROCEDURE RETURNS (CHARACTER);
       DECLARE  (L1,L2) CHARACTER;
       IF SELECT_TYPE = 0 THEN
          RETURN('/*** BREAK NOT ALLOWED IN THIS SELECT TYPE ***/');
       L1='                   /*';
       L2='BREAK         */   GO TO ' || SELECT_NAME ||';';
       L1 = SUBSTR(L1||SPACES,1,71);

       RETURN(L1||L2);
 %END;
 /*********************************************************************/
 %OTHERWISE :
     PROCEDURE RETURNS (CHARACTER);

       DECLARE  (L1,L2) CHARACTER;

       IF WHEN_CTR < 1 THEN
          RETURN('/** OTHERWISE OUT OF SEQUENCE **/');

       L1='              END; /*';
       L2='OTHERWISE     */  ELSE DO;';

       L1 = SUBSTR(L1||SPACES,1,71);
       L2 = SUBSTR(L2||SPACES,1,71);

       RETURN(L1||L2);
 %END;
 /*********************************************************************/
 %ENDSELECT :
     PROCEDURE RETURNS (CHARACTER);

       DECLARE  (L1,L2) CHARACTER;

       IF WHEN_CTR < 1 THEN
          RETURN('/** ENDSELECT OUT OF SEQUENCE **/');

       WHEN_CTR = 0;

       L1='              END; /*';
       L2='ENDSELECT     */  END;';

       L2 = L2 || SELECT_NAME || ':;';
       L1 = SUBSTR(L1||SPACES,1,71);

       RETURN(L1||L2);
 %END;
./  ENDUP
