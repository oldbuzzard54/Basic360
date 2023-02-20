Basic360 V3.0.0 Patch 01 Readme file

This patch only applies to BASIC1UP component of BASIC360.  It is optional.

The number of instructions executed with BASIC1UP is not limited like
BASICMON is.  By passing EXEC card parm to BASIC1UP, the enforcement
of the limit can be activated.

The purpose of the patch is to fix the bug that never enables the
limitation.  If you don't use BASIC1UP or don't care about limiting
the number of instructions executed, you can ignore this patch.

To apply this patch:
1) On TSO, allocate a dataset 'userid.BASICPAT.XMI' with a DCB of
   RECFM=FB,LRECL=80,BLKSIZE=3200.  If you don't do this, the
   transfer will not work properly.
2) Do a BINARY transfer of 'Basic Patch 01.xmi' to
   'userid.BASICBAT.XMI'.
3) Submit the 'Basic Patch 01.jcl'.  This should create a PDS called
   'userid.BASIC360.PATCH'.
4) submit 'userid.BASIC360.PATCH(PATCH)'.  This will apply the patch
   to the source code AND link edit the patched version of BASIC1UP.
5) That it.

