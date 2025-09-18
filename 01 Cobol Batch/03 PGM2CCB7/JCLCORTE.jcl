//KC03CB7C JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TIME=(,5)                                        
//************************************                          
//* EJEMPLO EJECUCION JOB BATCH      *                          
//************************************                          
//*                                                             
//STEP1    EXEC PGM=IDCAMS,COND=(8,LT)                          
//SYSPRINT DD SYSOUT=*                                          
//SYSIN    DD *                                                 
     DELETE   KC03CB7.SUCUR.CLAS                                
     SET MAXCC = 0                                              
//*                                                             
//*********************************                             
//*     SORT POR SUC              *                             
//*********************************                             
//STEP2     EXEC PGM=SORT,COND=EVEN                             
//SYSOUT    DD SYSOUT=*                                         
//SORTIN    DD DSN=KC03CB7.CURSOS.CORTE,DISP=SHR                
//SORTOUT   DD DSN=KC03CB7.SUCUR.CLAS,DISP=(,CATLG),            
//          UNIT=SYSDA,VOL=SER=ZASWO1,                          
//          DCB=(LRECL=20,BLKSIZE=2000,RECFM=FB),               
//          SPACE=(TRK,(1,1),RLSE)                              
//SYSIN     DD *                                                
     SORT       FORMAT=BI,FIELDS=(1,2,A)                        
//*                                             
//************************************          
//* EJECUCION PROGRAMA PGM2CCB7      *          
//************************************          
//STEP3    EXEC PGM=PGM2CCB7                     
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR
//DDSUCUR  DD DSN=KC03CB7.SUCUR.CLAS,DISP=SHR   
//SYSOUT   DD SYSOUT=*                          
//SYSUDUMP DD SYSOUT=*                          
//SYSIN    DD SYSOUT=*                          
