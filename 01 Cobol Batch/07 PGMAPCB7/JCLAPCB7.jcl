//KC03CB7A JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TIME=(,5)                         
//************************************                          
//*  EJECUCION JOB BATCH  PGMAPCB7   *                          
//************************************                          
//*                                                             
//STEP1    EXEC PGM=IDCAMS,COND=(8,LT)                          
//SYSPRINT DD SYSOUT=*                                          
//SYSIN    DD *                                                 
     DELETE   KC03CB7.CURSOS.CLIENTE.CLAS                       
     DELETE   KC03CB7.CURSOS.MOVIMICC.CLAS                      
     DELETE   KC03CB7.CURSOS.SALCLIEN.NUE                       
     SET MAXCC=0                                                
//*                                                             
//*********************************                             
//*     SORT POR CLIENTE          *                             
//*********************************                             
//STEP2       EXEC PGM=SORT,COND=EVEN                           
//SYSOUT    DD SYSOUT=*                                         
//SORTIN    DD DSN=KC03CB7.CURSOS.CLIENTE,DISP=SHR              
//SORTOUT   DD DSN=KC03CB7.CURSOS.CLIENTE.CLAS,DISP=(,CATLG),   
//          UNIT=SYSDA,VOL=SER=ZASWO1,                          
//          DCB=(LRECL=30,BLKSIZE=3000,RECFM=FB),               
//          SPACE=(TRK,(1,1),RLSE)           
//SYSIN     DD *                                                       
 SORT       FORMAT=BI,FIELDS=(6,2,A,8,8,A)                             
//*                                                                    
//*********************************                                    
//*     SORT POR MOVIMIENTOS      *                                    
//*********************************                                    
//STEP3     EXEC PGM=SORT,COND=EVEN                                  
//SYSOUT    DD SYSOUT=*                                                
//SORTIN    DD DSN=KC03CB7.CURSOS.MOVIMICC,DISP=SHR                    
//SORTOUT   DD DSN=KC03CB7.CURSOS.MOVIMICC.CLAS,DISP=(,CATLG),         
//          UNIT=SYSDA,VOL=SER=ZASWO1,                                 
//          DCB=(LRECL=80,BLKSIZE=8000,RECFM=FB),                      
//          SPACE=(TRK,(1,1),RLSE)                                     
//SYSIN     DD *                                                       
 SORT       FORMAT=BI,FIELDS=(6,2,A,8,8,A)                             
//*                                                                    
//*                                                                    
//************************************                                 
//* EJECUCION PROGRAMA PGMAPACB7     *                                 
//************************************                                 
//STEP4    EXEC PGM=PGMAPCB7                                           
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR                       
//DDCLIEN  DD DSN=KC03CB7.CURSOS.CLIENTE.CLAS,DISP=SHR                 
//DDMVNTOS DD DSN=KC03CB7.CURSOS.MOVIMICC.CLAS,DISP=SHR                
//DDSALIDA DD DSN=KC03CB7.CURSOS.SALCLIEN.NUE,DISP=(NEW,CATLG,CATLG),  
//            UNIT=SYSDA,VOL=SER=ZASWO1,           
//            DCB=(LRECL=30,BLKSIZE=3000,RECFM=FB),
//            SPACE=(TRK,(1,1),RLSE)               
//SYSOUT   DD SYSOUT=*                             
//SYSUDUMP DD SYSOUT=*                             
//SYSIN    DD *                                    
//                                                 
