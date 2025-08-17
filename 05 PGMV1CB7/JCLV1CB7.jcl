//KC03CB7C JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,    
//             TIME=(,5)                                            
//************************************                              
//* EJEMPLO EJECUCION JOB BATCH      *                              
//************************************                              
//*                                                                 
//STEP1    EXEC PGM=IDCAMS,COND=(8,LT)                              
//SYSPRINT DD SYSOUT=*                                              
//SYSIN    DD *                                                     
     DELETE   KC03CB7.ARCHIVOS.SALIDA.PGMV1CB7                      
     SET MAXCC = 0                                                  
//*                                                                 
//************************************                              
//* EJECUCION PROGRAMA PGMV1CB7      *                              
//************************************                              
//STEP2    EXEC PGM=PGMV1CB7                                        
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR                    
//DDCLIEN  DD DSN=KC03CB7.CURSOS.NOVCLIEN,DISP=SHR                  
//DDSALIDA DD DSN=KC03CB7.ARCHIVOS.SALIDA.PGMV1CB7,DISP=(,CATLG),   
//            UNIT=SYSDA,VOL=SER=KCTR06,                            
//            DCB=(LRECL=55,BLKSIZE=0,RECFM=FB),                    
//            SPACE=(TRK,(1,1),RLSE)                                
//SYSOUT   DD SYSOUT=*                                              
//SYSUDUMP DD SYSOUT=* 
//
