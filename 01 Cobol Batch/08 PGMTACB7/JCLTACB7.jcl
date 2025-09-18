//KC03CB7E JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TIME=(,5)                                        
//*******************************                               
//* EJECUCION PROGRAMA PGMTACB7 *                               
//*******************************                               
//STEP1    EXEC PGM=PGMTACB7                                    
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR                
//DDPRODUC DD DSN=KC03CB7.CURSOS.PRODUCT1,DISP=SHR              
//DDPRECIO DD DSN=KC03CB7.CURSOS.PRECIO,DISP=SHR                
//SYSOUT   DD SYSOUT=*                                          
//SYSPRINT DD SYSOUT=*                                          
//SYSUDUMP DD SYSOUT=*                                          
//SYSIN    DD SYSOUT=*                                          
//*                                                             
//                                                              
