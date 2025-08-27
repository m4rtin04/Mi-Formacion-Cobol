  IDENTIFICATION DIVISION.                                     
 *                                                        *    
   PROGRAM-ID PGMS18N1.                                        
 **********************************************************    
 *                                                        *    
 *  PROGRAMA PARA VSAM KSDS                               *    
 **********************************************************    
 *      MANTENIMIENTO DE PROGRAMA                         *    
 **********************************************************    
 *  FECHA   *    DETALLE        * COD *                        
 **************************************                        
 *          *                   *     *                        
 *          *                   *     *                        
 **************************************                        
  ENVIRONMENT DIVISION.                                        
  CONFIGURATION SECTION.                                       
  SPECIAL-NAMES.                                               
      DECIMAL-POINT IS COMMA.                                  
                                                               
  INPUT-OUTPUT SECTION.                                        
  FILE-CONTROL.                                                
        SELECT CLIENTE ASSIGN DDCLIEN                          
        ORGANIZATION IS INDEXED                                
        ACCESS IS SEQUENTIAL                                   
        RECORD KEY IS  KEY-CLIENTE                        
        FILE STATUS IS FS-CLIENTE.                        
                                                           
         SELECT NOVEDAD ASSIGN DDNOVE                      
         ACCESS IS SEQUENTIAL                              
         FILE STATUS IS FS-NOVEDAD.                        
                                                           
         SELECT SALIDA  ASSIGN DDSALIDA                    
         FILE STATUS IS FS-SALIDA.                         
                                                           
                                                           
   DATA DIVISION.                                          
   FILE SECTION.                                           
   FD CLIENTE.                                             
                                                           
   01 REG-CLIENTES.                                        
      05 KEY-CLIENTE PIC X(13).                            
      05 FILLER      PIC X(05).                            
      05 CLI-CLAVE   PIC 9(03).                            
      05 FILLER      PIC X(29).                            
                                                           
   FD NOVEDAD                                              
       RECORDING MODE IS F.                        
                                                                   
   FD SALIDA                                                   
       RECORDING MODE IS F.                                    
                                                               
   01 REG-SALIDA             PIC X(50).                        
                                                               
  **************************************                       
   WORKING-STORAGE SECTION.                                    
  **************************************                       
  **************************************************           
  *              FILE STATUS                       *           
  **************************************************           
   77  FS-CLIENTE        PIC XX    VALUE SPACES.               
   77  FS-NOVEDAD        PIC XX    VALUE SPACES.               
   77  FS-SALIDA         PIC XX    VALUE SPACES.               
                                                               
   01  WS-STATUS-FIN    PIC X.                                 
       88  WS-FIN-LECTURA         VALUE 'Y'.                   
       88  WS-NO-FIN-LECTURA      VALUE 'N'.                   
                                                               
   01  FS-REG-CLIENTES  PIC X.                                 
       88  WS-FIN-CLIENTE         VALUE 'Y'.                   
       88  WS-NO-FIN-CLIENTE      VALUE 'N'.                   
                                                               
   01  FS-REG-NOVEDAD   PIC X.                                 
           88  WS-FIN-NOVEDAD         VALUE 'Y'.                     
       88  WS-NO-FIN-NOVEDAD      VALUE 'N'.                     
                                                                 
                                                                 
  ***********************************************************    
  *                   CLAVE DE APAREO                       *    
  ***********************************************************    
   01 WS-KEY-CLIENTE.                                            
      02 CLIENTE-TIPO                   PIC X(02)  VALUE ZEROS.  
      02 CLIENTE-NRODOC                 PIC 9(11)  VALUE ZEROS.  
                                                                 
   01 KEY-NOVEDAD.                                               
      02 NOVEDAD-TIPO                   PIC X(02)  VALUE ZEROS.  
      02 NOVEDAD-NRODOC                 PIC 9(11)  VALUE ZEROS.  
  *************************************************************  
  *                    CONTADORES                             *  
  *************************************************************  
   01 CNT-CLIE-LEIDOS                   PIC 9(03)  VALUE ZEROS.  
   01 CNT-NOVE-LEIDAS                   PIC 9(03)  VALUE ZEROS.  
   01 CNT-NOVE-ENCONTRADA               PIC 9(03)  VALUE ZEROS.  
   01 CNT-NOVEDAD-NO-ENCONTRADA         PIC 9(03)  VALUE ZEROS.  
  *************************************************************  
  *                    MASCARAS                               *  
  *************************************************************  
   01 WS-MASCARA                        PIC ZZ9    VALUE ZEROS.  
     *************************************************************   
 *                    AREA DE COPYS                          *   
 *************************************************************   
  COPY CPCLIE.                                                   
                                                                 
  COPY CPNOVCLI.                                                 
                                                                 
  COPY CPCLIENS.                                                 
 ***************************************************************.
  PROCEDURE DIVISION.                                            
 **************************************                          
 *                                    *                          
 *  CUERPO PRINCIPAL DEL PROGRAMA     *                          
 *                                    *                          
 **************************************                          
  MAIN-PROGRAM.                                                  
                                                                 
      PERFORM 1000-INICIO  THRU   F-1000-INICIO                  
                                                                 
      PERFORM 2000-PROCESO  THRU  F-2000-PROCESO                 
                       UNTIL WS-FIN-LECTURA                      
                                                                 
      PERFORM 9999-FINAL    THRU  F-9999-FINAL                   
      .                                                          
  F-MAIN-PROGRAM. GOBACK.                                        
                                                                        
   **************************************                           
   *                                    *                           
   *  CUERPO INICIO APERTURA ARCHIVOS   *                           
   *  LECTURA ADELANTADA DE ARCHIVOS    *                           
   **************************************                           
    1000-INICIO.                                                    
                                                                    
        SET WS-NO-FIN-LECTURA TO TRUE                               
                                                                    
        PERFORM 1200-I-ABRIR-ARCHIVOS THRU 1200-F-ABRIR-ARCHIVOS    
                                                                    
        PERFORM 1400-I-LEER-CLIENTE THRU 1400-F-LEER-CLIENTE        
                                                                    
        PERFORM 1600-I-LEER-NOVEDADES  THRU 1600-F-LEER-NOVEDADES   
        .                                                           
    F-1000-INICIO.  EXIT.                                           
                                                                    
   **************************************                           
   *                                    *                           
   *  CUERPO PRINCIPAL DE PROCESOS      *                           
   *  APAREO ARCHIVOS DE ENTRADA        *                           
   *                                    *                           
   **************************************                           
    2000-PROCESO.                                                   
                                                                     
      EVALUATE TRUE                                              
                                                                 
         WHEN WS-KEY-CLIENTE = KEY-NOVEDAD                       
             ADD 1 TO CNT-NOVE-ENCONTRADA                        
             PERFORM 2400-I-GRABAR THRU 2400-F-GRABAR            
             PERFORM 1600-I-LEER-NOVEDADES THRU                  
                               1600-F-LEER-NOVEDADES             
                                                                 
         WHEN WS-KEY-CLIENTE > KEY-NOVEDAD                       
             ADD 1 TO CNT-NOVEDAD-NO-ENCONTRADA                  
             PERFORM 1600-I-LEER-NOVEDADES THRU                  
                                 1600-F-LEER-NOVEDADES           
                                                                 
         WHEN WS-KEY-CLIENTE < KEY-NOVEDAD                       
             PERFORM 1400-I-LEER-CLIENTE THRU 1400-F-LEER-CLIENTE
                                                                 
      END-EVALUATE                                               
      .                                                          
  F-2000-PROCESO. EXIT.                                          
                                                                 
                                                                 
 **************************************                          
 *                                    *                          
 *  CUERPO FINAL CIERRE DE FILES      *                          
 *                                    *                         
 **************************************                         
  9999-FINAL.                                                   
                                                                
      PERFORM 9600-I-CERRAR-ARCHIVOS THRU 9600-F-CERRAR-ARCHIVOS
                                                                
      PERFORM 9800-I-MOSTRAR-TOTALES THRU 9800-F-MOSTRAR-TOTALES
      .                                                         
  F-9999-FINAL. EXIT.                                           
                                                                
  1200-I-ABRIR-ARCHIVOS.                                        
                                                                
      OPEN INPUT CLIENTE                                        
      OPEN INPUT NOVEDAD                                        
      OPEN OUTPUT SALIDA                                        
                                                                
      IF FS-CLIENTE  IS NOT EQUAL '00'                          
         DISPLAY '* ERROR EN OPEN CLIENTE: ' FS-CLIENTE         
         MOVE 9999 TO RETURN-CODE                               
         SET  WS-FIN-LECTURA TO TRUE                            
      END-IF                                                    
                                                                
      IF FS-NOVEDAD IS NOT EQUAL '00'                           
         DISPLAY '* ERROR EN OPEN MOVIMIENTOS: ' FS-NOVEDAD     
         MOVE 9999 TO RETURN-CODE                               
             SET  WS-FIN-LECTURA TO TRUE                             
      END-IF                                                     
                                                                 
      IF FS-SALIDA  IS NOT EQUAL '00'                            
         DISPLAY '* ERROR EN OPEN SALIDA: '  FS-SALIDA           
         MOVE 9999 TO RETURN-CODE                                
         SET  WS-FIN-LECTURA TO TRUE                             
      END-IF                                                     
      .                                                          
  1200-F-ABRIR-ARCHIVOS. EXIT.                                   
                                                                 
  1400-I-LEER-CLIENTE.                                           
                                                                 
      READ CLIENTE  INTO  REG-CLIENTE                            
                                                                 
      EVALUATE FS-CLIENTE                                        
          WHEN '00'                                              
              MOVE CLI-TIP-DOC     TO CLIENTE-TIPO               
              MOVE CLI-NRO-DOC     TO CLIENTE-NRODOC             
              ADD  1 TO CNT-CLIE-LEIDOS                          
          WHEN '10'                                              
              SET WS-FIN-CLIENTE   TO TRUE                       
              MOVE HIGH-VALUES     TO WS-KEY-CLIENTE             
                                                                 
          WHEN OTHER                                             
                  DISPLAY 'ERROR EN READ CLIENTES: ' FS-CLIENTE        
              MOVE 9999 TO RETURN-CODE                             
              SET WS-FIN-LECTURA TO TRUE                           
      END-EVALUATE                                                 
      .                                                            
  1400-F-LEER-CLIENTE. EXIT.                                       
                                                                   
  1600-I-LEER-NOVEDADES.                                           
                                                                   
      READ NOVEDAD  INTO  WS-REG-NOVCLIE                           
                                                                   
      EVALUATE FS-NOVEDAD                                          
          WHEN '00'                                                
              MOVE NOV-TIP-DOC     TO NOVEDAD-TIPO                 
              MOVE NOV-NRO-DOC     TO NOVEDAD-NRODOC               
              ADD  1 TO CNT-NOVE-LEIDAS                            
                                                                   
          WHEN '10'                                                
              SET WS-FIN-NOVEDAD   TO TRUE                         
              MOVE HIGH-VALUES     TO KEY-NOVEDAD                  
                                                                   
          WHEN OTHER                                               
              DISPLAY 'ERROR EN READ MOVIMIENTOS: ' FS-NOVEDAD     
              MOVE 9999 TO RETURN-CODE                             
              SET WS-FIN-LECTURA TO TRUE                           
          END-EVALUATE                                             
      .                                                        
  1600-F-LEER-NOVEDADES. EXIT.                                 
                                                               
                                                               
  2400-I-GRABAR.                                               
                                                               
      WRITE REG-SALIDA FROM WS-REG-NOVCLIE                     
                                                               
      IF FS-SALIDA IS NOT EQUAL '00'                           
          DISPLAY 'ERROR EN WRITE SALIDA: ' FS-SALIDA          
          MOVE 9999 TO RETURN-CODE                             
          SET WS-FIN-LECTURA TO TRUE                           
      END-IF                                                   
      .                                                        
  2400-F-GRABAR. EXIT.                                         
                                                               
  9600-I-CERRAR-ARCHIVOS.                                      
                                                               
      CLOSE CLIENTE                                            
      CLOSE NOVEDAD                                            
      CLOSE SALIDA                                             
                                                               
      IF FS-CLIENTE IS NOT EQUAL '00'                          
         DISPLAY 'ERROR EN CLOSE CLIENTE: ' FS-CLIENTE         
             MOVE 9999 TO RETURN-CODE                            
         SET WS-FIN-LECTURA TO TRUE                          
      END-IF                                                 
                                                             
      IF FS-NOVEDAD IS NOT EQUAL '00'                        
         DISPLAY 'ERROR EN CLOSE MOVIMIENTOS: ' FS-NOVEDAD   
         MOVE 9999 TO RETURN-CODE                            
         SET WS-FIN-LECTURA TO TRUE                          
      END-IF                                                 
                                                             
      IF FS-SALIDA  IS NOT EQUAL '00'                        
         DISPLAY 'ERROR EN CLOSE SALIDA: '  FS-SALIDA        
         MOVE 9999 TO RETURN-CODE                            
         SET WS-FIN-LECTURA TO TRUE                          
      END-IF                                                 
      .                                                      
  9600-F-CERRAR-ARCHIVOS. EXIT.                              
                                                             
  9800-I-MOSTRAR-TOTALES.                                    
                                                             
      MOVE CNT-CLIE-LEIDOS TO WS-MASCARA                     
      DISPLAY ' '                                            
      DISPLAY '*CANTIDAD DE CLIENTES LEIDOS: ' WS-MASCARA    
      DISPLAY ' '                                            
      MOVE CNT-NOVE-LEIDAS TO WS-MASCARA                     
          DISPLAY '*CANTIDAD DE NOVEDADES LEIDAS: ' WS-MASCARA        
      DISPLAY ' '                                                 
      MOVE CNT-NOVE-ENCONTRADA TO WS-MASCARA                      
      DISPLAY '*CANTIDAD DE NOVEDADES ENCONTRADAS: ' WS-MASCARA   
      DISPLAY ' '                                                 
      MOVE CNT-NOVEDAD-NO-ENCONTRADA TO WS-MASCARA                
      DISPLAY '*CANTIDAD DE NOVEDADES NO ENCONTRADAS: ' WS-MASCARA
      .                                                           
  9800-F-MOSTRAR-TOTALES.                                         
                                                           
   01 REG-NOVEDAD            PIC X(50).                    
