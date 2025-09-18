   IDENTIFICATION DIVISION.                                   
    PROGRAM-ID PGMV1CB7.                                      
  **********************************************************  
  *                                                        *  
  *  VALIDACION DATOS DE ENTRADA                           *  
  *                                                        *  
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
                FILE STATUS IS FS-CLIENTE.                    
             SELECT SALIDA  ASSIGN DDSALIDA                       
               FILE STATUS IS FS-SALIDA.                     
                                                             
   DATA DIVISION.                                             
   FILE SECTION.                                              
   FD CLIENTE                                                 
       BLOCK CONTAINS 0 RECORDS                              
       RECORDING MODE IS F.                                  
                                                             
   01 REG-CLIENTE    PIC X(50).                               
                                                             
   FD SALIDA                                                  
       RECORDING MODE IS F.                                  
                                                             
   01 REG-SALIDA.                                             
     02 NOV-SECUEN    PIC 9(05).                             
     02 NOV-RESTO     PIC X(50).                             
                                                             
                                                             
 **************************************                      
  WORKING-STORAGE SECTION.                                   
 **************************************                      
  77  FILLER     PIC X(30) VALUE    'INICIO WORKING-STORAGE'.
                                                             
  01 WS-MASCARA      PIC ZZZ9 VALUE ZEROS.                   
  01 WS-GRABADOS      PIC 9(03) VALUE ZEROS.            
                                                         
  ****************************************               
  *            FILE STATUS               *               
  ****************************************               
   77  FS-CLIENTE      PIC X(02) VALUE SPACES.           
   77  FS-SALIDA       PIC X(02) VALUE SPACES.           
                                                         
  *****************************************              
  *          CONTROL DE FIN DE CICLO      *              
  *****************************************              
   01  WS-STATUS-FIN    PIC X.                           
       88  WS-FIN-LECTURA         VALUE 'Y'.             
       88  WS-NO-FIN-LECTURA      VALUE 'N'.             
  ***************************************
  *            CONTADORES               *                         
  ***************************************                         
   01  CNT-CONTADORES.                                            
      02 CNT-LEIDOS        PIC 9(03).                             
      02 CNT-GRABADOS      PIC 9(03).                             
      02 CNT-ERRORES       PIC 9(03).                             
                                                                  
  *******************************************                     
  *               COPYS                     *                     
  *******************************************                     
   COPY CPNOVCLI.                                                 
                                                                  
   01  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.
                                                                  
  ***************************************************************.
   PROCEDURE DIVISION.                                            
  **************************************                          
  *                                    *                          
  *  CUERPO PRINCIPAL DEL PROGRAMA     *                          
  *                                    *                          
  **************************************                          
   i-MAIN-PROGRAM.                                                  
                                                                  
       PERFORM 1000-I-INICIO  THRU  1000-F-INICIO                 
       PERFORM 2000-I-PROCESO  THRU  2000-F-PROCESO                
                   UNTIL WS-FIN-LECTURA                                
                                                                    
       PERFORM 9999-I-FINAL    THRU  9999-F-FINAL                  
        .                                                           
    F-MAIN-PROGRAM. GOBACK.                                         
                                                                    
   **************************************                           
   *                                    *                           
   *  CUERPO INICIO APERTURA ARCHIVOS   *                           
   *                                    *                           
   **************************************                           
    1000-I-INICIO.                                                  
                                                                    
        SET WS-NO-FIN-LECTURA TO TRUE                               
                                                                    
        PERFORM 1200-I-ABRIR-ARCHIVOS THRU 1200-F-ABRIR-ARCHIVOS    
                                                                    
        PERFORM 1400-I-LEER-INPUT  THRU 1400-F-LEER-INPUT           
        .                                                           
    1000-F-INICIO. EXIT.                                            
                                                                    
    2000-I-PROCESO.                                                 
                                                                    
        PERFORM 2200-I-VALIDAR-REGISTRO THRU 2200-F-VALIDAR-REGISTRO
                                                                       
        PERFORM 1400-I-LEER-INPUT THRU 1400-F-LEER-INPUT           
        .                                                          
   2000-F-PROCESO. EXIT.                                          
                                                                  
                                                                  
  **************************************                          
   9999-I-FINAL.                                                  
                                                                  
       PERFORM 9800-I-CERRAR-ARCHIVOS THRU 9800-I-CERRAR-ARCHIVOS 
                                                                  
       PERFORM 9700-I-MOSTRAR-TOTALES THRU 9700-F-MOSTRAR-TOTALES 
       .                                                          
   9999-F-FINAL. EXIT.                                            
                                                                  
   1200-I-ABRIR-ARCHIVOS.                                         
                                                                  
       OPEN INPUT CLIENTE                                         
       OPEN OUTPUT SALIDA                                         
                                                                  
       IF FS-CLIENTE NOT EQUAL '00'                               
           DISPLAY 'ERROR EN ABRIR ARCHIVO CLIENTE: ' FS-CLIENTE  
           MOVE 9999 TO RETURN-CODE                               
           SET WS-FIN-LECTURA TO TRUE                             
       END-IF                                                     
                                                                       
       IF FS-SALIDA NOT EQUAL '00'                                
           DISPLAY 'ERROR EN ABRIR SALIDA ' FS-SALIDA             
           MOVE 9999 TO RETURN-CODE                               
           SET WS-FIN-LECTURA TO TRUE                             
       END-IF                                                     
       .                                                          
   1200-F-ABRIR-ARCHIVOS. EXIT.                                   
                                                                  
   1400-I-LEER-INPUT.                                             
                                                                  
       READ CLIENTE INTO WS-REG-NOVCLIE                           
                                                                  
       EVALUATE FS-CLIENTE                                        
                                                                  
           WHEN '00'                                              
               ADD 1 TO CNT-LEIDOS                                
                                                                  
           WHEN '10'                                              
               SET WS-FIN-LECTURA TO TRUE                         
                                                                  
           WHEN OTHER                                             
               DISPLAY 'ERROR EN LEER CLIENTE: ' FS-CLIENTE       
               MOVE 9999 TO RETURN-CODE                           
               SET WS-FIN-LECTURA TO TRUE                         
                                                                         
       END-EVALUATE                                                 
       .                                                            
   1400-F-LEER-INPUT. EXIT.                                         
                                                                    
   2200-I-VALIDAR-REGISTRO.                                         
                                                                    
       EVALUATE NOV-TIP-DOC                                         
          WHEN 'DU'                                                 
             PERFORM 2400-I-GRABAR-SALIDA THRU 2400-F-GRABAR-SALIDA 
                                                                    
          WHEN 'PA'                                                 
              PERFORM 2400-I-GRABAR-SALIDA THRU 2400-F-GRABAR-SALIDA
                                                                    
          WHEN 'PE'                                                 
              PERFORM 2400-I-GRABAR-SALIDA THRU 2400-F-GRABAR-SALIDA
                                                                    
          WHEN 'CI'                                                 
              PERFORM 2400-I-GRABAR-SALIDA THRU 2400-F-GRABAR-SALIDA
                                                                    
          WHEN OTHER                                                
              ADD 1 TO CNT-ERRORES                                  
       END-EVALUATE                                                 
       .                                                            
   2200-F-VALIDAR-REGISTRO. EXIT.                                   
                                                                
   2400-I-GRABAR-SALIDA.                                   
                                                           
       ADD 1 TO WS-GRABADOS                                
                                                           
       MOVE WS-GRABADOS    TO NOV-SECUEN                   
       MOVE WS-REG-NOVCLIE TO NOV-RESTO                    
                                                           
       WRITE REG-SALIDA                                    
                                                           
       IF FS-SALIDA NOT EQUAL '00'                         
          DISPLAY 'ERROR EN GRABAR SALIDA: ' FS-SALIDA     
          MOVE 9999 TO RETURN-CODE                         
          SET WS-FIN-LECTURA TO TRUE                       
                                                           
       ELSE                                                
          ADD 1 TO CNT-GRABADOS                            
       END-IF                                              
       .                                                   
   2400-F-GRABAR-SALIDA. EXIT.                             
                                                           
   9700-I-MOSTRAR-TOTALES.                                 
                                                           
                                                           
       MOVE CNT-LEIDOS   TO WS-MASCARA                                                                                      
       DISPLAY ' '                                               
       DISPLAY '************************************************'
       DISPLAY '*                PROGRAMA PGMV1CB7             *'
       DISPLAY '************************************************'
       DISPLAY ' '                                               
       DISPLAY '* CANTIDAD DE REGISTROS LEIDOS: ' WS-MASCARA     
       DISPLAY '*                                              *'
                                                                 
       MOVE CNT-GRABADOS   TO WS-MASCARA                         
       DISPLAY '* CANTIDAD DE REGISTROS GRABADOS: ' WS-MASCARA   
       DISPLAY ' '                                               
                                                                 
       MOVE CNT-ERRORES TO WS-MASCARA                            
       DISPLAY '* CANTIDAD DE REGISTROS ERRONEOS: ' WS-MASCARA   
       DISPLAY '*                                              *'
       DISPLAY '************************************************'
       .                                                         
   9700-F-MOSTRAR-TOTALES. EXIT.                                 
                                                                 
   9800-I-CERRAR-ARCHIVOS.                                       
                                                                 
       CLOSE CLIENTE                                             
       CLOSE SALIDA                                              
       IF FS-CLIENTE NOT EQUAL '00'                          
           DISPLAY 'ERROR EN CIERRE CLIENTE: ' FS-CLIENTE    
           MOVE 9999 TO RETURN-CODE                          
           SET WS-FIN-LECTURA TO TRUE                        
       END-IF                                                
                                                             
       IF FS-SALIDA  NOT EQUAL '00'                          
           DISPLAY 'ERROR EN CIERRE SALIDA: ' FS-SALIDA      
           MOVE 9999 TO RETURN-CODE                          
           SET WS-FIN-LECTURA TO TRUE                        
       END-IF                                                
       .                                                     
   9800-F-CERRAR-ARCHIVOS. EXIT.                             
                                                             
