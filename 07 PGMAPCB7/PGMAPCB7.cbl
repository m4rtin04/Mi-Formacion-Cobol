  CBL TEST                                                  
  IDENTIFICATION DIVISION.                                  
 *                                                        * 
   PROGRAM-ID PGMAPCB7.                                     
 ********************************************************** 
 *                                                        * 
 *  PROGRAMA DE APAREO                                    * 
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
                                                           
        SELECT MVNTOS  ASSIGN DDMVNTOS                 
        FILE STATUS IS FS-MVNTOS.                      
                                                       
        SELECT SALIDA  ASSIGN DDSALIDA                 
        FILE STATUS IS FS-SALIDA.                      
                                                       
                                                       
  DATA DIVISION.                                       
  FILE SECTION.                                        
  FD CLIENTE                                           
      RECORDING MODE IS F.                             
                                                       
  01 REG-CLIENTE            PIC X(30).                 
                                                       
  FD MVNTOS                                            
      RECORDING MODE IS F.                             
                                                       
  01 REG-MVNTOS             PIC X(80).                 
                                                       
  FD SALIDA                                            
      RECORDING MODE IS F.                             
                                                       
  01 REG-SALIDA             PIC X(30).                 
 **************************************                      
   WORKING-STORAGE SECTION.                                   
 **************************************                      
 **************************************************          
 *              FILE STATUS                       *          
 **************************************************          
   77  FS-CLIENTE        PIC XX    VALUE SPACES.              
   77  FS-MVNTOS         PIC XX    VALUE SPACES.              
   77  FS-SALIDA         PIC XX    VALUE SPACES.              
                                                              
   01  WS-STATUS-FIN    PIC X.                                
       88  WS-FIN-LECTURA         VALUE 'Y'.                  
       88  WS-NO-FIN-LECTURA      VALUE 'N'.                  
                                                              
   01  FS-REG-CLIENTE   PIC X.                                
       88  WS-FIN-CLIENTE         VALUE 'Y'.                  
       88  WS-NO-FIN-CLIENTE      VALUE 'N'.                  
                                                              
   01  FS-REG-MVNTOS    PIC X.                                
       88  WS-FIN-MVNTOS          VALUE 'Y'.                  
       88  WS-NO-FIN-MVNTOS       VALUE 'N'.                  
                                                              
                                                              
  *********************************************************** 
  *                   CLAVE DE APAREO                       * 
   ***********************************************************     
  01 WS-CLAVE-CLIENTE.                                           
     02 CLIENTE-TIPO                   PIC 9(02)  VALUE ZEROS.   
     02 CLIENTE-CUENTA                 PIC 9(08)  VALUE ZEROS.   
                                                                 
                                                                 
  01 WS-CLAVE-MVNTO.                                             
     02 MVNTO-TIPO                     PIC 9(02)  VALUE ZEROS.   
     02 MVNTO-CUENTA                   PIC 9(08)  VALUE ZEROS.   
                                                                 
 *************************************************************   
 *                    AREA DE COPYS                          *   
 *************************************************************   
  COPY MOVIMCC.                                                  
                                                                 
  COPY CLIENTE.                                                  
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
                                                                   
      PERFORM 1600-I-LEER-MOVIMIENTOS THRU 1600-F-LEER-MOVIMIENTOS 
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
                                                               
          WHEN WS-CLAVE-CLIENTE = WS-CLAVE-MVNTO               
              PERFORM 2200-I-ACTUALIZAR-SALDO THRU             
                               2200-F-ACTUALIZAR-SALDO         
                                                               
              PERFORM 1600-I-LEER-MOVIMIENTOS THRU             
                                1600-F-LEER-MOVIMIENTOS        
                                                               
          WHEN WS-CLAVE-CLIENTE > WS-CLAVE-MVNTO               
              DISPLAY ' '                                      
              DISPLAY '* CLAVE DE MOVIMIENTO NO ENCONTRADA *'  
              DISPLAY '- NUMERO DE MOVIMIENTO: ' WS-MOV-NRO    
              DISPLAY '- TIPO DE CUENTA: ' WS-MOV-TIPO         
              DISPLAY '- NUMERO DE CUENTA: ' WS-MOV-CUENTA     
              PERFORM 1600-I-LEER-MOVIMIENTOS THRU             
                                      1600-F-LEER-MOVIMIENTOS         
                                                                  
          WHEN WS-CLAVE-CLIENTE < WS-CLAVE-MVNTO                  
              PERFORM 2400-I-GRABAR THRU 2400-F-GRABAR            
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
       .                                                          
   F-9999-FINAL. EXIT.                                            
                                                                  
   1200-I-ABRIR-ARCHIVOS.                                         
                                                                  
       OPEN INPUT CLIENTE                                         
       OPEN INPUT MVNTOS                                        
       OPEN OUTPUT SALIDA                                       
                                                               
      IF FS-CLIENTE  IS NOT EQUAL '00'                         
         DISPLAY '* ERROR EN OPEN CLIENTE: ' FS-CLIENTE        
         MOVE 9999 TO RETURN-CODE                              
         SET  WS-FIN-LECTURA TO TRUE                           
      END-IF                                                   
                                                               
      IF FS-MVNTOS  IS NOT EQUAL '00'                          
         DISPLAY '* ERROR EN OPEN MOVIMIENTOS: ' FS-MVNTOS     
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
           READ CLIENTE  INTO WS-REG-CLIENTE                         
                                                                 
       EVALUATE FS-CLIENTE                                       
           WHEN '00'                                             
               MOVE WS-CLI-TIPO     TO CLIENTE-TIPO              
               MOVE WS-CLI-CUENTA   TO CLIENTE-CUENTA            
           WHEN '10'                                             
               SET WS-FIN-CLIENTE   TO TRUE                      
               MOVE HIGH-VALUES     TO WS-CLAVE-CLIENTE          
                                                                 
           WHEN OTHER                                            
               DISPLAY 'ERROR EN READ CLIENTES: ' FS-CLIENTE     
               MOVE 9999 TO RETURN-CODE                          
               SET WS-FIN-LECTURA TO TRUE                        
       END-EVALUATE                                              
       .                                                         
   1400-F-LEER-CLIENTE. EXIT.                                    
                                                                 
   1600-I-LEER-MOVIMIENTOS.                                      
                                                                 
       READ MVNTOS   INTO WS-REG-MOVIMI                          
                                                                 
       EVALUATE FS-MVNTOS                                        
           WHEN '00'                                             
               MOVE WS-MOV-TIPO     TO MVNTO-TIPO                
                   MOVE WS-MOV-CUENTA   TO MVNTO-CUENTA           
                                                              
           WHEN '10'                                          
               SET WS-FIN-MVNTOS    TO TRUE                   
               MOVE HIGH-VALUES     TO WS-CLAVE-MVNTO         
                                                              
           WHEN OTHER                                         
               DISPLAY 'ERROR EN READ MOVIMIENTOS: ' FS-MVNTOS
               MOVE 9999 TO RETURN-CODE                       
               SET WS-FIN-LECTURA TO TRUE                     
       END-EVALUATE                                           
       .                                                      
   1600-F-LEER-MOVIMIENTOS. EXIT.                             
                                                              
   2200-I-ACTUALIZAR-SALDO.                                   
                                                              
       COMPUTE WS-CLI-SALDO = WS-CLI-SALDO + WS-MOV-IMPORTE   
       .                                                      
   2200-F-ACTUALIZAR-SALDO. EXIT.                             
                                                              
   2400-I-GRABAR.                                             
                                                              
       WRITE REG-SALIDA FROM WS-REG-CLIENTE                   
                                                              
       IF FS-SALIDA IS NOT EQUAL '00'                         
              DISPLAY 'ERROR EN WRITE SALIDA: ' FS-SALIDA       
          MOVE 9999 TO RETURN-CODE                          
          SET WS-FIN-LECTURA TO TRUE                        
      END-IF                                                
      .                                                     
  2400-F-GRABAR. EXIT.                                      
                                                            
  9600-I-CERRAR-ARCHIVOS.                                   
                                                            
      CLOSE CLIENTE                                         
      CLOSE MVNTOS                                          
      CLOSE SALIDA                                          
                                                            
      IF FS-CLIENTE IS NOT EQUAL '00'                       
         DISPLAY 'ERROR EN CLOSE CLIENTE: ' FS-CLIENTE      
         MOVE 9999 TO RETURN-CODE                           
         SET WS-FIN-LECTURA TO TRUE                         
      END-IF                                                
                                                            
      IF FS-MVNTOS  IS NOT EQUAL '00'                       
         DISPLAY 'ERROR EN CLOSE MOVIMIENTOS: ' FS-MVNTOS   
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
