  IDENTIFICATION DIVISION.                      
  PROGRAM-ID. PGM4CCB7.                         
                                                
  ENVIRONMENT DIVISION.                         
  CONFIGURATION SECTION.                        
  SPECIAL-NAMES.                                
      DECIMAL-POINT IS COMMA.                   
                                                
  INPUT-OUTPUT SECTION.                         
  FILE-CONTROL.                                 
      SELECT SUCURSAL ASSIGN DDSUCUR            
                FILE STATUS IS FS-SUCURSAL.     
                                                
  DATA DIVISION.                                
  FILE SECTION.                                 
  FD SUCURSAL                                   
      BLOCK CONTAINS 0 RECORDS                  
      RECORDING MODE IS F.                      
                                                
  01 REG-SUCURSAL PIC X(20).                    
                                                
  WORKING-STORAGE SECTION.                      
                                                
 ********************************************** 
  *     FILE STATUS                            *              
 **********************************************              
  77 FS-SUCURSAL    PIC X(02)      VALUE SPACES.             
                                                             
 **********************************************              
 *   CONTROL DE CICLO                         *              
 **********************************************              
  77 WS-STATUS-REG  PIC X(01).                               
      88 WS-NO-FIN-REGISTRO   VALUE 'Y'.                     
      88 WS-FIN-REGISTRO      VALUE 'N'.                     
                                                             
 **********************************************              
 * VARIABLES PARA EL CORTE                    *              
 **********************************************              
  01 WS-SUC-NRO-ANT PIC 9(02) VALUE ZEROS.                   
  01 WS-CLAVE-ACT   PIC 9(02) VALUE ZEROS.                   
                                                             
  01 WS-TIPO-ACT    PIC 9(02) VALUE ZEROS.                   
  01 WS-TIPO-ANT    PIC 9(02) VALUE ZEROS.                   
                                                             
 **********************************************              
 *      ACUMULADORES                          *              
 **********************************************              
  01 ACM-ACUMULADORES.                                       
     02 ACM-IMPORTE-SUC    PIC S9(07)V99  COMP-3 VALUE ZEROS.
     02 ACM-TOTAL          PIC S9(09)V99  COMP-3 VALUE ZEROS.
     02 ACM-IMPORTE-TIPO   PIC S9(07)V99  COMP-3 VALUE ZEROS.
                                                             
 **********************************************              
 *         MASCARAS DE EDICION                 *             
 **********************************************              
  01 MASCARAS-EDICION.                                       
      02 ACM-IMPORTE-SUC-EDIT  PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.     
      02 ACM-IMPORTE-TIPO-EDIT   PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.   
      02 ACM-IMPORTE-TOTAL-EDIT  PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.   
                                                             
 **********************************************              
 *        AREA DE COPYS                       *              
 **********************************************              
  COPY CORTE.                                                
                                                             
  PROCEDURE DIVISION.                                        
                                                             
  MAIN-PROGRAM.                                              
                                                             
      PERFORM 1000-I-INICIO THRU  1000-F-INICIO              
                                                             
      PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO             
                          UNTIL WS-FIN-REGISTRO     
     
      PERFORM 9999-I-FINAL THRU 9999-F-FINAL                      
     .                                                           
  F-MAIN-PROGRAM. GOBACK.                                         
                                                                 
  1000-I-INICIO.                                                  
                                                                 
     SET WS-NO-FIN-REGISTRO TO TRUE                              
                                                                 
     PERFORM 1200-I-ABRIR-ARCHIVOS THRU 1200-F-ABRIR-ARCHIVOS    
                                                                 
     PERFORM 1400-I-LEER-ARCHIVOS THRU 1400-F-LEER-ARCHIVOS      
                                                                 
     MOVE WS-CLAVE-ACT   TO WS-SUC-NRO-ANT                       
     MOVE WS-TIPO-ACT    TO WS-TIPO-ANT                          
     
     DISPLAY 'SUCURSAL:' WS-SUC-NRO-ANT                          
     .                                                           
  1000-F-INICIO. EXIT.                                            
                                                                 
  2000-I-PROCESO.                                                 
                                                                 
     EVALUATE TRUE                                               
         WHEN WS-CLAVE-ACT = WS-SUC-NRO-ANT                      
                  AND WS-TIPO-ACT = WS-TIPO-ANT                  
                                                                 
             PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO  
                                                                  
         WHEN WS-CLAVE-ACT  =  WS-SUC-NRO-ANT                     
                 AND WS-TIPO-ACT IS NOT EQUAL WS-TIPO-ANT         
                                                                  
             PERFORM 2500-I-CORTE-TIPO THRU 2500-F-CORTE-TIPO     
                                                                  
             PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO   
                                                                  
         WHEN WS-CLAVE-ACT IS NOT EQUAL WS-SUC-NRO-ANT            
                                                                  
             PERFORM 2500-I-CORTE-TIPO THRU 2500-F-CORTE-TIPO     
             PERFORM 2400-I-CORTE  THRU 2400-F-CORTE              
             DISPLAY 'SUCURSAL:' WS-SUC-NRO-ANT                   
             PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO   
*            MOVE 0 TO ACM-IMPORTE-TIPO                           
*            PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO   
*            DISPLAY 'SUCURSAL:' WS-SUC-NRO-ANT                   
      END-EVALUATE                                                
                                                                  
                                                                  
     PERFORM 1400-I-LEER-ARCHIVOS THRU 1400-F-LEER-ARCHIVOS       
     .                                                            
 2000-F-PROCESO. EXIT.                                            
                                                                  
 9999-I-FINAL.                                                    
                                                                  
      PERFORM 9800-I-CERRAR-ARCHIVOS THRU 9800-F-CERRAR-ARCHIVOS  
                                                                  
      MOVE ACM-TOTAL TO ACM-IMPORTE-TOTAL-EDIT                    
      DISPLAY '******************************************'        
      DISPLAY 'TOTAL ACUMULADO:' ACM-IMPORTE-TOTAL-EDIT           
      .                                                           
  9999-F-FINAL. EXIT.                                             
                                                                  
  1200-I-ABRIR-ARCHIVOS.                                          
                                                                  
       OPEN INPUT SUCURSAL                                        
                                                                  
       IF FS-SUCURSAL IS NOT EQUAL '00'                           
           DISPLAY 'ERROR EN ABRIR REGISTRO SUCURSAL:' FS-SUCURSAL
           MOVE 9999 TO RETURN-CODE                               
           SET WS-FIN-REGISTRO TO TRUE                            
       END-IF                                                     
       .                                                          
  1200-F-ABRIR-ARCHIVOS. EXIT.                                    
                                                                  
  1400-I-LEER-ARCHIVOS.                                           
                                                                  
      READ SUCURSAL INTO WS-REG-SUCURSAL                          
      EVALUATE FS-SUCURSAL                                        
          WHEN '00'                                               
              MOVE WS-SUC-NRO TO WS-CLAVE-ACT                     
              MOVE WS-SUC-TIPC1 TO WS-TIPO-ACT                    
                                                                  
          WHEN '10'                                               
              SET WS-FIN-REGISTRO TO TRUE                         
              PERFORM 2500-I-CORTE-TIPO THRU 2500-F-CORTE-TIPO    
              PERFORM 2400-I-CORTE THRU 2400-F-CORTE              
                                                                  
          WHEN OTHER                                              
              DISPLAY 'ERROR EN LECTURA DE SUCURSAL:' FS-SUCURSAL 
              MOVE 9999 TO RETURN-CODE                            
              SET WS-FIN-REGISTRO TO TRUE                         
                                                                  
      END-EVALUATE                                                
      .                                                           
  1400-F-LEER-ARCHIVOS. EXIT.                                     
                                                                  
  2200-I-SUMAR-SALDO.                                             
                                                                  
      COMPUTE ACM-IMPORTE-SUC = ACM-IMPORTE-SUC + WS-SUC-IMPORTE  
                                                                  
      COMPUTE ACM-IMPORTE-TIPO = ACM-IMPORTE-TIPO + WS-SUC-IMPORTE

      COMPUTE ACM-TOTAL = ACM-TOTAL + WS-SUC-IMPORTE              
      .                                                           
  2200-F-SUMAR-SALDO. EXIT.                                       
                                                                  
  2400-I-CORTE.                                                   
                                                                  
      MOVE ACM-IMPORTE-SUC TO ACM-IMPORTE-SUC-EDIT                
 *    DISPLAY 'SUCURSAL:' WS-SUC-NRO-ANT                          
      DISPLAY '           TOTAL: ' ACM-IMPORTE-SUC-EDIT           
                                                                  
      MOVE 0 TO ACM-IMPORTE-SUC                                   
      MOVE WS-CLAVE-ACT TO WS-SUC-NRO-ANT                         
      .                                                           
  2400-F-CORTE. EXIT.                                             
                                                                  
  2500-I-CORTE-TIPO.                                              
                                                                  
      MOVE ACM-IMPORTE-TIPO TO ACM-IMPORTE-TIPO-EDIT              
      DISPLAY 'TIPO DE CUENTA ' WS-TIPO-ANT  ACM-IMPORTE-TIPO-EDIT
                                                                  
      MOVE 0 TO ACM-IMPORTE-TIPO                                  
      MOVE WS-TIPO-ACT TO WS-TIPO-ANT                             
      .                                                           
  2500-F-CORTE-TIPO. EXIT.                                        

  9800-I-CERRAR-ARCHIVOS.                               
                                                        
      CLOSE SUCURSAL                                    
                                                        
      IF FS-SUCURSAL IS NOT EQUAL '00'                  
          DISPLAY 'ERROR EN CLOSE ARCHIVO: ' FS-SUCURSAL
          MOVE 9999 TO RETURN-CODE                      
          SET WS-FIN-REGISTRO TO TRUE                   
      END-IF                                            
      .                                                 
  9800-F-CERRAR-ARCHIVOS. EXIT.                         
                                                             
 
