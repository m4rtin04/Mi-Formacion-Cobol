 IDENTIFICATION DIVISION.                           
 PROGRAM-ID. PGMIMCB7.                              
                                                    
 ENVIRONMENT DIVISION.                              
 CONFIGURATION SECTION.                             
 SPECIAL-NAMES.                                     
     DECIMAL-POINT IS COMMA.                        
                                                    
 INPUT-OUTPUT SECTION.                              
 FILE-CONTROL.                                      
     SELECT CLIENTES ASSIGN DDCLIEN                 
               FILE STATUS IS FS-CLIENTES.          
                                                    
     SELECT LISTADO  ASSIGN DDLIST                  
               FILE STATUS IS FS-LISTADO.           
                                                    
 DATA DIVISION.                                     
 FILE SECTION.                                      
 FD CLIENTES                                        
     BLOCK CONTAINS 0 RECORDS                       
     RECORDING MODE IS F.                           
                                                    
 01 REG-CLIENTE  PIC X(50).                         
     FD LISTADO                                                   
      BLOCK CONTAINS 0 RECORDS                                 
      RECORDING MODE IS F.                                     
                                                               
  01 REG-LISTADO  PIC X(132).                                  
                                                               
  WORKING-STORAGE SECTION.                                     
                                                               
 **********************************************                
 *     VARIABLES                              *                
 **********************************************                
  01  WS-FECHA-SISTEMA.                                        
      02 WS-ANIO                       PIC 9(04) VALUE ZEROS.  
      02 WS-MES                        PIC 9(02) VALUE ZEROS.  
      02 WS-DIA                        PIC 9(02) VALUE ZEROS.  
                                                               
   01  WS-CONTROL-PAGINA.                                      
       02  WS-MAX-LINEAS                PIC 9(02)  VALUE 60.   
       02  WS-LINEA-ACTUAL              PIC 9(02)  VALUE ZEROS.
       02  WS-PAGINA-ACTUAL             PIC 9(02)  VALUE ZEROS.
                                                               
 **********************************************                
 *     FILE STATUS                            *                
 **********************************************                
  77 FS-CLIENTES    PIC X(02)      VALUE SPACES.             
  77 FS-LISTADO     PIC X(02)      VALUE SPACES.                
                                                                 
 **********************************************                 
 *   CONTROL DE CICLO                         *                 
 **********************************************                 
  77 WS-STATUS-REG  PIC X(01).                                  
       88 WS-NO-FIN-REGISTRO   VALUE 'Y'.                        
       88 WS-FIN-REGISTRO      VALUE 'N'.                        
                                                                 
  **********************************************                 
  * VARIABLES PARA EL CORTE                    *                 
  **********************************************                 
   01 WS-CLIS-TIP-DOC-ANT PIC X(02) VALUE SPACES.                
   01 WS-CLAVE-ACT        PIC X(02) VALUE SPACES.                
                                                                 
  **********************************************                 
  * ACUMULADORES                               *                 
  **********************************************                 
   01 ACM-ACUMULADORES.                                          
      02 ACM-IMPORTE        PIC S9(09)V99  COMP-3 VALUE ZEROS.   
      02 ACM-TOTAL          PIC S9(09)V99  COMP-3 VALUE ZEROS.   
  **********************************************                 
  * CONTADORES                                 *                 
  **********************************************                 
   01  CNT-CLI-LEIDOS       PIC 9(03)      VALUE ZEROS.          
   01  CNT-LISTADO-GRABADOS PIC 9(03)      VALUE ZEROS.         
  **********************************************                
  *         MASCARA DE EDICION                 *                
  **********************************************                
   01 MASCARAS-EDICION.                                         
       02 ACM-IMPORTE-SUC-EDIT  PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.       
       02 ACM-IMPORTE-TOTAL-EDIT  PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.     
                                                                
  **********************************************                
  *        AREA DE COPYS                       *                
  **********************************************                
   COPY CPCLIENS.                                               
                                                                
  **********************************************                
  *        FORMATO DE SUBTOTAL                 *                
  **********************************************                
   01  WS-SUBTOTAL.                                             
       02  FILLER          PIC X(66)   VALUE SPACES.            
       02  FILLER          PIC X(25)   VALUE 'SUBTOTAL TIPO DOCUMENTO: '.   
       02  SUBT-TIPO-CTA   PIC X(12).                           
       02  FILLER          PIC X(05)   VALUE SPACES.            
       02  SUBT-IMPORTE    PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.            
       02  FILLER          PIC X(15)   VALUE SPACES.            
  ***********************************************               
  *                    TITULO                   *          
  ***********************************************              
    01 WS-TITULO.                                               
       02 FILLER    PIC X(10)    VALUE SPACES.                  
       02 FILLER    PIC X(19)    VALUE 'DETALLE DE CLIENTES'.   
       02 FILLER    PIC X(11)    VALUE SPACES.                  
       02 FILLER    PIC X(07)    VALUE 'FECHA: '.               
       02 WS-FECHA  PIC X(10)   VALUE SPACES.                   
       02 FILLER    PIC X(13)    VALUE SPACES.                  
       02 FILLER    PIC X(08)    VALUE 'PAGINA: '.              
       02 WS-PAGINA PIC Z9     VALUE ZEROS.                     
       02 FILLER    PIC X(50)    VALUE SPACES.                  
                                                                
   ***********************************************              
   *                  SUBTITULO                  *              
   ***********************************************              
    01 WS-SUBTITULO.                                            
       02 FILLER     PIC X(18) VALUE ' TIPO DOCUMENTO | '.      
       02 FILLER     PIC X(20) VALUE ' NUMERO DOCUMENTO |'.     
       02 FILLER     PIC X(11) VALUE ' SUCURSAL |'.             
       02 FILLER     PIC X(17) VALUE ' TIPO DE CUENTA |'.       
       02 FILLER     PIC X(19) VALUE ' NUMERO DE CUENTA |'.     
       02 FILLER     PIC X(22) VALUE '       IMPORTE       |'.  
       02 FILLER     PIC X(13) VALUE '    FECHA   |'.           
       02 FILLER     PIC X(12) VALUE ' LOCALIDAD |'.            
       02 FILLER     PIC X(13) VALUE SPACES.              
   ***********************************************              
   *                 FORMATO DETALLE             *              
   ***********************************************                                                                
    01  WS-DETALLE.                                             
      02  FILLER          PIC X(05) VALUE SPACES.               
      02  DET-TIP-DOC     PIC X(02).                            
      02  FILLER          PIC X(10) VALUE '         |'.         
      02  DET-NRO-DOC     PIC 9(11).                            
      02  FILLER          PIC X(09) VALUE '        |'.          
      02  FILLER          PIC X(05) VALUE SPACES.               
      02  DET-SUC         PIC 9(02).                            
      02  FILLER          PIC X(07) VALUE '    |  '.            
      02  DET-TIPO-CTA    PIC X(12).                            
      02  FILLER          PIC X(09) VALUE '  |      '.          
      02  DET-NRO-CTA     PIC 9(03).                            
      02  FILLER          PIC X(10) VALUE '         |'.         
      02  DET-IMPORTE     PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99.             
      02  FILLER          PIC X(03) VALUE ' | '.                
      02  DET-FECHA       PIC X(10).                            
      02  FILLER          PIC X(03) VALUE ' | '.                
      02  DET-LOCALIDAD   PIC X(15).                            
      02  FILLER          PIC X(02) VALUE ' |'.                 
      02  FILLER          PIC X(14) VALUE SPACES.               
                                                               
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
                                                                 
       PERFORM 1600-I-OBTENER-FECHA THRU 1600-F-OBTENER-FECHA     
                                                                 
       PERFORM 1800-I-MOSTRAR-TITULO THRU 1800-F-MOSTRAR-TITULO   
                                                                 
       MOVE WS-CLAVE-ACT   TO WS-CLIS-TIP-DOC-ANT                                                                               
       .                                                        
   1000-F-INICIO. EXIT.                                          
                                                                
   2000-I-PROCESO.                                               
       EVALUATE TRUE                                             
                                                                
          WHEN WS-CLAVE-ACT = WS-CLIS-TIP-DOC-ANT               
              PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO
              PERFORM 2600-I-IMPRIMIR-REGISTRO THRU             
                               2600-F-IMPRIMIR-REGISTRO         
                                                                
          WHEN WS-CLAVE-ACT  IS NOT EQUAL WS-CLIS-TIP-DOC-ANT   
              PERFORM 2400-I-CORTE THRU 2400-F-CORTE            
                                                                
              PERFORM 2200-I-SUMAR-SALDO THRU 2200-F-SUMAR-SALDO
                                                                
              PERFORM 2600-I-IMPRIMIR-REGISTRO THRU             
                                  2600-I-IMPRIMIR-REGISTRO      
                                                                
       END-EVALUATE                                 
                                                                      
       PERFORM 1400-I-LEER-ARCHIVOS THRU 1400-F-LEER-ARCHIVOS       
       .                                                            
   2000-F-PROCESO. EXIT.                                            
                                                                   
   9999-I-FINAL.                                                    
                                                                   
       PERFORM 9800-I-CERRAR-ARCHIVOS THRU 9800-F-CERRAR-ARCHIVOS   
                                                                   
       DISPLAY '**************************************************' 
       DISPLAY '**************************************************' 
       DISPLAY '* CANTIDAD TOTAL DE REGISTROS LEIDOS: '             
                                         CNT-CLI-LEIDOS '     *'   
       DISPLAY '**************************************************' 
       DISPLAY '* CANTIDAD TOTAL DE REGISTROS GRABADOS: '           
                                         CNT-LISTADO-GRABADOS '  *'
       DISPLAY '**************************************************' 
       .                                                            
   9999-F-FINAL. EXIT.                                              
                                                                   
   1200-I-ABRIR-ARCHIVOS.                   
       OPEN INPUT CLIENTES                                         
       OPEN OUTPUT LISTADO                                         
                                                                   
       IF FS-CLIENTES IS NOT EQUAL '00'                            
           DISPLAY 'ERROR EN ABRIR REGISTRO SUCURSAL:' FS-CLIENTES 
           MOVE 9999 TO RETURN-CODE                                
           SET WS-FIN-REGISTRO TO TRUE                             
       END-IF                                                      
                                                                   
       IF FS-LISTADO  IS NOT EQUAL '00'                            
           DISPLAY 'ERROR EN ABRIR OUTPUT LISTADO:' FS-LISTADO     
           MOVE 9999 TO RETURN-CODE                                
           SET WS-FIN-REGISTRO TO TRUE  
       END-IF
       .                                                           
   1200-F-ABRIR-ARCHIVOS. EXIT.                                     
                                                                   
   1400-I-LEER-ARCHIVOS.                                            
                                                                   
       READ CLIENTES INTO REG-CLIENTES                              
                                                                   
       EVALUATE FS-CLIENTES                                         
          WHEN '00'                                                
              MOVE CLIS-TIP-DOC  TO WS-CLAVE-ACT                   
              ADD 1 TO CNT-CLI-LEIDOS                 
          WHEN '10'                                              
              SET WS-FIN-REGISTRO TO TRUE                        
              PERFORM 2400-I-CORTE THRU 2400-F-CORTE             
                                                                 
          WHEN OTHER                                             
              DISPLAY 'ERROR EN LECTURA DE SUCURSAL:' FS-CLIENTES
              MOVE 9999 TO RETURN-CODE                           
              SET WS-FIN-REGISTRO TO TRUE                        
                                                                 
      END-EVALUATE                                               
      .                                                          
  1400-F-LEER-ARCHIVOS. EXIT.                                    
                                                                 
  1600-I-OBTENER-FECHA.                                          
                                                                 
      ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD                 
                                                                 
      STRING WS-DIA   DELIMITED BY SIZE                          
                                                                 
                    "/"      DELIMITED BY SIZE                   
                                                                 
                    WS-MES   DELIMITED BY SIZE                   
                                                                 
                    "/"      DELIMITED BY SIZE                   
                     WS-ANIO  DELIMITED BY SIZE                   
                                                                
                INTO  WS-FECHA                                  
     END-STRING                                                 
     .                                                          
 1600-F-OBTENER-FECHA. EXIT.                                    
                                                                
 1800-I-MOSTRAR-TITULO.                                         
                                                                
     MOVE 0 TO WS-LINEA-ACTUAL                                  
                                                                
     ADD 1 TO WS-PAGINA-ACTUAL                                  
                                                                
     MOVE WS-PAGINA-ACTUAL TO WS-PAGINA                         
                                                                
     WRITE REG-LISTADO  FROM WS-TITULO  AFTER ADVANCING  PAGE   
                                                                
     WRITE REG-LISTADO  FROM WS-SUBTITULO AFTER ADVANCING 1 LINE
                                                                
     ADD 3 TO WS-LINEA-ACTUAL                                   
     .                                                          
 1800-F-MOSTRAR-TITULO. EXIT.                                   
                                                                
 2200-I-SUMAR-SALDO.                                           
     COMPUTE ACM-IMPORTE  = ACM-IMPORTE  + CLIS-IMPORTE        
                                                              
*    COMPUTE ACM-TOTAL = ACM-TOTAL + WS-SUC-IMPORTE            
     .                                                         
 2200-F-SUMAR-SALDO. EXIT.                                     
                                                              
 2400-I-CORTE.                                                 
                                                              
     MOVE SPACES TO REG-LISTADO                                
     WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                 
                                                              
     MOVE ALL "-" TO REG-LISTADO                               
                                                              
     WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                 
                                                              
     MOVE DET-TIP-DOC    TO SUBT-TIPO-CTA                      
     MOVE ACM-IMPORTE    TO SUBT-IMPORTE                       
                                                              
     WRITE REG-LISTADO  FROM WS-SUBTOTAL AFTER ADVANCING 1 LINE
                                                              
     MOVE ALL "-" TO REG-LISTADO                               
                                                              
     WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                 
     MOVE SPACES TO REG-LISTADO                                
       WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                    
                                                                 
    ADD 5 TO WS-LINEA-ACTUAL                                     
                                                                 
    MOVE ZEROS TO ACM-IMPORTE                                    
                                                                 
    MOVE WS-CLAVE-ACT TO WS-CLIS-TIP-DOC-ANT                     
    .                                                            
2400-F-CORTE. EXIT.                                              
                                                                 
2600-I-IMPRIMIR-REGISTRO.                                        
                                                                 
    IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                          
         PERFORM 1800-I-MOSTRAR-TITULO THRU 1800-F-MOSTRAR-TITULO
    END-IF                                                       
                                                                 
    MOVE CLIS-TIP-DOC      TO DET-TIP-DOC                        
    MOVE CLIS-NRO-DOC      TO DET-NRO-DOC                        
    MOVE CLIS-SUC          TO DET-SUC                            
    MOVE CLIS-TIPO         TO DET-TIPO-CTA                       
    MOVE CLIS-NRO          TO DET-NRO-CTA                        
    MOVE CLIS-IMPORTE      TO DET-IMPORTE                        
    MOVE CLIS-AAAAMMDD(7:2) TO DET-FECHA(1:2)                    
    MOVE '/'                TO DET-FECHA(3:1)                    
    MOVE CLIS-AAAAMMDD(5:2) TO DET-FECHA(4:2)                    
    MOVE '/'                TO DET-FECHA(6:1)                   
    MOVE CLIS-AAAAMMDD(1:4) TO DET-FECHA(7:4)                   
    MOVE CLIS-LOCALIDAD    TO DET-LOCALIDAD                     
                                                                  
    WRITE REG-LISTADO  FROM WS-DETALLE  AFTER ADVANCING 1 LINE  
                                                                  
      ADD 1 TO WS-LINEA-ACTUAL                                    
      ADD 1 TO CNT-LISTADO-GRABADOS                               
      .                                                           
  2600-F-IMPRIMIR-REGISTRO. EXIT.                                 
                                                                  
  9800-I-CERRAR-ARCHIVOS.                                         
                                                                  
      CLOSE CLIENTES                                              
      CLOSE LISTADO                                               
                                                                  
      IF FS-CLIENTES IS NOT EQUAL '00'                            
          DISPLAY 'ERROR EN CLOSE ARCHIVO: ' FS-CLIENTES          
          MOVE 9999 TO RETURN-CODE                                
          SET WS-FIN-REGISTRO TO TRUE                             
      END-IF                                                      
                                                                  
      IF FS-LISTADO IS NOT EQUAL '00'                             
          DISPLAY 'ERROR EN CLOSE LISTADO: ' FS-LISTADO           
          MOVE 9999 TO RETURN-CODE                   
           SET WS-FIN-REGISTRO TO TRUE
     END-IF                         
     .                              
 9800-F-CERRAR-ARCHIVOS. EXIT.      
