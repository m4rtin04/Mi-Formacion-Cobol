  IDENTIFICATION DIVISION.                                
  PROGRAM-ID PGMTACB7.                                    
 **************************************                   
 *      MANTENIMIENTO DE PROGRAMA     *                   
 **************************************                   
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
        SELECT PRODUCTO ASSIGN DDPRODUC                   
               FILE STATUS IS FS-PRODUCTO.                
                                                          
        SELECT RPRECIO  ASSIGN DDPRECIO                   
               FILE STATUS IS FS-PRECIO.                  
                                                          
  DATA DIVISION.       
  FILE SECTION.                                    
  FD PRODUCTO                                      
      BLOCK CONTAINS 0 RECORDS                    
      RECORDING MODE IS F.                        
                                                  
  01 REG-PRODUCTO.                                 
     03 COD-PRODUCTO     PIC 99.                  
     03 DENOMINACION     PIC X(30).               
                                                  
                                                  
  FD RPRECIO                                       
      BLOCK CONTAINS 0 RECORDS                    
      RECORDING MODE IS F.                        
                                                  
  01 REG-PRECIOS.                                  
    03 COD-PRODUCTO   PIC 99.                     
    03 PRECIO         PIC 9(3)V99.                
                                                  
 **************************************            
  WORKING-STORAGE SECTION.                         
 **************************************            
 **************************************************
 *                    FILE STATUS                 *
 **************************************************
  77  FS-PRODUCTO      PIC XX    VALUE SPACES.     
  77  FS-PRECIO        PIC XX    VALUE SPACES.                 
                                                                
  77 WS-FIN-PROD      PIC X VALUE 'N'.                         
       88 FIN-PROD      VALUE 'Y'.                              
       88 NO-FIN-PROD   VALUE 'N'.                              
                                                                
  77 WS-FIN-PRECIO    PIC X VALUE 'N'.                         
       88 FIN-PRECIO    VALUE 'Y'.                              
       88 NO-FIN-PRECIO VALUE 'N'.                              
                                                                
   01  WS-STATUS-FIN    PIC X.                                  
       88  WS-FIN-LECTURA         VALUE 'Y'.                    
       88  WS-NO-FIN-LECTURA      VALUE 'N'.                    
                                                                
  ******************************************************        
  *                    VECTOR                          *        
  ******************************************************        
   01  VECTOR-PRODUCTO.                                         
       03 V-ITEMS                   OCCURS 10 TIMES.            
          05 V-COD-PROD                   PIC 9(02).            
          05 V-DENOMINACION               PIC X(30).            
          05 V-PRECIO                     PIC 9(03)V99.         
                                                                
   01 INDICE-PRODUCTO  PIC 9(02) VALUE ZEROS.                   
  *******************************************************        
  *               BANDERA                               *        
  *******************************************************        
   77 ENCONTRADO       PIC X     VALUE 'N'.                      
  *******************************************************        
  *    LAYOUT REGISTRO PRODUCTOS Y REGISTRO PRECIOS     *        
  *******************************************************        
   01 WS-REG-PRODUCTO.                                           
     03 WS-COD-PRODUCTO   PIC 99.                               
     03 WS-DENOMINACION   PIC X(30).                            
                                                                
  01 WS-REG-PRECIO.                                             
     03 WS-COD-PRECIO     PIC 99.                               
     03 WS-PRECIO         PIC 9(3)V99.                          
 ********************************************************       
 *              MASCARAS DE EDICION                     *       
 ********************************************************       
  01 WS-PRECIO-MASC   PIC $ZZ9,99.                              
 ***************************************************************
  PROCEDURE DIVISION.                                           
 **************************************                         
 *                                    *                         
 *  CUERPO PRINCIPAL DEL PROGRAMA     *                         
 *                                    *                         
 **************************************                         
  MAIN-PROGRAM.                                                
                                                               
      PERFORM 1000-INICIO  THRU   F-1000-INICIO.               
                                                               
      PERFORM 2000-PROCESO  THRU  F-2000-PROCESO               
                  UNTIL  FIN-PROD.                                 
                                                               
      PERFORM 9999-FINAL    THRU  F-9999-FINAL.                
                                                               
  F-MAIN-PROGRAM. GOBACK.                                      
                                                               
 **************************************                        
 *                                    *                        
 *  CUERPO INICIO APERTURA ARCHIVOS   *                        
 *                                    *                        
 **************************************                        
  1000-INICIO.                                                 
                                                               
      SET WS-NO-FIN-LECTURA TO TRUE.                           
                                                               
      PERFORM 1200-I-ABRIR-ARCHIVOS THRU 1200-F-ABRIR-ARCHIVOS 
                                                               
      PERFORM 1400-I-LEER-PRODUCTOS THRU 1400-F-LEER-PRODUCTOS 
                                                               
      PERFORM 1600-I-LEER-PRECIOS  THRU 1600-F-LEER-PRECIOS    
      .                                                          
  F-1000-INICIO.   EXIT.                                         
                                                                 
 **************************************                          
  2000-PROCESO.                                                  
                                                                 
      PERFORM 2200-I-CARGAR-VECTOR THRU 2200-F-CARGAR-VECTOR     
                                                                 
      PERFORM 2400-I-ACTUALIZAR-PRECIOS THRU   2400-F-ACTUALIZAR-PRECIOS                                                         
      .                                                          
  F-2000-PROCESO. EXIT.                                          
                                                                 
 **************************************                          
 *                                    *                          
 *  CUERPO FINAL CIERRE DE FILES      *                          
 *                                    *                          
 **************************************                          
  9999-FINAL.                                                    
                                                                 
      PERFORM 9800-I-CERRAR-ARCHIVOS THRU 9800-F-CERRAR-ARCHIVOS 
                                                                 
      PERFORM 9900-I-MOSTRAR-TOTALES THRU 9900-F-MOSTRAR-TOTALES 
      .                                                          
  F-9999-FINAL. EXIT.                                            
                                                           
  1200-I-ABRIR-ARCHIVOS.                                
                                                         
      OPEN INPUT PRODUCTO                               
      OPEN INPUT RPRECIO                                
                                                         
      IF FS-PRODUCTO IS NOT EQUAL '00'                  
          DISPLAY '*ERROR EN OPEN PRODUCTO: ' FS-PRODUCTO
          MOVE 9999 TO RETURN-CODE                       
          SET  WS-FIN-LECTURA TO TRUE                    
      END-IF.                                           
                                                         
      IF FS-PRECIO  IS NOT EQUAL '00'                   
          DISPLAY '*ERROR EN OPEN PRECIO: ' FS-PRECIO    
          MOVE 9999 TO RETURN-CODE                       
          SET WS-FIN-LECTURA TO TRUE                     
      END-IF                                            
      .                                                 
  1200-F-ABRIR-ARCHIVOS. EXIT.                          
                                                         
  1400-I-LEER-PRODUCTOS.                                
                                                         
      READ PRODUCTO INTO  WS-REG-PRODUCTO               
                                                         
      EVALUATE FS-PRODUCTO                    
          WHEN '00'                                            
              CONTINUE                                         
          WHEN '10'                                            
              SET FIN-PROD  TO TRUE                            
          WHEN OTHER                                           
              DISPLAY '*ERROR EN READ PRODUCTO: ' FS-PRODUCTO  
              MOVE 9999 TO RETURN-CODE                         
              SET WS-FIN-LECTURA TO TRUE                       
       END-EVALUATE                                            
       .                                                       
   1400-F-LEER-PRODUCTOS. EXIT.                                
                                                               
   1600-I-LEER-PRECIOS.                                        
                                                               
       READ RPRECIO  INTO  WS-REG-PRECIO                       
                                                               
       EVALUATE FS-PRECIO                                      
          WHEN '00'                                            
              CONTINUE                                         
          WHEN '10'                                            
              SET FIN-PRECIO  TO TRUE                          
          WHEN OTHER                                           
              DISPLAY '*ERROR EN READ PRODUCTO: ' FS-PRECIO    
              MOVE 9999 TO RETURN-CODE                         
              SET WS-FIN-LECTURA TO TRUE                    
        END-EVALUATE                                                
        .                                                           
    1600-F-LEER-PRECIOS. EXIT.                                      
                                                                  
    2200-I-CARGAR-VECTOR.                                           
                                                                  
        PERFORM VARYING INDICE-PRODUCTO FROM 1 BY 1                 
                             UNTIL INDICE-PRODUCTO > 10           
                                                                  
 *        IF WS-FIN-LECTURA                                       
 *             DISPLAY 'ARCHIVO PRODUCTO TIENE MENOS DE 10 REGTRS'
 *             MOVE 9999 TO RETURN-CODE                           
 *             EXIT PERFORM                                       
 *        END-IF                                                  
                                                                  
          MOVE WS-COD-PRODUCTO TO V-COD-PROD (INDICE-PRODUCTO)    
          MOVE WS-DENOMINACION TO V-DENOMINACION (INDICE-PRODUCTO)
          MOVE 0   TO V-PRECIO (INDICE-PRODUCTO)         
                                                                  
          PERFORM 1400-I-LEER-PRODUCTOS THRU 1400-F-LEER-PRODUCTOS
      END-PERFORM                                                 
      .                                                           
  2200-F-CARGAR-VECTOR. EXIT.                                     
                                                                  
  2400-I-ACTUALIZAR-PRECIOS.                           
                                                                         
       PERFORM UNTIL FIN-PRECIO                                    
                                                                   
          MOVE 'N' TO ENCONTRADO                                   
                                                                   
          PERFORM VARYING INDICE-PRODUCTO FROM 1 BY 1              
                     UNTIL INDICE-PRODUCTO > 10 OR ENCONTRADO = 'S'
                                                                   
               IF V-COD-PROD (INDICE-PRODUCTO) = WS-COD-PRECIO     
                     MOVE WS-PRECIO TO V-PRECIO (INDICE-PRODUCTO)  
                     MOVE 'S' TO ENCONTRADO                        
               END-IF                                              
          END-PERFORM                                              
                                                                   
          IF ENCONTRADO  NOT  = 'S'                                
               DISPLAY "PRODUCTO NO ENCONTRADO: " WS-COD-PRECIO    
          END-IF                                                   
                                                                   
          PERFORM 1600-I-LEER-PRECIOS THRU 1600-F-LEER-PRECIOS     
                                                                   
       END-PERFORM                                                 
       .                                                           
   2400-F-ACTUALIZAR-PRECIOS. EXIT.                                
                                                                   
   9800-I-CERRAR-ARCHIVOS.                                         
                                                                    
       CLOSE PRODUCTO                                        
       CLOSE RPRECIO                                         
                                                             
       IF FS-PRODUCTO IS NOT EQUAL '00'                      
           DISPLAY '*ERROR EN CLOSE PRODUCTO: ' FS-PRODUCTO  
           MOVE 9999 TO RETURN-CODE                          
           SET WS-FIN-LECTURA TO TRUE                        
       END-IF                                                
                                                             
       IF FS-PRECIO  IS NOT EQUAL '00'                       
           DISPLAY '*ERROR EN CLOSE PRECIO: ' FS-PRECIO      
           MOVE 9999 TO RETURN-CODE                          
           SET WS-FIN-LECTURA TO TRUE                        
       END-IF                                                
       .                                                     
   9800-F-CERRAR-ARCHIVOS. EXIT.                             
                                                             
   9900-I-MOSTRAR-TOTALES.                                   
                                                             
       DISPLAY '=============================='              
       DISPLAY 'PRODUCTOS Y PRECIOS ACTUALIZADOS'            
       DISPLAY '=============================='              
                                                             
       PERFORM VARYING INDICE-PRODUCTO FROM 1 BY 1 UNTIL  INDICE-PRODUCTO > 10 
                                                 
          DISPLAY 'PRODUCTO NRO: ' INDICE-PRODUCTO                
          DISPLAY ' '                                             
          DISPLAY 'CODIGO: ' V-COD-PROD (INDICE-PRODUCTO)         
          DISPLAY ' '                                             
          DISPLAY 'DENOMINACION: ' V-DENOMINACION (INDICE-PRODUCTO)
          DISPLAY ' '                                             
         
          MOVE V-PRECIO (INDICE-PRODUCTO) TO WS-PRECIO-MASC       
          DISPLAY 'PRECIO: ' WS-PRECIO-MASC                       
          DISPLAY '****************************************'      
      END-PERFORM                                                 
      .                                                           
  9900-F-MOSTRAR-TOTALES. EXIT.                                   
