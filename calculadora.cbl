      ******************************************************************
      * Author: vanessa dndll
      * Date: 14/11/2024
      * Purpose: minha primeira calculadora em COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculadora.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
         01 WS-VARIAVEIS.
           03 WS-N1                PIC 9(04) VALUE ZEROS.
           03 WS-N2                PIC 9(04) VALUE ZEROS.
           03 WS-RESULTADO         PIC 99V99 VALUE ZEROS.
           03 WS-OPERACAO          PIC X(01) VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P100-INICIALIZA
           PERFORM P500-CALC
           PERFORM P999-FIM
           .

       P000-ERRO.
           DISPLAY 'ERRO DE PROCESSAMENTO'
           PERFORM P999-FIM.

       P100-INICIALIZA.
           INITIALIZE WS-VARIAVEIS.

       P500-CALC.
           DISPLAY 'Digite o primeiro numero: '
           ACCEPT WS-N1

           DISPLAY 'Escolha a operacao: + - * /'
           ACCEPT WS-OPERACAO

           DISPLAY 'Digite o segundo numero: '
           ACCEPT WS-N2

           EVALUATE WS-OPERACAO
               WHEN '+'
                   ADD WS-N1 TO WS-N2 GIVING WS-RESULTADO
                          ON SIZE ERROR PERFORM P000-ERRO
                   END-ADD
                   DISPLAY 'RESULTADO DA SOMA: ' WS-RESULTADO

               WHEN '-'
                   SUBTRACT WS-N1 FROM WS-N2 GIVING WS-RESULTADO
                                 ON SIZE ERROR PERFORM P000-ERRO
                   END-SUBTRACT
                   DISPLAY 'RESULTADO DA SUBTRACAO: ' WS-RESULTADO

               WHEN '*'
                   MULTIPLY WS-N1 BY WS-N2 GIVING WS-RESULTADO
                               ON SIZE ERROR PERFORM P000-ERRO
                   END-MULTIPLY
                   DISPLAY 'RESULTADO DA MULTIPLICACAO: ' WS-RESULTADO

               WHEN '/'
                   IF WS-N1 EQUALS 0 OR WS-N2 EQUALS 0
                       DISPLAY 'Divisao por zero!'
                       PERFORM P000-ERRO
                   ELSE
                       DIVIDE WS-N1 BY WS-N2 GIVING WS-RESULTADO
                                 ON SIZE ERROR PERFORM P000-ERRO
                       END-DIVIDE
                       DISPLAY 'RESULTADO DA DIVISAO: ' WS-RESULTADO

               WHEN OTHER
                   DISPLAY 'OPERACAO INVALIDA!'
           END-EVALUATE
           .

       P999-FIM.

            STOP RUN.
       END PROGRAM calculadora.
