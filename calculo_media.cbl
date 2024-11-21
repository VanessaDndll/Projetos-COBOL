      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. calculo_media.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CADASTRO.
         03 NOME                   PIC X(40) VALUE SPACE.
         03 MATERIA                PIC X(20) VALUE SPACE.
         03 NOTA-1                 PIC S99V99 VALUE 0.
         03 NOTA-2                 PIC S99V99 VALUE ZERO.
         77 NOTA-3                 PIC S99V99 VALUE ZERO.
         77 NOTA-4                 PIC S99V99 VALUE ZERO.
         77 MEDIA                  PIC 99V99 VALUE ZERO.
         77 RESULTADO              PIC X(10) VALUE SPACE.
         77 REPETIR                PIC A(1) VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM P001-INICIALIZA.
           PERFORM P100-NM         THRU P100-FIM.
           PERFORM P200-NOTAS      THRU P200-FIM.
           PERFORM P500-MEDIA      THRU P500-FIM.
           PERFORM P700-REPETIR    THRU P700-FIM.


       P001-INICIALIZA.
           INITIALIZE CADASTRO
           .
       P100-NM.
           DISPLAY '---BEM VINDO AO SISTEMA.---'
           DISPLAY 'DIGITE O NOME DO ALUNO: '
           ACCEPT NOME

           PERFORM UNTIL NOME IS ALPHABETIC
           DISPLAY 'VALOR INVALIDO! DIGITE O NOME: '
           ACCEPT NOME
           END-PERFORM

           DISPLAY 'DIGITE O NOME DA MATERIA: '
           ACCEPT MATERIA

           PERFORM UNTIL MATERIA IS ALPHABETIC
           DISPLAY 'VALOR INVALIDO! DIGITE UMA MATERIA VALIDA: '
           ACCEPT MATERIA
           END-PERFORM
           .
       P100-FIM.

       P200-NOTAS.
      ******* NOTA 1
           DISPLAY 'DIGITE A 1° NOTA: '
           ACCEPT NOTA-1

           PERFORM UNTIL NOTA-1 >= 0 AND NOTA-1 <= 10
           DISPLAY 'VALOR INVALIDO! DIGITE UM VALOR ENTRE 0 E 10: '
           ACCEPT NOTA-1
           END-PERFORM

      ******* NOTA 2
           DISPLAY 'DIGITE A 2° NOTA: '
           ACCEPT NOTA-2

           PERFORM UNTIL NOTA-2 >= 0 AND NOTA-2 <= 10
           DISPLAY 'VALOR INVALIDO! DIGITE UM VALOR ENTRE 0 E 10: '
           ACCEPT NOTA-2
           END-PERFORM

      ******* NOTA 3
           DISPLAY 'DIGITE A 3° NOTA: '
           ACCEPT NOTA-3

           PERFORM UNTIL NOTA-3 >= 0 AND NOTA-3 <= 10
           DISPLAY 'VALOR INVALIDO! DIGITE UM VALOR ENTRE 0 E 10: '
           ACCEPT NOTA-3
           END-PERFORM

      ******* NOTA 4
           DISPLAY 'DIGITE A 4° NOTA: '
           ACCEPT NOTA-4

           PERFORM UNTIL NOTA-4 >= 0 AND NOTA-4 <= 10
           DISPLAY 'VALOR INVALIDO! DIGITE UM VALOR ENTRE 0 E 10: '
           ACCEPT NOTA-4
           END-PERFORM
           .
       P200-FIM.

       P500-MEDIA.
           COMPUTE MEDIA = (NOTA-1 + NOTA-2 + NOTA-3 +NOTA-4) / 4
           DISPLAY MEDIA

           IF MEDIA >= 7
               MOVE 'APROVADO!'     TO RESULTADO
           ELSE
               MOVE 'REPROVADO!'   TO RESULTADO
           END-IF
           .
       P500-FIM.

       P700-REPETIR.
           DISPLAY '** RESULTADO DO PROCESSAMENTO **'
           DISPLAY 'NOME DO ALUNO: ' NOME
           DISPLAY 'MATERIA: ' MATERIA
           DISPLAY 'MEDIA DE ' NOME ': ' MEDIA
           DISPLAY 'STATUS DE ' NOME ': ' RESULTADO

           DISPLAY 'GOSTARIA DE FAZER UMA NOVA MEDIA? S/N '
           ACCEPT REPETIR
           EVALUATE REPETIR
               WHEN 'S'
                   PERFORM P001-INICIALIZA
               WHEN 'N'
                   PERFORM P999-FINAL
               WHEN OTHER
                   DISPLAY 'VALOR INVALIDO'
           .
       P700-FIM.

       P999-FINAL.
           DISPLAY 'PROCESSAMENTO FINALIZADO.'
            STOP RUN.
       END PROGRAM calculo_media.
