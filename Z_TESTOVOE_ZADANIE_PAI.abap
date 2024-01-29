*&---------------------------------------------------------------------*
*&  Include           Z_TESTOVOE_ZADANIE_PAI
*&---------------------------------------------------------------------*


MODULE pai INPUT.
  TRY .
      go_app->pai(
        EXPORTING  iv_ucomm = sy-ucomm
      ).
    CATCH zcx_exc INTO go_exc.
      MESSAGE go_exc->get_text( ) TYPE 'I'.
  ENDTRY.
ENDMODULE.

MODULE exit INPUT.
*  SET SCREEN 0.
*  LEAVE SCREEN.
*  LEAVE PROGRAM.
ENDMODULE.
