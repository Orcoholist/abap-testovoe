*&---------------------------------------------------------------------*
*&  Include           Z_TESTOVOE_ZADANIE_PBO
*&---------------------------------------------------------------------*

DATA: gv_ok_code LIKE sy-ucomm.
MODULE pbo OUTPUT.
  TRY .
      go_app->pbo( sy-dynnr ).

    CATCH zcx_exc INTO go_exc.
      MESSAGE go_exc->get_text( ) TYPE 'S'.
  ENDTRY.
ENDMODULE.



*&---------------------------------------------------------------------*
*&      Module  PBO_SUB  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0001 OUTPUT.
go_app->pbo_0001( ).

ENDMODULE.
