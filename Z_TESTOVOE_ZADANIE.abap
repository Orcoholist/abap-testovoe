*&---------------------------------------------------------------------*
*& Report  Z_TESTOVOE_ZADANIE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_testovoe_zadanie.

INCLUDE z_testovoe_zadanie_s01.
INCLUDE z_testovoe_zadanie_lif.

*INCLUDE z_testovoe_zadanie_model.
*INCLUDE z_testovoe_zadanie_viev.
INCLUDE z_testovoe_zadanie_app.

INCLUDE z_testovoe_zadanie_pbo.
INCLUDE z_testovoe_zadanie_pai.

INITIALIZATION.

  GET PARAMETER ID 'BUK' FIELD p_bukrs.

  MOVE :'I' TO s_bwtar-sign, 'EQ' TO s_bwtar-option, 101 TO s_bwtar-low. APPEND s_bwtar.
  MOVE :'I' TO s_bwtar-sign, 'EQ' TO s_bwtar-option, 241 TO s_bwtar-low. APPEND s_bwtar.

  TRY.
      go_app   = NEW lcl_app( ).
    CATCH zcx_exc INTO go_exc.
      MESSAGE go_exc->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
  ENDTRY.

START-OF-SELECTION.
  go_app->run( ).
