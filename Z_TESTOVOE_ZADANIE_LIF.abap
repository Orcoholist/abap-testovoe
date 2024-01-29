*&---------------------------------------------------------------------*
*&  Include           Z_TESTOVOE_ZADANIE_LIF
*&---------------------------------------------------------------------*
INTERFACE lif_screen.
  DATA: mv_dynnr TYPE sy-dynnr,
        mv_ucomm TYPE sy-ucomm.

  CONSTANTS: mk_uc_back TYPE sy-ucomm VALUE 'BACK'.

  METHODS pbo
    IMPORTING iv_dynnr TYPE sy-dynnr
    RAISING   zcx_exc.

  METHODS pai
    IMPORTING
*      VALUE(iv_dynnr) TYPE sy-dynnr
              VALUE(iv_ucomm) TYPE sy-ucomm
    RAISING   zcx_exc.
ENDINTERFACE.
