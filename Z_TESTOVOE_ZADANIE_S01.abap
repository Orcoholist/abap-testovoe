*&---------------------------------------------------------------------*
*&  Include           Z_TESTOVOE_ZADANIE_S01
*&---------------------------------------------------------------------*
CLASS lcl_app DEFINITION DEFERRED.



DATA:
  go_app  TYPE REF TO lcl_app,
  go_exc  TYPE REF TO zcx_exc.
*  go_spr  TYPE REF TO zcl_spr.
*  lr_date TYPE RANGE OF dats.

DATA:  ssc_bukrs TYPE bukrs,
       ssc_year  TYPE c LENGTH 4,
*       ssc_perio TYPE c LENGTH 255,
       ssc_perio TYPE string,
       ssc_bwtar TYPE c LENGTH 50,
       ssc_werks TYPE c LENGTH 255.


TABLES: bkpf, t001w, bseg,mseg.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs TYPE t001-bukrs OBLIGATORY ,          "БЕ
                p_year  TYPE gjahr OBLIGATORY DEFAULT 2021. "sy-datum(4).  "Год

SELECT-OPTIONS: s_perio FOR bkpf-monat OBLIGATORY DEFAULT 10   , "Месяц sy-datum+4(2)
                s_werks FOR t001w-werks DEFAULT 'KNOS' ,        "Завод
                s_bwtar FOR mseg-bwart OBLIGATORY MATCHCODE OBJECT h_t156. "Вид движения
SELECTION-SCREEN: END OF BLOCK b2.


*INITIALIZATION.

AT SELECTION-SCREEN ON s_perio.
  IF s_perio-low < 01 OR s_perio-low > 12.
    MESSAGE 'Месяц должен быть от 1 до 12' TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.

  IF s_perio-high IS NOT INITIAL.
    IF  s_perio-high < 01 OR s_perio-high > 12 .
      MESSAGE 'Месяц должен быть от 1 до 12' TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
