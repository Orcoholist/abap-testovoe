*&---------------------------------------------------------------------*
*&  Include           Z_TESTOVOE_ZADANIE_APP
*&---------------------------------------------------------------------*



CLASS lcl_app DEFINITION FINAL.
    PUBLIC SECTION.

      TYPES BEGIN OF ty_s_item.
              INCLUDE TYPE zalv_item .
      TYPES : style      TYPE lvc_t_styl,
              cellcolor  TYPE lvc_t_scol,
              line_color TYPE char4,
              END OF ty_s_item.

      INTERFACES lif_screen.

      ALIASES: pbo FOR lif_screen~pbo,
               pai FOR lif_screen~pai.

      TYPES: BEGIN OF ty_s_ssc,
               p_bukrs    TYPE bukrs,
               p_year     TYPE gjahr,
               s_perio    TYPE RANGE OF bkpf-monat,
               s_werks    TYPE RANGE OF mseg-werks,
               s_bwart    TYPE RANGE OF mseg-bwart,
               lr_date    TYPE RANGE OF dats,
               lr_exclude TYPE RANGE OF dats,
             END OF ty_s_ssc.
      DATA rs_ssc TYPE ty_s_ssc.

      DATA: ms_ssc     TYPE ty_s_ssc.
      data mv_ucomm TYPE sy-ucomm.

      DATA:
        lt_fcat       TYPE lvc_t_fcat,
        ls_fcat       TYPE lvc_s_fcat,
        ms_layout_bot TYPE lvc_s_layo,
        lt_fcat_head  TYPE lvc_t_fcat,
        lt_exclude    TYPE ui_functions,
        lv_message    TYPE string,
        lv_text       TYPE string,
*      wa_layout    TYPE lvc_s_layo,
        wa_style      TYPE LINE OF lvc_t_styl,
        wa_colors     TYPE LINE OF lvc_t_scol,
        cellcolor     TYPE lvc_t_scol,
        w_cellcolor   TYPE lvc_s_scol,
*      wa_color     TYPE lvc_s_colo, "For cell color
        wa_cellcolor  TYPE TABLE OF lvc_s_scol. "For cell color
      DATA: lt_alv TYPE slis_t_fieldcat_alv.
      DATA: wa_alv TYPE slis_fieldcat_alv.
      DATA: wa_layout TYPE slis_layout_alv.
      DATA wa_color TYPE lvc_s_scol.
*      FIELD-SYMBOLS:   <gfs_fc> TYPE lvc_s_fcat,
*                       <gfs_ty> TYPE ty_s_item.
      DATA: gs_cellcolor TYPE lvc_s_scol.


      METHODS get_ssc
        RETURNING VALUE(rs_ssc) TYPE ty_s_ssc.

*      METHODS constructor.

      METHODS run.
*
*      ALIASES: mv_dynnr FOR lif_screen~mv_dynnr,
*               mv_ucomm FOR lif_screen~mv_ucomm.

      METHODS display
        EXCEPTIONS error.

      METHODS pbo_0001.

      METHODS uc_dis_refresh
        EXCEPTIONS error.

    PROTECTED SECTION.

      METHODS write_to_excel.

      METHODS user_command
        RAISING zcx_exc.

      METHODS uc_dis_alv
        EXCEPTIONS error.

      DATA:
*          gt_alv_item TYPE TABLE OF zalv_item,
        gt_alv_item TYPE TABLE OF ty_s_item,
        gt_alv_head TYPE TABLE OF zalv_head,
        ty_t_outtab TYPE TABLE OF zalv_item.

      DATA:
        mt_fcat         TYPE lvc_t_fcat,
        mt_sort         TYPE lvc_t_sort,
        mt_qinfo        TYPE lvc_t_qinf,
        mt_excluding    TYPE ui_functions,
        mo_cont         TYPE REF TO cl_gui_docking_container,
        ms_layout_head  TYPE lvc_s_layo,
        ls_layo         TYPE lvc_s_layo,
        ms_vari         TYPE disvariant,
        es_var          TYPE disvariant,
        mo_cont_det     TYPE REF TO cl_gui_docking_container,
        mo_cont_top     TYPE REF TO cl_gui_docking_container,
        eo_cont         TYPE REF TO cl_gui_custom_container,
        mo_splitter     TYPE REF TO cl_gui_splitter_container,
        mo_splitter_top TYPE REF TO cl_gui_splitter_container,
        mo_doc_top      TYPE REF TO cl_dd_document,
        mo_doc_head     TYPE REF TO cl_dd_document,
        go_container_1  TYPE REF TO cl_gui_container,
        go_container_2  TYPE REF TO cl_gui_container,
        mo_toolbar      TYPE REF TO cl_gui_toolbar.
      DATA: mv_container_name TYPE dd02l-tabname,
            mv_structure_name TYPE dd02l-tabname,
            mt_outtab         TYPE REF TO data,
            mt_alv_item       TYPE REF TO data,
            mt_alv_head       TYPE REF TO data,
            mt_outtab_det     TYPE REF TO data.
      DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
            g_alv_grid_ref     TYPE REF TO cl_gui_alv_grid.

      METHODS get_fcat
        IMPORTING iv_structure_name TYPE dd02l-tabname
        RETURNING
                  VALUE(rt_fcat)    TYPE lvc_t_fcat.

      METHODS get_excluding
        RETURNING VALUE(pt_return) TYPE ui_functions.

      METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

      METHODS handle_hotspot_click_header FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

      METHODS handle_hotspot_click_bottom FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

      METHODS handle_delayed_callback  FOR EVENT delayed_callback OF cl_gui_alv_grid .


      METHODS handle_double_click_bottom FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_column e_row es_row_no.

      METHODS handle_data_changed
        FOR EVENT data_changed
            OF cl_gui_alv_grid
        IMPORTING
            er_data_changed.
*          sender.


      METHODS   handle_f4
        FOR EVENT onf4
            OF cl_gui_alv_grid
        IMPORTING
            e_fieldname
            er_event_data
            es_row_no
            sender.


      METHODS update_zmm_mseg_det
        EXPORTING ls_mseg_det TYPE zmm_mseg_det.

      METHODS handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .

      METHODS handle_delayed_changed_sel_cb
           FOR EVENT delayed_changed_sel_callback
           OF cl_gui_alv_grid.
      METHODS set_delay_time.

      METHODS click FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.

      METHODS : evh_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid IMPORTING sender,
        evh_del_change_selection FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.



      METHODS free
        EXCEPTIONS error.

      METHODS get_item
        EXPORTING ev_mblnr         TYPE mkpf-mblnr
        RETURNING VALUE(po_return) TYPE REF TO data.

      METHODS get_header
        RETURNING VALUE(po_return) TYPE REF TO data.

      DATA:
       mo_grid    TYPE REF TO cl_gui_alv_grid.
      DATA: o_splitter_main    TYPE REF TO cl_gui_splitter_container,
            o_container_top    TYPE REF TO cl_gui_container,
            o_container_header TYPE REF TO cl_gui_container,
            o_container_bottom TYPE REF TO cl_gui_container.

      DATA: mo_grid_header TYPE REF TO cl_gui_alv_grid,
            mo_cont_header TYPE REF TO cl_gui_custom_container,
            mo_grid_bottom TYPE REF TO cl_gui_alv_grid,
            mo_cont_bottom TYPE REF TO cl_gui_custom_container.

*    METHODS constructor
*      IMPORTING i_parent TYPE REF TO cl_gui_container.


      METHODS set_outtab
        IMPORTING it_outtab TYPE REF TO data.

    PRIVATE SECTION.



  ENDCLASS.


  CLASS lcl_app IMPLEMENTATION.

*    METHOD constructor.*
**      MESSAGE 'const' TYPE 'I' DISPLAY LIKE 'I'.*
**      IF go_spr IS INITIAL.
**        go_spr = NEW #( ).
**      ENDIF.
*    ENDMETHOD.


    METHOD run.
      ms_ssc = get_ssc( ). "долгая загрущзка
      uc_dis_alv( ).
    ENDMETHOD.

*    METHOD pbo.
**    IMPORTING iv_dynn type sy-dynnr.
*    ENDMETHOD.

**
    METHOD pai.
*    IMPORTING VALUE(iv_ucomm) TYPE sy-ucomm.
*    RAISING   zcx_exc.


      mv_ucomm = iv_ucomm.
      user_command( ).

    ENDMETHOD.

    METHOD user_command.
*      RAISING zcx_exc.

      CASE mv_ucomm.
        WHEN 'REFRESH'.
          uc_dis_refresh( EXCEPTIONS error = 1 ).
        WHEN 'EXCEL'.
          write_to_excel( ).
        WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
          LEAVE TO SCREEN 0.
        WHEN  OTHERS.
*          LEAVE TO SCREEN 0.
      ENDCASE.

    ENDMETHOD.


    METHOD get_ssc.
*      EXCEPTIONS error.
      DATA: lv_perio     TYPE string,
            lv_perio_buf TYPE string,
            lv_bwtar     TYPE string,
            lv_werks     TYPE string,
            cur_bwtar    TYPE i,
            lv_date_beg  TYPE dats,
            lv_date_end  TYPE dats.


      rs_ssc = VALUE #( p_bukrs = p_bukrs
                         p_year  = p_year
                         s_perio = s_perio[]
                         s_werks = s_werks[]
                         s_bwart = s_bwtar[]
                         ).
      "заполняем месяц
      LOOP AT s_perio ASSIGNING FIELD-SYMBOL(<fs_perio>) .
        IF <fs_perio>-sign EQ 'E'.
          lv_date_beg = p_year && <fs_perio>-low && '01'.
          CALL FUNCTION 'Z_GET_MONTH_LAST_DAY'
            EXPORTING
              fday = lv_date_beg " Актуальная дата сервера приложений
            IMPORTING
              lday = lv_date_end.
          APPEND VALUE #( sign = 'E' option = 'EQ' low = lv_date_beg high = lv_date_end  ) TO rs_ssc-lr_exclude.
          APPEND VALUE #( sign = 'E' option = 'EQ' low = lv_date_beg high = lv_date_end  ) TO rs_ssc-lr_date.
*          APPEND VALUE #( sign = 'E' option = 'EQ' low = <fs_perio>-low ) TO lr_.
        ELSE.
          IF <fs_perio>-option EQ 'EQ'.


            lv_date_beg = p_year && <fs_perio>-low && '01'.
            IF sy-tfill > 1.
              CALL FUNCTION 'Z_GET_MONTH_LAST_DAY'
                EXPORTING
                  fday = lv_date_beg " Актуальная дата сервера приложений
                IMPORTING
                  lday = lv_date_end. " Дата для произв. календаря и календаря праздничных дней

              APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_date_beg high = lv_date_end ) TO rs_ssc-lr_date.

*            rs_ssc-lr_date = VALUE #( ( sign = 'I' option = 'EQ' low = lv_date_beg   high = lv_date_end ) ).
              lv_perio_buf = | { lv_date_beg+6(2) }.{ lv_date_beg+4(2) }.{ lv_date_beg(4) } - { lv_date_end+6(2) }.{ lv_date_end+4(2) }.{ lv_date_end(4) } |.
              ssc_perio = |{ ssc_perio },{ lv_perio_buf }|.
              SHIFT ssc_perio LEFT DELETING LEADING space.
              SHIFT ssc_perio LEFT DELETING LEADING ','.
            ELSE.
              CALL FUNCTION 'Z_GET_MONTH_LAST_DAY'
                EXPORTING
                  fday = lv_date_beg " Актуальная дата сервера приложений
                IMPORTING
                  lday = lv_date_end. " Дата для произв. календаря и календаря праздничных дней

*            rs_ssc-lr_date = VALUE #( ( sign = 'I' option = 'EQ' low =  lv_date_beg   high =  lv_date_end ) ).*
              APPEND VALUE #( sign = 'I' option = 'BT' low = lv_date_beg high = lv_date_end ) TO rs_ssc-lr_date.
              ssc_perio = | { lv_date_beg+6(2) }.{ lv_date_beg+4(2) }.{ lv_date_beg(4) } - { lv_date_end+6(2) }.{ lv_date_end+4(2) }.{ lv_date_end(4) } |.
              SHIFT ssc_perio LEFT DELETING LEADING space.
            ENDIF.
          ELSE.
*          CHECK <fs_perio>-sign EQ 'E'.
            lv_date_beg = p_year && s_perio-high && '01'.
            CALL FUNCTION 'Z_GET_MONTH_LAST_DAY'
              EXPORTING
                fday = lv_date_beg " Актуальная дата сервера приложений
              IMPORTING
                lday = lv_date_end. " Дата для произв. календаря и календаря праздничных дней


*          APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_date_beg high = lv_date_end ) TO lr_date.
            APPEND VALUE #( sign = 'I' option = 'BT' low = lv_date_beg high = lv_date_end ) TO rs_ssc-lr_date.
            rs_ssc-lr_date = VALUE #( ( sign = 'I' option = 'BT' low = p_year && s_perio-low && '01'    high =  lv_date_end ) ).
            ssc_perio = | 01.{ s_perio-low }.{ lv_date_beg(4) } - { lv_date_end+6(2) }.{ lv_date_end+4(2) }.{ lv_date_end(4) } |.
            SHIFT ssc_perio LEFT DELETING LEADING space.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " заполняем Вид движения
      IF s_bwtar-option = 'EQ'.
        LOOP AT s_bwtar ASSIGNING FIELD-SYMBOL(<fs_bwtar>).
          ssc_bwtar = |{ ssc_bwtar },{ <fs_bwtar>-low }|.
        ENDLOOP.
      ELSE.
        cur_bwtar = s_bwtar-low.
        LOOP AT s_bwtar ASSIGNING FIELD-SYMBOL(<fs_bwtar1>).
          WHILE cur_bwtar  NE  s_bwtar-high + 1.
            ssc_bwtar = |{  ssc_bwtar },{ cur_bwtar }|.
            cur_bwtar = cur_bwtar + 1.
          ENDWHILE.
        ENDLOOP.
      ENDIF.
      SHIFT  ssc_bwtar LEFT DELETING LEADING ','.
      SHIFT  ssc_bwtar LEFT DELETING LEADING space.

      "Завод
      IF s_werks-option = 'EQ'.
        LOOP AT s_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
          ssc_werks = |{ ssc_werks },{ <fs_werks>-low }|.
        ENDLOOP.
      ELSE.
      ENDIF.
      SHIFT ssc_werks LEFT DELETING LEADING ','.
      SHIFT ssc_werks LEFT DELETING LEADING space.

      SORT rs_ssc-lr_date BY sign DESCENDING.


      ssc_bukrs = rs_ssc-p_bukrs.
      ssc_year = rs_ssc-p_year.
*      ssc_perio = lv_perio.
*      ssc_werks = lv_werks.
*      ssc_bwtar = lv_bwtar.



    ENDMETHOD.


    METHOD write_to_excel.
      MESSAGE 'Вы нажали кнопку Excel' TYPE 'I'.
    ENDMETHOD.



    METHOD uc_dis_refresh.
*    ( EXCEPTIONS error = 1 ).
*      IF go_app IS BOUND .
        set_outtab( it_outtab =  get_item(
                           IMPORTING
                              ev_mblnr  = gt_alv_item[ 1 ]-mblnr
                         ) ).
        MESSAGE 'Вы нажали кнопку REFRESH' TYPE 'S'.
*      ENDIF.

    ENDMETHOD.

    METHOD uc_dis_alv.
      mt_alv_item = get_item( ).
      IF gt_alv_item[] IS INITIAL.
        MESSAGE 'Данные не найдены.' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        mt_alv_head = get_header( ).
        CALL SCREEN 0001.
      ENDIF.

    ENDMETHOD.

    METHOD free.
*    EXCEPTIONS error.
*    FREE: mt_outtab.

    ENDMETHOD.


    METHOD get_item .
**        EXPORTING ev_mblnr type MKPF-MBLNR
**      RETURNING VALUE(po_return) TYPE REF TO data.

      IF gt_alv_item[] IS INITIAL.
        SELECT m~mblnr
          ,m~mjahr
          ,m~zeile
          ,bwart
          ,shkzg
          ,m~matnr
          ,ma~maktx AS matnr_name
          ,m~werks
          ,m~lgort
          ,t~lgobe AS lgort_name
          ,charg
          ,bwtar
          ,menge
          ,meins
          ,waers
          ,budat_mkpf
*        ,det~xcheck AS check
*        ,det~fipex AS fipex
          FROM mseg  AS m
          INNER JOIN makt AS ma ON ma~matnr = m~matnr AND ma~spras = @sy-langu
          INNER JOIN t001l AS t ON t~werks = m~werks AND t~lgort = m~lgort
*        INNER JOIN zmm_mseg_det AS det ON det~mblnr = m~mblnr AND det~mjahr = m~mjahr AND det~zeile = m~zeile
          WHERE m~mjahr = @ms_ssc-p_year
          AND bwart IN @ms_ssc-s_bwart
           AND m~werks IN @ms_ssc-s_werks
          AND budat_mkpf IN @ms_ssc-lr_date
          AND bukrs = @p_bukrs
**      and m~mblnr = @ev_mblnr
          INTO CORRESPONDING FIELDS OF TABLE @gt_alv_item .
      ELSE.
        SELECT m~mblnr
    ,m~mjahr
    ,m~zeile
    ,bwart
    ,shkzg
    ,m~matnr
    ,ma~maktx AS matnr_name
    ,m~werks
    ,m~lgort
    ,t~lgobe AS lgort_name
    ,charg
    ,bwtar
    ,menge
    ,meins
    ,waers
    ,budat_mkpf
*  ,det~xcheck AS check
*  ,det~fipex AS fipex
    FROM mseg  AS m
    INNER JOIN makt AS ma ON ma~matnr = m~matnr AND ma~spras = @sy-langu
    INNER JOIN t001l AS t ON t~werks = m~werks AND t~lgort = m~lgort
*  INNER JOIN zmm_mseg_det AS det ON det~mblnr = m~mblnr AND det~mjahr = m~mjahr AND det~zeile = m~zeile
    WHERE m~mjahr = @ms_ssc-p_year
    AND bwart IN @ms_ssc-s_bwart
     AND m~werks IN @ms_ssc-s_werks
    AND budat_mkpf IN @ms_ssc-lr_date
          AND bukrs = @p_bukrs

        AND m~mblnr = @ev_mblnr
    INTO CORRESPONDING FIELDS OF TABLE @gt_alv_item.

      ENDIF.
*
      SELECT mblnr
        ,mjahr
        ,zeile
        ,xcheck
        ,fipex
        FROM zmm_mseg_det
         FOR ALL ENTRIES IN @gt_alv_item
      WHERE mblnr = @gt_alv_item-mblnr
        AND mjahr = @gt_alv_item-mjahr
        AND zeile = @gt_alv_item-zeile
        INTO TABLE @DATA(lt_mseg_det).

      IF lt_mseg_det[] IS NOT INITIAL.
        LOOP AT gt_alv_item ASSIGNING FIELD-SYMBOL(<fs_alv_item>).
          READ TABLE lt_mseg_det ASSIGNING FIELD-SYMBOL(<fs_mseg_det>) WITH KEY mblnr = <fs_alv_item>-mblnr zeile = <fs_alv_item>-zeile.
          IF sy-subrc = 0.
            <fs_alv_item>-check = <fs_mseg_det>-xcheck.
            <fs_alv_item>-fipex = <fs_mseg_det>-fipex.
          ENDIF.
          IF <fs_alv_item>-check = 'X'.
            CLEAR <fs_alv_item>-cellcolor.
            DO 2 TIMES.
              CLEAR gs_cellcolor.
              gs_cellcolor-fname = 'CHECK'.
              gs_cellcolor-color-col = '7'.
              gs_cellcolor-color-int = '1'.
              gs_cellcolor-color-inv = '0'.
              APPEND gs_cellcolor TO <fs_alv_item>-cellcolor.

              CLEAR gs_cellcolor.
              gs_cellcolor-fname = 'FIPEX'.
              gs_cellcolor-color-col = '7'.
              gs_cellcolor-color-int = '1'.
              gs_cellcolor-color-inv = '0'.
              APPEND gs_cellcolor TO <fs_alv_item>-cellcolor.
            ENDDO.
          ELSE.
            CLEAR <fs_alv_item>-cellcolor.
            DO 2 TIMES.
              CLEAR gs_cellcolor.
              gs_cellcolor-fname = 'CHECK'.
              gs_cellcolor-color-col = '0'.
              gs_cellcolor-color-int = '1'.
              gs_cellcolor-color-inv = '0'.
              APPEND gs_cellcolor TO <fs_alv_item>-cellcolor.

              CLEAR gs_cellcolor.
              gs_cellcolor-fname = 'FIPEX'.
              gs_cellcolor-color-col = '0'.
              gs_cellcolor-color-int = '1'.
              gs_cellcolor-color-inv = '0'.
              APPEND gs_cellcolor TO <fs_alv_item>-cellcolor.
            ENDDO.
          ENDIF.

          IF ms_ssc-lr_exclude[] IS NOT INITIAL.
            READ TABLE ms_ssc-lr_exclude ASSIGNING FIELD-SYMBOL(<fs_exclude>) WITH KEY low+4(2) = <fs_alv_item>-budat_mkpf+4(2) .
            IF sy-subrc = 0 .
              DELETE gt_alv_item .
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.

      po_return = REF #( gt_alv_item ).


    ENDMETHOD.

    METHOD  get_header.
*      RETURNING VALUE(po_return) TYPE REF TO data.

      "remove adjustment duplicates from
      " сделать общий select для

      SELECT mblnr
        ,mjahr
        ,m~blart
        ,bldat
        ,budat
        ,usnam
        ,t~ltext AS blart_name
        FROM mkpf AS m
        INNER JOIN t003t AS t ON t~blart = m~blart AND t~spras = @sy-langu
        FOR ALL ENTRIES IN @gt_alv_item
        WHERE budat  IN @ms_ssc-lr_date
        AND mblnr   =  @gt_alv_item-mblnr
        AND mjahr   = @gt_alv_item-mjahr
*        and bukrs = @p_bukrs
        INTO CORRESPONDING FIELDS OF TABLE @gt_alv_head.


      LOOP AT gt_alv_item ASSIGNING FIELD-SYMBOL(<fs_item>).
        IF sy-tabix > 1.
          DELETE gt_alv_item.
        ENDIF.
      ENDLOOP.

      SORT gt_alv_head .


      po_return = REF #( gt_alv_head ).

    ENDMETHOD.

    METHOD pbo.


    ENDMETHOD.


*  METHOD constructor.
*    super->constructor(
*      EXPORTING
*        i_parent          =  i_parent  ). " Parent Container
*    me->set_delay_change_selection( 10 ).
*  ENDMETHOD.


    METHOD pbo_0001.
      DATA: l_gui_titlebar TYPE char40,
            l_gui_pfstat   TYPE char40,
            lt_exclude     TYPE TABLE OF sy-ucomm,
            lv_text        TYPE string.



      lv_text = 'Документы движения материалов (тестовая программа)'.

*  clear gt_alv_item[].
*  clear gt_alv_head[].
      IF  gt_alv_item[] IS INITIAL AND  gt_alv_head[] IS INITIAL . "&& go_model->gt_alv_item[]
        DATA(hidden_buttons) = VALUE syucomm_t(
     ( 'REFRESH' )
     ( 'EXCEL' ) ).
        SET PF-STATUS 'MAIN' EXCLUDING hidden_buttons .
      ELSE.
        SET PF-STATUS 'MAIN'.
        SET TITLEBAR 'MAIN' WITH lv_text.
      ENDIF.

      IF mo_grid_bottom IS NOT BOUND.
        go_app->display( ).
      ENDIF.


      IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDMETHOD.

    METHOD display.

      ASSIGN mt_alv_item->* TO FIELD-SYMBOL(<lt_item>).
      ASSIGN mt_alv_head->* TO FIELD-SYMBOL(<lt_head>).
      ms_layout_head  = VALUE #( zebra = abap_true  cwidth_opt = abap_true ).
      ms_layout_bot  = VALUE #( zebra = abap_true  cwidth_opt = abap_true info_fname = 'LINE_COLOR' ctab_fname = 'CELLCOLOR' ).

      IF   mo_grid_header   IS INITIAL.
        lt_fcat     = get_fcat( iv_structure_name = 'ZALV_ITEM' ). "ГОТОВИМ ФИЛДКАТАЛОГ
        lt_exclude = get_excluding( ) .
        lt_fcat_head = get_fcat( iv_structure_name = 'ZALV_HEAD' ).

**CONT_BOTTOM
        CREATE OBJECT mo_cont_bottom
          EXPORTING
            container_name = 'CONT_BOTTOM'.  " Name of the Screen CustCtrl Name to Link Container To

        CREATE OBJECT mo_grid_bottom
          EXPORTING
            i_parent = mo_cont_bottom. " Parent Container

        wa_layout-coltab_fieldname = 'CELLCOLOR'.

        mo_grid_bottom->set_table_for_first_display(
       EXPORTING
         it_toolbar_excluding  = lt_exclude
         is_layout             =    ms_layout_bot " Layout
         CHANGING
           it_outtab           =    <lt_item>  " Output Table
           it_fieldcatalog     = lt_fcat
       ).

        SET HANDLER handle_hotspot_click_bottom      FOR mo_grid_bottom.
        SET HANDLER handle_double_click_bottom       FOR mo_grid_bottom.
        SET HANDLER handle_f4                        FOR mo_grid_bottom.
        SET HANDLER handle_delayed_changed_sel_cb    FOR mo_grid_bottom.

*        SET HANDLER handle_data_changed              FOR mo_grid_bottom.
*        SET HANDLER handle_user_command              FOR mo_grid_bottom.


        CALL METHOD mo_grid_bottom->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified. " Event ID   ДЛЯ ГАЛОЧКИ

        DATA lt_f4       TYPE lvc_t_f4.
        APPEND VALUE #(  fieldname = 'FIPEX'
                     register  = abap_true  ) TO lt_f4.
        mo_grid_bottom->register_f4_for_fields( it_f4 =  lt_f4 ).

*CONT_HEADER
        CREATE OBJECT mo_cont_header
          EXPORTING
            container_name = 'CONT_HEADER'.  " Name of the Screen CustCtrl Name to Link Container To

        CREATE OBJECT mo_grid_header
          EXPORTING
            i_parent = mo_cont_header. " Parent Container

        mo_grid_header->set_table_for_first_display(
        EXPORTING
          it_toolbar_excluding  = lt_exclude
          is_layout             =    ms_layout_head
          CHANGING
            it_outtab           =   <lt_head>  " Output Table
            it_fieldcatalog     = lt_fcat_head

        ).

        mo_grid_header->register_delayed_event( i_event_id = cl_gui_alv_grid=>mc_evt_delayed_move_curr_cell ). "отвечает за обновление по клику

        SET HANDLER handle_delayed_callback       FOR  mo_grid_header.
        SET HANDLER handle_hotspot_click_header   FOR mo_grid_header.
        SET HANDLER handle_hotspot_click          FOR mo_grid_header.

*      SET HANDLER handle_delayed_changed_sel_cb   FOR mo_grid_header. "1.5 sec delay
*      SET HANDLER handle_delayed_callback       FOR  mo_grid_bottom.
*      SET HANDLER handle_user_command  FOR mo_grid_bottom.
*      SET HANDLER click FOR ALL INSTANCES.
*      SET HANDLER evh_after_refresh FOR ALL INSTANCES.
*      mo_grid_header->set_delay_change_selection( 1 ).

      ELSE.
        mo_grid_header->refresh_table_display( ).
        mo_grid_bottom->refresh_table_display( ).
        CALL METHOD cl_gui_cfw=>flush.
      ENDIF.


    ENDMETHOD.

    METHOD set_outtab.
*    IMPORTING it_outtab         TYPE REF TO data.
      mt_outtab = it_outtab.

    ENDMETHOD.

    METHOD handle_hotspot_click_header.
*    IMPORTING e_row_id e_column_id es_row_no.
      DATA: lt_table  TYPE TABLE OF zfm_smmtr_alv,
            lr_reestr TYPE RANGE OF zmtrt_req_dgrp-reg_num.
      DATA: ls_cell TYPE  lvc_t_cell.
      DATA: message TYPE string.
      DATA lv_col TYPE c LENGTH 10.
      DATA lv_col_con TYPE c LENGTH 10.

      DATA: ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.

      mo_grid_header->get_selected_cells(
        IMPORTING
          et_cell =  ls_cell   " Selected Cells
      ).

      LOOP AT ls_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
        CASE <fs_cell>-col_id.
          WHEN 'MBLNR'.
            READ TABLE gt_alv_head INDEX <fs_cell>-row_id ASSIGNING FIELD-SYMBOL(<fs_head>).
            IF sy-subrc = 0.
              lv_col = <fs_cell>-col_id.
              lv_col_con = <fs_head>-mblnr.
              message = |Событие HOTSPOT поле { lv_col } = { lv_col_con } ALV GRID(заголовок)|.
              MESSAGE message TYPE 'I' DISPLAY LIKE 'I' .
            ENDIF.
          WHEN 'MJAHR'.
            MESSAGE 'todo' TYPE 'I'.
          WHEN OTHERS.
            MESSAGE 'todo' TYPE 'I'.
        ENDCASE.
      ENDLOOP.

    ENDMETHOD.
    METHOD handle_hotspot_click_bottom.

      DATA: ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.

*      mo_grid_header->get_selected_rows(
*         IMPORTING
*           et_index_rows =   ls_idx_r  " Indexes of Selected Rows
*           et_row_no     =   ls_row_no  " Numeric IDs of Selected Rows
*       ).

      READ TABLE gt_alv_item ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX e_row_id-index.
      CLEAR <fs_row>-cellcolor.

      IF <fs_row>-check = 'X'.
        <fs_row>-check = ''.
      ELSE.
        <fs_row>-check = 'X'.
      ENDIF.

      IF <fs_row>-check EQ 'X'.
        APPEND VALUE #( fname     = 'FIPEX'
                       color-col = 7
                       color-inv = 0
                       color-int = 1 )
                       TO <fs_row>-cellcolor.
        APPEND VALUE #( fname     = 'CHECK'
                      color-col = 7
                      color-inv = 0
                      color-int = 1 )
                      TO <fs_row>-cellcolor.
      ENDIF.
      mo_grid_bottom->refresh_table_display( ).

    ENDMETHOD.

    METHOD handle_hotspot_click .
*    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no .

*    MESSAGE 'do' TYPE 'I'.

    ENDMETHOD.

    METHOD click.
      MESSAGE 'click' TYPE 'I'.
    ENDMETHOD.
    METHOD evh_after_refresh.
*    MESSAGE 'evh_after_refresh' TYPE 'I'.

    ENDMETHOD.

    METHOD evh_del_change_selection .
      MESSAGE 'evh_del' TYPE 'I'.
    ENDMETHOD.

    METHOD handle_delayed_changed_sel_cb.

      DATA: ls_cell   TYPE  lvc_t_cell,
            ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.


      MESSAGE 'handle_delayed_changed_sel_cb' TYPE 'I'.

      mo_grid_header->get_selected_rows(
         IMPORTING
           et_index_rows =   ls_idx_r  " Indexes of Selected Rows
           et_row_no     =   ls_row_no  " Numeric IDs of Selected Rows
       ).
      mo_grid_header->get_selected_cells(
         IMPORTING
           et_cell =  ls_cell   " Selected Cells
       ).

      LOOP AT ls_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
        READ TABLE gt_alv_head INDEX <fs_cell>-row_id ASSIGNING FIELD-SYMBOL(<fs_head>).

        get_item(
          IMPORTING
            ev_mblnr  = <fs_head>-mblnr
          RECEIVING
            po_return = mt_alv_item
        ).

      ENDLOOP.

      mo_grid_bottom->refresh_table_display( ).

*  display( ).


    ENDMETHOD.



    METHOD set_delay_time.
*    me->set_delay_change_selection( 10 ).

    ENDMETHOD.

    METHOD get_excluding.
      pt_return = VALUE #(
*      ( cl_gui_alv_grid=>mc_fc_info )
        ( cl_gui_alv_grid=>mc_fc_loc_append_row )
        ( cl_gui_alv_grid=>mc_fc_loc_copy )
        ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
        ( cl_gui_alv_grid=>mc_fc_loc_cut )
        ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
        ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
        ( cl_gui_alv_grid=>mc_fc_loc_move_row )
        ( cl_gui_alv_grid=>mc_fc_loc_paste )
        ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
        ( cl_gui_alv_grid=>mc_fc_loc_undo )
*      ( cl_gui_alv_grid=>mc_fc_print )
*      ( cl_gui_alv_grid=>mc_fc_graph )
        ).
    ENDMETHOD.

    METHOD get_fcat.
*  *      RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

      DATA: lt_fcat  TYPE lvc_t_fcat,
            i_layout TYPE slis_layout_alv.
      FIELD-SYMBOLS <ls_fcat> TYPE lvc_s_fcat.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name   = iv_structure_name "Подаем имя структуры в виде текста
          i_bypassing_buffer = abap_true
        CHANGING
          ct_fieldcat        = lt_fcat
        EXCEPTIONS
          OTHERS             = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DEFINE macro_fill_stext.
        <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-reptext = &1.
      END-OF-DEFINITION.

      LOOP AT lt_fcat ASSIGNING <ls_fcat>.
        CASE <ls_fcat>-fieldname.
          WHEN 'MBLNR'.      macro_fill_stext 'Номер документа '.          <ls_fcat>-just  = 'C'. <ls_fcat>-hotspot = 'X'.
          WHEN 'MJAHR'.      macro_fill_stext 'Год'.                       <ls_fcat>-just  = 'C'.
          WHEN 'ZEILE'.      macro_fill_stext 'Позиция'.                   <ls_fcat>-just  = 'C'.
          WHEN 'BWART'.      macro_fill_stext 'Вид движения'.              <ls_fcat>-just  = 'C'.  "<ls_fcat>-edit_mask = '==ZSTMS'.
          WHEN 'SHKZG'.      macro_fill_stext 'дебет/кредит'.              <ls_fcat>-just  = 'C'. <ls_fcat>-no_out = 'X'.
          WHEN 'MATNR'.      macro_fill_stext 'Материал'.                  <ls_fcat>-just  = 'C'.
          WHEN 'MATNR_NAME'. macro_fill_stext 'Материал (наим.)'.          <ls_fcat>-just  = 'L'.
          WHEN 'WERKS'.      macro_fill_stext 'Завод'.                     <ls_fcat>-just  = 'C'.
          WHEN 'LGORT'.      macro_fill_stext 'Склад'.                     <ls_fcat>-just  = 'C'.
          WHEN 'LGORT_NAME'. macro_fill_stext 'Склад (наим.)'.             <ls_fcat>-just  = 'L'. "<ls_fcat>-do_sum = 'X'."<ls_fcat>-edit_mask = '==ZSTMS'.
          WHEN 'CHARG'.      macro_fill_stext 'Номер партии'.              <ls_fcat>-just  = 'C'.
          WHEN 'BWTAR'.      macro_fill_stext 'Вид оценки'.                <ls_fcat>-just  = 'C'.
          WHEN 'MENGE'.      macro_fill_stext 'Количество'.                <ls_fcat>-just  = 'R'.
          WHEN 'MEINS'.      macro_fill_stext 'Ед. изм.'.                  <ls_fcat>-just  = 'C'.
          WHEN 'WAERS'.      macro_fill_stext 'Код валюты.'.               <ls_fcat>-just  = 'C'.
          WHEN 'DMBTR'.      macro_fill_stext 'Сумма.'.                    <ls_fcat>-just  = 'R'.
          WHEN 'BUDAT_MKPF'. macro_fill_stext 'Дата проводки в документе'. <ls_fcat>-just  = 'C'.<ls_fcat>-no_out = 'X'.
          WHEN 'CHECK'.      macro_fill_stext 'Флаг'.                      <ls_fcat>-just  = 'C'.<ls_fcat>-edit = 'X'.<ls_fcat>-checkbox = 'X'.<ls_fcat>-col_opt = 'X'.<ls_fcat>-hotspot = 'X'.
          WHEN 'FIPEX'.      macro_fill_stext 'Фин. позиция'.              <ls_fcat>-just  = 'C'.<ls_fcat>-edit = 'X'.<ls_fcat>-f4availabl = 'X'.
          WHEN 'BLART'.      macro_fill_stext 'Вид документа'.             <ls_fcat>-just  = 'C'.<ls_fcat>-col_opt = 'X'.
          WHEN 'BLART_NAME'. macro_fill_stext 'Вид документа (наим.)'.     <ls_fcat>-just  = 'C'.
          WHEN 'BLDAT'.      macro_fill_stext 'Дата документа'.            <ls_fcat>-just  = 'C'.
          WHEN 'BUDAT'.      macro_fill_stext 'Дата проводки'.             <ls_fcat>-just  = 'C'.
          WHEN 'USNAM'.      macro_fill_stext 'Имя пользователя'.          <ls_fcat>-just  = 'L'.
          WHEN OTHERS.
            <ls_fcat>-tech  = abap_true.
            <ls_fcat>-no_out = 'X'.
            <ls_fcat>-col_opt = 'X'.

        ENDCASE.
      ENDLOOP.
      rt_fcat = lt_fcat.






    ENDMETHOD.

    METHOD handle_delayed_callback.
*      FOR EVENT delayed_callback OF cl_gui_alv_grid ,
      DATA: lt_index_rows TYPE lvc_t_row,
            lt_row_no     TYPE lvc_t_roid,
            ls_row_id     TYPE lvc_s_row,
            ls_col_id     TYPE lvc_s_col,
            lv_value      TYPE c,
            row_id        TYPE i.

      DATA: ls_cell   TYPE  lvc_t_cell,
            ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.

      mo_grid_header->get_selected_cells(
         IMPORTING
           et_cell =  ls_cell   " Selected Cells
       ).

      LOOP AT ls_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
        READ TABLE gt_alv_head INDEX <fs_cell>-row_id ASSIGNING FIELD-SYMBOL(<fs_head>).
        get_item(
          IMPORTING
            ev_mblnr  = <fs_head>-mblnr
          RECEIVING
            po_return = mt_alv_item
        ).

      ENDLOOP.

      mo_grid_bottom->refresh_table_display( ).

      row_id = ls_row_id-index.
*    me->refresh_alv_usl( row_id  ).
    ENDMETHOD.


    METHOD handle_double_click_bottom.
*    *    IMPORTING e_row_id e_column_id es_row_no.
      DATA: lt_table  TYPE TABLE OF zfm_smmtr_alv,
            lr_reestr TYPE RANGE OF zmtrt_req_dgrp-reg_num.
      DATA: ls_cell TYPE  lvc_t_cell.
      DATA: message TYPE string.
      DATA a TYPE c LENGTH 10.
      DATA b TYPE c LENGTH 10.

      mo_grid_bottom->get_selected_cells(
        IMPORTING
          et_cell =  ls_cell   " Selected Cells
      ).

      LOOP AT ls_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
        CASE <fs_cell>-col_id.
          WHEN 'MBLNR'.
            READ TABLE gt_alv_item INDEX <fs_cell>-row_id ASSIGNING FIELD-SYMBOL(<fs_item>).
            IF sy-subrc = 0.
              a = <fs_cell>-col_id.
              b = <fs_item>-mblnr.
              message = |Событие HOTSPOT поле { a } = { b } ALV GRID(позиции)|.
              MESSAGE message TYPE 'I' DISPLAY LIKE 'E' .
            ENDIF.
        ENDCASE.
      ENDLOOP.


    ENDMETHOD.


    METHOD handle_data_changed.

*      DATA val TYPE i.
*      DATA e_v TYPE char01.
*
*      DATA: ls_good TYPE lvc_s_modi.
*      DATA : error_in_data TYPE c VALUE space.

*
*    LOOP AT er_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<is_cells>).
*      READ TABLE go_model->gt_alv_item INDEX <is_cells>-row_id ASSIGNING FIELD-SYMBOL(<fs_alv_item>).
*      <fs_alv_item>-check = <is_cells>-value.
*    ENDLOOP.

    ENDMETHOD.
    METHOD update_zmm_mseg_det.
**EXPORTING ls_mseg_det type zmm_mseg_det.

      DATA lt_mseg_det TYPE TABLE OF zmm_mseg_det.

      IF ls_mseg_det IS NOT INITIAL.
        APPEND ls_mseg_det TO lt_mseg_det.
        MODIFY zmm_mseg_det FROM TABLE  lt_mseg_det.
      ENDIF.

      COMMIT WORK AND WAIT.

      IF sy-subrc = 0.
        MESSAGE 'Данные успешно сохранены в таблицу zmm_mseg_det.' TYPE 'S' .
      ENDIF.


    ENDMETHOD.

    METHOD handle_f4.

      TYPES:BEGIN OF ty_s_fipex,
              fikrs TYPE fmcit-fikrs,
              fipex TYPE fmcit-fipex,
              bezei TYPE fmcit-bezei,
            END OF ty_s_fipex.

      TYPES:BEGIN OF ty_s_show,
              fipex TYPE fmcit-fipex,
              bezei TYPE fmcit-bezei,
            END OF ty_s_show.

      DATA: ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.

      DATA:  lt_fipex    TYPE TABLE OF ty_s_fipex,
             lt_show     TYPE TABLE OF ty_s_show,
             lo_f4       TYPE REF TO cl_reca_gui_f4_popup,
             et_mseg_det TYPE zmm_mseg_det.

      SELECT fikrs
            , fipex
             ,bezei
        FROM fmcit
        WHERE spras = @sy-langu
        AND fikrs = 'KNOS'
*      AND ( fipex NE 'Z%' OR  fipex NE 'W%' AND fipex NE 'S%')
        INTO TABLE    @lt_fipex.
      SORT lt_fipex.
      DELETE lt_fipex WHERE ( fipex(1) CP 'Z' OR fipex(1) CP 'V' OR fipex(1) CP  'S' ).

      MOVE-CORRESPONDING lt_fipex TO lt_show.

      lo_f4 = cl_reca_gui_f4_popup=>factory_grid(
      id_title      = ''  " Заголовок окна
      id_key_field  = 'FIPEX'
      if_multi      = abap_false   " Единичный выбор
      it_f4value    = lt_show    " Вн. таблица с данными
    ).

*    handle_data_changed( ).

      lo_f4->display( IMPORTING et_result = lt_show ).

      FIELD-SYMBOLS: <fs_outtab> TYPE ty_s_item.

      READ TABLE lt_show
        ASSIGNING FIELD-SYMBOL(<ls_result>)
        INDEX 1.
      IF sy-subrc = 0.
        READ TABLE gt_alv_item INDEX es_row_no-row_id ASSIGNING <fs_outtab>.

        <fs_outtab>-fipex = <ls_result>-fipex.
        et_mseg_det-mblnr = <fs_outtab>-mblnr.
        et_mseg_det-mjahr = <fs_outtab>-mjahr.
        et_mseg_det-zeile = <fs_outtab>-zeile.
        et_mseg_det-xcheck = <fs_outtab>-check.
        et_mseg_det-fipex = <fs_outtab>-fipex.


        IF <fs_outtab>-check = 'X'.
          CLEAR <fs_outtab>-cellcolor.
          DO 2 TIMES.
*          <fs_alv_item>-line_color = 'C710'.
            CLEAR gs_cellcolor.
            gs_cellcolor-fname = 'CHECK'.
            gs_cellcolor-color-col = '7'.
            gs_cellcolor-color-int = '1'.
            gs_cellcolor-color-inv = '0'.
            APPEND gs_cellcolor TO <fs_outtab>-cellcolor.

            CLEAR gs_cellcolor.
            gs_cellcolor-fname = 'FIPEX'.
            gs_cellcolor-color-col = '7'.
            gs_cellcolor-color-int = '1'.
            gs_cellcolor-color-inv = '0'.
            APPEND gs_cellcolor TO <fs_outtab>-cellcolor.
          ENDDO.
        ELSE.
          CLEAR <fs_outtab>-cellcolor.
          DO 2 TIMES.
            CLEAR gs_cellcolor.
            gs_cellcolor-fname = 'CHECK'.
            gs_cellcolor-color-col = '0'.
            gs_cellcolor-color-int = '1'.
            gs_cellcolor-color-inv = '0'.
            APPEND gs_cellcolor TO <fs_outtab>-cellcolor.

            CLEAR gs_cellcolor.
            gs_cellcolor-fname = 'FIPEX'.
            gs_cellcolor-color-col = '0'.
            gs_cellcolor-color-int = '1'.
            gs_cellcolor-color-inv = '0'.
            APPEND gs_cellcolor TO <fs_outtab>-cellcolor.
          ENDDO.

        ENDIF.

        update_zmm_mseg_det(
          IMPORTING
            ls_mseg_det =  et_mseg_det
        ).

        er_event_data->m_event_handled = 'X'.

        mo_grid_bottom->refresh_table_display( ).



      ENDIF.
      lo_f4->free( ).

    ENDMETHOD.

    METHOD handle_user_command.

      DATA: ls_idx_r  TYPE  lvc_t_row,
            ls_row_no TYPE  lvc_t_roid.
*
      mo_grid_header->get_selected_rows(
        IMPORTING
          et_index_rows =   ls_idx_r  " Indexes of Selected Rows
          et_row_no     =   ls_row_no  " Numeric IDs of Selected Rows
      ).



      cl_gui_cfw=>set_new_ok_code(
        EXPORTING
          new_code = e_ucomm    " New OK_CODE
      ).
      cl_gui_cfw=>flush( ).

    ENDMETHOD.
  ENDCLASS.
