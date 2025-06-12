*&---------------------------------------------------------------------*
*& Report z_t_taler_statistic
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_statistic.

DATA: lo_statistic     TYPE REF TO zcl_taler_statistic,
      lv_json_orders   TYPE string,
      lv_json_tax      TYPE string,
      lv_json_net      TYPE string,
      lv_json_units    TYPE string,
      lv_line          TYPE string,
      lv_offset        TYPE i,
      lv_chunk_length  TYPE i VALUE 250,
      lv_total_length  TYPE i.

START-OF-SELECTION.

  CREATE OBJECT lo_statistic.

  TRY.
      " Orders Status
      WRITE: / '--- Order Status JSON ---'.
      lv_json_orders = lo_statistic->get_orders_status_json( iv_days = 7 ).
      lv_total_length = strlen( lv_json_orders ).
      lv_offset = 0.
      WHILE lv_offset < lv_total_length.
        DATA(lv_len_order) = lv_chunk_length.
        IF lv_offset + lv_chunk_length > lv_total_length.
          lv_len_order = lv_total_length - lv_offset.
        ENDIF.
        lv_line = lv_json_orders+lv_offset(lv_len_order).
        WRITE: / lv_line.
        lv_offset += lv_chunk_length.
      ENDWHILE.

      SKIP 2.

      " Tax Statistic
      WRITE: / '--- Tax Statistic JSON ---'.
      lv_json_tax = lo_statistic->get_tax_statistic_json( iv_days = 2 ).
      lv_total_length = strlen( lv_json_tax ).
      lv_offset = 0.
      WHILE lv_offset < lv_total_length.
        DATA(lv_len_tax) = lv_chunk_length.
        IF lv_offset + lv_chunk_length > lv_total_length.
          lv_len_tax = lv_total_length - lv_offset.
        ENDIF.
        lv_line = lv_json_tax+lv_offset(lv_len_tax).
        WRITE: / lv_line.
        lv_offset += lv_chunk_length.
      ENDWHILE.

      SKIP 2.

      " Net Statistic
      WRITE: / '--- Net Statistic JSON ---'.
      lv_json_net = lo_statistic->get_net_statistic_json( iv_days = 2 ).
      lv_total_length = strlen( lv_json_net ).
      lv_offset = 0.
      WHILE lv_offset < lv_total_length.
        DATA(lv_len_net) = lv_chunk_length.
        IF lv_offset + lv_chunk_length > lv_total_length.
          lv_len_net = lv_total_length - lv_offset.
        ENDIF.
        lv_line = lv_json_net+lv_offset(lv_len_net).
        WRITE: / lv_line.
        lv_offset += lv_chunk_length.
      ENDWHILE.

      SKIP 2.

      " Units Sold Statistic
      WRITE: / '--- Units Sold JSON ---'.
      lv_json_units = lo_statistic->get_units_sold_json( iv_days = 2 ).
      lv_total_length = strlen( lv_json_units ).
      lv_offset = 0.
      WHILE lv_offset < lv_total_length.
        DATA(lv_len_units) = lv_chunk_length.
        IF lv_offset + lv_chunk_length > lv_total_length.
          lv_len_units = lv_total_length - lv_offset.
        ENDIF.
        lv_line = lv_json_units+lv_offset(lv_len_units).
        WRITE: / lv_line.
        lv_offset += lv_chunk_length.
      ENDWHILE.

    CATCH cx_root INTO DATA(lx_error).
      WRITE: / 'Error:', lx_error->get_text( ).
  ENDTRY.
