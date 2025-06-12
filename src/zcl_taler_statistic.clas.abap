CLASS zcl_taler_statistic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get_orders_status_json
      IMPORTING iv_days TYPE i DEFAULT 365
      RETURNING VALUE(rv_json) TYPE string.

    METHODS get_tax_statistic_json
      IMPORTING iv_days TYPE i DEFAULT 365
      RETURNING VALUE(rv_json) TYPE string.

    METHODS get_net_statistic_json
      IMPORTING iv_days TYPE i DEFAULT 365
      RETURNING VALUE(rv_json) TYPE string.

    METHODS get_units_sold_json
      IMPORTING iv_days TYPE i DEFAULT 365
      RETURNING VALUE(rv_json) TYPE string.



ENDCLASS.

CLASS zcl_taler_statistic IMPLEMENTATION.

  METHOD get_orders_status_json.

    TYPES: BEGIN OF ty_stat,
             key   TYPE string,
             count TYPE i,
           END OF ty_stat.

    DATA: lt_log         TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
          lt_days        TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
          lt_counts      TYPE STANDARD TABLE OF ty_stat WITH EMPTY KEY,
          ls_count       TYPE ty_stat,
          lv_cutoff_date TYPE d,
          lv_cutoff_str  TYPE string,
          lv_ts_string   TYPE string,
          lv_day         TYPE string,
          lv_state       TYPE string,
          lv_key         TYPE string,
          lv_count_str   TYPE string,
          rv_inner       TYPE string,
          rv_json_body   TYPE string.

    FIELD-SYMBOLS: <ls_log>   TYPE ztlr_order_log,
                   <ls_count> TYPE ty_stat.

    " Cutoff: 7 days ago
    lv_cutoff_date = sy-datum - iv_days.
    lv_cutoff_str  = lv_cutoff_date+0(8).

    " Load all logs
    SELECT * FROM ztlr_order_log INTO TABLE lt_log.

    LOOP AT lt_log ASSIGNING <ls_log>.
      lv_ts_string = |{ <ls_log>-sap_timestamp }|.

      IF strlen( lv_ts_string ) < 8.
        CONTINUE. " Skip invalid timestamps
      ENDIF.

      lv_day = lv_ts_string+0(8).

      IF lv_day < lv_cutoff_str.
        CONTINUE. " Skip if before cutoff
      ENDIF.

      " Track unique days
      READ TABLE lt_days WITH TABLE KEY table_line = lv_day TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        INSERT lv_day INTO TABLE lt_days.
      ENDIF.

      " Build aggregation key
      lv_state = <ls_log>-state.
      CONCATENATE lv_day lv_state INTO lv_key SEPARATED BY '_'.

      " Aggregate count
      READ TABLE lt_counts ASSIGNING <ls_count> WITH KEY key = lv_key.
      IF sy-subrc = 0.
        <ls_count>-count += 1.
      ELSE.
        ls_count-key = lv_key.
        ls_count-count = 1.
        APPEND ls_count TO lt_counts.
      ENDIF.
    ENDLOOP.

    " Assemble JSON output
    LOOP AT lt_days INTO lv_day.
      CLEAR rv_inner.

      LOOP AT lt_counts ASSIGNING <ls_count>.
        SPLIT <ls_count>-key AT '_' INTO DATA(key_day) lv_state.
        IF key_day = lv_day.
          lv_count_str = <ls_count>-count.
          CONCATENATE rv_inner '"' lv_state '": ' lv_count_str ',' INTO rv_inner.
        ENDIF.
      ENDLOOP.

      CONCATENATE rv_json_body '"' lv_day '": {' rv_inner '},' INTO rv_json_body.
    ENDLOOP.

    CONCATENATE '{"orders": {' rv_json_body '}}' INTO rv_json.
    REPLACE ALL OCCURRENCES OF ',}' IN rv_json WITH '}'.


  ENDMETHOD.

  METHOD get_tax_statistic_json.

  TYPES: BEGIN OF ty_day_tax,
           date      TYPE string,
           tax_total TYPE mwsbp,
         END OF ty_day_tax.

  DATA: lt_log         TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
        lt_result      TYPE STANDARD TABLE OF ty_day_tax WITH EMPTY KEY,
        lv_cutoff_date TYPE d,
        lv_day         TYPE string,
        lv_ts_string   TYPE string,
        lv_cutoff_str  TYPE string,
        lv_json_body   TYPE string.

  FIELD-SYMBOLS: <ls_log>  TYPE ztlr_order_log,
                 <ls_day>  TYPE ty_day_tax.

  lv_cutoff_date = sy-datum - iv_days.
  lv_cutoff_str = lv_cutoff_date+0(8).

  SELECT * FROM ztlr_order_log INTO TABLE lt_log.

  LOOP AT lt_log ASSIGNING <ls_log>.

    " Consider only 'paid' or 'completed' orders
    IF <ls_log>-state <> 'paid' AND <ls_log>-state <> 'completed'.
      CONTINUE.
    ENDIF.

    lv_ts_string = |{ <ls_log>-sap_timestamp }|.

    IF strlen( lv_ts_string ) < 8.
      CONTINUE.
    ENDIF.

    lv_day = lv_ts_string+0(8).
    IF lv_day < lv_cutoff_str.
      CONTINUE.
    ENDIF.

    " Aggregate tax by day
    READ TABLE lt_result ASSIGNING <ls_day> WITH KEY date = lv_day.
    IF sy-subrc = 0.
      <ls_day>-tax_total += <ls_log>-tax_amount.
    ELSE.
      APPEND VALUE #( date = lv_day tax_total = <ls_log>-tax_amount ) TO lt_result.
    ENDIF.

  ENDLOOP.

  " Assemble JSON
  LOOP AT lt_result ASSIGNING <ls_day>.
    DATA(lv_tax_str) = |{ <ls_day>-tax_total DECIMALS = 2 }|.
    CONCATENATE lv_json_body '"' <ls_day>-date '": ' lv_tax_str ',' INTO lv_json_body.
  ENDLOOP.

  CONCATENATE '{"tax_statistic": {' lv_json_body '}}' INTO rv_json.
  REPLACE ALL OCCURRENCES OF ',}' IN rv_json WITH '}'.

ENDMETHOD.

  METHOD get_net_statistic_json.

  TYPES: BEGIN OF ty_day_net,
           date      TYPE string,
           net_total TYPE netwr,
         END OF ty_day_net.

  DATA: lt_log         TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
        lt_result      TYPE STANDARD TABLE OF ty_day_net WITH EMPTY KEY,
        lv_cutoff_date TYPE d,
        lv_day         TYPE string,
        lv_ts_string   TYPE string,
        lv_cutoff_str  TYPE string,
        lv_json_body   TYPE string.

  FIELD-SYMBOLS: <ls_log> TYPE ztlr_order_log,
                 <ls_day> TYPE ty_day_net.

  lv_cutoff_date = sy-datum - iv_days.
  lv_cutoff_str = lv_cutoff_date+0(8).

  SELECT * FROM ztlr_order_log INTO TABLE lt_log.

  LOOP AT lt_log ASSIGNING <ls_log>.

    IF <ls_log>-state <> 'paid' AND <ls_log>-state <> 'completed'.
      CONTINUE.
    ENDIF.

    lv_ts_string = |{ <ls_log>-sap_timestamp }|.

    IF strlen( lv_ts_string ) < 8.
      CONTINUE.
    ENDIF.

    lv_day = lv_ts_string+0(8).
    IF lv_day < lv_cutoff_str.
      CONTINUE.
    ENDIF.

    READ TABLE lt_result ASSIGNING <ls_day> WITH KEY date = lv_day.
    IF sy-subrc = 0.
      <ls_day>-net_total += <ls_log>-amount.
    ELSE.
      APPEND VALUE #( date = lv_day net_total = <ls_log>-amount ) TO lt_result.
    ENDIF.

  ENDLOOP.

  " Assemble JSON
  LOOP AT lt_result ASSIGNING <ls_day>.
    DATA(lv_net_str) = |{ <ls_day>-net_total DECIMALS = 2 }|.
    CONCATENATE lv_json_body '"' <ls_day>-date '": ' lv_net_str ',' INTO lv_json_body.
  ENDLOOP.

  CONCATENATE '{"net_statistic": {' lv_json_body '}}' INTO rv_json.
  REPLACE ALL OCCURRENCES OF ',}' IN rv_json WITH '}'.

ENDMETHOD.


METHOD get_units_sold_json.

  TYPES: BEGIN OF ty_day_units,
           date       TYPE string,
           unit_count TYPE i,
         END OF ty_day_units.

  DATA: lt_logs     TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
        lt_prods    TYPE STANDARD TABLE OF ztlr_order_prod WITH EMPTY KEY,
        lt_result   TYPE STANDARD TABLE OF ty_day_units WITH EMPTY KEY,
        lv_cutoff   TYPE d,
        lv_day      TYPE string,
        lv_json     TYPE string.

  FIELD-SYMBOLS: <ls_log> TYPE ztlr_order_log,
                 <ls_prod> TYPE ztlr_order_prod,
                 <ls_day> TYPE ty_day_units.

  " Define cutoff date
  lv_cutoff = sy-datum - iv_days.

  " Read logs with timestamps and allowed states
  SELECT * FROM ztlr_order_log INTO TABLE lt_logs
    WHERE state IN ('paid', 'completed').

  LOOP AT lt_logs ASSIGNING <ls_log>.
    DATA(lv_date_str) = |{ <ls_log>-sap_timestamp }|.

    IF strlen( lv_date_str ) < 8.
      CONTINUE.
    ENDIF.

    lv_day = lv_date_str+0(8).

    IF lv_day < lv_cutoff+0(8).
      CONTINUE.
    ENDIF.

    " Read products for this sales order
    SELECT * FROM ztlr_order_prod
      INTO TABLE @lt_prods
      WHERE sales_order = @<ls_log>-sales_order.

    LOOP AT lt_prods ASSIGNING <ls_prod>.
      READ TABLE lt_result ASSIGNING <ls_day> WITH KEY date = lv_day.
      IF sy-subrc = 0.
        <ls_day>-unit_count += <ls_prod>-quantity.
      ELSE.
        APPEND VALUE #( date = lv_day unit_count = <ls_prod>-quantity ) TO lt_result.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

  " Assemble JSON
  LOOP AT lt_result ASSIGNING <ls_day>.
    DATA(lv_unit_str) = |{ <ls_day>-unit_count }|.
    CONCATENATE lv_json '"' <ls_day>-date '": ' lv_unit_str ',' INTO lv_json.
  ENDLOOP.

  CONCATENATE '{"units_sold": {' lv_json '}}' INTO rv_json.
  REPLACE ALL OCCURRENCES OF ',}' IN rv_json WITH '}'.

ENDMETHOD.


ENDCLASS.

