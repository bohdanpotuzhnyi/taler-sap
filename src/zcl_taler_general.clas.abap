CLASS zcl_taler_general DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS validate_config_entry
      IMPORTING
        is_config TYPE ztlr_config
      RETURNING
        VALUE(rv_correct) TYPE abap_bool.

    METHODS validate_all_configs.

    METHODS get_last_correct_config
      RETURNING VALUE(rs_config) TYPE ztlr_config.

    METHODS sync_all.

    METHODS add_notification
      IMPORTING
        iv_system_part   TYPE char21     " 'config' | 'inventory' | 'orders'
        iv_not_type      TYPE char20     " 'info'   | 'warning'  | 'error'
        iv_short_message TYPE char1024_cs
        iv_long_message  TYPE dstring
      RETURNING VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.

    CONSTANTS:
      co_regex_uri TYPE string VALUE
        '^(https?|http)://[A-Za-z0-9\.\-]+(:[0-9]+)?$'.

ENDCLASS.



CLASS zcl_taler_general IMPLEMENTATION.

  METHOD validate_config_entry.

    DATA: lv_uri_ok       TYPE abap_bool VALUE abap_false,
          lv_http_ok      TYPE abap_bool VALUE abap_false,
          lv_sap_ok       TYPE abap_bool VALUE abap_false,
          lv_target_uri   TYPE string,
          lv_mgnt_uri     TYPE string,
          lv_status       TYPE i,
          lv_json         TYPE string,
          lv_bsn          TYPE char1024_cs,
          lv_dummy        TYPE c LENGTH 1,
          lo_client       TYPE REF TO if_http_client,
          lv_long_message TYPE string,
          lv_auth         TYPE string,

          ls_cfg          TYPE ztlr_config.   " copy for update

*--------------------------------------------------------------------*
* A. Syntax & reachability of the base URI                           *
*--------------------------------------------------------------------*
*--- 1. URI syntax --------------------------------------------------*

    IF cl_abap_matcher=>create(
         pattern = co_regex_uri
         text    = is_config-taler_uri )->match( ).
      lv_uri_ok = abap_true.
    ELSE.
      " ► Invalid URI format ------------------------------------------
      CONCATENATE
        `{ "merchant_url":"` is_config-taler_uri `",`
          `"instance":"` is_config-taler_username `",`
          `"type":"error",`
          `"information":"Taler URI failed regex validation"}`
      INTO lv_long_message.


      me->add_notification(
        iv_system_part   = 'config'
        iv_not_type      = 'error'
        iv_short_message = |{ is_config-taler_username }, invalid URI|
        iv_long_message  = lv_long_message
      ).
    ENDIF.

*--- 2. HTTP availability & business-name check ---------------------*
    IF lv_uri_ok = abap_true.

      " private API root
      IF to_lower( is_config-taler_username ) = 'admin'.
        lv_target_uri = |{ is_config-taler_uri }/private/|.
      ELSE.
        lv_target_uri = |{ is_config-taler_uri }/instances/{ is_config-taler_username }/private/|.
      ENDIF.

      " management endpoint to get business-name
      lv_mgnt_uri = |{ is_config-taler_uri }/management/instances/{ is_config-taler_username }|.

      cl_http_client=>create_by_url(
        EXPORTING url    = lv_mgnt_uri
        IMPORTING client = lo_client ).

      IF lo_client IS BOUND.

        lv_auth = |Bearer secret-token:{ is_config-taler_password }|.

        lo_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
        lo_client->request->set_header_field( name = 'Accept'        value = 'application/json' ).
        lo_client->request->set_method( if_http_request=>co_request_method_get ).

        lo_client->send( ).
        lo_client->receive( ).
        lo_client->response->get_status( IMPORTING code = lv_status ).

        IF lv_status = 200.

          lv_json = lo_client->response->get_cdata( ).
          CLEAR lv_bsn.
          FIND REGEX '"name"\s*:\s*"([^"]+)"'
               IN lv_json SUBMATCHES lv_bsn.

          IF lv_bsn IS NOT INITIAL.
            lv_http_ok = abap_true.
          ENDIF.
        ELSE.
          CONCATENATE
          `{ "merchant_url":"` is_config-taler_uri `",`
            `"instance":"` is_config-taler_username `",`
            `"type":"error",`
            `"information":"Management endpoint unreachable or business-name missing"}`
          INTO lv_long_message.

          " ► HTTP endpoint unreachable / wrong credentials -------------
          me->add_notification(
            iv_system_part   = 'config'
            iv_not_type      = 'error'
            iv_short_message = |{ is_config-taler_username }, HTTP check failed|
            iv_long_message  = lv_long_message
          ).
        ENDIF.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------*
* B.  Cross-checks of SAP keys                                       *
*--------------------------------------------------------------------*
    DATA lv_subrc TYPE sy-subrc.

    SELECT SINGLE zlsch FROM t042z
      WHERE zlsch = @is_config-sap_pay_method
      INTO @lv_dummy.
    lv_subrc = sy-subrc.

    IF lv_subrc = 0.
      SELECT SINGLE land1 FROM t005
        WHERE land1 = @is_config-sap_country
        INTO @lv_dummy.
      lv_subrc = sy-subrc.
    ENDIF.

    IF lv_subrc = 0.
      SELECT SINGLE vkorg FROM tvko
        WHERE vkorg = @is_config-sap_sales_org
        INTO @lv_dummy.
      lv_subrc = sy-subrc.
    ENDIF.

    IF lv_subrc = 0.
      SELECT SINGLE werks FROM t001w
        WHERE werks = @is_config-sap_plant
        INTO @lv_dummy.
      lv_subrc = sy-subrc.
    ENDIF.

    IF lv_subrc = 0.
      SELECT SINGLE lgort
        FROM t001l
        WHERE werks = @is_config-sap_plant
          AND lgort = @is_config-sap_stor_loc
        INTO @lv_dummy.
      lv_subrc = sy-subrc.
    ENDIF.

    IF lv_subrc = 0.
      lv_sap_ok = abap_true.
    ELSE.
      CONCATENATE
        `{ "merchant_url":"` is_config-taler_uri `",`
          `"instance":"` is_config-taler_username `",`
          `"type":"error",`
          `"information":"At least one SAP key (pay method, country, sales org, plant or storage location) is not found"}`
      INTO lv_long_message.

      " ► One or more SAP keys invalid --------------------------------
      me->add_notification(
        iv_system_part   = 'config'
        iv_not_type      = 'error'
        iv_short_message = |{ is_config-taler_username }, SAP key check failed|
        iv_long_message  = lv_long_message
      ).
    ENDIF.

*--------------------------------------------------------------------*
* Final result + persist back to ZTLR_CONFIG                         *
*--------------------------------------------------------------------*
    rv_correct = COND abap_bool(
        WHEN lv_uri_ok = abap_true AND
             lv_http_ok = abap_true AND
             lv_sap_ok = abap_true
        THEN abap_true
        ELSE abap_false ).

    CONCATENATE
      `{ "merchant_url":"` is_config-taler_uri `",`
        `"instance":"` is_config-taler_username `",`
        `"type":"info",`
        `"information":"Configuration verified successfully"}`
    INTO lv_long_message.

    " ► All good – drop an informational note -------------------------
    IF rv_correct = abap_true.
      me->add_notification(
        iv_system_part   = 'config'
        iv_not_type      = 'info'
        iv_short_message = |{ is_config-taler_username }, configuration valid|
        iv_long_message  = lv_long_message
      ).
    ENDIF.

    " copy import structure, enrich & save
    ls_cfg = is_config.
    ls_cfg-taler_bsn     = lv_bsn.
    ls_cfg-taler_req_uri = lv_target_uri.
    ls_cfg-correct       = rv_correct.
    ls_cfg-check_timestamp = sy-datum && sy-uzeit.

    MODIFY ztlr_config FROM ls_cfg.

  ENDMETHOD.


  METHOD validate_all_configs.

    TYPES: BEGIN OF ty_key,
             taler_uri       TYPE ztlr_config-taler_uri,
             taler_username  TYPE ztlr_config-taler_username,
             taler_password  TYPE ztlr_config-taler_password,
             taler_cur_repl  TYPE ztlr_config-taler_cur_repl,
             def_prod_desc   TYPE ztlr_config-def_prod_desc,
             def_orded_desc  TYPE ztlr_config-def_orded_desc,
             sap_pay_method  TYPE ztlr_config-sap_pay_method,
             sap_country     TYPE ztlr_config-sap_country,
             sap_sales_org   TYPE ztlr_config-sap_sales_org,
             sap_plant       TYPE ztlr_config-sap_plant,
             sap_stor_loc    TYPE ztlr_config-sap_stor_loc,
           END OF ty_key.

    DATA: lt_config        TYPE STANDARD TABLE OF ztlr_config,
          ls_config        TYPE ztlr_config,
          lt_seen          TYPE HASHED TABLE OF ty_key
                              WITH UNIQUE KEY table_line,
          ls_key           TYPE ty_key,
          lt_duplicates    TYPE STANDARD TABLE OF ztlr_config.


    SELECT * FROM ztlr_config INTO TABLE lt_config.

*--------------------------------------------------------------------*
* 1. Detect and collect duplicates                                   *
*--------------------------------------------------------------------*
    LOOP AT lt_config INTO ls_config.

      ls_key = CORRESPONDING ty_key( ls_config ).

      READ TABLE lt_seen WITH TABLE KEY table_line = ls_key TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        " already seen → mark current row for deletion
        APPEND ls_config TO lt_duplicates.
      ELSE.
        INSERT ls_key INTO TABLE lt_seen.
      ENDIF.

    ENDLOOP.

*--------------------------------------------------------------------*
* 2. Physically delete duplicates (if any)                           *
*--------------------------------------------------------------------*
    IF lt_duplicates IS NOT INITIAL.
      DELETE ztlr_config FROM TABLE lt_duplicates.
      COMMIT WORK AND WAIT.
    ENDIF.

*--------------------------------------------------------------------*
* 3. Re-select clean list and validate each entry                    *
*--------------------------------------------------------------------*
    CLEAR lt_config.
    SELECT * FROM ztlr_config INTO TABLE lt_config.

    LOOP AT lt_config INTO ls_config.
      validate_config_entry( ls_config ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_last_correct_config.

    DATA: lt_config TYPE STANDARD TABLE OF ztlr_config,
        ls_config TYPE ztlr_config,
        lv_latest_ts TYPE timestamp,
        lv_ts        TYPE timestamp.

    CLEAR rs_config.
    lv_latest_ts = ''.

    " Fetch all correct configs
    SELECT * FROM ztlr_config
      INTO TABLE lt_config
      WHERE correct = abap_true.

    LOOP AT lt_config INTO ls_config.
      lv_ts = ls_config-check_timestamp.
      IF lv_ts > lv_latest_ts.
        lv_latest_ts = lv_ts.
        rs_config = ls_config.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD sync_all.

    DATA(lo_util)   = NEW zcl_taler_general( ).
    DATA(lo_order)  = NEW zcl_taler_order( ).
    DATA(lo_inv)    = NEW zcl_taler_inv_mgmt( ).
    DATA(ls_cfg)    = lo_util->get_last_correct_config( ).

    " 1. Validate all configs
    TRY.
        lo_util->validate_all_configs( ).
      CATCH cx_root INTO DATA(lx_valcfg).
        MESSAGE 'Error validating configs' TYPE 'E'.
        RETURN.
    ENDTRY.

    " 2. Process orders via object instance
    TRY.
        lo_order->update_taler_billing_docs( ).
      CATCH cx_root INTO DATA(lx_order).
        MESSAGE 'Error updating Taler billing docs' TYPE 'E'.
        RETURN.
    ENDTRY.

    " 3. Sync inventory using the config parameters
    TRY.
        IF ls_cfg IS NOT INITIAL.
          lo_inv->sync_inventory(
            EXPORTING
              p_plant = ls_cfg-sap_plant
              p_lgort = ls_cfg-sap_stor_loc ).
        ELSE.
          MESSAGE 'No valid config found for inventory sync' TYPE 'E'.
        ENDIF.
      CATCH cx_root INTO DATA(lx_inv).
        MESSAGE 'Error syncing inventory' TYPE 'E'.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD add_notification.

    CONSTANTS: gc_part_config    TYPE char21 VALUE 'config',
               gc_part_inventory TYPE char21 VALUE 'inventory',
               gc_part_orders    TYPE char21 VALUE 'orders',
               gc_type_info      TYPE char20 VALUE 'info',
               gc_type_warning   TYPE char20 VALUE 'warning',
               gc_type_error     TYPE char20 VALUE 'error'.

    TYPES:  tt_part  TYPE STANDARD TABLE OF char21 WITH EMPTY KEY,
            tt_type  TYPE STANDARD TABLE OF char20 WITH EMPTY KEY.

    DATA: lv_ts        TYPE timestamp,
          ls_note      TYPE ztlr_notes,
          lv_rand      TYPE i.

*--------------------------------------------------------------------*
* 1. Validate input                                                  *
*--------------------------------------------------------------------*
    CHECK iv_short_message IS NOT INITIAL.          "must have something to display

*--------------------------------------------------------------------*
* 2. Prepare note line                                               *
*--------------------------------------------------------------------*
    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_min = 1
        ran_int_max = 999999
      IMPORTING
        ran_int     = lv_rand.

    GET TIME STAMP FIELD lv_ts.
    ls_note-entry_id = |{ lv_ts }_{ sy-mandt }_{ iv_not_type }_{ lv_rand }|.

    ls_note-mandt          = sy-mandt.
    ls_note-system_part    = iv_system_part.
    ls_note-not_type       = iv_not_type.
    ls_note-short_message  = iv_short_message.
    ls_note-long_message   = iv_long_message.
    ls_note-timestamp      = lv_ts.

*--------------------------------------------------------------------*
* 3. Persist                                                         *
*--------------------------------------------------------------------*
    INSERT ztlr_notes FROM ls_note.
    rv_success = COND abap_bool( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


ENDCLASS.

