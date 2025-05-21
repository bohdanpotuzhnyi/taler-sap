*&---------------------------------------------------------------------*
*& Report Z_T_TALER_SETTINGS_INIT
*&---------------------------------------------------------------------*
*& Initializes Taler config if ZTLR_CONFIG is empty
*&---------------------------------------------------------------------*
REPORT z_t_taler_settings_init.

DATA: lt_config TYPE STANDARD TABLE OF ztlr_config,
      ls_config TYPE ztlr_config,
      lv_timestamp TYPE string.

"Check if config table is empty
SELECT * FROM ztlr_config INTO TABLE lt_config.

IF lt_config IS INITIAL.
  lv_timestamp = |{ sy-datum }_{ sy-uzeit }|.
  REPLACE ALL OCCURRENCES OF ':' IN lv_timestamp WITH ''.
  ls_config-conf_id = |{ sy-uname }_{ lv_timestamp }|.

  "Fill default configuration values
  ls_config-mandt            = sy-mandt.
  ls_config-taler_uri        = 'https://backoffice.talerintosap.us/'.
  ls_config-taler_username   = 'admin'.
  ls_config-taler_password   = 'TALERintoSAP2527'.
  ls_config-taler_cur_repl   = 'KUDOS'.
  ls_config-def_prod_desc    = 'Product from SAP'.
  ls_config-def_orded_desc   = 'Order from SAP'.
  ls_config-sap_pay_method   = 'D'.
  ls_config-sap_country      = 'DE'.
  ls_config-sap_sales_org    = 'DS00'.
  ls_config-sap_plant        = 'HD00'.
  ls_config-sap_stor_loc     = 'BIEL'.
  GET TIME STAMP FIELD ls_config-last_changed_on.
  ls_config-last_changed_by  = sy-uname.

  "Insert into database
  INSERT ztlr_config FROM ls_config.
  IF sy-subrc = 0.
    COMMIT WORK.
    WRITE: / 'Default configuration inserted.'.
  ELSE.
    WRITE: / 'Insert failed.'.
  ENDIF.
ELSE.
  WRITE: / 'Configuration already exists. No action taken.'.
ENDIF.
