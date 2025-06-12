*&---------------------------------------------------------------------*
*& Report z_t_taler_val_config
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_val_config.

START-OF-SELECTION.

  DATA(lo_validator) = NEW zcl_taler_general( ).

  TRY.
      lo_validator->validate_all_configs( ).
      WRITE: / 'Validation of all Taler configs completed successfully.'.
    CATCH cx_root INTO DATA(lx_error).
      WRITE: / 'An error occurred:', lx_error->get_text( ).
  ENDTRY.
