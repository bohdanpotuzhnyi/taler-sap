*&---------------------------------------------------------------------*
*& Report z_t_taler_inv_mgmt_gmd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_inv_mgmt_gmd.

PARAMETERS: p_matnr TYPE char18 DEFAULT 'TALER_BOTTLE01',
            p_plant TYPE werks_d DEFAULT 'HD00',
            p_lgort TYPE lgort_d DEFAULT 'BIEL'.

START-OF-SELECTION.

  DATA(lo_inv_mgmt) = NEW zcl_taler_inv_mgmt( ).
  DATA lv_json TYPE string.


lo_inv_mgmt->sync_inventory(
  EXPORTING
    p_plant = 'HD00'
    p_lgort = 'BIEL' ).
