*&---------------------------------------------------------------------*
*& Report z_t_taler_inv_mgmt_gmd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_inv_mgmt_gmd.

PARAMETERS: p_matnr TYPE char18 DEFAULT 'TALER_BOTTLE01',
            p_plant TYPE char18 DEFAULT 'HD00',
            p_lgort TYPE lgort_d DEFAULT 'BIEL'.

START-OF-SELECTION.

  DATA(lo_inv_mgmt) = NEW zcl_taler_inv_mgmt( ).
  DATA lv_json TYPE string.

lv_json = lo_inv_mgmt->get_material_data(
  EXPORTING
    p_matnr = p_matnr
    p_plant = p_plant
    p_lgort = p_lgort ).


    WRITE: / lv_json.
