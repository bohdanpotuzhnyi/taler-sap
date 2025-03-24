*&---------------------------------------------------------------------*
*& Report z_t_taler_inv_mgmt_gmd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_inv_mgmt_gmd.

PARAMETERS: p_matnr TYPE matnr DEFAULT 'TALER_BOTTLE01',
            p_plant TYPE werks_d DEFAULT 'HD00'.

START-OF-SELECTION.

  DATA(lo_inv_mgmt) = NEW zcl_taler_inv_mgmt( ).

  lo_inv_mgmt->get_material_data(
    iv_matnr = p_matnr
    iv_plant = p_plant
  ).
