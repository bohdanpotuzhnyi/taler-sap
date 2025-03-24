*&---------------------------------------------------------------------*
*& Report z_t_taler_inv_mgmt_gmd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_inv_mgmt_gmd.

PARAMETERS: p_matnr TYPE char18 DEFAULT 'TALER_BOTTLE01',
            p_plant TYPE char18 DEFAULT 'HD00'.

START-OF-SELECTION.

  DATA(lo_inv_mgmt) = NEW zcl_taler_inv_mgmt( ).

  lo_inv_mgmt->get_material_data(
    p_matnr = p_matnr
  ).
