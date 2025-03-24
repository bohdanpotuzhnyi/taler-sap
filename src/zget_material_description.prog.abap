*&---------------------------------------------------------------------*
*& Report zget_material_description
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zget_material_description.

PARAMETERS: p_matnr TYPE char18 OBLIGATORY.

DATA: lv_material_general_data TYPE bapimatdoa.
DATA: lv_return TYPE bapireturn.


CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
  EXPORTING
    material              = p_matnr   " MATNR
  IMPORTING
    material_general_data = lv_material_general_data
    return                = lv_return.

WRITE: / 'Description: ', lv_material_general_data-matl_desc.
