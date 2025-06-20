*&---------------------------------------------------------------------*
*& Report z_t_post_wire
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_post_wire.

PARAMETERS: p_bilnr TYPE vbrk-vbeln OBLIGATORY,
            p_wired TYPE char70     OBLIGATORY.

DATA: lo_order  TYPE REF TO zcl_taler_order,
      lt_return TYPE bapiret2_t,
      ls_ret    TYPE bapiret2.

START-OF-SELECTION.

  " Instantiate class
  CREATE OBJECT lo_order.

  " Call the method
  CALL METHOD lo_order->post_incoming_payment
    EXPORTING
      iv_billing_doc = p_bilnr
      iv_wiref_id    = p_wired
    RECEIVING
      rt_return      = lt_return.

  " Show results
  IF lt_return IS INITIAL.
    WRITE: / 'No return messages – posting assumed successful.'.
    RETURN.
  ENDIF.

  LOOP AT lt_return INTO ls_ret.
    WRITE: / ls_ret-type, ls_ret-id, ls_ret-number, ls_ret-message.
  ENDLOOP.
