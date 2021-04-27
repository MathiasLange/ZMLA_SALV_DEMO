*&---------------------------------------------------------------------*
*&  Include           ZMLA_FLIGHT_MONITOR_TOP
*&---------------------------------------------------------------------*
REPORT  zmla_flight_monitor.
**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF gty_s_alv_output_data.
         INCLUDE STRUCTURE sflight.
         "Need this to make individual cells/columns editable
         TYPES:   celltab TYPE lvc_t_styl,
       END OF gty_s_alv_output_data.

TYPES: gty_t_output_data TYPE STANDARD TABLE OF gty_s_alv_output_data.

**********************************************************************
* Constants
**********************************************************************

*--------------------------------------------------------------------*
* TABLES
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Global Variables
*--------------------------------------------------------------------*
DATA: go_no_handler    TYPE REF TO cx_sy_no_handler,
      go_precondition  TYPE REF TO zcx_violated_precondition,
      go_postcondition TYPE REF TO zcx_violated_postcondition,
      gd_error_class   TYPE string.

**********************************************************************
* Local Classes
**********************************************************************
INCLUDE zmla_flight_monitor_cd01.

DATA:   go_selections    TYPE REF TO lcl_selections.
