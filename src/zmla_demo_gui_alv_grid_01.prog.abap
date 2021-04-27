*&---------------------------------------------------------------------*
*& Report ZMLA_DEMO_GUI_ALV_GRID_01
*&---------------------------------------------------------------------*
*& ALV report using the CL_GUI_ALV_GRID class                          *
*&---------------------------------------------------------------------*
*& Switch some columns to editable / display by button click                                                 *
*&---------------------------------------------------------------------*

REPORT zmla_demo_gui_alv_grid_01.

**********************************************************************
*
* Variablen
*
**********************************************************************
DATA: gv_carrid   TYPE spfli-carrid.
DATA: gv_connid   TYPE spfli-connid.

**********************************************************************
*
* SELECTION-SCREEN
*
**********************************************************************
SELECT-OPTIONS: so_carr FOR gv_carrid.
SELECT-OPTIONS: so_conn FOR gv_connid.

*----------------------------------------------------------------------*
*       CLASS lcl_sflight DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sflight DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF lty_sflight,
             carrid	   TYPE s_carr_id,
             connid	   TYPE s_conn_id,
             fldate	   TYPE s_date,
             price     TYPE s_price,
             currency	 TYPE s_currcode,
             planetype TYPE s_planetye,
             seatsmax	 TYPE s_seatsmax,
             seatsocc	 TYPE s_seatsocc,
           END OF lty_sflight.

    METHODS:
      get_sflight_data,
      build_fieldcatlog,
      display_alv.

    DATA: "gt_sflight TYPE STANDARD TABLE OF lty_sflight,
      gt_sflight TYPE STANDARD TABLE OF sflight,
      gt_fcat    TYPE lvc_t_fcat.

ENDCLASS.                    "lcl_sflight DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_events DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_events DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object
        e_interactive.

    CLASS-METHODS: on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm
        sender.
ENDCLASS.                    "lcl_events DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_sflight IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sflight IMPLEMENTATION.

* Get SFLIGHT data
  METHOD get_sflight_data.
*    SELECT carrid connid fldate price currency planetype seatsmax seatsocc
    SELECT * FROM sflight
      INTO TABLE gt_sflight
      WHERE carrid IN so_carr
      AND   connid IN so_conn.
  ENDMETHOD.                    "get_sflight_data

* Build FieldCatlog
  METHOD build_fieldcatlog.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'SFLIGHT'
      CHANGING
        ct_fieldcat            = gt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "build_fieldcatlog

* Display ALV
  METHOD display_alv.
    CALL SCREEN 100.
  ENDMETHOD.                    "display
ENDCLASS.                    "lcl_sflight IMPLEMENTATION


START-OF-SELECTION.

  DATA: lo_sflight       TYPE REF TO lcl_sflight,
        lo_container_100 TYPE REF TO cl_gui_custom_container,
        lo_grid          TYPE REF TO cl_gui_alv_grid.

* Create instance for the local class
  CREATE OBJECT lo_sflight.

* Get the SFLIGHT table data
  lo_sflight->get_sflight_data( ).

* 1. Create container instance
  CREATE OBJECT lo_container_100
    EXPORTING
      container_name = 'ALV'.

* 2. Create ALV grid instance by using the container instance
  lo_grid = NEW cl_gui_alv_grid( i_parent      = lo_container_100
                                 i_appl_events = abap_true ).

* Eventhandler registrieren
  SET HANDLER lcl_events=>on_toolbar FOR lo_grid.
  SET HANDLER lcl_events=>on_user_command FOR lo_grid.

  lo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

* ALV-Grid selektionsbereit setzen
  lo_grid->set_ready_for_input( i_ready_for_input = 1 ).


* 3. Build Field Catlog
*  lo_sflight->build_fieldcatlog( ).

* 3.a Layout des ALV setzen
  DATA(lv_layout) = VALUE lvc_s_layo( grid_title = 'Flugverbindungen' " Titel
                                      no_toolbar = abap_false         " Toolbar sichtbar
                                      smalltitle = abap_false         " große Überschrift
                                      zebra      = abap_true          " Zebrastreifen
                                      cwidth_opt = abap_true ).       " Spaltenbreiten optimieren

* 3.b Feldkatalog automatisch durch SALV erstellen lassen
  DATA: o_salv TYPE REF TO cl_salv_table.

  cl_salv_table=>factory( IMPORTING
                            r_salv_table = o_salv
                          CHANGING
                            t_table      = lo_sflight->gt_sflight ).

  DATA(it_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = o_salv->get_columns( )
                                                                     r_aggregations = o_salv->get_aggregations( ) ).

* 4. Spalten/Zell-Eigenschaften (Anzeige, F4-Hilfe, Edit ...) des Feldkatalogs setzen
  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'MANDT'.
*        <fs_fcat>-no_out = abap_true.     " ausblenden
        <fs_fcat>-tech = abap_true.        " Technical field
    ENDCASE.
  ENDLOOP.

* 5. Call the ALV
  CALL METHOD lo_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = lv_layout
    CHANGING
      it_outtab                     = lo_sflight->gt_sflight
*     it_fieldcatalog               = lo_sflight->gt_fcat
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* Display the ALV
  lo_sflight->display_alv( ).

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.


*----------------------------------------------------------------------*
*       CLASS lcl_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.
* Toolbar-Buttons hinzufügen:
* butn_type   Bezeichung
* 0           Button (normal)
* 1           Menü + Defaultbutton
* 2           Menü
* 3           Separator
* 4           Radiobutton
* 5           Auswahlknopf (Checkbox)
* 6           Menüeintrag
  METHOD on_toolbar.
* Separator hinzufügen
    APPEND VALUE #( butn_type = 3 ) TO e_object->mt_toolbar.
* Edit-Button hinzufügen
    APPEND VALUE #( butn_type = 5 text = 'Edit' icon = icon_change_text function = 'EDIT_DATA' quickinfo = 'Editieren' disabled = ' ' ) TO e_object->mt_toolbar.
  ENDMETHOD.

* Benutzerkommandos behandeln
  METHOD on_user_command.
    CASE e_ucomm.
* Editmodus umschalten
      WHEN 'EDIT_DATA'.
        DATA: it_fcat TYPE lvc_t_fcat.
        DATA: lv_edit TYPE abap_bool VALUE abap_false.

* Feldkatalog holen
        sender->get_frontend_fieldcatalog( IMPORTING
                                             et_fieldcatalog = it_fcat ).

*        IF lines( it_fcat ) > 0.
*          lv_edit = it_fcat[ 1 ]-edit.
*        ENDIF.

*        CASE lv_edit.
*          WHEN abap_true.
*            lv_edit = abap_false.
*          WHEN OTHERS.
*            lv_edit = abap_true.
*        ENDCASE.

* im Feldkatalog alle Zellen des ALV-Grids auf editierbar stellen
        LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
          CASE <fcat>-fieldname.
            WHEN 'SEATSOCC' OR 'SEATSOCC_B' OR 'SEATSOCC_F'.
              CASE <fcat>-edit.
                WHEN abap_true.
                  <fcat>-edit = abap_false.
                WHEN OTHERS.
                  <fcat>-edit = abap_true.
              ENDCASE.
            WHEN OTHERS.
              <fcat>-edit = abap_false.
          ENDCASE.
*          <fcat>-edit = lv_edit.
        ENDLOOP.

* Feldkatalog zurückgeben
        sender->set_frontend_fieldcatalog( it_fieldcatalog = it_fcat ).

* Refresh ALV
        sender->refresh_table_display( ).

*        sender->set_table_for_first_display( CHANGING
*                                              it_fieldcatalog = it_fcat
*                                              it_outtab = lo_sflight->gt_sflight ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
