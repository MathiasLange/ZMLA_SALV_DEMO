*&---------------------------------------------------------------------*
*& Report ZMLA_DEMO_GUI_ALV_GRID_03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmla_demo_gui_alv_grid_03.

**********************************************************************
*
* Variablen
*
**********************************************************************
DATA: gv_screen_status TYPE string VALUE 'INIT'.
DATA: gv_carrid TYPE spfli-carrid.
DATA: gv_connid TYPE spfli-connid.
DATA: o_alv TYPE REF TO cl_gui_alv_grid.
DATA: it_spfli TYPE STANDARD TABLE OF spfli WITH DEFAULT KEY.

**********************************************************************
*
* leeres Dynpro als Dummy für ALV-Grid
*
**********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 2000.
SELECTION-SCREEN END OF SCREEN 2000.
**********************************************************************
*
* SELECTION-SCREEN
*
**********************************************************************
SELECT-OPTIONS: so_carr FOR gv_carrid.
SELECT-OPTIONS: so_conn FOR gv_connid.

**********************************************************************
*
* Eventhandler
*
**********************************************************************
CLASS lcl_events DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          sender.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.

  METHOD on_data_changed.
* geänderte Zellen durchgehen
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_changed>).
      IF <fs_changed> IS ASSIGNED.
* Zeile x aus der iTab it_mara rausholen und daraus die Zelle anhand des Spaltennamens (Feldnamens) holen
        ASSIGN COMPONENT <fs_changed>-fieldname OF STRUCTURE it_spfli[ <fs_changed>-row_id ] TO FIELD-SYMBOL(<fs_mara_field>).

* Änderungswert in die Zelle der iTab (it_mara) rückschreiben
        <fs_mara_field> = <fs_changed>-value.
      ENDIF.

    ENDLOOP.

* DB Update
    FIELD-SYMBOLS: <fs_tab> TYPE table.
    FIELD-SYMBOLS: <fs_row> TYPE spfli.

    ASSIGN er_data_changed->mp_mod_rows->* TO <fs_tab>.

    LOOP AT <fs_tab> ASSIGNING <fs_row>.
* DB Update hier
    ENDLOOP.

  ENDMETHOD.

  METHOD on_toolbar.
* alle Buttons entfernen, bis auf folgende:
    DELETE e_object->mt_toolbar WHERE
        function NE cl_gui_alv_grid=>mc_fc_refresh          " Refresh
    AND function NE cl_gui_alv_grid=>mc_mb_export           " Excel
    AND function NE cl_gui_alv_grid=>mc_fc_current_variant. " Layout

  ENDMETHOD.
ENDCLASS.

**********************************************************************
*
* INITIALIZATION
*
**********************************************************************
INITIALIZATION.

* Vorbelegungen für Selektionsbild
  so_carr[] = VALUE #( ( sign = 'I' option = 'EQ' low = 'LH' ) ).

**********************************************************************
*
* AT SELECTION-SCREEN OUTPUT
*
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

* Wenn vorher das Selektionsbild 1000 angezeigt wurde
  IF gv_screen_status = 'IN_SELECTION'.
* Daten holen
    SELECT * FROM spfli INTO TABLE @it_spfli
       WHERE carrid IN @so_carr
         AND connid IN @so_conn.
* ALV-Gitter anzeigen
    o_alv = NEW #( i_parent      = cl_gui_container=>default_screen
                   i_appl_events = abap_true ).

* Eventhandler registrieren
    SET HANDLER lcl_events=>on_toolbar FOR o_alv.
    SET HANDLER lcl_events=>on_data_changed FOR o_alv.

* Ereignisse registrieren
    o_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    o_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

* ALV-Grid selektionsbereit setzen
    o_alv->set_ready_for_input( i_ready_for_input = 1 ).

* Layout des ALV setzen
    DATA(lv_layout) = VALUE lvc_s_layo( zebra      = abap_true
                                        cwidth_opt = 'A'
                                        grid_title = 'Flugverbindungen' ).

* Feldkatalog automatisch durch SALV erstellen lassen
    DATA: o_salv TYPE REF TO cl_salv_table.

    cl_salv_table=>factory( IMPORTING
                              r_salv_table = o_salv
                            CHANGING
                              t_table      = it_spfli ).

    DATA(it_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = o_salv->get_columns( )
                                                                       r_aggregations = o_salv->get_aggregations( ) ).

* Drop-Down-Liste definieren und an das ALV-Gitter übergeben
    DATA(it_dropdown) = VALUE lvc_t_drop( ( handle = 1
                                            value  = 'BERLIN' )
                                          ( handle = 1
                                            value  = 'FRANKFURT' )
                                          ( handle = 1
                                            value  = 'NEW YORK' ) ).

    o_alv->set_drop_down_table( it_drop_down = it_dropdown ).

* im Feldkatalog alle Zellen der Spalte "CITYFROM" des ALV-Grids auf
* editierbar stellen, die restlichen Zellen sind nicht editierbar
    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CASE <fcat>-fieldname.
        WHEN 'CITYFROM'.
          <fcat>-edit = abap_true.
          <fcat>-drdn_hndl = 1.      " Drop-Down-Liste mit Handle = 1 für die Zelle setzen
          <fcat>-outputlen = 10.
        WHEN OTHERS.
          <fcat>-edit = abap_false.
      ENDCASE.
    ENDLOOP.

* ALV anzeigen
    o_alv->set_table_for_first_display( EXPORTING
                                          i_bypassing_buffer = abap_false
                                          i_save             = 'A'
                                          is_layout          = lv_layout
                                        CHANGING
                                          it_fieldcatalog    = it_fcat
                                          it_outtab          = it_spfli ).

* Focus auf ALV setzen
    cl_gui_alv_grid=>set_focus( control = o_alv ).

* leere SAP-Toolbar ausblenden
    cl_abap_list_layout=>suppress_toolbar( ).

* Flag für Screen-Status auf ALV-Anzeige setzen
    gv_screen_status = 'IN_ALV'.
  ENDIF.

**********************************************************************
*
* START-OF-SELECTION
*
**********************************************************************
START-OF-SELECTION.

* Wir befinden uns im Anzeigebereich des Selektionsbildes
  gv_screen_status = 'IN_SELECTION'.

* Trick: leeren Dummy-Screen 2000 anzeigen und intern für das ALV-Grid in
* AT SELECTION-SCREEN OUTPUT als cl_gui_container=>default_screen nutzen
  CALL SELECTION-SCREEN 2000.
