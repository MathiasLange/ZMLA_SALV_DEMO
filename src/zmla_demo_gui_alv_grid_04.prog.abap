*&---------------------------------------------------------------------*
*& Report ZMLA_DEMO_GUI_ALV_GRID_04
*&---------------------------------------------------------------------*
*& ALV-Grid ohne Dynpro – Daten anzeigen, editieren
*& und als CSV-Datei speichern
*&---------------------------------------------------------------------*
REPORT zmla_demo_gui_alv_grid_04.

DATA: it_mara TYPE STANDARD TABLE OF mara.

CLASS lcl_events DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object
        e_interactive.

    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        er_data_changed
        sender.

    CLASS-METHODS: on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm
        sender.
ENDCLASS.

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
* Speichern-Button hinzufügen
    APPEND VALUE #( butn_type = 5 text = 'Speichern' icon = icon_system_save function = 'SAVE_DATA' quickinfo = 'Speichern' disabled = ' ' ) TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_data_changed.
* geänderte Zellen durchgehen
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<c>).
      IF <c> IS ASSIGNED.
* Zeile x aus der iTab it_mara rausholen und daraus die Zelle anhand des Spaltennamens (Feldnamens) holen
        ASSIGN COMPONENT <c>-fieldname OF STRUCTURE it_mara[ <c>-row_id ] TO FIELD-SYMBOL(<f>).

        IF <f> IS ASSIGNED.
* Änderungswert in die Zelle der iTab (it_mara) rückschreiben
          <f> = <c>-value.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

* Benutzerkommandos behandeln
  METHOD on_user_command.
    CASE e_ucomm.
* Daten speichern
      WHEN 'SAVE_DATA'.
        DATA: lv_action TYPE i.
        DATA: lv_filename TYPE string.
        DATA: lv_fullpath TYPE string.
        DATA: lv_path TYPE string.
* FileSave-Dialog aufrufen
        TRY.
            cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                                          default_extension   = 'txt'
                                                          file_filter         = |TXT (*.txt)\|*.txt\|{ cl_gui_frontend_services=>filetype_all }|
                                                          prompt_on_overwrite = abap_true
                                                        CHANGING
                                                          filename            = lv_filename     " Dateiname
                                                          path                = lv_path         " Pfad
                                                          fullpath            = lv_fullpath     " Pfad + Dateiname
                                                          user_action         = lv_action ).    " Benutzeraktion

            IF lv_action EQ cl_gui_frontend_services=>action_ok.
* iTab nach CSV konvertieren und speichern
              cl_icf_csv=>request_for_write_into_csv( it_data            = it_mara
                                                      iv_hdr_struct_name = 'MARA'
                                                      iv_init_dir        = lv_path
                                                      iv_file_name       = lv_filename ).

              MESSAGE 'Daten erfolgreich gespeichert.' TYPE 'I'.
            ENDIF.

          CATCH cx_root INTO DATA(e_text).          " Oberklasse für Exceptions abfangen und Kurztext übergeben
            MESSAGE e_text->get_text( ) TYPE 'I'.   " Exception Kurztext ausgeben
        ENDTRY.

* Editmodus umschalten
      WHEN 'EDIT_DATA'.
        DATA: it_fcat TYPE lvc_t_fcat.
        DATA: lv_edit TYPE abap_bool VALUE abap_false.

* Feldkatalog holen
        sender->get_frontend_fieldcatalog( IMPORTING
                                             et_fieldcatalog = it_fcat ).

        IF lines( it_fcat ) > 0.
          lv_edit = it_fcat[ 1 ]-edit.
        ENDIF.

        CASE lv_edit.
          WHEN abap_true.
            lv_edit = abap_false.
          WHEN OTHERS.
            lv_edit = abap_true.
        ENDCASE.

* im Feldkatalog alle Zellen des ALV-Grids auf editierbar stellen
        LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
          <fcat>-edit = lv_edit.
        ENDLOOP.

* Feldkatalog zurückgeben
        sender->set_table_for_first_display( CHANGING
                                              it_fieldcatalog = it_fcat
                                              it_outtab = it_mara ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  SELECT * FROM mara INTO TABLE it_mara UP TO 100 ROWS.

  DATA(o_alv) = NEW cl_gui_alv_grid( i_parent = cl_gui_container=>default_screen
                                     i_appl_events = abap_true ).

* Eventhandler registrieren
  SET HANDLER lcl_events=>on_toolbar FOR o_alv.
  SET HANDLER lcl_events=>on_data_changed FOR o_alv.
  SET HANDLER lcl_events=>on_user_command FOR o_alv.

  o_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

* ALV-Grid selektionsbereit setzen
  o_alv->set_ready_for_input( i_ready_for_input = 1 ).

  DATA(lv_layout) = VALUE lvc_s_layo( zebra = abap_true
                                      cwidth_opt = 'A'
                                      grid_title = 'Editierbares ALV-Gitter ohne extra Dynpro' ).

  o_alv->set_table_for_first_display( EXPORTING
                                        i_bypassing_buffer = abap_true
                                        i_save             = 'A'
                                        is_layout          = lv_layout
                                        i_structure_name   = 'MARA'
                                      CHANGING
                                        it_outtab        = it_mara ).

  cl_gui_alv_grid=>set_focus( control = o_alv ).

  cl_gui_cfw=>flush( ).

* leere Toolbar ausblenden
  cl_abap_list_layout=>suppress_toolbar( ).

* Listenausgabe für cl_gui_container=>default_screen erzwingen
  WRITE: space.
