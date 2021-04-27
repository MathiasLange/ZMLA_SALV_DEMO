*&---------------------------------------------------------------------*
*& Report  ZDEMO_EDITABLE_CL_SALV_TABLE - Demonstration of class LCL_SALV_BUDDY
*&         This is a working example of using class LCL_SALV_BUDDY with a fullscreen CL_SALV_TABLE object.
*&         a) This report reads all records from standard table T001P into a internal table
*&         b) This table is displayed using the CL_SALV_TABLE class
*&         c) Each time you double-click on any table line/column,
*&            the columns MOLGA and JUPER toggle between ready and NOT ready for input
*&---------------------------------------------------------------------*
REPORT zdemo_editable_cl_salv_table.

*----------------------------------------------------------------------*
*       CLASS lcl_salv_buddy DEFINITION
*----------------------------------------------------------------------*
*
* Developed by Lino Lopes (lino2112@gmail.com) - Version 1.0 - 31.07.2016
*
* This class is meant to be used with SALV objects to gain access to the GUI control that exist behind them.
* All SALV objects are descendants of the CL_SALV_MODEL_BASE class. Currently, there are the following SALV object classes:
*   CL_SALV_TABLE
*   CL_SALV_HIERSEQ_TABLE
*   CL_SALV_TREE
*
* We inherit from the abstract class CL_SAL_CONTROLLER because SALV objects have it as a FRIEND, thus
*   making us a FRIEND of SALV objects too. That allows us to have access to ALL attributes and methods of
*   SALV objects, regardless of them being PUBLIC, PROTECTED or PRIVATE. Example: R_CONTROLLER.
* Since this class only has STATIC methods, there's no need to instantiate objects of its type, thus the CRIATE PRIVATE.
* Finally, I see no need to inherit from this class, so this class is declared FINAL.
*----------------------------------------------------------------------*
CLASS lcl_salv_buddy DEFINITION INHERITING FROM cl_salv_controller CREATE PRIVATE FINAL.

  PUBLIC SECTION.

*----------------------------------------------------------------------*
* GET_CONTROL_RTTI - Returns runtime type information for the control that is behind a SALV object.
*   Parameter E_ADAPTER_TYPE returns the adapter type of a SALV object.
*     Based on this information, method GET_CONTROL will return a different control in its returrning parameter R_CONTROL.
*     You can use this runtime type information to choose the right control object to supply to the returning parameter R_CONTROL of method GET_CONTROL.
*   Parameter E_CONTROL_RTTI returns a TYPE HANDLE that you can use to create an object compatible with the returning parameter R_CONTROL of method GET_CONTROL.
* Below there is a correspondence between the adapter type returned in parameter E_ADAPTER_TYPE and
* the type of the control expected in parameter R_CONTROL of method GET_CONTROL:
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_FULLSCREEN       CL_GUI_ALV_GRID
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_GRID             CL_GUI_ALV_GRID
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_HIERSEQ          nothing (null)
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_LIST             CL_SALV_TABLE
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_TREE             CL_SALV_GUI_TREE
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_APPEND           nothing (null)
*----------------------------------------------------------------------*
    CLASS-METHODS: get_control_rtti IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                    EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                              e_control_rtti TYPE REF TO cl_abap_typedescr,

*----------------------------------------------------------------------*
* GET_CONTROL - Returns the control that is behind the SALV object.
*   MUST be called after the DISPLAY method of the SALV object, so that its control gets created.
*   See method GET_CONTROL_RTTI above for a correspondence between what you supply in paramter I_SALV and what you get back in parameter R_CONTROL.
*----------------------------------------------------------------------*
      get_control IMPORTING i_salv           TYPE REF TO cl_salv_model_base
                  RETURNING VALUE(r_control) TYPE REF TO object,

*----------------------------------------------------------------------*
* SET_EDITABLE - Enables OR disables editing on a CL_SALV_TABLE object.
*   If you supply parameter I_FIELDNAME and supply it NOT INITIAL, you get that
*     field enabled or disabled for editing, depending on parameter I_EDITABLE.
*   If you do not supply parameter I_FIELDNAME or supply it INITIAL, you get
*     all fields of the table enabled or disabled for editing, depending on parameter I_EDITABLE.
*   Parameter I_SALV_TABLE is the CL_SALV_TABLE object you want to enable or disable editing for.
*   Parameter I_EDITABLE should be ABAP_TRUE or ABAP_FALSE to enable or disable editing.
*   Parameter I_REFRESH indicates whether you want the control to be refreshed or not. You'll only see the changes
*     you've made using this method AFTER you do a refresh on the CL_SALV_TABLE object.
*   NOTE: If you want field per field editing capabilities, you MUST make sure editing for the whole table is disabled.
*         You can disable editing for the whole table simply by issuing a call to this method, omitting parameter I_FIELDNAME and
*         passing parameter I_EDITABLE as ABAP_FALSE. After that you can enable or disable editing on a field per field basis.
*         The CL_SALV_TABLE is disabled for editing by default.
*----------------------------------------------------------------------*
      set_editable IMPORTING VALUE(i_fieldname) TYPE csequence OPTIONAL
                             i_salv_table       TYPE REF TO cl_salv_table
                             VALUE(i_editable)  TYPE abap_bool DEFAULT abap_true
                             VALUE(i_refresh)   TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.

*----------------------------------------------------------------------*
* GET_CONTROL_INTERNAL - It is the guts of method GET_CONTROL. It does all the heavy work and method GET_CONTROL gets all the credits
*----------------------------------------------------------------------*
    CLASS-METHODS: get_control_internal IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                        EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                                  e_control      TYPE REF TO object.

ENDCLASS.                    "lcl_salv_buddy DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_salv_buddy IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_salv_buddy IMPLEMENTATION.


  METHOD get_control_internal.

    DATA: lo_controller            TYPE REF TO cl_salv_controller_model,
          lo_adapter               TYPE REF TO cl_salv_adapter,
          lo_fullscreen_adapter    TYPE REF TO cl_salv_fullscreen_adapter,
          lo_grid_adapter          TYPE REF TO cl_salv_grid_adapter,
          lo_table_display_adapter TYPE REF TO if_salv_table_display_adapter,
          lo_tree_adapter_base     TYPE REF TO cl_salv_tree_adapter_base.

    CHECK e_adapter_type IS REQUESTED OR
          e_control      IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control IS REQUESTED.
      CLEAR e_control.
    ENDIF.

    lo_controller = i_salv->r_controller.
    CHECK lo_controller IS BOUND.

    lo_adapter = lo_controller->r_adapter.
    CHECK lo_adapter IS BOUND.

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lo_adapter->type.
    ENDIF.

    CHECK e_control IS REQUESTED.

    CASE lo_adapter->type.
      WHEN lo_adapter->if_salv_adapter~c_adapter_type_fullscreen.
        lo_fullscreen_adapter ?= lo_adapter.
        e_control = lo_fullscreen_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_grid.
        lo_grid_adapter ?= lo_adapter.
        e_control = lo_grid_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_hierseq.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_list.
        lo_table_display_adapter ?= lo_adapter.
        e_control = lo_table_display_adapter->r_table.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_tree.
        lo_tree_adapter_base ?= lo_adapter.
        e_control = lo_tree_adapter_base->r_tree.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_append.

    ENDCASE.

  ENDMETHOD.                    "get_control_internal

  METHOD get_control_rtti.

    DATA: lv_adapter_type TYPE salv_de_adapter_type,
          lo_control      TYPE REF TO object.

    CHECK e_adapter_type IS REQUESTED OR
          e_control_rtti IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control_rtti IS REQUESTED.
      CLEAR e_control_rtti.
    ENDIF.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_adapter_type = lv_adapter_type e_control = lo_control ).

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lv_adapter_type.
    ENDIF.

    IF e_control_rtti IS REQUESTED.
      e_control_rtti = cl_abap_typedescr=>describe_by_object_ref( lo_control ).
    ENDIF.

  ENDMETHOD.                    "get_control_rtti

  METHOD get_control.

    CHECK r_control IS REQUESTED.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_control = r_control ).

  ENDMETHOD.                    "get_control

  METHOD set_editable.
    CONSTANTS: lc_stable TYPE lvc_s_stbl VALUE 'XX'.

    DATA: lo_grid     TYPE REF TO cl_gui_alv_grid,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_layout   TYPE lvc_s_layo.

    FIELD-SYMBOLS: <fs_fieldcat> LIKE LINE OF lt_fieldcat.

    lo_grid ?= get_control( i_salv_table ).
    CHECK lo_grid IS BOUND.

    IF i_fieldname IS SUPPLIED AND
       i_fieldname IS NOT INITIAL.
      lo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcat ).
      READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat> WITH KEY fieldname = i_fieldname.
      CHECK sy-subrc = 0.
      <fs_fieldcat>-edit = i_editable.
      lo_grid->set_frontend_fieldcatalog( lt_fieldcat ).
    ELSE.
      lo_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).
      ls_layout-edit = i_editable.
      lo_grid->set_frontend_layout( EXPORTING is_layout = ls_layout ).
    ENDIF.

    CHECK i_refresh = abap_true.
    i_salv_table->refresh( lc_stable ).

  ENDMETHOD.                    "set_editable

ENDCLASS.                    "lcl_salv_buddy IMPLEMENTATION

DATA: gv_editable   TYPE abap_bool,
      go_salv_table TYPE REF TO cl_salv_table,
      gt_salv_table TYPE TABLE OF t001p,
      go_events     TYPE REF TO cl_salv_events_table,
      go_grid       TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_double_click.
    IF gv_editable = abap_false.
      gv_editable = abap_true.
    ELSE.
      gv_editable = abap_false.
    ENDIF.
    lcl_salv_buddy=>set_editable( i_fieldname = 'MOLGA' i_salv_table = go_salv_table i_editable = gv_editable ).
    lcl_salv_buddy=>set_editable( i_fieldname = 'JUPER' i_salv_table = go_salv_table i_editable = gv_editable ).
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

START-OF-SELECTION.
  SELECT *
    INTO TABLE gt_salv_table
    FROM t001p.

END-OF-SELECTION.
  cl_salv_table=>factory(
    EXPORTING
      list_display   = if_salv_c_bool_sap=>false    " ALV Displayed in List Mode
    IMPORTING
      r_salv_table   = go_salv_table
    CHANGING
      t_table        = gt_salv_table ).

  go_events = go_salv_table->get_event( ).

  SET HANDLER lcl_event_handler=>handle_double_click FOR go_events.

  go_salv_table->display( ).
