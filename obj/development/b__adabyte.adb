pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__adabyte.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__adabyte.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E075 : Short_Integer; pragma Import (Ada, E075, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "ada__exceptions_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__soft_links_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "system__exception_table_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__containers_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__io_exceptions_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "ada__numerics_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__strings__maps__constants_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "interfaces__c_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__exceptions_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "system__object_reader_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "system__dwarf_lines_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__soft_links__initialize_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "system__traceback__symbolic_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__img_int_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "system__img_uns_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__strings__utf_encoding_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__tags_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__strings__text_buffers_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "ada__streams_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__file_control_block_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__finalization_root_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "ada__finalization_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "system__file_io_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__storage_pools_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__finalization_masters_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "system__storage_pools__subpools_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__text_io_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "system__pool_global_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__img_llu_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "types_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "instances_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "instructions_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "wasm_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "stack_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "interpreter_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E175 := E175 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "interpreter__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "wasm__finalize_spec");
      begin
         E181 := E181 - 1;
         F2;
      end;
      E134 := E134 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "instances__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "types__finalize_spec");
      begin
         E143 := E143 - 1;
         F4;
      end;
      E140 := E140 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__pool_global__finalize_spec");
      begin
         F5;
      end;
      E118 := E118 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__text_io__finalize_spec");
      begin
         F6;
      end;
      E162 := E162 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__storage_pools__subpools__finalize_spec");
      begin
         F7;
      end;
      E136 := E136 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__finalization_masters__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E128 := E128 - 1;
         F9;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := '8';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E008 := E008 + 1;
      Ada.Containers'Elab_Spec;
      E005 := E005 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E070 := E070 + 1;
      Ada.Numerics'Elab_Spec;
      E023 := E023 + 1;
      Ada.Strings'Elab_Spec;
      E057 := E057 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E059 := E059 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E062 := E062 + 1;
      Interfaces.C'Elab_Spec;
      E045 := E045 + 1;
      System.Exceptions'Elab_Spec;
      E017 := E017 + 1;
      System.Object_Reader'Elab_Spec;
      E084 := E084 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E075 := E075 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E098 := E098 + 1;
      E010 := E010 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E040 := E040 + 1;
      System.Img_Int'Elab_Spec;
      E022 := E022 + 1;
      E014 := E014 + 1;
      System.Img_Uns'Elab_Spec;
      E065 := E065 + 1;
      E052 := E052 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E104 := E104 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E112 := E112 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E102 := E102 + 1;
      Ada.Streams'Elab_Spec;
      E120 := E120 + 1;
      System.File_Control_Block'Elab_Spec;
      E132 := E132 + 1;
      System.Finalization_Root'Elab_Spec;
      E131 := E131 + 1;
      Ada.Finalization'Elab_Spec;
      E129 := E129 + 1;
      System.File_Io'Elab_Body;
      E128 := E128 + 1;
      System.Storage_Pools'Elab_Spec;
      E138 := E138 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E136 := E136 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E162 := E162 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E118 := E118 + 1;
      System.Pool_Global'Elab_Spec;
      E140 := E140 + 1;
      System.Img_Llu'Elab_Spec;
      E157 := E157 + 1;
      Types'Elab_Spec;
      E143 := E143 + 1;
      Instances'Elab_Spec;
      E134 := E134 + 1;
      E173 := E173 + 1;
      Wasm'Elab_Spec;
      E181 := E181 + 1;
      E180 := E180 + 1;
      Interpreter'Elab_Spec;
      E175 := E175 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_adabyte");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/moubarik/Desktop/adabyte/obj/development/types.o
   --   /home/moubarik/Desktop/adabyte/obj/development/values.o
   --   /home/moubarik/Desktop/adabyte/obj/development/instances.o
   --   /home/moubarik/Desktop/adabyte/obj/development/instructions.o
   --   /home/moubarik/Desktop/adabyte/obj/development/wasm.o
   --   /home/moubarik/Desktop/adabyte/obj/development/stack.o
   --   /home/moubarik/Desktop/adabyte/obj/development/interpreter.o
   --   /home/moubarik/Desktop/adabyte/obj/development/adabyte.o
   --   -L/home/moubarik/Desktop/adabyte/obj/development/
   --   -L/home/moubarik/Desktop/adabyte/obj/development/
   --   -L/home/moubarik/.local/share/alire/toolchains/gnat_native_13.2.1_788a01f9/lib/gcc/x86_64-pc-linux-gnu/13.2.0/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
