pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__adabyte.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__adabyte.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__exception_table_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "ada__containers_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__io_exceptions_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "ada__numerics_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__strings_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__strings__maps_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__strings__maps__constants_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "interfaces__c_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exceptions_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__object_reader_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "system__dwarf_lines_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__soft_links__initialize_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "system__traceback__symbolic_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__img_int_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "system__img_uns_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__strings__utf_encoding_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__tags_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__strings__text_buffers_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "gnat_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "ada__streams_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__file_control_block_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__finalization_root_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "ada__finalization_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "system__file_io_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__storage_pools_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__finalization_masters_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "system__storage_pools__subpools_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__text_io_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__pool_global_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "system__img_llu_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "types_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "instances_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "wasm_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "parser_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "stack_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "interpreter_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E187 := E187 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "interpreter__finalize_spec");
      begin
         F1;
      end;
      E192 := E192 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "stack__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "wasm__finalize_spec");
      begin
         E193 := E193 - 1;
         F3;
      end;
      E134 := E134 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "instances__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "types__finalize_spec");
      begin
         E144 := E144 - 1;
         F5;
      end;
      E141 := E141 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__pool_global__finalize_spec");
      begin
         F6;
      end;
      E118 := E118 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__text_io__finalize_spec");
      begin
         F7;
      end;
      E175 := E175 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__storage_pools__subpools__finalize_spec");
      begin
         F8;
      end;
      E137 := E137 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__finalization_masters__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__file_io__finalize_body");
      begin
         E128 := E128 - 1;
         F10;
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
      E010 := E010 + 1;
      Ada.Containers'Elab_Spec;
      E043 := E043 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E072 := E072 + 1;
      Ada.Numerics'Elab_Spec;
      E025 := E025 + 1;
      Ada.Strings'Elab_Spec;
      E007 := E007 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E061 := E061 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E064 := E064 + 1;
      Interfaces.C'Elab_Spec;
      E048 := E048 + 1;
      System.Exceptions'Elab_Spec;
      E019 := E019 + 1;
      System.Object_Reader'Elab_Spec;
      E086 := E086 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E100 := E100 + 1;
      E012 := E012 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E042 := E042 + 1;
      System.Img_Int'Elab_Spec;
      E024 := E024 + 1;
      E016 := E016 + 1;
      System.Img_Uns'Elab_Spec;
      E067 := E067 + 1;
      E055 := E055 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E104 := E104 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E112 := E112 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E006 := E006 + 1;
      Gnat'Elab_Spec;
      E196 := E196 + 1;
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
      E139 := E139 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E137 := E137 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E175 := E175 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E118 := E118 + 1;
      System.Pool_Global'Elab_Spec;
      E141 := E141 + 1;
      System.Img_Llu'Elab_Spec;
      E162 := E162 + 1;
      Types'Elab_Spec;
      E144 := E144 + 1;
      Instances'Elab_Spec;
      E134 := E134 + 1;
      Wasm'Elab_Spec;
      E193 := E193 + 1;
      E199 := E199 + 1;
      Stack'Elab_Spec;
      E192 := E192 + 1;
      Interpreter'Elab_Spec;
      E187 := E187 + 1;
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
   --   /home/moubarik/Desktop/adabyte/obj/development/module.o
   --   /home/moubarik/Desktop/adabyte/obj/development/parser.o
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
