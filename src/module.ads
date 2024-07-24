------------------------------------------------------------------------------
--  Package: Module
--
--  comment...
--  Author: Moubarik Zineb
--  Date: 24-07-2024
------------------------------------------------------------------------------
with Wasm;        use Wasm;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Instances;   use Instances;

--  where we parse the code from a file to a wasm_module

package Module is
   pragma Elaborate_Body;
   type Module_Type is record
      Data : Wasm_Module;
   end record;

   function Parse_File (Fd : File_Descriptor) return Module_Type;

   function Instantiate_Module
     (Module : Module_Type; Store : Store_Type) return Module_Instance;

   function Get_Id (Module : Module_Instance) return Module_Instance_Addr;

end Module;
