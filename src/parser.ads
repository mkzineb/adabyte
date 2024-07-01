with Types;      use Types;
with Values;     use Values;
with Wasm;       use Wasm;
with Interfaces; use Interfaces;
package Parser is

   --  procedure Init_Module (Module : out Wasm_Module);
   --  procedure Parse_Module
   --    (Wasm_Data : Bytes_Array_Acc; Module : in out Wasm_Module);
   --  function Validate_Module (Module : Wasm_Module) return Boolean;
   --  procedure Execute_Module (Module : Wasm_Module);

   function Read_Wasm_File
     (File_Path : String; Data : out Bytes_Array_Acc) return Raw_Wasm_Value;

end Parser;
