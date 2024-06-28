with Ada.Containers;
with Instances;    use Instances;
with Instructions; use Instructions;
with Types;        use Types;
package Wasm is

   type Option_FuncAddr (Is_Null : Boolean := True) is record
      case Is_Null is
         when False =>
            Satrt_Addr : Func_Addr;
         when True =>
            null; -- no parameters
      end case;
   end record;

   type Chunk is array (Natural range <>) of Instruction_Acc;
   type Instruction_Sequence is access Chunk;

   type Function_Spec is record
      Instructions : Instruction_Sequence;
      Locals       : Val_Type_Array_Acc;
      Func_Ty      : Function_Type;
   end record;

   type Global is record
      Glb_Type : Global_Type;
      Init     : Constant_Expression;
   end record;

   type Array_Of_Functions is array (Natural range <>) of Function_Spec;
   type Functions is access Array_Of_Functions;

   type Array_Of_Func_Types is array (Natural range <>) of Function_Type;
   type F_Types is access Array_Of_Func_Types;

   type Array_Of_Global is array (Natural range <>) of Global;
   type Globals is access Array_Of_Global;

   type Array_Of_Table_Types is array (Natural range <>) of Table_Type;
   type Tab_Types is access Array_Of_Table_Types;

   type Wasm_Module is record
      Start_Func  : Option_FuncAddr := (Is_Null => True);
      Funcs       : Functions;
      Func_Types  : F_Types;
      Glbs        : Globals;
      Table_Types : Tab_Types;
      --  Exports --  later
   end record;

end Wasm;
