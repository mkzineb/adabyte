with Ada.Containers;
with Instructions; use Instructions;
with Types;        use Types;
with Interfaces;   use Interfaces;
with Instances;    use Instances;
--  Specification for the sections of the module
with Types; use Types;
package Wasm is
   use Value_Type_Vectors;
   --  WebAssembly Address

   type Option_FuncAddr (Is_Null : Boolean := True) is record
      case Is_Null is
         when False =>
            Satrt_Addr : Func_Addr;
         when True =>
            null; -- no parameters
      end case;
   end record;

   type Function_Spec is record
      Instructions : Instruction_Sequence;
      Locals       : Vector;
      Func_Ty      : Function_Type;
   end record;

   type Global is record
      Glb_Type : Global_Type;
      Init     : Constant_Expression;
   end record;

   type Export is record
      Index : Unsigned_32;
   end record;

   --  switch case
   type Import_Kind is record
      Func   : Type_Addr;
      Table  : Table_Type;
      Memory : Memory_Type;
      Global : Global_Type;
   end record;

   type Import is record
      Name   : Unsigned_32;
      kind   : Import_Kind;
      Module : Unsigned_32;
   end record;

   type Data_Kind is (Active, Passive);

   type Data is record
      Data : Vector_Of_Bytes_Acc;
      Kind : Data_Kind;
      --  range
   end record;

   type Element_Kind is (Passive, Active, Declared);
   type Element_Kind_Record (Kind : Element_Kind := Passive) is record
      case Kind is
         when Passive | Declared =>
            null;
         when Active =>
            Table  : Table_Addr;
            Offset : Numeric_Instruction;  --  only const
      end case;
   end record;

   type Inst_Seq is (Func, Expr);
   type Element_Item (Item : Inst_Seq := Func) is record
      case Item is
         when Func =>
            Func : Func_Addr;
         when Expr =>
            Expr : Numeric_Instruction;
      end case;
   end record;

   type Element is record
      Kind  : Element_Kind;
      Items : Integer; --  array of element item
      Ty    : Value_Type;
   end record;

   type Array_Of_Functions is array (Natural range <>) of Function_Spec;
   type Wasm_Functions is access Array_Of_Functions;

   type Array_Of_Func_Types is array (Natural range <>) of Function_Type;
   type Wasm_Functions_Types is access Array_Of_Func_Types;

   type Array_Of_Global is array (Natural range <>) of Global;
   type Wasm_Globals is access Array_Of_Global;

   type Array_Of_Table_Types is array (Natural range <>) of Table_Type;
   type Wasm_Tab_Types is access Array_Of_Table_Types;

   type Array_Export is array (Natural range <>) of Export;
   type Wasm_Export is access Array_Export;

   type Array_Memory is array (Natural range <>) of Memory_Type;
   type Wasm_Memory_Types is access Array_Memory;

   type Array_Imports is array (Natural range <>) of Import;
   type Wasm_Imports is access Array_Imports;

   type Array_Data is array (Natural range <>) of Data;
   type Wasm_Data is access Array_Data;

   type Array_Elements is array (Natural range <>) of Element;
   type Wasm_Elements is access Array_Elements;

   type Wasm_Module is record
      Start_Func   : Option_FuncAddr := (Is_Null => True);
      Funcs        : Wasm_Functions_Types;
      Func_Types   : Wasm_Functions;
      Globals      : Wasm_Globals;
      Table_Types  : Wasm_Tab_Types;
      Exports      : Wasm_Export;
      Memory_Types : Wasm_Memory_Types;
      Imports      : Wasm_Imports;
      Data         : Wasm_Data;
      Elements     : Wasm_Elements;
      l            : Ft_Array_Access_Types.Array_Access;
   end record;

end Wasm;
