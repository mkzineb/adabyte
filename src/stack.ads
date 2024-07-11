with Interfaces;   use Interfaces;
with Types;        use Types;
with Instances;    use Instances;
with Wasm;         use Wasm;
with Types;        use Types;
with Instructions; use Instructions;
with Ada.Containers;
package Stack is
   use Value_Type_Vectors;
   use Ada.Containers;

   --  labels / control instr
   type Block_Frame is record
      Instr_ptr        : Unsigned_32;
      End_Instr_Offset : Unsigned_32;
      Results          : Unsigned_8;
      Params           : Unsigned_8;
      B_Type           : Block_Type;
   end record;

   type Call_Frame is record
      Instr_Ptr   : Unsigned_32 := 0;
      Block_Ptr   : Unsigned_32 := 0;
      Func_Inst   : Function_Spec;
      Module_Addr : Module_Instance_Addr;
      Locals      : Value_Type_Vectors.Vector;
   end record;

   type Value_Stack is record
      Val : Value_Type;
   end record;

   function Add_Elements_To_Call_Frame_Locals
     (Func_Locals : Vector; params : Parameter_Type) return Vector;

end Stack;
