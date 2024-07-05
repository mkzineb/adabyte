with Interfaces;   use Interfaces;
with Types;        use Types;
with Instances;    use Instances;
with Wasm;         use Wasm;
with Types;        use Types;
with Instructions; use Instructions;
package Stack is

   --  labels / control instr
   type Block_Frame is record
      Instr_ptr        : Unsigned_32;
      End_Instr_Offset : Unsigned_32;
      Results          : Unsigned_8;
      Params           : Unsigned_8;
      B_Type           : Block_Type;
   end record;

   type Locals_Array is array (Local_Addr range <>) of Value_Type;
   type Locals_Vector is access Locals_Array;

   type Call_Frame is record
      Instr_Ptr   : Unsigned_32 := 0;
      Block_Ptr   : Unsigned_32 := 0;
      Func_Inst   : Function_Spec;
      Module_Addr : Module_Instance_Addr;
      Locals      : Locals_Vector;
   end record;

   type Value_Stack is record
      Val : Value_Type;
   end record;

   function New_Call_Frame
     (Func_Inst : Function_Spec; Owner : Module_Instance_Addr;
      Params    : Value_Type; Block_Ptr : Unsigned_32) return Call_Frame;

end Stack;
