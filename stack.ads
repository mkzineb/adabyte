with Interfaces;   use Interfaces;
with Instructions; use Instructions;
with Types;        use Types;
with Instances;    use Instances;
with Wasm;         use Wasm;
package Stack is

   type Block_Type is (Loop_Block, If_Block, Else_Block, Block);

   --  labels / control instr
   type Block_Frame is record
      Instr_ptr        : Unsigned_32;
      End_Instr_Offset : Unsigned_32;
      Stack_Ptr        : Unsigned_32;
      Results          : Unsigned_8;
      Params           : Unsigned_8;
      B_Type           : Block_Type;
   end record;

   --
   type Call_Frame is record
      Instr_Ptr   : Unsigned_32 := 0;
      Block_Ptr   : Unsigned_32 := 0;
      Func_Inst   : Function_Spec;
      Module_Addr : Module_Instance_Addr;
      --  Locals      : Locals_Vector; -- todo
   end record;

   type Value_Stack is record
      Val : Value_Type;
   end record;

   procedure Get_Inst_Ptr;
   procedure New_Call_Frame;

end Stack;
