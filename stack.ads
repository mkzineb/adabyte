with Interfaces;   use Interfaces;
with Instructions; use Instructions;
with Types;        use Types;
with Runtime;      use Runtime;
package Stack is

   type Block_Type is (Loop_Block, If_Block, Else_Block, Block);

   type Block_Frame is record
      Instr_ptr        : Unsigned_32;
      End_Instr_Offset : Unsigned_32;
      Stack_Ptr        : Unsigned_32;
      Results          : Unsigned_8;
      Params           : Unsigned_8;
      B_Type           : Block_Type;
   end record;

   type Call_Frame is record
      Instr_Ptr     : Unsigned_32;
      Block_Ptr     : Unsigned_32;
      Func_Instance : Integer; -- todo
      Module_Addr   : Module_Instance_Addr;
      Locals        : Integer; -- todo

   end record;

   --  operands
   type Value_Stack is array (Natural range <>) of Value_Type;
   --  labels
   type Block_Stack is array (Natural range <>) of Block_Frame;
   --  call funcs
   type Call_Stack is array (Natural range <>) of Call_Frame;

   type Stack is record
      Values : Value_Stack;
      Blocks : Block_Stack;
      Calls  : Call_Stack;
   end record;

end Stack;
