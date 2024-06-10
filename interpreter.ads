with Interfaces;                            use Interfaces;
with Instructions;                          use Instructions;
with Runtime;                               use Runtime;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;

package Interpreter is

   function Hash (x : Unsigned_32) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unsigned_32, Element_Type => Label_Info, Hash => Hash,
      Equivalent_Keys => "=");

   use Symbols_Table;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Element_Variation);
   use Vectors;

   type Pc is record
      Ip   : Natural;
      Code : Chunk_Acc;
   end record;
   --  record to manage store stack ip, block control and reduction
   type Vm is record
      Code    : Chunk_Acc;
      Stack   : Vector;
      Sym_Tab : Symbols_Table.Map;
      Ip      : Natural; --  instruction pointer
   end record;

   type Block_Context;
   type Block_Context_Acc is access all Block_Context;
   type Block_Context is record
      Current_Block   : Instruction_Sequence;
      Nested_Contexts : Block_Context_Acc;
   end record;

   type Evaluation_Context is record
      inst : Instruction_Acc; --  todo : enable reducing inside instruction sequences and admin forms as well as traps
   end record;

   type Thread is record
      Current_Frame : Frame;
      Instructions  : Instruction_Sequence;
      Eval_Context  : Evaluation_Context;
   end record;

   type Configuration is record
      Current_Store    : Store;
      Executing_Thread : Thread;
   end record;

   function Init_Interpreter return Vm; --  reset the stack

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   --  decoding instruction

   procedure Interpret (Interpreted : in out Vm);

   function Interpret_Control_Instruction
     (Inst : Control_Instruction; Interpreted : in out Vm) return Natural;

   procedure Interpret_Reference_Instruction
     (Inst : Reference_Instruction; Stack : in out Vector);

   procedure Interpret_Parametric_Instruction
     (Inst : Parametric_Instruction; Stack : in out Vector);

   procedure Interpret_Variable_Instruction
     (Inst : Variable_Instruction; Stack : in out Vector);

   procedure Interpret_Table_Instruction
     (Inst : Table_Instruction; Stack : in out Vector);

   procedure Interpret_Memory_Instruction
     (Inst : Memory_Instruction; Stack : in out Vector);

   procedure Interpret_Numeric_Instruction
     (Inst : Numeric_Instruction; Stack : in out Vector);

   procedure Interpret_Vector128_Instruction
     (Inst : Vector_Instruction; Stack : in out Vector);

   procedure Interpret_Expression_Instruction
     (Insts : Instruction_Sequence; Stack : in out Vector);

end Interpreter;
