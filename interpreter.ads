with Interfaces;                            use Interfaces;
with Instructions;                          use Instructions;
with Runtime;                               use Runtime;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Types;                                 use Types;
package Interpreter is

   type Element is (Value, Label, Activation_Call);
   type Element_Variation (Elt : Element := Value) is record
      case Elt is
         when Value =>
            Val : Number_Type;
         when Label =>
            Lab : Unsigned_32;
         when Activation_Call =>
            Frame : Integer;
      end case;
   end record;

   type Label_Info is record
      Label_Index : Unsigned_32; --  ?
      Arity       : Positive;
      Offset      : Natural;
   end record;

   function Hash (x : Unsigned_32) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unsigned_32, Element_Type => Label_Info, Hash => Hash,
      Equivalent_Keys => "=");

   use Symbols_Table;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Element_Variation);
   use Vectors;

   type Pointer is record
      Pointer : Natural;
      Code    : Instruction_Sequence;
   end record;
   --  record to manage store stack ip, block control and reduction
   type Env is record
      Stack               : Vector;
      Sym_Tab             : Symbols_Table.Map;
      Instruction_Pointer : Pointer; --  instruction pointer
   end record;

   type Block_Context;
   type Block_Context_Acc is access all Block_Context;
   type Block_Context is record
      Current_Block   : Instruction_Sequence;
      Nested_Contexts : Block_Context_Acc;
   end record;

   type Evaluation_Context is record
      inst : Instruction_Acc;
      --  todo : enable reducing inside instruction sequences
      --  and admin forms as well as traps
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

   function Init_Interpreter (P : Pointer) return Env; --  reset the stack

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   --  decoding instruction

   procedure Interpret (Interpreted : in out Env);

   function Interpret_Control_Instruction
     (Inst : Control_Instruction; Interpreted : in out Env) return Natural;

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
