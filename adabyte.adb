with Ada.Text_IO;  use Ada.Text_IO;
with Instances;    use Instances;
with Instructions; use Instructions;
with Interpreter;  use Interpreter;
with Types;        use Types;
with Interfaces;   use Interfaces;
with Stack;        use Stack;
with Ada.Containers.Vectors;
procedure Adabyte is
   Executor : Env;
   St       : Store_Type;
   Seq      : Instruction_Sequence := new Chunk (0 .. 2);
   stac     : Vectors.Vector       := Vectors.Empty_Vector;
   Result   : Interpret_Result;

   blk : Instruction_Acc :=
     new Instruction'
       (Control,
        Control_Inst =>
          (Block, Label => 0, Block_Arguments => (Func_Type, Func_Ty => Block),
           End_Block_Ptr => 2));

   const32  : Instruction_Acc :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 12)));
   const456 : Instruction_Acc :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 12)));

   branching : Instruction_Acc :=
     new Instruction'(Control, Control_Inst => (Branch, Label_Br => (0)));

   nopp  : Instruction_Acc :=
     new Instruction'(Control, Control_Inst => (Op => NOP));
   unrea : Instruction_Acc :=
     new Instruction'(Control, Control_Inst => (Op => Unreachable));

begin

   Seq (0)  := blk;
   Seq (1)  := const32;
   Seq (2)  := unrea;
   --  Seq (2)  := branching;
   Executor := Init_Environment (St, stac, Seq);

   Result := Run (Executor);
   --  Put_Line (Integer_32'Image (program.Stack.Last_Element.Val.I_32));

end Adabyte;
