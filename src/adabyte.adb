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
   Seq      : Instruction_Sequence := new Chunk (0 .. 4);
   stac     : Vectors.Vector       := Vectors.Empty_Vector;
   Result   : Interpret_Result;

   blk : Instruction_Acc :=
     new Instruction'
       (Control,
        Control_Inst =>  --  0  generated
          (Block, Block_Label => 55,
           Block_Arguments => (Func_Type, Func_Ty => 1), End_Block_Ptr => 8));

   end_blk : Instruction_Acc :=
     new Instruction'(Control, Control_Inst => (Op => End_Block));

   loop_i : Instruction_Acc :=
     new Instruction'
       (Control,
        Control_Inst =>
          (Loop_Inst, Loop_Label => 56,
           Loop_Arguments => (Func_Type, Func_Ty => 1), End_Loop_Block => 8));

   branch_to_blk : Instruction_Acc :=
     new Instruction'(Control, Control_Inst => (Branch, Label_Br => 1));

   if_i : Instruction_Acc :=
     new Instruction'
       (Control,
        Control_Inst =>
          (Op => If_Inst, If_Block_Args => (Func_Type, Func_Ty => 1),
           Else_If_Offset => 0, End_If_Offset => 4));

   --  else = 0 no else statement

begin
   Seq (0) :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2)));

   Seq (1) :=
     new Instruction'
       (Control,
        Control_Inst =>
          (Op => If_Inst, If_Block_Args => (Func_Type, Func_Ty => 1),
           Else_If_Offset => 0, End_If_Offset => 4));

   Seq (2) :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 3)));

   Seq (3) :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 4)));

   Seq (4) := new Instruction'(Control, Control_Inst => (Op => Return_Inst));

   Executor := Init_Environment (St, stac, Seq);

   Result := Run (Executor);

end Adabyte;
