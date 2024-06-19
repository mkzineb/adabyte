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
   Seq      : Instruction_Sequence := new Chunk (0 .. 1);
   stac     : Vectors.Vector       := Vectors.Empty_Vector;
   Result   : Interpret_Result;
   const32  : Instruction_Acc      :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 12)));
   const456 : Instruction_Acc      :=
     new Instruction'
       (Numeric,
        Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 12)));
begin
   Seq (0) := const456;
   Seq (1) := const32;

   Executor := Init_Environment (St, stac, Seq);
   Result   := Run (Executor);
   --  Put_Line (Integer_32'Image (program.Stack.Last_Element.Val.I_32));

end Adabyte;
