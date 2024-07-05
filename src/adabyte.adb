with Ada.Text_IO;  use Ada.Text_IO;
with Instances;    use Instances;
with Instructions; use Instructions;
with Interpreter;  use Interpreter;
with Types;        use Types;
with Interfaces;   use Interfaces;
with Stack;        use Stack;
with Ada.Containers.Vectors;
with Parser;       use Parser;
with Values;       use Values;
with Wasm;         use Wasm;
with Module;       use Module;
procedure Adabyte is
   Executor : Env;
   St       : Store_Type;
   Seq      : Instruction_Sequence := new Chunk (0 .. 8);
   stack    : Vectors.Vector       := Vectors.Empty_Vector;
   Result   : Interpret_Result;

   procedure Build_Instruction_Sequence_Loop (Seq : Instruction_Sequence) is
   begin
      Seq (0) :=
        (new Instruction'
           (Control,
            Control_Inst =>  --  0  généré
              (Block, Block_Label => 55,
               Block_Arguments    => (Func_Type, Func_Ty => 1),
               End_Block_Ptr      => 8)));

      Seq (1) :=
        (new Instruction'
           (Control,
            Control_Inst =>
              (Loop_Inst, Loop_Label => 56,
               Loop_Arguments        => (Func_Type, Func_Ty => 1),
               End_Loop_Block        => 8)));

      Seq (2) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

      Seq (3) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

      Seq (4) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 4))));

      Seq (5) :=
        (new Instruction'(Control, Control_Inst => (Op => End_Block)));

      Seq (6) :=
        (new Instruction'(Control, Control_Inst => (Op => Return_Inst)));
   end Build_Instruction_Sequence_Loop;

   procedure Build_Instruction_Sequence_If (Seq : Instruction_Sequence) is
   begin
      Seq (0) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

      Seq (1) :=
        (new Instruction'
           (Control,
            Control_Inst =>  --  0  généré
              (Op             => If_Inst, If_Block_Args => (B => Empty),
               Else_If_Offset => 4, End_If_Offset => 5)));

      Seq (2) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 235))));

      Seq (3) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 569))));

      Seq (4) :=
        (new Instruction'
           (Numeric,
            Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 497))));

      Seq (5) :=
        (new Instruction'(Control, Control_Inst => (Op => End_Block)));

      Seq (6) :=
        (new Instruction'(Control, Control_Inst => (Op => Return_Inst)));
   end Build_Instruction_Sequence_If;

begin

   Executor := Init_Environment (St, stack, Seq);

   Build_Instruction_Sequence_If (Seq);

   Result := Run (Executor);

   --  Module :=
   --    Read_Wasm_File
   --      ("/home/moubarik/Desktop/WASM/adabyte/examples/add.wasm", Module);

end Adabyte;
