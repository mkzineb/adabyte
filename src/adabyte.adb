with Ada.Text_IO;  use Ada.Text_IO;
with Instances;    use Instances;
with Instructions; use Instructions;
with Interpreter;  use Interpreter;
with Types;        use Types;
with Interfaces;   use Interfaces;
with Stack;        use Stack;
with Parser;       use Parser;
with Values;       use Values;
with Wasm;         use Wasm;
with Module;       use Module;
procedure Adabyte is

--  procedure Build_Instruction_Sequence_Loop (Seq : Instruction_Sequence) is
--  begin
--     Seq (0) :=
--       (new Instruction'
--          (Control,
--           Control_Inst =>  --  0  généré
--             (Block, Block_Label => 55,
--              Block_Arguments    => (Func_Type, Func_Ty => 1),
--              End_Block_Ptr      => 8)));

--     Seq (1) :=
--       (new Instruction'
--          (Control,
--           Control_Inst =>
--             (Loop_Inst, Loop_Label => 56,
--              Loop_Arguments        => (Func_Type, Func_Ty => 1),
--              End_Loop_Block        => 8)));

--     Seq (2) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

--     Seq (3) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

--     Seq (4) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 4))));

--     Seq (5) :=
--       (new Instruction'(Control, Control_Inst => (Op => End_Block)));

--     Seq (6) :=
--       (new Instruction'(Control, Control_Inst => (Op => Return_Inst)));
--  end Build_Instruction_Sequence_Loop;

--  function Build_Instruction_Sequence_If
--    (Seq : Instruction_Sequence) return Instruction_Sequence
--  is
--  begin
--     Seq (0) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 2))));

--     Seq (1) :=
--       (new Instruction'
--          (Control,
--           Control_Inst =>  --  0  généré
--             (Op             => If_Inst, If_Block_Args => (B => Empty),
--              Else_If_Offset => 4, End_If_Offset => 5)));

--     Seq (2) :=
--       (new Instruction'
--          (Variable, Variable_Inst => (Local_Get, Local_Get_Id => 0)));

--     Seq (3) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 569))));

--     Seq (4) :=
--       (new Instruction'
--          (Numeric,
--           Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 497))));

--     Seq (5) :=
--       (new Instruction'(Control, Control_Inst => (Op => End_Block)));

--     Seq (6) :=
--       (new Instruction'(Control, Control_Inst => (Op => Return_Inst)));
--     return Seq;
--  end Build_Instruction_Sequence_If;

--  function Build_loop_cr
--    (Seq : Instruction_Sequence) return Instruction_Sequence
--  is
--  begin
--     Seq (0) :=
--       new Instruction'
--         (Control,
--          Control_Inst =>
--            (Op             => Loop_Inst, Loop_Label => 687,
--             Loop_Arguments => (Func_Type, Func_Ty => 1),
--             End_Loop_Block => 8));

--     Seq (1) :=
--       new Instruction'
--         (Variable, Variable_Inst => (Local_Get, Local_Get_Id => 0));

--     Seq (2) :=
--       new Instruction'
--         (Numeric,
--          Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 1)));

--     Seq (3) := new Instruction'(Numeric, Numeric_Inst => (Op => I32_Add));

--     Seq (4) :=
--       new Instruction'
--         (Variable, Variable_Inst => (Local_Set, Local_Set_Id => 0));

--     Seq (5) :=
--       new Instruction'
--         (Variable, Variable_Inst => (Local_Get, Local_Get_Id => 0));

--     Seq (6) :=
--       new Instruction'
--         (Numeric,
--          Numeric_Inst => (I32_Const, I32_Constant => (I_32, I_32 => 3)));

--     Seq (7) := new Instruction'(Numeric, Numeric_Inst => (Op => I32_Lt_S));

--     Seq (8) :=
--       new Instruction'
--         (Control, Control_Inst => (Branch_If, Label_Br_If => 0));

--     Seq (9) := new Instruction'(Control, Control_Inst => (Op => End_Block));

--     Seq (10) :=
--       new Instruction'(Control, Control_Inst => (Op => Return_Inst));
--     return Seq;
--  end Build_loop_cr;

--  use Value_Type_Vectors;
--  Executor       : Env;
--  Store          : Store_Type;
--  Seq            : Instruction_Sequence      := new Chunk (0 .. 10);
--  Stack          : Vectors.Vector;
--  Start_function : Function_Spec;
--  Locals_func    : Value_Type_Vectors.Vector := Empty (5);
--  Params         : Parameter_Type;
--  Results        : Result_Type;
--  Inst_Seq       : Instruction_Sequence;
--  Return_Message : Interpret_Result;
begin
   --  local i32 $n (initialized to 0)
   --  Append
   --    (Locals_func,
   --     (Number_Value, Num_Value => (S, Num_S => (I_32, I_32 => 23))));

   --  --  one params a = 1
   --  Append
   --    (Params, (Number_Value, Num_Value => (S, Num_S => (I_32, I_32 => 1))));

   --  --  one var to return $n (only need to specify the type of the result)
   --  Append
   --    (Results, (Number_Value, Num_Value => (S, Num_S => (I_32, I_32 => 0))));

   --  Start_function :=
   --    (Instructions => Build_loop_cr (Seq),
   --     Locals       => Add_Elements_To_Call_Frame_Locals (Locals_func, Params),
   --     Func_Ty      => (Params => Params, Results => Results));

   --  Executor := Init_Environment (Store, Stack, Start_function);

   --  Return_Message := Run (Executor);

   --  Module :=
   --    Read_Wasm_File
   --      ("/home/moubarik/Desktop/WASM/adabyte/examples/add.wasm", Module);
   null;
end Adabyte;
