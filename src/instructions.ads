with Types;      use Types;
with Interfaces; use Interfaces;
with Instances;  use Instances;

package Instructions with
  SPARK_Mode => On
is

   subtype End_Offset is Unsigned_32;
   subtype Else_Offset is Unsigned_32;
   subtype Br_Table_Default is Unsigned_32;
   subtype Br_Table_Len is Unsigned_32;

   type Instruction;
   type Instruction_Acc is access all Instruction;

   type Chunk is array (Natural range <>) of Instruction_Acc;
   type Instruction_Sequence is access Chunk;

   type Blocks_Variation is (Empty, TType, Func_Type);

   type Block_Args (B : Blocks_Variation := Empty) is record
      case B is
         when Empty =>
            null;
         when TType =>
            Val_Ty : Value_Type;
         when Func_Type =>
            Func_Ty : Unsigned_32;
      end case;
   end record;

   type Mem_Arg is record
      Offset       : Unsigned_64;
      Arg_Mem_Addr : Mem_Addr;
   end record;

   type Sign_Extension is (Signed, Unsigned);

   type Opcode is
     (Numeric, Reference, Parametric, Variable, Table, Memory, Control,
      Vector_128);

   type Opcode_Numeric is
     (I32_Const, I64_Const, F32_Const, F64_Const, I32_Eqz, I32_Eq, I32_Ne,
      I32_Lt_S, I32_Lt_U, I32_Gt_S, I32_Gt_U, I32_Le_S, I32_Le_U, I32_Ge_S,
      I32_Ge_U, I64_Eqz, I64_Eq, I64_Ne, I64_Lt_S, I64_Lt_U, I64_Gt_S,
      I64_Gt_U, I64_Le_S, I64_Le_U, I64_Ge_S, I64_Ge_U, F32_Eq, F32_Ne, F32_Lt,
      F32_Gt, F32_Le, F32_Ge, F64_Eq, F64_Ne, F64_Lt, F64_Gt, F64_Le, F64_Ge,
      I32_Clz, I32_Ctz, I32_Popcnt, I32_Add, I32_Sub, I32_Mul, I32_Div_S,
      I32_Div_U, I32_Rem_S, I32_Rem_U, I64_Clz, I64_Ctz, I64_Popcnt, I64_Add,
      I64_Sub, I64_Mul, I64_Div_S, I64_Div_U, I64_Rem_S, I64_Rem_U, I32_And,
      I32_Or, I32_Xor, I32_Shl, I32_Shr_S, I32_Shr_U, I32_Rotl, I32_Rotr,
      I64_And, I64_Or, I64_Xor, I64_Shl, I64_Shr_S, I64_Shr_U, I64_Rotl,
      I64_Rotr, F32_Abs, F32_Neg, F32_Ceil, F32_Floor, F32_Trunc, F32_Nearest,
      F32_Sqrt, F32_Add, F32_Sub, F32_Mul, F32_Div, F32_Min, F32_Max,
      F32_Copysign, F64_Abs, F64_Neg, F64_Ceil, F64_Floor, F64_Trunc,
      F64_Nearest, F64_Sqrt, F64_Add, F64_Sub, F64_Mul, F64_Div, F64_Min,
      F64_Max, F64_Copysign, I32_Wrap_I64, I32_Trunc_F32_S, I32_Trunc_F32_U,
      I32_Trunc_F64_S, I32_Trunc_F64_U, I32_Extend_8_S, I32_Extend_16_S,
      I64_Extend_8_S, I64_Extend_16_S, I64_Extend_32_S, I64_Extend_I32_S,
      I64_Extend_I32_U, I64_Trunc_F32_S, I64_Trunc_F32_U, I64_Trunc_F64_S,
      I64_Trunc_F64_U, F32_Convert_I32_S, F32_Convert_I32_U, F32_Convert_I64_S,
      F32_Convert_I64_U, F32_Demote_F64, F64_Convert_I32_S, F64_Convert_I32_U,
      F64_Convert_I64_S, F64_Convert_I64_U, F64_Promote_F32,
      I32_Reinterpret_F32, I64_Reinterpret_F64, F32_Reinterpret_I32,
      F64_Reinterpret_I64, I32_Trunc_Sat_F32_S, I32_Trunc_Sat_F32_U,
      I32_Trunc_Sat_F64_S, I32_Trunc_Sat_F64_U, I64_Trunc_Sat_F32_S,
      I64_Trunc_Sat_F32_U, I64_Trunc_Sat_F64_S, I64_Trunc_Sat_F64_U);

   type Opcode_Reference is (Null_Value, Is_Null, Func_Ref);

   type Opcode_Control is
     (Unreachable, NOP, Block, Loop_Inst, If_Inst, Else_Inst, End_Block,
      Branch, Branch_If, Branch_Table, Return_Inst, Call, Call_Indirect);

   type Opcode_Parametric is (Drop, Select_Inst);
   --  select possibly followed by a type notation
   type Opcode_Variable is
     (Local_Get, Local_Set, Local_Tee, Global_Get, Global_Set);

   type Opcode_Table is
     (Table_Get, Table_Set, Table_Init, Table_Drop, Table_Copy, Table_Grow,
      Table_Size, Table_Fill);

   type Opcode_Memory is
     (I32_Load, I64_Load, F32_Load, F64_Load, I32_Load_8_S, I32_Load_8_U,
      I32_Load_16_S, I32_Load_16_U, I64_Load_8_S, I64_Load_8_U, I64_Load_16_S,
      I64_Load_16_U, I64_Load_32_S, I64_Load_32_U, I32_Store, I64_Store,
      F32_Store, F64_Store, I32_Store_8, I32_Store_16, I64_Store_8,
      I64_Store_16, I64_Store_32, Mem_Size, Mem_Grow,
                         --  bulk memory inst
                         Mem_Init, Mem_Drop,
      Mem_Copy, Mem_Fill);

   type Opcode_Vector is (Extract, Splat, Pmin, Pmax);

   type Numeric_Instruction (Op : Opcode_Numeric := I32_Const) is record
      case Op is
         when I32_Const =>
            I32_Constant : Number_Type (Num => I_32);
         when I64_Const =>
            I64_Constant : Number_Type (Num => I_64);
         when F32_Const =>
            F32_Constant : Number_Type (Num => F_32);
         when F64_Const =>
            F64_Constant : Number_Type (Num => F_64);
         when others =>
            null;
      end case;
   end record;

   type Reference_Instruction (Op : Opcode_Reference := Null_Value) is record
      case Op is
         when Null_Value =>
            Ref_Type : Value_Type;
         when Is_Null =>
            null;
         when Func_Ref =>
            Function_Addr : Func_Addr;
      end case;
   end record;

   type Control_Instruction (Op : Opcode_Control := NOP) is record
      case Op is
         when Block =>
            Block_Label     : Unsigned_32;
            Block_Arguments : Block_Args;
            End_Block_Ptr   : End_Offset;
         when End_Block =>
            null;
         when If_Inst =>
            If_Block_Args  : Block_Args;
            Else_If_Offset : Else_Offset; --  0 when no else statement
            End_If_Offset  : End_Offset;
         when Else_Inst =>
            End_Else_Offset : End_Offset;
         when Loop_Inst =>
            Loop_Label     : Unsigned_32;
            Loop_Arguments : Block_Args;
            End_Loop_Block : End_Offset;
         when Branch_If =>
            Label_Br_If : Unsigned_32;
         when Branch =>
            Label_Br : Unsigned_32;
         when NOP =>
            null;
         when Branch_Table =>
            Table_Default : Br_Table_Default;
            Table_Len     : Br_Table_Len;
         when Return_Inst =>
            null;
         when Call =>
            call_addr : Func_Addr;
         when Call_Indirect =>
            Call_Type_Addr  : Type_Addr;
            Call_Table_Addr : Table_Addr;
         when Unreachable =>
            null;
      end case;
   end record;

   type Table_Instruction (Op : Opcode_Table := Table_Get) is record
      case Op is
         when Table_Get =>
            Table_Get : Table_Addr;
         when Table_Set =>
            Table_Set : Table_Addr;
         when Table_Size =>
            Table_Size : Table_Addr;
         when Table_Fill =>
            Table_Fill : Table_Addr;
         when Table_Init =>
            Elem_Init  : Table_Addr;
            Table_Init : Table_Addr;
         when Table_Drop =>
            Elem_Drop : Table_Addr;
         when Table_Grow =>
            Table_Grow : Table_Addr;
         when Table_Copy =>
            From : Table_Addr;
            To   : Table_Addr;
      end case;
   end record;

   type Variable_Instruction (Op : Opcode_Variable := Local_Get) is record
      case Op is
         when Local_Get =>
            Local_Get_Id : Local_Addr;
         when Local_Set =>
            Local_Set_Id : Local_Addr;
         when Local_Tee =>
            Local_Tee_Id : Local_Addr;
         when Global_Get =>
            Glb_Get_Addr : Global_Addr;
         when Global_Set =>
            Glb_Set_Addr : Global_Addr;
      end case;
   end record;

   type Parametric_Instruction (Op : Opcode_Parametric := Drop) is record
      case Op is
         when Drop =>
            null;
         when Select_Inst =>
            Array_Val_Type : Val_Type_Array_Acc;
      end case;
   end record;

   type Memory_Instruction (Op : Opcode_Memory := I32_Load) is record
      case Op is
         when I32_Load =>
            Offset_I32_Load   : Unsigned_64;
            Mem_Addr_I32_Load : Mem_Addr;
         when I64_Load =>
            Offset_I64_Load   : Unsigned_64;
            Mem_Addr_I64_Load : Mem_Addr;
         when F32_Load =>
            Offset_F32_Load   : Unsigned_64;
            Mem_Addr_F32_Load : Mem_Addr;
         when F64_Load =>
            Offset_F64_Load   : Unsigned_64;
            Mem_Addr_F64_Load : Mem_Addr;
         when I32_Load_8_S =>
            Offset_I32_Load_8_S   : Unsigned_64;
            Mem_Addr_I32_Load_8_S : Mem_Addr;
         when I32_Load_8_U =>
            Offset_I32_Load_8_U   : Unsigned_64;
            Mem_Addr_I32_Load_8_U : Mem_Addr;
         when I32_Load_16_S =>
            Offset_I32_Load_16_S   : Unsigned_64;
            Mem_Addr_I32_Load_16_S : Mem_Addr;
         when I32_Load_16_U =>
            Offset_I32_Load_16_U   : Unsigned_64;
            Mem_Addr_I32_Load_16_U : Mem_Addr;
         when I64_Load_8_S =>
            Offset_I64_Load_8_S   : Unsigned_64;
            Mem_Addr_I64_Load_8_S : Mem_Addr;
         when I64_Load_8_U =>
            Offset_I64_Load_8_U   : Unsigned_64;
            Mem_Addr_I64_Load_8_U : Mem_Addr;
         when I64_Load_16_S =>
            Offset_I64_Load_16_S   : Unsigned_64;
            Mem_Addr_I64_Load_16_S : Mem_Addr;
         when I64_Load_16_U =>
            Offset_I64_Load_16_U   : Unsigned_64;
            Mem_Addr_I64_Load_16_U : Mem_Addr;
         when I64_Load_32_S =>
            Offset_I64_Load_32_S   : Unsigned_64;
            Mem_Addr_I64_Load_32_S : Mem_Addr;
         when I64_Load_32_U =>
            Offset_I64_Load_32_U   : Unsigned_64;
            Mem_Addr_I64_Load_32_U : Mem_Addr;
         when I32_Store =>
            Offset_I32_Store   : Unsigned_64;
            Mem_Addr_I32_Store : Mem_Addr;
         when I64_Store =>
            Offset_I64_Store   : Unsigned_64;
            Mem_Addr_I64_Store : Mem_Addr;
         when F32_Store =>
            Offset_F32_Store   : Unsigned_64;
            Mem_Addr_F32_Store : Mem_Addr;
         when F64_Store =>
            Offset_F64_Store   : Unsigned_64;
            Mem_Addr_F64_Store : Mem_Addr;
         when I32_Store_8 =>
            Offset_I32_Store_8   : Unsigned_64;
            Mem_Addr_I32_Store_8 : Mem_Addr;
         when I32_Store_16 =>
            Offset_I32_Store_16   : Unsigned_64;
            Mem_Addr_I32_Store_16 : Mem_Addr;
         when I64_Store_8 =>
            Offset_I64_Store_8   : Unsigned_64;
            Mem_Addr_I64_Store_8 : Mem_Addr;
         when I64_Store_16 =>
            Offset_I64_Store_16   : Unsigned_64;
            Mem_Addr_I64_Store_16 : Mem_Addr;
         when I64_Store_32 =>
            Offset_I64_Store_32   : Unsigned_64;
            Mem_Addr_I64_Store_32 : Mem_Addr;
         when Mem_Size =>
            Offset_Mem_Size   : Unsigned_64;
            Mem_Addr_Mem_Size : Mem_Addr;
         when Mem_Grow =>
            Offset_Mem_Grow   : Unsigned_64;
            Mem_Addr_Mem_Grow : Mem_Addr;
         when Mem_Init =>
            Offset_Mem_Init   : Unsigned_64;
            Mem_Addr_Mem_Init : Mem_Addr;
         when Mem_Drop =>
            Offset_Mem_Drop   : Unsigned_64;
            Mem_Addr_Mem_Drop : Mem_Addr;
         when Mem_Copy =>
            Offset_Mem_Copy   : Unsigned_64;
            Mem_Addr_Mem_Copy : Mem_Addr;
         when Mem_Fill =>
            Offset_Mem_Fill   : Unsigned_64;
            Mem_Addr_Mem_Fill : Mem_Addr;
      end case;
   end record;

   type Vector_Instruction (Op : Opcode_Vector := Extract) is record
      case Op is
         when others =>
            null;
      end case;
   end record;

   type Optional_Instruction_Sequence (Has_Target : Boolean := False) is record
      case Has_Target is
         when True =>
            Instr_target : Instruction_Sequence;
         when False =>
            null;
      end case;
   end record;

   type Instruction (Op : Opcode := Numeric) is record
      case Op is
         when Numeric =>
            Numeric_Inst : Numeric_Instruction;
         when Reference =>
            Reference_Inst : Reference_Instruction;
         when Control =>
            Control_Inst : Control_Instruction;
         when Table =>
            Table_Inst : Table_Instruction;
         when Parametric =>
            Parametric_Inst : Parametric_Instruction;
         when Variable =>
            Variable_Inst : Variable_Instruction;
         when Memory =>
            Memory_Inst : Memory_Instruction;
         when Vector_128 =>
            Vector_Inst : Vector_Instruction;
      end case;
   end record;

   subtype Constant_Expression is Instruction (Numeric);  --  todo

   function Get_Block_Type return Block_Type;

end Instructions;
