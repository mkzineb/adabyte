with Types;      use Types;
with Interfaces; use Interfaces;
with Runtime;    use Runtime;
package Instructions with
  SPARK_Mode => On
is

   subtype Function_Index is Unsigned_32;
   subtype Type_Index is Unsigned_32;
   subtype Table_Index is Unsigned_32;
   subtype Memory_Index is Unsigned_32;
   subtype Global_Index is Unsigned_32;
   subtype Element_Index is Unsigned_32;
   subtype Data_Index is Unsigned_32;
   subtype Local_Index is Unsigned_32;
   subtype Label_Index is Unsigned_32;

   type Empty_Type is (E);
   for Empty_Type use (E => 40);

   type Instruction;
   type Instruction_Acc is access all Instruction;

   type Chunk is array (Natural range <>) of Instruction_Acc;
   type Chunk_Acc is access Chunk;

   type Instruction_Sequence is record
      Sequence : Chunk_Acc;
   end record;

   type B_Type is (Index, Empty, Val);
   type Block_Type (B : B_Type := Index) is record
      case B is
         when Index =>
            Index : Type_Index;
         when Empty =>
            Empty : Empty_Type;
         when Val =>
            Block_Value : Value_Type;
      end case;
   end record;

   type Sign_Extension is (Signed, Unsigned);

   type Opcode is
     (Numeric, Reference, Parametric, Variable, Table, Memory, Control,
      Vector_128, Expression, Admin);

   type Opcode_Numeric is
     (I32_Constant, I64_Constant, F32_Constant, F64_Constant, Count_Lead_Zeros,
      Count_Trail_Zeros, Count_Ones, Absolute_Value, Negate, Square_Root,
      Ceiling, Floor, Truncate, Nearest, Add, Substract, Multiply,
      Divide_Integer, Divide_Float, Remainder, And_Inst, Or_Inst, Xor_Inst,
      Shift_Left, Shift_Right, Rotate_Left, Rotate_Right, Minimum, Maximum,
      Copy_Sign, Equal_To_Zero, Equal, Not_Equal, Less_Than_Integer,
      Less_Than_Float, Greater_Than_Integer, Greater_Than_Float,
      Less_Than_Or_Equal_To_Integer, Less_Than_Or_Equal_To_Float,
      Greater_Than_Or_Equal_To_Integer, Greater_Than_Or_Equal_To_Float,
      Extend_Signed_8, Extend_Signed_16, Extend_Signed_32, Wrap,
      Extend_With_Sign_Extension, Convert_And_Truncate,
      Convert_And_Truncate_With_Saturation, Demote, Promote, Convert,
      Reinterpret_Float, Reinterpret_Integer);

   type Opcode_Reference is (Null_Value, Is_Null, Func_Ref);

   type Opcode_Control is
     (Unreachable, NOP, Block, Loop_Inst, If_Inst, Branch, Branch_If,
      Branch_Table, Return_Inst, Call, Call_Indirect);

   type Opcode_Parametric is (Drop, Select_Inst);
   --  select possibly followed by a type notation
   type Opcode_Variable is
     (Local_Get, Local_Set, Local_Tee, Global_Get, Global_Set);

   type Opcode_Table is (Get, Set, Init, Drop, Copy, Grow, Size, Fill);

   type Mem_Arg is record
      Align  : Unsigned_32;
      Offset : Unsigned_32;
   end record;

   type Opcode_Memory is
     (I32_Load, I64_Load, F32_Load, F64_Load, I32_Load_8_S, I32_Load_8_U,
      I32_Load_16_S, I32_Load_16_U, I64_Load_8_S, I64_Load_8_U, I64_Load_16_S,
      I64_Load_16_U, I64_Load_32_S, I64_Load_32_U, I32_Store, I64_Store,
      F32_Store, F64_Store, I32_Store_8, I32_Store_16, I64_Store_8,
      I64_Store_16, I64_Store_32, Mem_Size, Mem_Grow, Mem_Init, Mem_Drop,
      Mem_Copy, Mem_Fill);

   type Opcode_Vector is (Extract, Splat, Pmin, Pmax);

   type Opcode_Admin is
     (Trap, Ref_Inst, Ref_Extern_Inst, Invoke, Label, Frames);
   --  todo

   type Numeric_Instruction (Op : Opcode_Numeric := I32_Constant) is record
      case Op is
         when I32_Constant =>
            I32_Constant : Number_Type (Num => I_32);
         when I64_Constant =>
            I64_Constant : Number_Type (Num => I_64);
         when F32_Constant =>
            F32_Constant : Number_Type (Num => F_32);
         when F64_Constant =>
            F64_Constant : Number_Type (Num => F_64);
         when Count_Lead_Zeros =>
            Count_L_Zeros : Integer_Type;
         when Count_Trail_Zeros =>
            Count_T_Zeros : Integer_Type;
         when Count_Ones =>
            Count_Ones : Integer_Type;
         when Absolute_Value =>
            Abs_Value : Float_Type;
         when Negate =>
            Neg : Float_Type;
         when Square_Root =>
            Square_Root : Float_Type;
         when Ceiling =>
            Ceiling : Float_Type;
         when Floor =>
            Floor : Float_Type;
         when Truncate =>
            Truncate : Float_Type;
         when Nearest =>
            Nearest : Float_Type;
         when Add =>
            null;
         when Substract =>
            Sub : Number_Type;
         when Multiply =>
            Mul : Number_Type;
         when Divide_Integer =>
            Div_Int  : Integer_Type;
            Div_Sign : Sign_Extension;
         when Divide_Float =>
            Div_Float : Float_Type;
         when Remainder =>
            Remainder : Integer_Type;
            Rem_Sign  : Sign_Extension;
         when And_Inst =>
            And_Inst : Integer_Type;
         when Or_Inst =>
            Or_Inst : Integer_Type;
         when Xor_Inst =>
            Xor_Inst : Integer_Type;
         when Shift_Left =>
            Shift_Left : Integer_Type;
         when Shift_Right =>
            Shift_Right : Integer_Type;
            Shift_Sign  : Sign_Extension;
         when Rotate_Left =>
            null;
         when Rotate_Right =>
            null;
         when Minimum =>
            null;
         when Maximum =>
            null;
         when Copy_Sign =>
            null;
         when Equal_To_Zero =>
            null;
         when Equal =>
            null;
         when Not_Equal =>
            null;
         when Less_Than_Integer =>
            null;
         when Less_Than_Float =>
            null;
         when Greater_Than_Integer =>
            null;
         when Greater_Than_Float =>
            null;
         when Less_Than_Or_Equal_To_Integer =>
            null;
         when Less_Than_Or_Equal_To_Float =>
            null;
         when Greater_Than_Or_Equal_To_Integer =>
            null;
         when Greater_Than_Or_Equal_To_Float =>
            null;
         when Extend_Signed_8 =>
            null;
         when Extend_Signed_16 =>
            null;
         when Extend_Signed_32 =>
            null;
         when Wrap =>
            null;
         when Extend_With_Sign_Extension =>
            null;
         when Convert_And_Truncate =>
            null;
         when Convert_And_Truncate_With_Saturation =>
            null;
         when Demote =>
            null;
         when Promote =>
            null;
         when Convert =>
            Int       : Integer_Type;
            Float     : Float_Type;
            Conv_Sign : Sign_Extension;
         when Reinterpret_Float =>
            Re_Float : Float_Type;
         when Reinterpret_Integer =>
            Re_Int : Integer_Type;
      end case;
   end record;

   type Reference_Instruction (Op : Opcode_Reference := Null_Value) is record
      case Op is
         when Null_Value =>
            Ref_Type : Reference_Type; -- func producing a null value
         when Is_Null =>
            null;  --  check for a null value
         when Func_Ref =>
            Func_Idx : Function_Index; -- func producing a ref to a func
      end case;
   end record;

   type Control_Instruction (Op : Opcode_Control := NOP) is record
      case Op is
         when Block =>
            B_Block : Block_Type;
            B_Inst  : Instruction_Sequence;
         when If_Inst =>
            If_Block       : Block_Type;  --  variable produced
            Then_Chunk     : Instruction_Sequence;
            Else_Opt_Chunk : Instruction_Sequence;  --  optional
         when Loop_Inst =>
            Loop_Block : Block_Type;
            Block      : Instruction_Sequence;
         when Branch_If =>
            Label_BR_If : Unsigned_32;
            Cond        : Boolean;
         when Branch =>
            Label_Br : Unsigned_32;
         when NOP =>
            null;
         when Branch_Table =>
            null;
         when Return_Inst =>
            null;
         when Call =>
            null;
         when Call_Indirect =>
            null;
         when Unreachable =>
            null;
      end case;
   end record;

   type Table_Instruction (Op : Opcode_Table := Get) is record
      --  Get, Set, Size, Grow, Fill, Copy, Init, Element_Drop
      case Op is
         when others =>
            null;
      end case;
   end record;

   type Variable_Instruction (Op : Opcode_Variable := Local_Get) is record
      case Op is
         when Local_Get =>
            Local_Get_Id : Local_Index;
         when Local_Set =>
            Local_Set_Id : Local_Index;
         when Local_Tee =>
            Local_Tee_Id : Local_Index;
         when Global_Get =>
            null;
         when Global_Set =>
            null;
      end case;

   end record;

   type Parametric_Instruction (Op : Opcode_Parametric := Drop) is record
      case Op is
         when others =>
            null;
      end case;
   end record;

   type Memory_Instruction (Op : Opcode_Memory := I32_Load) is record
      case Op is
         when others =>
            null;
      end case;
   end record;

   type Vector_Instruction (Op : Opcode_Vector := Extract) is record
      case Op is
         when others =>
            null;
      end case;
   end record;

   type Label_Branch is record
      Arity         : Unsigned_32;
      Target_Branch : Instruction_Sequence; --  empty
   end record;

   type Administrative_Instruction (Op : Opcode_Admin := Trap) is record
      case Op is
         when Trap =>
            Msg : Character;
         when Ref_Inst =>
            null;
         when Ref_Extern_Inst =>
            null; --  not supported
         when Invoke =>
            Invoke_Func_Addr : Integer;
         when Label =>
            Label_br : Label_Branch;
            Instr    : Instruction_Sequence; -- todo
         when Frames =>
            Nested_Frame : Frame;
            Frame_Instr  : Instruction_Sequence;
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
         when Expression =>
            Expression : Instruction_Sequence;
         when Admin =>
            Admin_Instruction : Administrative_Instruction;
      end case;

   end record;

   function Reduce_Instruction return Instruction;

end Instructions;
