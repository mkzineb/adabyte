with Types;      use Types;
with Interfaces; use Interfaces;
with Indices;    use Indices;
package Instructions is

   --    Control Instructions
   --    Reference Instructions
   --    Parametric Instructions
   --    Variable Instructions
   --    Table Instructions
   --    Memory Instructions
   --    Numeric Instructions
   --    Vector Instructions

   type Block_Type is record
      Index : Type_Index;
      Value : Value_Type;
   end record;
   type Sign_Extension is (Signed, Unsigned);

   type Opcode is
     (Numeric, Reference, Parametric, Variable, Table, Memory, Control,
      Vector);

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
     (Unreachable, NOP, Block, Lopp_Inst, If_Inst, Branch, Branch_If,
      Return_Inst, Call, Call_Indirect);

   type Opcode_Parametric is (Drop, Select_Inst);

   type Opcode_Variable is
     (Local_Get, Local_Set, Local_Tee, Global_Get, Global_Set);

   type Opcode_Table is (Get, Set, Size, Grow, Fill, Copy, Init, Element_Drop);

   type Opcode_Memory is
     (Load, Store, Load_8, Load_16, Load_32, Store_8, Store_16, Store_32,
      Size);

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
            Add : Number_Type;
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
            null;--  check for a null value
         when Func_Ref =>
            Func_Idx : Function_Index; -- func producing a ref to a func
      end case;
   end record;

   type Control_Instruction (Op : Opcode_Control := NOP) is record
      case Op is
         when If_Inst =>
            Block : Block_Type;

         when others =>
            null;
      end case;
   end record;

   --  type Parametric_Instruction;
   --  type Variable_Instruction;
   --  type Table_Instruction;
   --  type Memory_Instruction;
   --  type Vector_Instruction;

   type Instruction (Op : Opcode := Numeric) is record
      case Op is
         when Numeric =>
            Numeric_Inst : Numeric_Instruction;
         when Reference =>
            Reference_Inst : Reference_Instruction;
         when Control =>
            Control_Inst : Control_Instruction;
            --  when Parametric =>
            --     Parametric_Inst : Parametric_Instruction;
            --  when Variable =>
            --     Variable_Inst : Variable_Instruction;
            --  when Table =>
            --     Table_Inst : Table_Instruction;
            --  when Memory =>
            --     Memory_Inst : Memory_Instruction;
            --  when Vector =>
            --     Vector_Inst : Vector_Instruction;
         when others =>
            null;
      end case;

   end record;

end Instructions;
