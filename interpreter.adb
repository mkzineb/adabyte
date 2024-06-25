with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Interpreter is

   procedure Free is new Ada.Unchecked_Deallocation
     (Chunk, Instruction_Sequence);

   function Count_Lables (Environment : Env) return Natural is
      Label_Count : Natural := 0;
   begin
      for I in Environment.Stack.First_Index .. Environment.Stack.Last_Index
      loop
         if Environment.Stack (I).Elt = Label then
            Label_Count := Label_Count + 1;
         end if;
      end loop;
      return Label_Count;
   end Count_Lables;

   function Init_Environment
     (S : Store_Type; Stack : Vector; i : Instruction_Sequence) return Env
   is
      Current_Frame  : Call_Frame;
      Current_Module : Module_Instance;
      Tab            : Symbols_Table.Map;

   begin
      return
        (Stack  => Stack, Store => S, Symbols => Tab, Cf => Current_Frame,
         Module => Current_Module, Temp => i);
   end Init_Environment;

   function Exec_Return (Environment : in out Env) return Control_Flow is
      Old : Unsigned_32;
   begin
      Old := Environment.Cf.Block_Ptr;
      --     --  no call frame is in the stack
      --     --  pop the last call frame in the stack
      --     --  delete block
      --  Swap_Module_Context
      return Continue;
   end Exec_Return;

   function Count_Values_Top_Stack (Environment : Env) return Natural is
      Value_Size : Natural := 0;
   begin
      for I in Environment.Stack.First_Index .. Environment.Stack.Last_Index
      loop
         if Environment.Stack (I).Elt = Value then
            Value_Size := Value_Size + 1;
         else
            exit;
         end if;
      end loop;
      return Value_Size;
   end Count_Values_Top_Stack;

   function Pop_Values (N : Natural; Environment : in out Env) return Vector is
      Popped_Values : Vector;
   begin
      for i in 0 .. N loop
         Popped_Values.Append (Last_Element (Environment.Stack));
         Delete_Last (Environment.Stack);
      end loop;
      return Popped_Values;
   end Pop_Values;

   procedure Pop_Label_Or_Values (Position : Natural; Stack : in out Vector) is
   begin
      for i in 1 .. Position + 1 loop
         if Stack.Last_Element.Elt = Label then
            Delete_Last (Stack);
         else
            for j in 1 .. Position + 1 loop
               pragma Assert
                 (Stack.Length > 0, "Not enough elements in the stack.");

               Delete_Last (Stack);
            end loop;
            exit;
         end if;
      end loop;
   end Pop_Label_Or_Values;

   procedure Push_Values (Temp : Vector; N : Natural; Environment : in out Env)
   is
   begin
      for i in 0 .. N loop
         Environment.Stack.Append_Vector (Temp);
      end loop;
   end Push_Values;

   function Get_Lab_Position (To : Unsigned_32; Stack : Vector) return Natural
   is
   begin
      for Index in Stack.First_Index .. Stack.Last_Index loop
         if Stack (Index).Elt = Label then
            if Stack (Index).Special_Id = To then
               return Index;
            end if;
         end if;
      end loop;
      raise Program_Error;
   end Get_Lab_Position;

   function Get_Lab_From_Position
     (Pos : Natural; Stack : Vector) return Unsigned_32
   is
   begin
      if (Pos >= First_Index (Stack)) and (Pos <= Last_Index (Stack)) then
         pragma Assert
           (Stack (Pos).Element.Elt = Label,
            "Not a Label at Position: " & Pos'Img);
         return Stack (Pos).Special_Id;
      else
         raise Constraint_Error with "Position out of range";
      end if;
   end Get_Lab_From_Position;

   procedure Jump_If_Block_Exists (To : Unsigned_32; Environment : in out Env)
   is
      Lab_Arity   : Natural;
      Position    : Natural;
      Lab_Addr    : Unsigned_32;
      Tmp_Storage : Vector;
   begin
      Position := Get_Lab_Position (To, Environment.Stack);
      Lab_Addr := Get_Lab_From_Position (Position, Environment.Stack);

      if Symbols_Table.Contains (Environment.Symbols, Lab_Addr) then
         pragma Assert
           (Count_Lables (Environment) >= Position + 1,
            "Stack does not contain enough labels");

         Lab_Arity :=
           Symbols_Table.Element (Environment.Symbols, Lab_Addr).Arity;

         pragma Assert (Count_Values_Top_Stack (Environment) >= Lab_Arity);

         Tmp_Storage := Pop_Values (Lab_Arity, Environment);

         Pop_Label_Or_Values (Position, Environment.Stack);

         Push_Values (Tmp_Storage, Lab_Arity, Environment);

         Environment.Cf.Instr_Ptr := Unsigned_32 (Lab_Addr);
      else
         raise Program_Error with "Label not found in symbol table.";
      end if;
   end Jump_If_Block_Exists;

   function Execute_Branching
     (To : Unsigned_32; Environment : in out Env) return Control_Flow
   is
   begin
      Jump_If_Block_Exists (To, Environment);
      Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
      return Continue;
   end Execute_Branching;

   procedure Execute_Const (Stack : in out Vector; Var : Number_Type) is
   begin
      case Var.Num is
         when I_32 =>
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_32, I_32 => Var.I_32)))));
         when I_64 =>
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_64, I_64 => Var.I_64)))));
         when F_32 =>
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (F_32, F_32 => Var.F_32)))));
         when F_64 =>
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (F_64, F_64 => Var.F_64)))));
         when others =>
            null;
      end case;
   end Execute_Const;

   procedure Execute_Compare_To_Zero (Stack : in out Vector; Var : Number_Type)
   is
   begin
      --     pragma Assert
      --       (Stack.Last_Element = Value_Type,
      --        "Not 'Value Type' on top of the stack");

      Delete_Last (Stack);
      if Var.I_32 = 0 or else Var.I_64 = 0 then
         case Var.Num is
            when I_32 =>
               Stack.Append
                 ((Elt => Value,
                   Val =>
                     (Val =>
                        (Number_Value,
                          Num_Value => (I_32, I_32 => Var.I_32)))));
            when I_64 =>
               Stack.Append
                 ((Elt => Value,
                   Val =>
                     (Val =>
                        (Number_Value,
                          Num_Value => (I_64, I_64 => Var.I_64)))));
            when others =>
               null;
         end case;
      end if;
   end Execute_Compare_To_Zero;

   procedure Push_Value (Stack : in out Vector; Num : Number_Type) is
   begin
      case Num.Num is
         when I_32 =>
            Stack.Append
              ((Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_32, I_32 => Num.I_32)))));
         when I_64 =>
            Stack.Append
              ((Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_32, I_32 => Num.I_32)))));
         when F_32 =>
            Stack.Append
              ((Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_32, I_32 => Num.I_32)))));
         when F_64 =>
            Stack.Append
              ((Value,
                Val =>
                  (Val =>
                     (Number_Value, Num_Value => (I_32, I_32 => Num.I_32)))));
         when others =>
            null;
      end case;
   end Push_Value;

   procedure Compare_And_Push_I32
     (Stack : in out Vector; A, B : Integer_32; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (I_32, I_32 => 0));
         else
            Push_Value (Stack, (I_32, I_32 => 1));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_I32;

   procedure Compare_And_Push_I64
     (Stack : in out Vector; A, B : Integer_64; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (I_64, I_64 => 0));
         else
            Push_Value (Stack, (I_64, I_64 => 1));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_I64;

   procedure Compare_And_Push_F32
     (Stack : in out Vector; A, B : IEEE_Float_32; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (F_32, F_32 => 0.0));
         else
            Push_Value (Stack, (F_32, F_32 => 1.0));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_F32;

   procedure Compare_And_Push_F64
     (Stack : in out Vector; A, B : IEEE_Float_64; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (F_64, F_64 => 0.0));
         else
            Push_Value (Stack, (F_64, F_64 => 1.0));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_F64;

   procedure Compare_And_Push_U32
     (Stack : in out Vector; A, B : Unsigned_32; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (U_32, U_32 => 0));
         else
            Push_Value (Stack, (U_32, U_32 => 1));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_U32;

   procedure Compare_And_Push_U64
     (Stack : in out Vector; A, B : Unsigned_64; Op : String)
   is
   begin
      if Op = "==" then
         if A = B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      elsif Op = "!=" then
         if A /= B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      elsif Op = "<" then
         if A < B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      elsif Op = ">" then
         if A > B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      elsif Op = "<=" then
         if A <= B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      elsif Op = ">=" then
         if A >= B then
            Push_Value (Stack, (U_64, U_64 => 0));
         else
            Push_Value (Stack, (U_64, U_64 => 1));
         end if;
      else
         raise Program_Error with "Unsupported operation";
      end if;
   end Compare_And_Push_U64;

   procedure Execute_Compare (Stack : in out Vector; Nt : Number; Op : String)
   is
      A, B : Element_Variation;

   begin
      -- pragma assert tp 2 value type
      A := Last_Element (Stack);
      Delete_Last (Stack);
      B := Last_Element (Stack);
      Delete_Last (Stack);
      case Nt is
         when I_32 =>
            Compare_And_Push_I32
              (Stack, A.Val.Val.Num_Value.I_32, B.Val.Val.Num_Value.I_32, Op);
         when I_64 =>
            Compare_And_Push_I64
              (Stack, A.Val.Val.Num_Value.I_64, B.Val.Val.Num_Value.I_64, Op);
         when U_32 =>
            Compare_And_Push_U32
              (Stack, A.Val.Val.Num_Value.U_32, B.Val.Val.Num_Value.U_32, Op);
         when U_64 =>
            Compare_And_Push_U64
              (Stack, A.Val.Val.Num_Value.U_64, B.Val.Val.Num_Value.U_64, Op);
         when F_32 =>
            Compare_And_Push_F32
              (Stack, A.Val.Val.Num_Value.F_32, B.Val.Val.Num_Value.F_32, Op);
         when F_64 =>
            Compare_And_Push_F64
              (Stack, A.Val.Val.Num_Value.F_64, B.Val.Val.Num_Value.F_64, Op);
      end case;
   end Execute_Compare;

   procedure Count_Leading_Zeros (Value : Integer_Type) return Natural is
      Result : Natural := 0;
   begin
      if Value /= 0 then
         while Value'First <= Result loop
            exit when Value and 1 /= 0;
            Value  := Shift_Right (Value, 1);
            Result := Result + 1;
         end loop;
      end if;
      return Result;
   end Count_Leading_Zeros;

   procedure Execute_Numeric_Instruction
     (Instr : Numeric_Instruction; Environment : in out Env)
   is
   begin
      case Instr.Op is
         when I32_Const =>
            Execute_Const (Environment.Stack, Instr.I32_Constant);
         when I64_Const =>
            Execute_Const (Environment.Stack, Instr.I64_Constant);
         when F32_Const =>
            Execute_Const (Environment.Stack, Instr.F32_Constant);
         when F64_Const =>
            Execute_Const (Environment.Stack, Instr.F64_Constant);
         when I32_Eqz =>
            Execute_Compare_To_Zero (Environment.Stack, Instr.I32_Constant);
         when I32_Eq =>
            Execute_Compare (Environment.Stack, I_32, "==");
         when I32_Ne =>
            Execute_Compare (Environment.Stack, I_32, "!=");
         when I32_Lt_S =>
            Execute_Compare (Environment.Stack, I_32, "<<");
         when I32_Lt_U =>
            Execute_Compare (Environment.Stack, U_32, "<<");
         when I32_Gt_S =>
            Execute_Compare (Environment.Stack, I_32, ">>");
         when I32_Gt_U =>
            Execute_Compare (Environment.Stack, U_32, ">>");
         when I32_Le_S =>
            Execute_Compare (Environment.Stack, I_32, "<=");
         when I32_Le_U =>
            Execute_Compare (Environment.Stack, U_32, "<=");
         when I32_Ge_S =>
            Execute_Compare (Environment.Stack, I_32, ">=");
         when I32_Ge_U =>
            Execute_Compare (Environment.Stack, U_32, ">=");
         when I64_Eqz =>
            Execute_Compare_To_Zero (Environment.Stack, Instr.I64_Constant);
         when I64_Eq =>
            Execute_Compare (Environment.Stack, I_64, "==");
         when I64_Ne =>
            Execute_Compare (Environment.Stack, I_64, "!=");
         when I64_Lt_S =>
            Execute_Compare (Environment.Stack, I_64, "<<");
         when I64_Lt_U =>
            Execute_Compare (Environment.Stack, U_64, "<<");
         when I64_Gt_S =>
            Execute_Compare (Environment.Stack, I_64, ">>");
         when I64_Gt_U =>
            Execute_Compare (Environment.Stack, U_64, ">>");
         when I64_Le_S =>
            Execute_Compare (Environment.Stack, I_64, "<=");
         when I64_Le_U =>
            Execute_Compare (Environment.Stack, U_64, "<=");
         when I64_Ge_S =>
            Execute_Compare (Environment.Stack, I_64, ">=");
         when I64_Ge_U =>
            Execute_Compare (Environment.Stack, U_64, ">=");
         when F32_Eq =>
            Execute_Compare (Environment.Stack, F_32, "==");
         when F32_Ne =>
            Execute_Compare (Environment.Stack, F_32, "!=");
         when F32_Lt =>
            Execute_Compare (Environment.Stack, F_32, "<<");
         when F32_Gt =>
            Execute_Compare (Environment.Stack, F_32, ">>");
         when F32_Le =>
            Execute_Compare (Environment.Stack, F_32, "<=");
         when F32_Ge =>
            Execute_Compare (Environment.Stack, F_32, ">=");
         when F64_Eq =>
            Execute_Compare (Environment.Stack, F_64, "==");
         when F64_Ne =>
            Execute_Compare (Environment.Stack, F_64, "!=");
         when F64_Lt =>
            Execute_Compare (Environment.Stack, F_64, "<<");
         when F64_Gt =>
            Execute_Compare (Environment.Stack, F_64, ">>");
         when F64_Le =>
            Execute_Compare (Environment.Stack, F_64, "<=");
         when F64_Ge =>
            Execute_Compare (Environment.Stack, F_64, ">=");
         when I32_Clz =>
            Execute_Count (Environment.Stack, I_32);
         when I32_Ctz =>
            null;
         when I32_Popcnt =>
            null;
         when I32_Add =>
            null;
         when I32_Sub =>
            null;
         when I32_Mul =>
            null;
         when I32_Div_S =>
            null;
         when I32_Div_U =>
            null;
         when I32_Rem_S =>
            null;
         when I32_Rem_U =>
            null;
         when I64_Clz =>
            null;
         when I64_Ctz =>
            null;
         when I64_Popcnt =>
            null;
         when I64_Add =>
            null;
         when I64_Sub =>
            null;
         when I64_Mul =>
            null;
         when I64_Div_S =>
            null;
         when I64_Div_U =>
            null;
         when I64_Rem_S =>
            null;
         when I64_Rem_U =>
            null;
         when I32_And =>
            null;
         when I32_Or =>
            null;
         when I32_Xor =>
            null;
         when I32_Shl =>
            null;
         when I32_Shr_S =>
            null;
         when I32_Shr_U =>
            null;
         when I32_Rotl =>
            null;
         when I32_Rotr =>
            null;
         when I64_And =>
            null;
         when I64_Or =>
            null;
         when I64_Xor =>
            null;
         when I64_Shl =>
            null;
         when I64_Shr_S =>
            null;
         when I64_Shr_U =>
            null;
         when I64_Rotl =>
            null;
         when I64_Rotr =>
            null;
         when F32_Abs =>
            null;
         when F32_Neg =>
            null;
         when F32_Ceil =>
            null;
         when F32_Floor =>
            null;
         when F32_Trunc =>
            null;
         when F32_Nearest =>
            null;
         when F32_Sqrt =>
            null;
         when F32_Add =>
            null;
         when F32_Sub =>
            null;
         when F32_Mul =>
            null;
         when F32_Div =>
            null;
         when F32_Min =>
            null;
         when F32_Max =>
            null;
         when F32_Copysign =>
            null;
         when F64_Abs =>
            null;
         when F64_Neg =>
            null;
         when F64_Ceil =>
            null;
         when F64_Floor =>
            null;
         when F64_Trunc =>
            null;
         when F64_Nearest =>
            null;
         when F64_Sqrt =>
            null;
         when F64_Add =>
            null;
         when F64_Sub =>
            null;
         when F64_Mul =>
            null;
         when F64_Div =>
            null;
         when F64_Min =>
            null;
         when F64_Max =>
            null;
         when F64_Copysign =>
            null;
         when I32_Wrap_I64 =>
            null;
         when I32_Trunc_F32_S =>
            null;
         when I32_Trunc_F32_U =>
            null;
         when I32_Trunc_F64_S =>
            null;
         when I32_Trunc_F64_U =>
            null;
         when I32_Extend_8_S =>
            null;
         when I32_Extend_16_S =>
            null;
         when I64_Extend_8_S =>
            null;
         when I64_Extend_16_S =>
            null;
         when I64_Extend_32_S =>
            null;
         when I64_Extend_I32_S =>
            null;
         when I64_Extend_I32_U =>
            null;
         when I64_Trunc_F32_S =>
            null;
         when I64_Trunc_F32_U =>
            null;
         when I64_Trunc_F64_S =>
            null;
         when I64_Trunc_F64_U =>
            null;
         when F32_Convert_I32_S =>
            null;
         when F32_Convert_I32_U =>
            null;
         when F32_Convert_I64_S =>
            null;
         when F32_Convert_I64_U =>
            null;
         when F32_Demote_F64 =>
            null;
         when F64_Convert_I32_S =>
            null;
         when F64_Convert_I32_U =>
            null;
         when F64_Convert_I64_S =>
            null;
         when F64_Convert_I64_U =>
            null;
         when F64_Promote_F32 =>
            null;
         when I32_Reinterpret_F32 =>
            null;
         when I64_Reinterpret_F64 =>
            null;
         when F32_Reinterpret_I32 =>
            null;
         when F64_Reinterpret_I64 =>
            null;
         when I32_Trunc_Sat_F32_S =>
            null;
         when I32_Trunc_Sat_F32_U =>
            null;
         when I32_Trunc_Sat_F64_S =>
            null;
         when I32_Trunc_Sat_F64_U =>
            null;
         when I64_Trunc_Sat_F32_S =>
            null;
         when I64_Trunc_Sat_F32_U =>
            null;
         when I64_Trunc_Sat_F64_S =>
            null;
         when I64_Trunc_Sat_F64_U =>
            null;
      end case;
   end Execute_Numeric_Instruction;

   procedure Execute_Block_Entry
     (Instr     : Control_Instruction; Environment : in out Env;
      Instr_Ptr : Unsigned_32; End_Instr_Offset : Unsigned_32; Ty : Block_Type;
      Args      : Block_Args)
   is
      Params, Results : Unsigned_8 := 0;
      New_Block_Frame : Block_Frame;
      Arity           : Natural    := 0;
      Lab_Ad          : Address    := 0;
   begin
      Put_Line ("   Entering a block");
      case Args.B is
         when Empty =>
            Params  := 0;
            Results := 0;
         when TType =>
            Params  := 0;
            Results := 1;
         when Func_Type =>
            declare
               Typ : Block_Type := Args.Func_Ty;
            begin
               Params  := 0;
               Results := 0;
            end;
            --  get params/results from function
            --  get type of block as well  (loop, block, if...)
      end case;
      New_Block_Frame :=
        (Instr_ptr        => Environment.Cf.Instr_Ptr,
         End_Instr_Offset => Instr.End_Block_Ptr, Results => Results,
         Params           => Params, B_Type => Ty);
      --  push label in stack
      Put_Line ("       Adding label to the stack");

      Environment.Stack.Append
        ((Elt => Label, Special_Id => Instr.Label, Block => New_Block_Frame));

      Put_Line ("       Adding label to the Symbols table");

      --  add info in table
      Environment.Symbols.Insert
        (Instr.Label, (Arity => Arity, Lab_Addr => Lab_Ad));

      Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
   end Execute_Block_Entry;

   function Get_Blocks_Length (Stack : Vector) return Natural is
      Length : Natural := 0;
   begin
      for I in Stack.First_Index .. Stack.Last_Index loop
         Length := Length + 1;
      end loop;
      return Length;
   end Get_Blocks_Length;

   procedure Remove_Last_Block_Label (Stack : in out Vector) is
   begin
      for I in Stack.First_Index .. Stack.Last_Index loop
         if Stack (I).Element.Elt = Label then
            Delete (Stack, I);
            exit;
         end if;
      end loop;
   end Remove_Last_Block_Label;

   procedure Execute_End_Block
     (Instr : Control_Instruction; Environment : in out Env)
   is
      Block : Block_Frame;
   begin
      if Get_Blocks_Length (Environment.Stack) > 0 then
         Remove_Last_Block_Label (Environment.Stack);
      else
         raise Program_Error with "No Block has been found";
      end if;
   end Execute_End_Block;

   procedure Execute_If (Instr : Control_Instruction; Environment : in out Env)
   is
      Value_To_Evaluate : Unsigned_32;
      Old               : Unsigned_32 := 0;
   begin
      Value_To_Evaluate :=
        Unsigned_32 (Environment.Stack.Last_Element.Val.Val.Num_Value.I_32);
      Delete_Last (Environment.Stack);

      if Value_To_Evaluate /= 0 then
         Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
         Execute_Block_Entry
           (Instr, Environment, Environment.Cf.Instr_Ptr, Instr.End_If_Offset,
            If_Block, Instr.If_Block_Args);
      end if;

      if Instr.Else_If_Offset = 0 then
         Environment.Cf.Instr_Ptr :=
           Environment.Cf.Instr_Ptr + Instr.End_Else_Offset;
      end if;

      Old                      := Environment.Cf.Instr_Ptr;
      Environment.Cf.Instr_Ptr :=
        Environment.Cf.Instr_Ptr + Instr.Else_If_Offset;
      Execute_Block_Entry
        (Instr, Environment, Old + Instr.Else_If_Offset,
         Instr.End_If_Offset - Instr.Else_If_Offset, Else_Block,
         Instr.If_Block_Args);
   end Execute_If;

   function Execute_Branching_If
     (Instr : Control_Instruction; Environment : in out Env)
      return Control_Flow
   is
      Value_To_Evaluate : Unsigned_32;
   begin
      Value_To_Evaluate :=
        Unsigned_32 (Environment.Stack.Last_Element.Val.Val.Num_Value.I_32);
      Delete_Last (Environment.Stack);
      if Value_To_Evaluate /= 0 then
         return Execute_Branching (Instr.Label_Br_If, Environment);
      else
         Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
      end if;
      return Continue;
   end Execute_Branching_If;

   procedure Execute_Nop is
   begin
      null;
   end Execute_Nop;

   procedure Execute_Branching_Table
     (Instr : Control_Instruction; Environment : in out Env)
   is
   begin
      null;
   end Execute_Branching_Table;

   procedure Execute_Else
     (Instr : Control_Instruction; Environment : in out Env)
   is
   begin
      Execute_End_Block (Instr, Environment);
      Environment.Cf.Instr_Ptr :=
        Environment.Cf.Instr_Ptr + Instr.End_Else_Offset;
   end Execute_Else;

   function Execute_Unreachable return Control_Flow is
   begin
      return Trap;
   end Execute_Unreachable;

   function Execute_Control_Instruction
     (Instr : Control_Instruction; Environment : in out Env)
      return Control_Flow
   is
   begin
      case Instr.Op is
         when Block =>
            Put_Line ("  Block instruction");
            Execute_Block_Entry
              (Instr, Environment, Environment.Cf.Instr_Ptr,
               Instr.End_Block_Ptr, Instr.Block_Arguments.Func_Ty,
               Instr.Block_Arguments);
         when End_Block =>
            Execute_End_Block (Instr, Environment);
         when Branch =>
            return Execute_Branching (Instr.Label_Br, Environment);
         when If_Inst =>
            Execute_If (Instr, Environment);
         when Else_Inst =>
            Execute_Else (Instr, Environment);
         when Loop_Inst =>
            Execute_Block_Entry
              (Instr, Environment, Environment.Cf.Instr_Ptr,
               Instr.End_Loop_Block, Instr.Loop_Arguments.Func_Ty,
               Instr.Loop_Arguments);
         when Branch_If =>
            return Execute_Branching_If (Instr, Environment);
         when NOP =>
            Execute_Nop;
         when Branch_Table =>
            Execute_Branching_Table (Instr, Environment);
         when Return_Inst =>
            return Exec_Return (Environment);
         when Call =>
            null;
         when Call_Indirect =>
            null;
         when Unreachable =>
            return Execute_Unreachable;
      end case;
      return Continue;
   end Execute_Control_Instruction;

   procedure Execute_Reference_Instruction
     (Instr : Reference_Instruction; Environment : in out Env)
   is
   begin
      null;
   end Execute_Reference_Instruction;

   procedure Execute_Table_Instruction
     (Instr : Table_Instruction; Stack : Vector)
   is
   begin
      null;
   end Execute_Table_Instruction;

   procedure Execute_Parametric_Instruction
     (Instr : Parametric_Instruction; Stack : Vector)
   is
   begin
      null;
   end Execute_Parametric_Instruction;

   procedure Execute_Variable_Instruction
     (Instr : Variable_Instruction; Stack : Vector)
   is
   begin
      null;

   end Execute_Variable_Instruction;

   procedure Execute_Memory_Instruction
     (Instr : Memory_Instruction; Stack : Vector)
   is
   begin
      null;
   end Execute_Memory_Instruction;

   procedure Execute_Vector_128_Instruction
     (Instr : Vector_Instruction; Stack : Vector)
   is
   begin
      null;
   end Execute_Vector_128_Instruction;

   procedure Execute_Expression_Instruction
     (Sequence : Instruction_Sequence; Stack : Vector)
   is
   begin
      null;
   end Execute_Expression_Instruction;

   function Execute_Next_Instr (Environment : in out Env) return Control_Flow
   is
      Inst : Instruction_Acc;
   begin
      Inst := Environment.Temp (Natural (Environment.Cf.Instr_Ptr));
      Put_Line (Environment.Cf.Instr_Ptr'Img & " instruction");
      case Inst.Op is
         when Numeric =>
            Put_Line ("numeric instruction");
            Execute_Numeric_Instruction (Inst.Numeric_Inst, Environment);
         when Control =>
            Put_Line ("control instruction");
            return
              Execute_Control_Instruction (Inst.Control_Inst, Environment);
         when Reference =>
            Execute_Reference_Instruction (Inst.Reference_Inst, Environment);
         when Table =>
            Execute_Table_Instruction (Inst.Table_Inst, Environment.Stack);
         when Parametric =>
            Execute_Parametric_Instruction
              (Inst.Parametric_Inst, Environment.Stack);
         when Variable =>
            Execute_Variable_Instruction
              (Inst.Variable_Inst, Environment.Stack);
         when Memory =>
            Execute_Memory_Instruction (Inst.Memory_Inst, Environment.Stack);
         when Vector_128 =>
            Execute_Vector_128_Instruction
              (Inst.Vector_Inst, Environment.Stack);
         when Expression =>
            Execute_Expression_Instruction
              (Inst.Expression, Environment.Stack);
      end case;
      Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
      return Continue;
   end Execute_Next_Instr;

   function Run (Environment : in out Env) return Interpret_Result is
      Control : Control_Flow;
   begin
      loop
         Control := Execute_Next_Instr (Environment);
         case Control is
            when Break =>
               return INTERPRET_OK;
            when Continue =>
               null;
            when Trap =>
               return INTERPRET_RUNTIME_ERROR;
               exit;
         end case;
      end loop;
   end Run;
end Interpreter;
