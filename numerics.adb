with Types;                                 use Types;
with Interpreter;                           use Interpreter;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
package body Numerics is

   type Result_Types is (Result_I32, Result_I64, Result_F32, Result_F64);
   type Result (R : Result_Types := Result_I32) is record
      case R is
         when Result_I32 =>
            Result_I32 : Number_Type;
         when Result_I64 =>
            Result_I64 : Number_Type (I_64);
         when Result_F32 =>
            Result_F32 : Number_Type (F_32);
         when Result_F64 =>
            Result_F64 : Number_Type (F_64);
      end case;
   end record;

   procedure Execute_Unary_Inst
     (Instr       :        Numeric_Instruction; Op : Opcode_Numeric;
      Environment : in out Env)
   is
      R           : Result;
      Tmp_Integer : Element_Variation;
   begin
      case Instr.Op is
         when I32_Constant =>
            Environment.Stack.Append
              ((Elt => Value, Val => Instr.I32_Constant));
         when I64_Constant =>
            Environment.Stack.Append
              ((Elt => Value, Val => Instr.I64_Constant));
         when F32_Constant =>
            Environment.Stack.Append
              ((Elt => Value, Val => Instr.F32_Constant));
         when F64_Constant =>
            Environment.Stack.Append
              ((Elt => Value, Val => Instr.F64_Constant));
         when Count_Lead_Zeros =>
            Tmp_Integer := Environment.Stack.Last_Element;
            --  compute
            Environment.Stack.Append (Tmp_Integer);
         when others =>
            null;
      end case;
   end Execute_Unary_Inst;

end Numerics;
