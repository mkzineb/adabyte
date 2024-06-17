with Instructions;                          use Instructions;
with Interpreter;                           use Interpreter;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
package Numerics is

   --  generic
   --     type Operand is private;
   --     with function Execute_Unary
   --       (Instr : Numeric_Instruction; Op : Operand) return Integer;

   procedure Execute_Unary_Inst
     (Instr       :        Numeric_Instruction; Op : Opcode_Numeric;
      Environment : in out Env);

end Numerics;
