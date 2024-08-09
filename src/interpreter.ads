------------------------------------------------------------------------------
--  Package: Interpreter
--
--  This package is the core of the interpreter, conatins all the logic for
--  executing Wasm instructions and all environmenet data structures.
--  Author: Moubarik Zineb
--  Date: 24-07-2024
------------------------------------------------------------------------------
with Interfaces;                            use Interfaces;
with Instructions;                          use Instructions;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Types;                                 use Types;
with Stack;                                 use Stack;
with Instances;                             use Instances;
with Wasm;                                  use Wasm;
with Ada.Containers.Vectors;
package Interpreter is
   use Stack_Vector;
   use Ada.Containers;

   --  Generic For comparision instructions
   type Comparison is
     (Equal, Not_Equal, Less, Greater, Less_Equal, Greater_Equal);
   generic
      type T is private;
      with function Compare (A, B : T; Op : Comparison) return Boolean;
      with procedure Push_To_Stack (Stack : in out Vector; Value : Integer);
   procedure Check_Push
     (Stack      : in out Vector; A, B : T; Comp_True : Integer;
      Comp_False :        Integer; Op : Comparison);

   type Arithmetic is (Addition, Substraction, Division, Multiplication);
   --  Generic for signed integers
   generic
      type S is private;
      with function Calculate_Signed
        (A, B : S; Op : Arithmetic; Size : Signed) return S;
      with procedure Push_Signed (Stack : in out Vector; R : S);
   procedure Calculate_Push_Signed
     (Stack : in out Vector; A, B : S; Op : Arithmetic; Size : Signed);

   --  Generic for floats
   generic
      type F is digits <>;
      with function Calculate_Float (A, B : F; Op : Arithmetic) return F;
      with procedure Push_Float (Stack : in out Vector; R : F);
   procedure Calculate_Push_Float
     (Stack : in out Vector; A, B : F; Op : Arithmetic);

   type Label_Info is record
      Arity    : Natural;
      Lab_Addr : Label_Addr;
   end record;

   function Hash (x : Unsigned_32) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unsigned_32, Element_Type => Label_Info, Hash => Hash,
      Equivalent_Keys => "=");

   use Symbols_Table;

   type Env is record
      Stack   : Stack_Vector.Vector;
      Store   : Store_Type;
      Symbols : Symbols_Table.Map;
      Cf      : Call_Frame;
      Module  : Module_Instance;
   end record;

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   type Control_Flow is (Break, Continue, Trap);

   function Count_Labels (Environment : Env) return Natural;

   function Init_Environment
     (S : Store_Type; Stack : in out Vector; Start_Func : Function_Spec)
      return Env;

   function Run (Environment : in out Env) return Interpret_Result;

   function Execute_Next_Instr (Environment : in out Env) return Control_Flow;

   type Unary_Op is (clz, ctz, popcnt);
   type Binary_Op is
     (add, sub, mul, div, rem_s, and_op, or_op, xor_op, shl, shr_s, rotl,
      rotr);

end Interpreter;
