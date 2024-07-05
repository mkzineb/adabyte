with Interfaces;                            use Interfaces;
with Instructions;                          use Instructions;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Types;                                 use Types;
with Stack;                                 use Stack;
with Instances;                             use Instances;
package Interpreter is

   type Element is (Value, Label, Activation_Call);
   type Element_Variation (Elt : Element := Value) is record
      case Elt is
         when Value =>
            Val : Value_Stack;
         when Label =>
            Special_Id : Unsigned_32;
            Block      : Block_Frame;
         when Activation_Call =>
            Call : Call_Frame;
      end case;
   end record;

   type Label_Info is record
      Arity    : Natural;
      Lab_Addr : Label_Addr;
   end record;

   function Hash (x : Unsigned_32) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unsigned_32, Element_Type => Label_Info, Hash => Hash,
      Equivalent_Keys => "=");

   use Symbols_Table;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Element_Variation);
   use Vectors;

   type Env is record
      Stack   : Vector;
      Store   : Store_Type;
      Symbols : Symbols_Table.Map;
      Cf      : Call_Frame;
      Module  : Module_Instance;
      Temp    : Instruction_Sequence;  --  temporarly
   end record;

   type Interpret_Result is
     (INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR);

   type Control_Flow is (Break, Continue, Trap);

   function Count_Labels (Environment : Env) return Natural;

   function Init_Environment
     (S : Store_Type; Stack : Vector; i : Instruction_Sequence) return Env;

   function Run (Environment : in out Env) return Interpret_Result;

   function Execute_Next_Instr (Environment : in out Env) return Control_Flow;

   --  type Unary_Op is (clz, ctz, popcnt);
   --  type Binary_Op is
   --    (add, sub, mul, div, rem_s, and_op, or_op, xor_op, shl, shr_s, rotl,
   --     rotr);
   --  type Comparison is
   --    (Equal, Not_Equal, Less, Greater, Less_Equal, Greater_Equal);
   --  generic
   --     type T is private;
   --     with function "=" (Left, Right : T) return Boolean;
   --     with function "<" (Left, Right : T) return Boolean;
   --     with function ">" (Left, Right : T) return Boolean;
   --     with function "<=" (Left, Right : T) return Boolean;
   --     with function ">=" (Left, Right : T) return Boolean;
   --  procedure Compare_And_Push
   --    (Stack : in out Vector; A, B : T; Op : Comparison);

end Interpreter;
