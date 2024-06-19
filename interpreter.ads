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
            Block : Block_Frame;
         when Activation_Call =>
            Call : Call_Frame;
      end case;
   end record;

   type Label_Info is record
      Arity             : Natural;
      Position_In_Stack : Natural;

   end record;

   function Hash (x : Label_Addr) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Label_Addr, Element_Type => Label_Info, Hash => Hash,
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

   type Control_Flow is (Break, Continue);

   function Next_Instr (Environment : Env) return Integer;

   function Count_Lables (Environment : Env) return Natural;
   function Init_Environment
     (S : Store_Type; Stack : Vector; i : Instruction_Sequence) return Env;

   function Run (Environment : in out Env) return Interpret_Result;
   function Execute_Next_Instr (Environment : in out Env) return Control_Flow;

end Interpreter;
