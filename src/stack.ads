------------------------------------------------------------------------------
--  Package: Stack
--
--  comment...
--  Author: Moubarik Zineb
--  Date: 24-07-2024
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with Types;        use Types;
with Instances;    use Instances;
with Wasm;         use Wasm;
with Instructions; use Instructions;
with Ada.Containers.Vectors;
package Stack is
   pragma Elaborate_Body;
   use Value_Type_Vectors;
   use Ada.Containers;

   --  labels / control instr
   type Block_Frame is record
      Instr_ptr        : Unsigned_32;
      End_Instr_Offset : Unsigned_32;
      Results          : Unsigned_8;
      Params           : Unsigned_8;
      B_Type           : Block_Type;
   end record;

   type Call_Frame is record
      Instr_Ptr   : Unsigned_32 := 0;
      Block_Ptr   : Unsigned_32 := 0;
      Func_Inst   : Function_Spec;
      Module_Addr : Module_Instance_Addr;
      Locals      : Value_Type_Vectors.Vector;
   end record;

   type Value_Stack is record
      Val : Value_Type;
   end record;

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

   package Stack_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Element_Variation);
   use Stack_Vector;

   --  function Add_Elements_To_Call_Frame_Locals
   --    (Func_Locals : Vector; params : Parameter_Type) return Vector;

   --  procedure Push_Value_Type
   --    (Stack : in out Stack_Vector.Vector; T : S_F; Value : Numeric_Type);

end Stack;
