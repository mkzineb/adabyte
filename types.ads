with Interfaces; use Interfaces;
package Types is
   --  Number Types
   --  Vector Types
   --  Reference Types
   --  Value Types
   --  Result Types
   --  Function Types
   --  Limits
   --  Memory Types
   --  Table Types
   --  Global Types

   type Number is (I_32, I_64, F_32, F_64);
   type Number_Type (Num : Number := I_32) is record
      case Num is
         when I_32 =>
            I_32 : Integer_32;
         when I_64 =>
            I_64 : Integer_64;
         when F_32 =>
            F_32 : IEEE_Float_32;
         when F_64 =>
            F_64 : IEEE_Float_64;
      end case;
   end record;

   subtype Integer_Type is Number range I_32 .. I_64;
   subtype Float_Type is Number range F_32 .. F_64;

   type Vector_Type is (v_128);

   type Reference_Type is (funcref, externref);

   type Value is (Number_Value, Vector, Reference);
   type Value_Type (val : Value := Number_Value) is record
      case val is
         when Number_Value =>
            Num_Value : Number;
         when Vector =>
            Vec_Value : Vector_Type;
         when Reference =>
            Ref_Value : Reference_Type;
      end case;
   end record;

   --  TODO
   --  type Result_Type is array (Value_Type range <>) of Vector_Type;
   --  type Func_Type;
   --  type Limits;
   --  type Table_Type;
   --  type Memory_Type;
   --  type Global_Type;
end Types;
