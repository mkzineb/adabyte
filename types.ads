with Interfaces; use Interfaces;
with Ada.Containers.Vectors;
package Types with
  SPARK_Mode => On
is
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

   type Reference_Type is (Null_Ref, Func_Ref, Extern_Ref);

   type Value_Posibilities is (Number_Value, Vector_Value, Reference_Value);
   type Value_Type (Val : Value_Posibilities := Number_Value) is record
      case Val is
         when Number_Value =>
            Num_Value : Number_Type;
         when Vector_Value =>
            Vec_Value : Vector_Type;
         when Reference_Value =>
            Ref_Value : Reference_Type;
      end case;
   end record;

   package Value_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Value_Type);
   use Value_Type_Vectors;

   subtype Result_Type is Vector;

   subtype Parameter_Type is Vector;

   type Function_Type is record
      Params  : Parameter_Type;
      Results : Result_Type;
   end record;

   type Limit_Type is record
      Min        : Natural;
      MAx        : Natural := 0;
      Max_Exists : Boolean := False;
   end record;

   subtype Memory_Type is Limit_Type;

   type Table_Type is record
      Reference : Reference_Type;
      Limit     : Limit_Type;
   end record;

   type Mutable is (Const, Var);
   for Mutable use (Const => 0, Var => 1);

   type Global_Type is record
      Val_Type : Value_Type;
      Mut      : Mutable;
   end record;

end Types;
