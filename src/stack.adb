package body Stack is

   function Add_Elements_To_Call_Frame_Locals
     (Func_Locals : Vector; params : Parameter_Type) return Vector
   is
      Result : Vector;
   begin
      Reserve_Capacity (Result, (Func_Locals.Length + params.Length));

      for I in 0 .. Natural (Func_Locals.Length) - 1 loop
         Append (Result, Func_Locals (I));
      end loop;

      for J in 0 .. Natural (params.Length) - 1 loop
         Append (Result, params (J));
      end loop;

      return Result;
   end Add_Elements_To_Call_Frame_Locals;
end Stack;
