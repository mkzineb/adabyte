------------------------------------------------------------------------------
--  Package: Values
--
--  comment...
--  Author: Moubarik Zineb
--  Date: 24-07-2024
------------------------------------------------------------------------------
with Ada.Wide_Characters; use Ada.Wide_Characters;
with Interfaces;          use Interfaces;
package Values with
  SPARK_Mode => On
is
   subtype Name is Character;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Bytes_Array_Acc is access Byte_Array;

   type Raw_Wasm_Value is record
      Bytes : Bytes_Array_Acc;
   end record;

end Values;
