with Ada.Wide_Characters; use Ada.Wide_Characters;
with Interfaces;          use Interfaces;
package Values with
  SPARK_Mode => On
is
   subtype Name is Character;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Bytes is access Byte_Array;
end Values;
