package Values with
  SPARK_Mode => On
is

   subtype Name is Character;

   subtype Byte is Integer;

   type Sequence_Bytes is array (Positive range <>) of Byte;
   type Bytes is access Sequence_Bytes;

end Values;
