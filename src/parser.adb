with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Values;      use Values;
package body Parser is

   type Four_Byte_Buffer is array (0 .. 3) of Unsigned_8;

   function Read_Magic_Number (Fd : File_Descriptor) return Boolean is
      Count  : Integer;
      Buffer : Four_Byte_Buffer;
      Magic  : constant Four_Byte_Buffer := (16#00#, 16#61#, 16#73#, 16#6D#);
   begin
      Count := Read (Fd, Buffer'Address, 4);
      pragma Assert (Count = 4, " 32 bits not read entirely");
      if Buffer = Magic then
         return True;
      else
         return False;
      end if;
   end Read_Magic_Number;

   function Read_Version (Fd : File_Descriptor) return Boolean is
      Count   : Integer;
      Buffer  : Four_Byte_Buffer;
      Version : constant Four_Byte_Buffer := (16#01#, 16#00#, 16#00#, 16#00#);

   begin
      Count := Read (Fd, Buffer'Address, 4);
      pragma Assert (Count = 4, " 32 bits not read entirely");
      if Buffer = Version then
         return True;
      else
         return False;
      end if;
   end Read_Version;

   function Read_Wasm_File
     (File_Path : String; Data : out Bytes_Array_Acc) return Raw_Wasm_Value
   is
      Fd           : File_Descriptor;
      Magic_Number : Boolean;
      Version      : Boolean;
      wasm         : Raw_Wasm_Value;
   begin
      Fd           := Open_Read (File_Path, Binary);
      Magic_Number := Read_Magic_Number (Fd);
      Version      := Read_Version (Fd);
      pragma Assert
        (Magic_Number = True,
         "Magic Number did not match, The file does not conform to WebAssembly (Wasm) standards. ");
      pragma Assert
        (Version = True,
         "Magic Number did not match, The file does not conform to WebAssembly (Wasm) standards. ");

      return wasm;
   end Read_Wasm_File;

end Parser;
