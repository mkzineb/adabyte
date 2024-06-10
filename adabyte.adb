with Ada.Text_IO;  use Ada.Text_IO;
with Runtime;      use Runtime;
with Instructions; use Instructions;
with Interpreter;  use Interpreter;
with Types;        use Types;
with Interfaces;   use Interfaces;
with Modules;      use Modules;
procedure Adabyte is
   program : Vm := Init_Interpreter;

begin
   Interpret (program);
   Put_Line (Integer_32'Image (program.Stack.Last_Element.Val.I_32));
   null;
end Adabyte;
