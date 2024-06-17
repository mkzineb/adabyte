with Interfaces; use Interfaces;
with Values;     use Values;
with Types;      use Types;
package Modules with
  SPARK_Mode => On
is

   type Section_Id is range 0 .. 12;
   --  Id     Section
   --  0      Custom Section
   --  1      Type Section
   --  2      Import Section
   --  3      Function Section
   --  4      Table Section
   --  5      Memory Section
   --  6      Global Section
   --  7      Export Section
   --  8      Start Section
   --  9      Element Section
   --  10     Code Section
   --  11     Data Section
   --  12     Data Count Section

   type Elem is
     (Function_Section, Type_Section, Import_Section, Table_Section,
      Memory_Section, Global_Section, Export_Section, Element_Section,
      Code_Section, Data_Section, Data_Count_Section);

   type Locals_info is record
      Count       : Unsigned_32;
      Local_Value : Value_Type;
   end record;
   type Locals_Vec is array (Positive range <>) of Locals_info;
   type Locals_Vector is access Locals_Vec;

   type Element_Type (E : Elem := Function_Section) is record
      case E is
         when Function_Section =>
            --  Func_Type_Elem : Function_Index;
            null;
         when Type_Section =>
            Type_Type_Elem : Function_Type;
         when Import_Section =>
            Module_N    : Name;
            Nm          : Name;
            Import_Desc : Integer; --  todo
         when Table_Section =>
            Table_Type_Elem : Table_Type;
         when Memory_Section =>
            Mem_Elem : Memory_Type;
         when Global_Section =>
            Gt : Global_Type;
            --  Gt_Elem : Instruction (Expression);
         when Export_Section =>
            null;
         when Element_Section =>
            null; --  ?
         when Code_Section =>
            Code_Size : Unsigned_32;
            Locals    : Locals_Vector;
            --  Expr      : Instruction (Expression);
         when Data_Section =>
            Data : Integer; --  Todo
         when Data_Count_Section =>
            Data_Segments_Count : Unsigned_32;

      end case;
   end record;

   type Generic_Element_Array is array (Positive range <>) of Element_Type;
   type Generic_Element is access Generic_Element_Array;

   type Generic_Vector (Id : Section_Id := 0) is record
      Elements : Generic_Element;
   end record;

   type Section (Id : Section_Id := 0) is record
      Size : Unsigned_32;
      case Id is
         when 0 =>
            Custom_Name   : Name;
            Byte_Sequence : Bytes;
         when 1 =>
            Type_Content : Generic_Vector (1);
         when 2 =>
            Import_Content : Generic_Vector (2);
         when 3 =>
            Func_Content : Generic_Vector (3);
         when 4 =>
            Table_Content : Generic_Vector (4);
         when 5 =>
            Memory_Content : Generic_Vector (5);
         when 6 =>
            Global_Content : Generic_Vector (6);
         when 7 =>
            Export_Content : Generic_Vector (7);
         when 8 =>
            --  Start_Content : Function_Index;
            null;
         when 9 =>
            Element_Content : Generic_Vector (9);
         when 10 =>
            Code_Content : Generic_Vector (10);
         when 11 =>
            Data_Content : Generic_Vector (11);
         when 12 =>
            Data_Count_Content : Generic_Vector (12);
      end case;
   end record;

   type Location is record
      Offset : Unsigned_128;
   end record;

   type Module_Fields is array (Natural range <>) of Section;
   type Module_Fields_List is access Module_Fields;

   type Module is record
      Loc    : Location;
      Name   : Character;
      Fields : Module_Fields_List;
      --  imports
      --  sections
   end record;

   function Create_Module return Module; --  respect order of module fileds

end Modules;
