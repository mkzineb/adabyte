with Types;                                 use Types;
with Values;                                use Values;
with Ada.Containers;                        use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Interfaces;                            use Interfaces;
with Modules;                               use Modules;
package Runtime is
   pragma Elaborate_Body;
   --  runtime

   PAGE_SIZE : constant Unsigned_32 := 65_536;
   MAX_PAGES : constant Unsigned_32 := 65_536;
   MAX_SIZE  : constant Unsigned_64 := Unsigned_64 (PAGE_SIZE * MAX_PAGES);

   type Address is range 0 .. 100;
   subtype Func_Addr is Address;
   subtype Table_Addr is Address;
   subtype Mem_Addr is Address;
   subtype Global_Addr is Address;
   subtype Elem_Addr is Address;
   subtype Data_Addr is Address;
   subtype Extern_Addr is Address;

   --  additional addresses
   subtype Type_Addr is Address;
   subtype Local_Addr is Address;
   subtype Label_Addr is Address;
   subtype Module_Instance_Addr is Address;

   type Vector_Of_Bytes is array (Natural range <>) of Unsigned_8;
   type Vector_Of_Bytes_Acc is access Vector_Of_Bytes;

   type Vector_Of_Table_Element is array (Natural range <>) of Table_Type;
   type Vector_Of_Table_Element_Acc is access Vector_Of_Table_Element;

   type External_Values_Type is (Func, Table, Mem, Global);
   type External_Values (T : External_Values_Type := Func) is record
      case T is
         when Func =>
            Func : Func_Addr;
         when Table =>
            Table : Table_Addr;
         when Mem =>
            Mem : Mem_Addr;
         when Global =>
            Global : Global_Addr;
      end case;
   end record;

   type Module_Instance is record
      Types        : Integer;
      Func_Addrs   : Integer;
      Table_Addrs  : Integer;
      Mem_Addrs    : Integer;
      Global_Addrs : Integer;
      Elem_Addrs   : Integer;
      Data_Addrs   : Integer;
      Exports      : Integer;
   end record;

   type Func_Instance_Type is (Host_Func, Module_Func);
   type Func_Instance (T : Func_Instance_Type := Module_Func) is record
      case T is
         when Host_Func =>
            null;
         when Module_Func =>
            Func_Type : Function_Type;
            Owner     : Module_Instance_Addr;
      end case;
   end record;

   type Table_Instance is record
      Kind     : Table_Type;
      Elements : Vector_Of_Table_Element_Acc;
      Owner    : Module_Instance_Addr;
   end record;

   type Memory_Instance is record
      Kind       : Memory_Type;
      Data       : Vector_Of_Bytes_Acc;
      Page_Count : Unsigned_32 := PAGE_SIZE;
      Owner      : Module_Instance_Addr;
   end record;

   type Global_Instance is record
      Glb_Type : Global_Type;
      Value    : Integer;  --  a definir
      Owner    : Module_Instance_Addr;
   end record;

   type Element_Instance is record
      Items : Vector_Of_Table_Element_Acc;
      Owner : Module_Instance_Addr;
      Kind  : Reference_Type;
   end record;

   type Data_Instance is record
      Data  : Vector_Of_Bytes_Acc;
      Owner : Module_Instance_Addr;
   end record;

   type Export_Instance is record
      Name_export  : Name;
      External_Val : External_Values;
   end record;

   type Function_Instance_List is array (Natural range <>) of Func_Instance;
   type Function_Instances is access Function_Instance_List;
   type Table_Instance_List is array (Natural range <>) of Table_Instance;
   type Table_Instances is access Table_Instance_List;
   type Mems_Instance_List is array (Natural range <>) of Memory_Instance;
   type Mem_Instances is access Mems_Instance_List;
   type Global_Instance_List is array (Natural range <>) of Global_Instance;
   type Global_Instances is access Global_Instance_List;
   type Elem_Instance_List is array (Natural range <>) of Element_Instance;
   type Elem_Instances is access Elem_Instance_List;
   type Data_Instance_List is array (Natural range <>) of Data_Instance;
   type Data_Instances is access Data_Instance_List;

   type Module_Instance_List is array (Natural range <>) of Module_Instance;
   type Module_Instances_Acc is access Module_Instance_List;

   type Store is record
      Funcs            : Function_Instances;
      Tables           : Table_Instances;
      Memories         : Mem_Instances;
      Globals          : Global_Instances;
      Elements         : Elem_Instances;
      Datas            : Data_Instances;
      Module_Instances : Module_Instances_Acc;
   end record;

   type Frame_State is record
      Locals     : Store;  --  var & args
      Module_ist : Module_Instance;
   end record;

   type Frame;
   type Frame_Acc is access Frame;
   type Frame is record
      Instr_Ptr     : Unsigned_32;
      Block_Ptr     : Unsigned_32;
      Func_Instance : Func_Addr;  --  ref counting
      Module_addr   : Module_Instance_Addr;
      Locals        : Value_Type;  --  vector of args and local vars
   end record;

   type Exec_Env is (modInstance, current_func, stack, frame_stt);

   procedure New_Data_Instance (Data : Integer; Owner : Module_Instance_Addr);

end Runtime;
