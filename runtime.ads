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
   type Element is (Value, Label, Activation_Call);
   type Element_Variation (Elt : Element := Value) is record
      case Elt is
         when Value =>
            Val : Number_Type;
         when Label =>
            Lab : Unsigned_32;
         when Activation_Call =>
            Frame : Integer;
      end case;
   end record;

   type Label_Info is record
      Label  : Unsigned_32;
      Arity  : Positive;
      Offset : Natural;
   end record;

   type Address is range 0 .. 100;
   subtype Func_Addr is Address;
   subtype Table_Addr is Address;
   subtype Mem_Addr is Address;
   subtype Global_Addr is Address;
   subtype Elem_Addr is Address;
   subtype Data_Addr is Address;
   subtype Extern_Addr is Address;

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
   type Func_Instance (T : Func_Instance_Type := Host_Func) is record
      case T is
         when Host_Func =>
            null;
         when Module_Func =>
            Func_Type   : Function_Type;
            Module_Inst : Module_Instance;
            Code        : Integer; -- todo
      end case;
   end record;

   type Table_Instance is record
      T_Type : Table_Type;
      Elem   : Reference_Type;  --  list of ref
   end record;

   type Memory_Instance is record
      Mem_Type : Memory_Type;
      Data     : Byte;  --  vec of type byte
   end record;

   type Global_Instance is record
      Glb_Type : Global_Type;
      Value    : Value_Type;
   end record;

   type Element_Instance is record
      Ref_Tp  : Reference_Type;
      Element : Vector_Type; --  vec of refs
   end record;

   type Data_Instance is record
      Data : Vector_Type; --  vecs of bytes
   end record;

   type Export_Instance is record
      Name_export  : Name;
      External_Val : External_Values;
   end record;

   type Function_Instance_List is array (Positive range <>) of Func_Instance;
   type Function_Instances is access Function_Instance_List;
   type Table_Instance_List is array (Positive range <>) of Table_Instance;
   type Table_Instances is access Table_Instance_List;
   type Mems_Instance_List is array (Positive range <>) of Memory_Instance;
   type Mem_Instances is access Mems_Instance_List;
   type Global_Instance_List is array (Positive range <>) of Global_Instance;
   type Global_Instances is access Global_Instance_List;
   type Elem_Instance_List is array (Positive range <>) of Element_Instance;
   type Elem_Instances is access Elem_Instance_List;
   type Data_Instance_List is array (Positive range <>) of Data_Instance;
   type Data_Instances is access Data_Instance_List;

   type Store is record
      Funcs   : Function_Instances;
      Tables  : Table_Instances;
      Mems    : Mem_Instances;
      Globals : Global_Instances;
      Elems   : Elem_Instances;
      Datas   : Data_Instances;
   end record;

   type Frame_State is record
      Locals     : Store;
      Module_ist : Module_Instance;
   end record;

   type Frame is record
      Func_Arity : Integer;
      State      : Frame_State;
   end record;

   type Exec_Env is (modInstance, current_func, stack, frame_stt);

end Runtime;
