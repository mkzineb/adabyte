with Types;                                 use Types;
with Values;                                use Values;
with Ada.Containers;                        use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Interfaces;                            use Interfaces;
--  Validated Module instance ready for execution

package Instances is
   pragma Elaborate_Body;

   --  WebAssembly Address

   type Address is range 0 .. 100;
   subtype Func_Addr is Address;
   subtype Table_Addr is Address;
   subtype Mem_Addr is Address;
   subtype Global_Addr is Address;
   subtype Elem_Addr is Address;
   subtype Data_Addr is Address;
   subtype Extern_Addr is Address;

   subtype Type_Addr is Address;
   subtype Local_Addr is Address;
   subtype Label_Addr is Address;
   subtype Module_Instance_Addr is Address;

   type Val_Type_Vector is array (Local_Addr range <>) of Value_Type;
   type Locals_Vector is access Val_Type_Vector;

   type Vector_Of_Bytes is array (Natural range <>) of Unsigned_8;
   type Vector_Of_Bytes_Acc is access Vector_Of_Bytes;

   type Vector_Of_Table_Element is array (Natural range <>) of Table_Type;
   type Vector_Of_Table_Element_Acc is access Vector_Of_Table_Element;

   PAGE_SIZE : constant Unsigned_32 := 65_536;
   MAX_PAGES : constant Unsigned_32 := 65_536;
   MAX_SIZE  : constant Unsigned_64 := Unsigned_64 (PAGE_SIZE * MAX_PAGES);

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

   --  MOVE TO WASM
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

   --  type Exec_Env is (modInstance, current_func, stack, frame_stt);
   generic
      type Element_Type is private;
      type Index_Type is (<>);
   package Array_Access_Types is
      type Array_Type is array (Index_Type) of Element_Type;
      type Array_Access is access all Array_Type;
   end Array_Access_Types;

   package Unsigned_8_Array_Access_Types is new Array_Access_Types
     (Element_Type => Unsigned_8, Index_Type => Natural);

   package Ft_Array_Access_Types is new Array_Access_Types
     (Element_Type => Function_Type, Index_Type => Natural);

   package Fa_Array_Access_Types is new Array_Access_Types
     (Element_Type => Func_Addr, Index_Type => Natural);

   package Ta_Array_Access_Types is new Array_Access_Types
     (Element_Type => Table_Addr, Index_Type => Natural);

   package Ma_Array_Access_Types is new Array_Access_Types
     (Element_Type => Mem_Addr, Index_Type => Natural);

   package Ga_Array_Access_Types is new Array_Access_Types
     (Element_Type => Global_Addr, Index_Type => Natural);

   package Ea_Array_Access_Types is new Array_Access_Types
     (Element_Type => Elem_Addr, Index_Type => Natural);

   package Da_Array_Access_Types is new Array_Access_Types
     (Element_Type => Data_Addr, Index_Type => Natural);

   type Module_Instance is record
      Failed_To_Instatiate_Module : Boolean;
      Store_Id                    : Unsigned_32;
      Module_Addr                 : Module_Instance_Addr;

      Types        : Ft_Array_Access_Types.Array_Access;
      Func_Addrs   : Fa_Array_Access_Types.Array_Access;
      Table_Addrs  : Ta_Array_Access_Types.Array_Access;
      Mem_Addrs    : Ma_Array_Access_Types.Array_Access;
      Global_Addrs : Ga_Array_Access_Types.Array_Access;
      Elem_Addrs   : Ea_Array_Access_Types.Array_Access;
      Data_Addrs   : Da_Array_Access_Types.Array_Access;
      Imports      : Integer;--  TODO
      Exports      : Integer;
   end record;
   type Module_Instance_List is array (Natural range <>) of Module_Instance;
   type Module_Instances_Acc is access Module_Instance_List;

   type Store_Type is record
      Funcs           : Function_Instances;
      Tables          : Table_Instances;
      Memories        : Mem_Instances;
      Globals         : Global_Instances;
      Elements        : Elem_Instances;
      Datas           : Data_Instances;
      Module_Instance : Module_Instances_Acc;
   end record;

   function Get_Module_Instance
     (Addr : Module_Instance_Addr) return Module_Instance;

   procedure New_Data_Instance (Data : Integer; Owner : Module_Instance_Addr);

   function Func_Ty (Ty : Unsigned_32) return Function_Type;

end Instances;
