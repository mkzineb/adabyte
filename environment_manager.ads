with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Interfaces;                            use Interfaces;
package Environment_Manager is
   function Hash (x : Unsigned_32) return Hash_Type is (Hash_Type (x));
   package Symbols_Table is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unsigned_32, Element_Type => Integer_32, Hash => Hash,
      Equivalent_Keys => "=");

   use Symbols_Table;
   --  Env
   type Environment;
   type Environment_Acc is access Environment;

   type Environment is record
      Parent_Env : Environment_Acc;
      Values     : Symbols_Table.Map;
   end record;

   function Init_Env (Env : out Environment) return Environment;

   function Init_Env_With_Parent
     (Parent_Env : Environment_Acc; Sym_Tab : Symbols_Table.Map)
      return Environment;

   procedure Define
     (Env : out Environment; Id : Unsigned_32; Value : Integer_32);

   function Get (Env : Environment; Id : Unsigned_32) return Integer_32;

   procedure Assign (Env : Environment; Id : Unsigned_32; Value : Integer_32);

   --  Nested Environment / var scopes & binding

   function Find_Parent_Env
     (Env : Environment; Distance : Integer) return Environment_Acc;

   function Get_At
     (Env : Environment; Distance : Integer; Id : Unsigned_32)
      return Integer_32;

   procedure Assign_At
     (Env   : Environment; Distance : Integer; Id : Unsigned_32;
      Value : Integer_32);

end Environment_Manager;
