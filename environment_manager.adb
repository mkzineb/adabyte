package body Environment_Manager is
   RunTime_Error : exception;

   --  delete
   function Init_Env (Env : out Environment) return Environment is
   begin
      Env.Parent_Env := null;
      return Env;
   end Init_Env;

   function Init_Env_With_Parent
     (Parent_Env : Environment_Acc; Sym_Tab : Symbols_Table.Map)
      return Environment
   is
   begin
      return (Parent_Env => Parent_Env, Values => Sym_Tab);
   end Init_Env_With_Parent;

   procedure Define
     (Env : out Environment; Id : Unsigned_32; Value : Integer_32)
   is
   begin
      Env.Values.Include (Id, Value);
   end Define;

   function Get (Env : Environment; Id : Unsigned_32) return Integer_32 is
   begin
      if Env.Values.Contains (Id) then
         return Env.Values.Element (Id);
      else
         raise RunTime_Error with "Undefined Variable '" & Id'Img & " '.";
      end if;
      --  handle error
   end Get;

   procedure Assign (Env : Environment; Id : Unsigned_32; Value : Integer_32)
   is
   begin
      null;
   end Assign;

   function Find_Parent_Env
     (Env : Environment; Distance : Integer) return Environment_Acc
   is
   begin
      return Env.Parent_Env;
   end Find_Parent_Env;

   function Get_At
     (Env : Environment; Distance : Integer; Id : Unsigned_32)
      return Integer_32
   is
   begin
      return 0;
   end Get_At;

   procedure Assign_At
     (Env   : Environment; Distance : Integer; Id : Unsigned_32;
      Value : Integer_32)
   is
   begin
      null;
   end Assign_At;

end Environment_Manager;
