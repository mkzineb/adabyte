package body Module is

   function Parse_File (Fd : File_Descriptor) return Module_Type is
      Module : Module_Type;
   begin
      return Module;
   end Parse_File;

   function Instantiate_Module
     (Module : Module_Type; Store : Store_Type) return Module_Instance
   is
      Module_Inst : Module_Instance;
   begin
      return Module_Inst;
   end Instantiate_Module;

   function Get_Id (Module : Module_Instance) return Module_Instance_Addr is
   begin
      return Module.Module_Addr;
   end Get_Id;
end Module;
