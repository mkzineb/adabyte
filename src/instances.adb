package body Instances is

   procedure Initialize_Frame is
   begin
      null;
   end Initialize_Frame;

   procedure Initialize_Configuration is
   begin
      null;
   end Initialize_Configuration;
   procedure Initialize_Globals is
   begin
      null;
   end Initialize_Globals;
   procedure Initialize_References is
   begin
      null;
   end Initialize_References;
   procedure Instatiate_Module is
   begin
      null;
      --  validate module
      Initialize_Frame;
      Initialize_Globals;
      Initialize_References;
      Initialize_Configuration;
   end Instatiate_Module;

   procedure New_Data_Instance (Data : Integer; Owner : Module_Instance_Addr)
   is
   begin
      null;
   end New_Data_Instance;

   function Func_Ty (Ty : Unsigned_32) return Function_Type is
      Params  : Parameter_Type;
      Results : Result_Type;
   begin
      return (Params => Params, Results => Results);
   end Func_Ty;

   function Get_Module_Instance
     (Addr : Module_Instance_Addr) return Module_Instance
   is
      m : Module_Instance;
   begin
      return m;

   end Get_Module_Instance;

end Instances;
