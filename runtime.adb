package body Runtime is

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

end Runtime;
