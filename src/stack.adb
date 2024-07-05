package body Stack is

   function New_Call_Frame
     (Func_Inst : Function_Spec; Owner : Module_Instance_Addr;
      Params    : Value_Type; Block_Ptr : Unsigned_32) return Call_Frame
   is
      Locals : Locals_Vector;
   begin

      return
        (Instr_Ptr   => 0, Block_Ptr => Block_Ptr, Func_Inst => Func_Inst,
         Module_Addr => Owner, Locals => Locals);
   end New_Call_Frame;

end Stack;
