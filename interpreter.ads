with Environment_Manager; use Environment_Manager;
with Interfaces;          use Interfaces;

package Interpreter is

   Globals : Environment;

   procedure Execute;

   procedure Interpret_Program;

   --  Inside an Environment --------------------------

   procedure Execute_Inside_Env;

   --  function Look_Up_Variable
   --    (Id : Unsigned_32; Expr : Expression) return Boolean;

   --  procedure Get_Variable_Value (Id : Unsigned_32);

   --  procedure Resolve (Expr : Expression);

   --  procedure Assign_Value (Expr : Expression);

   -----------------------------------------------------
private
   Env    : Environment := Globals;
   Locals : Symbols_Table.Map;

end Interpreter;
