with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Types;       use Types;
with Instances;   use Instances;
package body Interpreter is

   function Next_Instr (Environment : Env) return Integer
   is --  return Instruction_Acc is
   begin
      return 0;
   end Next_Instr;

   procedure Free is new Ada.Unchecked_Deallocation
     (Chunk, Instruction_Sequence);

   function Count_Lables (Environment : Env) return Natural is
      Label_Count : Natural;
   begin
      for I in Environment.Stack.First_Index .. Environment.Stack.Last_Index
      loop
         if Environment.Stack (I).Elt = Label then
            Label_Count := Label_Count + 1;
         end if;
      end loop;
      return Label_Count;
   end Count_Lables;

   function Init_Environment
     (S : Store_Type; Stack : Vector; i : Instruction_Sequence) return Env
   is
      Current_Frame  : Call_Frame;
      Current_Module : Module_Instance;
      Environment    : Env;
      Tab            : Symbols_Table.Map;

   begin
      return
        (Stack  => Stack, Store => S, Symbols => Tab, Cf => Current_Frame,
         Module => Current_Module, Temp => i);
   end Init_Environment;

   procedure Exec_Return (Cf : in out Call_Frame) is
   begin
      Cf.Instr_Ptr := 0;
   end Exec_Return;

   function Count_Values_Top_Stack (Stack : Vector) return Natural is
   begin
      return 0;
   end Count_Values_Top_Stack;

   function Pop_Values (N : Natural; Environment : in out Env) return Vector is
      Popped_Values : Vector;
   begin
      for i in 0 .. N loop
         Popped_Values.Append (Last_Element (Environment.Stack));
         Delete_Last (Environment.Stack);
      end loop;
      return Popped_Values;
   end Pop_Values;

   procedure Pop_Label_Or_Values (Position : Natural; Stack : in out Vector) is
   begin
      for i in 1 .. Position + 1 loop
         if Stack.Last_Element.Elt = Label then
            Delete_Last (Stack);
         else
            for j in 1 .. Position + 1 loop
               pragma Assert
                 (Stack.Length > 0, "Not enough elements in the stack.");

               Delete_Last (Stack);
            end loop;
            exit;
         end if;
      end loop;
   end Pop_Label_Or_Values;

   procedure Push_Values (Temp : Vector; N : Natural; Environment : in out Env)
   is
   begin
      for i in 0 .. N loop
         Environment.Stack.Append_Vector (Temp);
      end loop;
   end Push_Values;

   procedure Jump_If_Block_Exists (To : Label_Addr; Environment : in out Env)
   is
      Lab_Arity   : Natural;
      Position    : Natural;
      Tmp_Storage : Vector;
   begin
      if Symbols_Table.Contains (Environment.Symbols, To) then
         Position :=
           Symbols_Table.Element (Environment.Symbols, To).Position_In_Stack;

         pragma Assert
           (Count_Lables (Environment) >= Position + 1,
            "Stack does not contain enough labels");

         Lab_Arity := Symbols_Table.Element (Environment.Symbols, To).Arity;

         pragma Assert
           (Count_Values_Top_Stack (Environment.Stack) >= Lab_Arity);

         Tmp_Storage := Pop_Values (Lab_Arity, Environment);

         Pop_Label_Or_Values (Position, Environment.Stack);

         Push_Values (Tmp_Storage, Lab_Arity, Environment);

         Environment.Cf.Instr_Ptr := Unsigned_32 (To);

      else
         raise Program_Error with "Label not found in symbol table.";
      end if;
   end Jump_If_Block_Exists;

   procedure Execute_Branching (To : Label_Addr; Environment : in out Env) is
   begin
      Jump_If_Block_Exists (To, Environment);
      Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
   end Execute_Branching;

   procedure Execute_Numeric_Instruction
     (Stack : in out Vector; Instr : Numeric_Instruction)
   is
   begin
      case Instr.Op is
         when I32_Const =>
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Val =>
                     (Number_Value,
                       Num_Value => (I_32, I_32 => Instr.I32_Constant.I_32)))));
         when others =>
            null;
      end case;
   end Execute_Numeric_Instruction;

   procedure Execute_Control_Instruction
     (Instr : Control_Instruction; Environment : in out Env)
   is
   begin
      case Instr.Op is
         when Branch =>
            Execute_Branching (Instr.Label_Br, Environment);
         when others =>
            null;
      end case;
   end Execute_Control_Instruction;

   function Execute_Next_Instr (Environment : in out Env) return Control_Flow
   is
      Inst : Instruction_Acc;
   begin
      Inst := Environment.Temp (Natural (Environment.Cf.Instr_Ptr));
      Put_Line ("next instr");
      case Inst.Op is
         when Numeric =>
            Execute_Numeric_Instruction (Environment.Stack, Inst.Numeric_Inst);
         when Control =>
            Execute_Control_Instruction (Inst.Control_Inst, Environment);
         when others =>
            null;
      end case;
      Environment.Cf.Instr_Ptr := Environment.Cf.Instr_Ptr + 1;
      return Continue;
   end Execute_Next_Instr;

   function Run (Environment : in out Env) return Interpret_Result is
      Control : Control_Flow;
   begin
      loop
         Control := Execute_Next_Instr (Environment);
         Put_Line ("control return");
         case Control is
            when Break =>
               return INTERPRET_OK;
               Put_Line ("ok");
            when Continue =>
               null;
               Put_Line ("continue");
         end case;
      end loop;
   end Run;
end Interpreter;
