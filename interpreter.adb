with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Types;       use Types;

package body Interpreter is

   function Init_Interpreter (P : Pointer) return Env is
      Tab  : Symbols_Table.Map;
      Intp : Env;
   begin
      Tab.Insert
        (Key => 14, New_Item => (Label_Index => 0, Arity => 1, Offset => 0));
      Intp :=
        (Stack => Empty_Vector, Sym_Tab => Tab, Instruction_Pointer => P);
      return Intp;
   end Init_Interpreter;

   procedure Next_Instr_At (skip : Natural; Environment : in out Env) is
   begin
      Environment.Instruction_Pointer.Pointer :=
        Environment.Instruction_Pointer.Pointer + skip;
   end Next_Instr_At;

   procedure Next_Instr (Environment : in out Env) is
   begin
      Next_Instr_At (1, Environment);
   end Next_Instr;

   procedure Free is new Ada.Unchecked_Deallocation
     (Chunk, Instruction_Sequence);

   function Count_Labels (Environment : Env) return Natural is
      Label_Count : Natural := 0;
   begin
      for I in Environment.Stack.First_Index .. Environment.Stack.Last_Index
      loop
         if Environment.Stack (I).Elt = Label then
            Label_Count := Label_Count + 1;
         end if;
      end loop;
      return Label_Count;
   end Count_Labels;

   procedure Execute_Branch_Instr
     (Environment : in out Env; Lab_Index : Natural)
   is
      Label_Info_Entry : Label_Info;
      Target_Label     : Unsigned_32;
      Lab_Arity        : Positive;
      Tmp_Storage      : Vector;
   begin
      --  Assert that the stack contains at least l+1 labels
      pragma Assert
        (Count_Labels (Environment) >= Lab_Index + 1,
         "Stack does not contain enough labels");
      --  get target label
      Target_Label := Environment.Stack (Lab_Index).Lab; -- NOT CORRECT INDEX
      --  check symbol table
      if Environment.Sym_Tab.Contains (Target_Label) then
         Label_Info_Entry := Environment.Sym_Tab (Target_Label);
         --  Assert there are at least n values on the top of the stack.
         Lab_Arity        := Label_Info_Entry.Arity;
         pragma Assert (Lab_Arity >= 1); -- WHY NOT ZERO?
         for i in 1 .. Lab_Arity loop
            --  pop and stock
            Tmp_Storage.Append (Environment.Stack.Last_Element);
            Delete_Last (Environment.Stack);
         end loop;
         --  delete the label -----------------------------------
         --  loop l+1 while top is value
         for i in 1 .. Lab_Index + 1 loop
            --  assert top label
            if Environment.Stack.Last_Element.Elt = Label then
               Delete_Last (Environment.Stack);
               --  pop label from stack;
            else
               --  pop l+1 values
               for j in 1 .. Lab_Index + 1 loop -- WHY DOUBLE LOOP?
                  --  enough elements to pop
                  pragma Assert
                    (Environment.Stack.Length > 0,
                     "Not enough elements in the stack.");
                  Delete_Last (Environment.Stack);
               end loop;
               --  Exit the loop
               exit;
            end if;
         end loop;
         for i in 1 .. Lab_Arity loop
            --  push val n
            Environment.Stack.Append (Tmp_Storage.Last_Element);
         end loop;
         --  jump to continuation
         Environment.Instruction_Pointer.Pointer := Label_Info_Entry.Offset;
      else
         raise Program_Error with "Label not found in symbol table.";
      end if;
   end Execute_Branch_Instr;

   function Reduce_Block_To_Admin
     (Block_Instr : Control_Instruction) return Administrative_Instruction
   is
   begin
      return
        (Label, Label_br    => 0, N => 2, M => 1,
         Target_Info        => (Has_Target => False),
         Instr_Continuation => Block_Instr.B_Inst);
   end Reduce_Block_To_Admin;

   function Interpret_Control_Instruction
     (Inst : Control_Instruction; Interpreted : in out Env) return Natural
   is
      Ofs : Natural;
   begin
      case Inst.Op is
         when Unreachable =>
            null;
         when Branch =>
            Put_Line ("---- branch");
         when Block =>
            Put_Line ("---- block");
            --  pop m values
            --  push l on the stack
            --  jump to the start of inst*
         when others =>
            null;
      end case;
      return Ofs;
   end Interpret_Control_Instruction;

   procedure Interpret_Reference_Instruction
     (Inst : Reference_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Reference_Instruction;

   procedure Interpret_Parametric_Instruction
     (Inst : Parametric_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Parametric_Instruction;

   procedure Interpret_Variable_Instruction
     (Inst : Variable_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Variable_Instruction;

   procedure Interpret_Table_Instruction
     (Inst : Table_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Table_Instruction;

   procedure Interpret_Memory_Instruction
     (Inst : Memory_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Memory_Instruction;

   procedure Interpret_Numeric_Instruction
     (Inst : Numeric_Instruction; Stack : in out Vector)
   is
      First_Operand  : Element_Variation :=
        (Elt => Value, Val => (Num => I_32, I_32 => 0));
      Second_Operand : Element_Variation :=
        (Elt => Value, Val => (Num => I_32, I_32 => 0));
   begin
      case Inst.Op is
         when I32_Constant =>
            Stack.Append ((Elt => Value, Val => Inst.I32_Constant));
            Put_Line ("---- const");

         when Add =>
            Put_Line ("---- addition");

            First_Operand := Last_Element (Stack);
            Delete_Last (Stack);
            Second_Operand := Last_Element (Stack);
            Delete_Last (Stack);
            Stack.Append
              ((Elt => Value,
                Val =>
                  (Num  => I_32,
                   I_32 =>
                     (First_Operand.Val.I_32 + Second_Operand.Val.I_32))));
         when others =>
            Put_Line ("other numeric");
      end case;

   end Interpret_Numeric_Instruction;

   procedure Interpret_Vector128_Instruction
     (Inst : Vector_Instruction; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Vector128_Instruction;

   procedure Interpret_Expression_Instruction
     (Insts : Instruction_Sequence; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Expression_Instruction;

   --  module Instance
   --  exec env
   --  cur_func (instance func)
   --  prev frame
   procedure Interpret (Interpreted : in out Env) is
      i    : Natural := 0;
      Jump : Natural;
   begin
      while i <= Interpreted.Instruction_Pointer.Code'Last loop
         case Interpreted.Instruction_Pointer.Code (i).Op is
            when Numeric =>
               Put_Line ("Numeric inst");
               Interpret_Numeric_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Numeric_Inst,
                  Interpreted.Stack);
            when Control =>
               Put_Line ("Control inst");
               Jump :=
                 Interpret_Control_Instruction
                   (Interpreted.Instruction_Pointer.Code (i).Control_Inst,
                    Interpreted);
            when Reference =>
               Interpret_Reference_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Reference_Inst,
                  Interpreted.Stack);
            when Parametric =>
               Interpret_Parametric_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Parametric_Inst,
                  Interpreted.Stack);
            when Variable =>
               Interpret_Variable_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Variable_Inst,
                  Interpreted.Stack);
            when Table =>
               Interpret_Table_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Table_Inst,
                  Interpreted.Stack);
            when Memory =>
               Interpret_Memory_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Memory_Inst,
                  Interpreted.Stack);
            when Vector_128 =>
               Interpret_Vector128_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Vector_Inst,
                  Interpreted.Stack);
            when Expression =>
               Interpret_Expression_Instruction
                 (Interpreted.Instruction_Pointer.Code (i).Expression,
                  Interpreted.Stack);
         end case;
         if Interpreted.Instruction_Pointer.Code (i).Op = Control
           and then Interpreted.Instruction_Pointer.Code (i).Control_Inst.Op =
             Branch
         then
            i := Jump;
            Put_Line ("--- > Jump");
            Put_Line ("branchin ______________");
         else
            i := i + 1;
         end if;
      end loop;
   end Interpret;
end Interpreter;
