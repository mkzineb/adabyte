with Ada.Text_IO;   use Ada.Text_IO;
with Error_Handler; use Error_Handler;
with Ada.Unchecked_Deallocation;
with Types;         use Types;

package body Interpreter is

   Branch_inst : constant Instruction_Acc :=
     new Instruction'
       (Control, Control_Inst => Control_Instruction'(Branch, Label_Br => 0));

   first : constant Instruction_Acc :=
     new Instruction'
       (Numeric,
        Numeric_Inst =>
          Numeric_Instruction'
            (I32_Constant, I32_Constant => (Num => I_32, I_32 => 20)));

   second : constant Instruction_Acc :=
     new Instruction'
       (Numeric,
        Numeric_Inst =>
          Numeric_Instruction'
            (I32_Constant, I32_Constant => (Num => I_32, I_32 => 40)));

   ad : constant Instruction_Acc :=
     new Instruction'
       (Numeric, Numeric_Inst => Numeric_Instruction'(Op => Add));

   Inst : Chunk_Acc := new Chunk'(first, second, ad, Branch_inst);

   ----   TODO
   seq : Instruction_Sequence := (Sequence => Inst);
   Blo : Block_Type;

   if_instd : Instruction_Acc :=
     new Instruction'
       (Control,
        Control_Inst =>
          Control_Instruction'
            (If_Inst, If_Block => Blo, Then_Chunk => seq,
             Else_Opt_Chunk    => seq));

   function Init_Interpreter return Vm is
      Tab  : Symbols_Table.Map;
      Intp : Vm;
   begin
      Tab.Insert
        (Key => 14, New_Item => (Label => 14, Arity => 1, Offset => 0));
      Intp := (Code => Inst, Stack => Empty_Vector, Sym_Tab => Tab, Ip => 0);
      return Intp;
   end Init_Interpreter;

   procedure Exec_Branch_If (Program : in out Vm; Label : Unsigned_32) is
   begin
      null; --  continue to the next instruction (goto)
   end Exec_Branch_If;

   function Exec_Branch
     (Interp : in out Vm; Label : Unsigned_32) return Natural
   is
      Val            : Element_Variation (Value);
      arity_of_label : Integer := 1;
      Br             : Natural;
   begin
      --  assert (validation)
      arity_of_label := Interp.Sym_Tab.Element (14).Arity;
      Put_Line ("arity n :");
      Put_Line (Integer'Image (arity_of_label));
      Put_Line ("----------------");

      --  assert that are at least n values
      --  pop values
      Val := Last_Element (Interp.Stack);
      --  Delete_Last (Interp.Stack);
      Delete_Last (Interp.Stack);

      Put_Line ("popping n values :");
      Put_Line (Integer_32'Image (Val.Val.I_32));
      Put_Line ("----------------");
      --  repeat l + 1 times while value
      --       pop the value
      --  assert top is a label
      --       pop the label
      Delete_Last (Interp.Stack);
      --  push the value
      Put_Line ("pushing the values back");
      Interp.Stack.Append (Val);
      Interp.Ip := Interp.Sym_Tab.Element (14).Offset;
      Br        := Interp.Ip;
      return Br;
   end Exec_Branch;

   procedure Exec_Loop (Program : in out Vm) is
   begin
      null;
      --  current frame (state of the program)
      --  assert (validation)
      --
      --  assert m values in the stack
      --  pop all values
      --  enter the block with label
   end Exec_Loop;

   function Exec_Unreachable return Integer is
   begin
      null;
      --  OS_Exit (1);
      return Handle_Trap;
   end Exec_Unreachable;

   procedure Free is new Ada.Unchecked_Deallocation (Chunk, Chunk_Acc);

   --  decoding instruction
   function Interpret_Control_Instruction
     (Inst : Control_Instruction; Interpreted : in out Vm) return Natural
   is
      Ofs : Natural;
   begin
      case Inst.Op is
         when Unreachable =>
            null;
         when Branch =>
            Put_Line ("---- branch");
            Ofs := Exec_Branch (Interpreted, 14);
         when Block =>
            Put_Line ("---- block");
            Interpreted.Stack.Append ((Elt => Label, Lab => 14));
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

   --  Todo
   procedure Interpret_Expression_Instruction
     (Insts : Instruction_Sequence; Stack : in out Vector)
   is
   begin
      null;
   end Interpret_Expression_Instruction;

   procedure Interpret (Interpreted : in out Vm) is
      i    : Natural := 0;
      Jump : Natural;
   begin
      while i <= Interpreted.Code'Last loop
         case Interpreted.Code (i).Op is
            when Numeric =>
               Put_Line ("Numeric inst");
               Interpret_Numeric_Instruction
                 (Interpreted.Code (i).Numeric_Inst, Interpreted.Stack);
            when Control =>
               Put_Line ("Control inst");
               Jump :=
                 Interpret_Control_Instruction
                   (Interpreted.Code (i).Control_Inst, Interpreted);
            when Reference =>
               Interpret_Reference_Instruction
                 (Interpreted.Code (i).Reference_Inst, Interpreted.Stack);
            when Parametric =>
               Interpret_Parametric_Instruction
                 (Interpreted.Code (i).Parametric_Inst, Interpreted.Stack);
            when Variable =>
               Interpret_Variable_Instruction
                 (Interpreted.Code (i).Variable_Inst, Interpreted.Stack);
            when Table =>
               Interpret_Table_Instruction
                 (Interpreted.Code (i).Table_Inst, Interpreted.Stack);
            when Memory =>
               Interpret_Memory_Instruction
                 (Interpreted.Code (i).Memory_Inst, Interpreted.Stack);
            when Vector_128 =>
               Interpret_Vector128_Instruction
                 (Interpreted.Code (i).Vector_Inst, Interpreted.Stack);
            when Expression =>
               Interpret_Expression_Instruction
                 (Interpreted.Code (i).Expression, Interpreted.Stack);
            when Admin =>
               null;
         end case;
         if Interpreted.Code (i).Op = Control
           and then Interpreted.Code (i).Control_Inst.Op = Branch
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
