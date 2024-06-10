with Interfaces; use Interfaces;
package Lexer with
  SPARK_Mode => On
is
   type Token_Type is (add);

   type Token is record
      Token : Token_Type;
      Value : Unsigned_32;
   end record;

end Lexer;
