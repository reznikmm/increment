--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Incr.Lexers.Batch_Lexers is

   -------------------------
   -- Get_Start_Condition --
   -------------------------

   function Get_Start_Condition
     (Self : Batch_Lexer'Class) return State is
   begin
      return Self.Start;
   end Get_Start_Condition;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : Batch_Lexer'Class) return League.Strings.Universal_String is
   begin
      return League.Strings.To_Universal_String (Self.Buffer (1 .. Self.To));
   end Get_Text;

   ----------------------
   -- Get_Token_Length --
   ----------------------

   function Get_Token_Length (Self : Batch_Lexer'Class) return Positive is
   begin
      return Self.To;
   end Get_Token_Length;

   -------------------------
   -- Get_Token_Lookahead --
   -------------------------

   function Get_Token_Lookahead (Self : Batch_Lexer'Class) return Positive is
   begin
      return Self.Next - 1;
   end Get_Token_Lookahead;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Self   : in out Batch_Lexer'Class;
      Source : not null Source_Access) is
   begin
      Self.Source := Source;
      Self.Next := 1;
      Self.To := 0;
   end Set_Source;

   -------------------------
   -- Set_Start_Condition --
   -------------------------

   procedure Set_Start_Condition
     (Self : in out Batch_Lexer'Class; Condition : State) is
   begin
      Self.Start := Condition;
   end Set_Start_Condition;

end Incr.Lexers.Batch_Lexers;
