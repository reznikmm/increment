------------------------------------------------------------------------------
--                                                                          --
--                               Gela Project                               --
--                                                                          --
--                Programming Language Construction Framework               --
--                                                                          --
--                      Incremental Analysis Component                      --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2015, Maxim Reznik <max@gela.work>                           --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Maxim Reznik, IE nor the names of its         --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------
--  $Revision$ $Date$
------------------------------------------------------------------------------



with Matreshka.Internals.Unicode;

package body Incr.Lexers.Batch_Lexers is

   package Tables is
      function To_Class (Value : Matreshka.Internals.Unicode.Code_Point)
        return Character_Class;
      pragma Inline (To_Class);

      function Switch (S : State; Class : Character_Class) return State;
      pragma Inline (Switch);

      function Rule (S : State) return Rule_Index;
      pragma Inline (Rule);

      Error_State : constant State := 4;
   end Tables;

   package body Tables is separate;

   use Tables;

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

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Self   : access Batch_Lexer'Class;
      Result : out Rule_Index)
   is
      use type Matreshka.Internals.Unicode.Code_Unit_32;
      Input         : Wide_Wide_Character;
      Code_Unit     : Matreshka.Internals.Unicode.Code_Unit_32;
      Pos           : Buffer_Index := 1;
      Current_State : State := Self.Start;
      Char          : Character_Class;
      Next_Rule     : Rule_Index;
   begin
      if Self.To = 1 and then Self.Buffer (1) = End_Of_Input then
         Result := 0;
         return;
      end if;

      Self.Classes (1 .. Self.Next - Self.To - 1) :=
        Self.Classes (Self.To + 1 .. Self.Next - 1);

      Self.Buffer (1 .. Self.Next - Self.To - 1) :=
        Self.Buffer (Self.To + 1 .. Self.Next - 1);

      Self.Next := Self.Next - Self.To;
      Self.To   := 0;
      Self.Rule := 0;

      loop
         if Pos = Self.Next then
            Input := Self.Source.Get_Next;
            Self.Buffer (Pos) := Input;
            Code_Unit := Wide_Wide_Character'Pos (Input);

            if Input in End_Of_Input then
               Self.Classes (Pos) := Error_Character;
            else
               Self.Classes (Pos) := To_Class (Code_Unit);
            end if;

            Self.Next := Self.Next + 1;
         end if;

         Char := Self.Classes (Pos);

         exit when Char = Error_Character;

         Current_State := Switch (Current_State, Char);

         exit when Current_State = Tables.Error_State;

         Next_Rule := Rule (Current_State);

         if Next_Rule /= 0 then
            Self.Rule := Next_Rule;
            Self.To := Pos;
         end if;

         Pos := Pos + 1;
      end loop;

      Result := Self.Rule;
   end Get_Token;

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
