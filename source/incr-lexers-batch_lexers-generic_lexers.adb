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
-- Copyright © 2015-2017, Maxim Reznik <max@gela.work>                      --
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

package body Incr.Lexers.Batch_Lexers.Generic_Lexers is

   ---------------
   -- Get_Token --
   ---------------

   overriding procedure Get_Token
     (Self   : access Batch_Lexer;
      Result : out Rule_Index)
   is
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

         exit when Current_State = Error_State;

         Next_Rule := Rule (Current_State);

         if Next_Rule /= 0 then
            Self.Rule := Next_Rule;
            Self.To := Pos;
         end if;

         Pos := Pos + 1;
      end loop;

      Result := Self.Rule;
   end Get_Token;


end Incr.Lexers.Batch_Lexers.Generic_Lexers;
