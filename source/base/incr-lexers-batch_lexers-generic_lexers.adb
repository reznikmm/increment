--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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
