--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Lexers.Batch_Lexers.Generic_Lexers;
with Matreshka.Internals.Unicode;

package Tests.Lexers is

   package Tables is
      use Incr.Lexers.Batch_Lexers;

      function To_Class (Value : Matreshka.Internals.Unicode.Code_Point)
        return Character_Class;
      pragma Inline (To_Class);

      function Switch (S : State; Class : Character_Class) return State;
      pragma Inline (Switch);

      function Rule (S : State) return Rule_Index;
      pragma Inline (Rule);

      First_Final  : constant State := 1;
      Last_Looping : constant State := 2;
      Error_State  : constant State := 5;

      subtype Looping_State is State range 0 .. Last_Looping;
      subtype Final_State is State range First_Final .. Error_State - 1;
   end Tables;

   package Test_Lexers is new Incr.Lexers.Batch_Lexers.Generic_Lexers
     (To_Class     => Tables.To_Class,
      Switch       => Tables.Switch,
      Rule         => Tables.Rule,
      First_Final  => Tables.First_Final,
      Last_Looping => Tables.Last_Looping,
      Error_State  => Tables.Error_State);

end Tests.Lexers;
