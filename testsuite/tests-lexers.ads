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

   end Tables;

   package Test_Lexers is new Incr.Lexers.Batch_Lexers.Generic_Lexers
     (To_Class    => Tables.To_Class,
      Switch      => Tables.Switch,
      Rule        => Tables.Rule,
      Error_State => 5);

end Tests.Lexers;
