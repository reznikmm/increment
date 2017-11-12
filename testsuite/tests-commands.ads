--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;

with League.Strings;

with XML.Templates.Streams;

package Tests.Commands is
   pragma Preelaborate;

   type Command_Kind is
     (Commit,
      Dump_Tree,
      Run,
      Set_EOS_Text,
      Set_Token_Text);

   type Command (Kind : Command_Kind := Commit) is record
      case Kind is
         when Commit | Run =>
            null;
         when Set_EOS_Text | Set_Token_Text =>
            Text : League.Strings.Universal_String;

            case Kind is
               when Set_Token_Text =>
                  Token : Positive;
               when others =>
                  null;
            end case;
         when Dump_Tree =>
            Dump : XML.Templates.Streams.XML_Stream_Element_Vectors.Vector;
      end case;
   end record;

   package Command_Vectors is
     new Ada.Containers.Vectors (Positive, Command);

end Tests.Commands;
