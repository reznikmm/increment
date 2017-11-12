--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.Templates.Streams;

with Tests.Commands;

package Tests.Parser_Data.XML_Reader is
   type Reader (Data : access Provider) is limited
     new XML.SAX.Content_Handlers.SAX_Content_Handler with private;

   function Get_Commands
     (Self : Reader) return Tests.Commands.Command_Vectors.Vector;

private
   type Reader (Data : access Provider) is limited
   new XML.SAX.Content_Handlers.SAX_Content_Handler with record
      Collect_Text : Boolean := False;
      Collect_XML  : Boolean := False;
      Index        : Positive;
      Text         : League.Strings.Universal_String;
      List         : League.String_Vectors.Universal_String_Vector;
      Commands     : Tests.Commands.Command_Vectors.Vector;
      Vector       : XML.Templates.Streams.XML_Stream_Element_Vectors.Vector;
   end record;

   overriding procedure Characters
    (Self    : in out Reader;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean);

   overriding procedure End_Element
    (Self           : in out Reader;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Success        : in out Boolean);

   overriding function Error_String
    (Self : Reader) return League.Strings.Universal_String;

   overriding procedure Start_Element
    (Self           : in out Reader;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Attributes     : XML.SAX.Attributes.SAX_Attributes;
     Success        : in out Boolean);

end Tests.Parser_Data.XML_Reader;
