--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with XML.SAX.Writers;

with Incr.Documents;
with Incr.Parsers.Incremental;

package Incr.Debug is
   package P renames Incr.Parsers.Incremental.Parser_Data_Providers;

   procedure Dump
     (Doc      : Incr.Documents.Document'Class;
      Provider : P.Parser_Data_Provider'Class;
      Output   : in out XML.SAX.Writers.SAX_Writer'Class);
end Incr.Debug;
