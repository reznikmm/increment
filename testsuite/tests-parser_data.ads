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

with League.String_Vectors;

with Incr.Documents;
with Incr.Nodes;
with Incr.Parsers.Incremental;

package Tests.Parser_Data is
   package P renames Incr.Parsers.Incremental.Parser_Data_Providers;

   type Provider (Document  : Incr.Documents.Document_Access)
     is new P.Parser_Data_Provider and P.Node_Factory with private;

   overriding function Actions
     (Self : Provider) return P.Action_Table_Access;

   overriding function States
     (Self : Provider) return P.State_Table_Access;

   overriding function Part_Counts
     (Self : Provider) return P.Parts_Count_Table_Access;

   overriding function Kind_Image
     (Self : Provider;
      Kind : Incr.Nodes.Node_Kind) return Wide_Wide_String;

   overriding procedure Create_Node
     (Self     : aliased in out Provider;
      Prod     : Incr.Parsers.Incremental.
        Parser_Data_Providers.Production_Index;
      Children : Incr.Nodes.Node_Array;
      Node     : out Incr.Nodes.Node_Access;
      Kind     : out Incr.Nodes.Node_Kind);

   type Node_Kind_Array is array (P.Production_Index range <>) of
     Incr.Nodes.Node_Kind;

private
   package Constructors is
      function Create
        (Document  : Incr.Documents.Document_Access;
         NT        : Node_Kind_Array;
         Parts     : P.Parts_Count_Table;
         Names     : League.String_Vectors.Universal_String_Vector;
         Max_State : P.Parser_State;
         Max_Term  : Incr.Nodes.Token_Kind) return Provider;
   end Constructors;

   type Node_Kind_Array_Access is access all Node_Kind_Array;

   type Action_Table_Access is access P.Action_Table;
   type State_Table_Access is access P.State_Table;
   type Parts_Count_Table_Access is access P.Parts_Count_Table;

   type Provider
     (Document  : Incr.Documents.Document_Access)
   is new P.Parser_Data_Provider and P.Node_Factory with record
      Max_Term  : Incr.Nodes.Token_Kind;
      Max_NT    : Incr.Nodes.Node_Kind;
      Names     : League.String_Vectors.Universal_String_Vector;
      Actions   : Action_Table_Access;
      States    : State_Table_Access;
      NT        : Node_Kind_Array_Access;
      Parts     : Parts_Count_Table_Access;
   end record;

end Tests.Parser_Data;
