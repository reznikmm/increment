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

with Incr.Version_Trees;
limited with Incr.Nodes.Tokens;

package Incr.Documents is
   --  @summary
   --  Document representation
   --
   --  @description
   --  This package provides Document type.
   --
   --  Document represents single parsing tree together with its history.
   --  Three sentinel nodes surround actual parsing tree:
   --  * Start_Of_Stream - empty token at the beginning of the document
   --  * End_Of_Stream   - empty token at the end of the document
   --  * Ultra_Root      - contains these two tokens and root of parsing tree
   --  These three nodes are created in prehistoric time and exist forever.
   --
   --  Original grammar is augmented by new top rule:
   --     ultra_root ::= <start_of_stream> [original_root] <end_of_stream>
   --
   --  As result any new empty document always matches the grammar and could
   --  serve as base for incremental analysis.

   type Document (History : access Version_Trees.Version_Tree'Class) is
     tagged limited private;
   --  Document representation

   type Document_Access is access all Document'Class;

   not overriding function Ultra_Root
     (Self : Document) return Nodes.Node_Access;
   --  Return ultra-root sentinel node of given Document.

   not overriding function Start_Of_Stream
     (Self : Document) return Nodes.Tokens.Token_Access;
   --  Return start-of-stream sentinel token of given Document.

   not overriding function End_Of_Stream
     (Self : Document) return Nodes.Tokens.Token_Access;
   --  Return end-of-stream sentinel token of given Document.

   not overriding procedure Commit (Self : in out Document);
   --  Single point commit routine.

   package Constructors is
      procedure Initialize (Self  : aliased in out Document);
      --  Initialze Self as new Document. Create sentinel nodes and reset
      --  parsing tree root to null pointer.
   end Constructors;

private

   type Document (History : access Version_Trees.Version_Tree'Class) is
     tagged limited
   record
      Ultra_Root : access Nodes.Node'Class;
   end record;

end Incr.Documents;
