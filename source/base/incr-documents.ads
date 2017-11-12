--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

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
   --  These three nodes are created on initialization and exist forever.
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
      procedure Initialize
        (Self : aliased in out Document'Class;
         Root : Nodes.Node_Access);
      --  Initialze Self as new Document. Create sentinel nodes and reset
      --  parsing tree root to Root pointer.
   end Constructors;

private

   type Document (History : access Version_Trees.Version_Tree'Class) is
     tagged limited
   record
      Ultra_Root : access Nodes.Node'Class;
   end record;

end Incr.Documents;
