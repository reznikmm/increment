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

with Ada.Characters.Wide_Wide_Latin_1;

with Incr.Documents;
limited with Incr.Nodes.Tokens;

with Incr.Version_Trees;

package Incr.Nodes is
   --  @summary
   --  Nodes of parse trees
   --
   --  @description
   --  This package provides abstract Node type.
   --  Actuall nodes could be one of two kinds:
   --  * Tokens represent terminal symbols and contains text of the document
   --  * interconnecting nodes represent non-terminal symbols and have
   --  one or more children.
   --  Number of children is constant over node's life time.
   --
   --  When node changes (by modification text of the token, assigning new
   --  children to mode, etc) it's marked as having local changes.
   --  This mark propagates up over the tree and aggregates as nested changes
   --  flag on ancestor nodes. This flag stored in history and allows one to
   --  locate the changed subtrees quickly. Nested changes flag doesn't count
   --  node's local changes.

   type Node
     (Document : Documents.Document_Access) is abstract tagged private;
   --  Node abstract type represents a node inside parse tree.

   type Node_Access is access all Node'Class;

   type Node_Array is array (Positive range <>) of Node_Access;

   not overriding function Is_Token (Self : Node) return Boolean is abstract;
   --  Check if given node is token.

   not overriding function Arity (Self : Node) return Natural is abstract;
   --  Return number of node's children.

   type Node_Kind is new Integer;
   subtype Token_Kind is Node_Kind range 0 .. Node_Kind'Last;

   not overriding function Kind (Self : Node) return Node_Kind is abstract;
   --  Return type of the node. Kind is not expected to change

   not overriding function Child
     (Self  : Node;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access is abstract;
   --  Get node's child by its position. Result could be null if child absent.

   not overriding procedure Set_Child
     (Self  : aliased in out Node;
      Index : Positive;
      Value : Node_Access) is abstract;
   --  Assign node's child by its position.

   not overriding function Nested_Changes
     (Self : Node;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean is abstract;
   --  Check if any node under subtree rooted by Self (exluding Self node)
   --  was changed in any way between given two versions.

   not overriding function Local_Changes
     (Self : Node;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean is abstract;
   --  Check if node itself was changed in any way between given two versions.

   not overriding function Nested_Errors
     (Self : Node;
      Time : Version_Trees.Version) return Boolean is abstract;
   --  Check if any node under subtree rooted by Self (exluding Self node)
   --  has errors in given version.

   not overriding function Local_Errors
     (Self : Node;
      Time : Version_Trees.Version) return Boolean is abstract;
   --  Check if node itself has errors in given version.

   not overriding procedure Set_Local_Errors
     (Self  : in out Node;
      Value : Boolean := True) is abstract;

   not overriding function Parent
     (Self  : Node;
      Time  : Version_Trees.Version) return Node_Access is abstract;
   --  Return parent of the node. Could be temporary null in changing version.

   not overriding procedure Set_Parent
     (Self  : in out Node;
      Value : Node_Access) is abstract;
   --  ??? should it be hidden

   not overriding procedure Discard (Self  : in out Node) is abstract;
   --  Discards any uncommitted modifications to this node.

   not overriding function Exists
     (Self  : Node;
      Time  : Version_Trees.Version) return Boolean is abstract;
   --  Check if given node still exist at given time/version.

   type Transient_Flags is (Need_Analysis);
   --  These flags valid only at changing version and reset by commit.
   --
   --  @value Need_Analysis mark nodes to analyse them latter

   not overriding function Get_Flag
     (Self  : Node;
      Flag  : Transient_Flags) return Boolean is abstract;
   --  Return given transient flag of the node

   not overriding procedure Set_Flag
     (Self  : in out Node;
      Flag  : Transient_Flags;
      Value : Boolean := True) is abstract;
   --  Set given transient flag on the node

   type Span_Kinds is (Text_Length, Token_Count, Line_Count);

   not overriding function Span
     (Self : aliased in out Node;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural is abstract;
   --  Length of the node in term of character/line/token count

   LF : constant Wide_Wide_Character := Ada.Characters.Wide_Wide_Latin_1.LF;

   function First_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version)
      return Tokens.Token_Access;
   --  Return first token in subtree rooted by given Node if any

   function Last_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version) return Tokens.Token_Access;
   --  Return last token in subtree rooted by given Node if any

   function Next_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access;
   --  Return next subtree:
   --  * return sibling node if Node is not the last child of its parent
   --  * return next subtree of the parent otherwise

   function Previous_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access;
   --  Return previous subtree:
   --  * return sibling node if Node is not the first child of its parent
   --  * return previous subtree of the parent otherwise

   --  Internal subprograms:

   not overriding procedure On_Commit (Self : in out Node) is abstract;

   not overriding procedure On_Nested_Changes
     (Self : in out Node;
      Diff : Integer) is abstract;

   not overriding procedure Discard_Parent (Self : in out Node) is abstract;

private

   type Flag_Array is array (Transient_Flags) of Boolean with Pack;

   No_Flags : constant Flag_Array := (others => False);

   Local_Changes_Mask : constant Flag_Array :=
     (Need_Analysis => True);

   package Versioned_Booleans is new Version_Trees.Versioned_Values (Boolean);
   package Versioned_Nodes is new Version_Trees.Versioned_Values (Node_Access);

   type Node
     (Document : Documents.Document_Access) is abstract tagged null record;

   procedure Propagate_Nested_Changes
     (Self : in out Node'Class;
      Diff : Integer);

   type Node_With_Exist is abstract new Node with record
      Exist : Versioned_Booleans.Container;
      LC    : Versioned_Booleans.Container;  -- Versioned local changes
      LE    : Versioned_Booleans.Container;  -- Versioned local errors
      --  Transient fields valid during current version only
      Local_Changes  : Natural := 0;
      Nested_Changes : Natural := 0;
      Flag           : Flag_Array := No_Flags;
   end record;

   not overriding procedure Update_Local_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer);

   overriding procedure On_Nested_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer);

   overriding function Exists
     (Self  : Node_With_Exist;
      Time  : Version_Trees.Version) return Boolean;

   overriding function Get_Flag
     (Self  : Node_With_Exist;
      Flag  : Transient_Flags) return Boolean;

   overriding procedure Set_Flag
     (Self  : in out Node_With_Exist;
      Flag  : Transient_Flags;
      Value : Boolean := True);

   overriding procedure On_Commit (Self : in out Node_With_Exist);

   overriding function Local_Changes
     (Self : Node_With_Exist;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Local_Errors
     (Self : Node_With_Exist;
      Time : Version_Trees.Version) return Boolean;

   overriding procedure Set_Local_Errors
     (Self  : in out Node_With_Exist;
      Value : Boolean := True);

   type Node_With_Parent is abstract new Node_With_Exist with record
      Parent : Versioned_Nodes.Container;
      --  Parent is NOT a property of the node! Don't account it in calculation
      --  of local/nested change flags.
   end record;

   overriding function Parent
     (Self  : Node_With_Parent;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Parent
     (Self  : in out Node_With_Parent;
      Value : Node_Access);

   overriding procedure Discard_Parent (Self : in out Node_With_Parent);

   package Constructors is
      procedure Initialize
        (Self    : aliased in out Node_With_Parent'Class);
      --  Initialize Self as nonexistent node.

      procedure Initialize_Ancient
        (Self    : aliased in out Node_With_Parent'Class;
         Parent  : Node_Access);
      --  Initialize Self as node existent in initial version of the document.
   end Constructors;

end Incr.Nodes;
