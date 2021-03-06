--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Nodes.Tokens;

package Incr.Nodes.Ultra_Roots is
   --  @summary
   --  Ultra_Root nodes of parse trees
   --
   --  @description
   --  This package provides implementation of ultra_root nodes

   type Ultra_Root is new Node with private;
   type Ultra_Root_Access is access all Ultra_Root'Class;

   package Constructors is
      procedure Initialize
        (Self  : out Ultra_Root'Class;
         Root  : Nodes.Node_Access);
   end Constructors;

private

   type Versioned_Node_Array is array (Positive range <>) of
     Versioned_Nodes.Container;

   type Ultra_Root is new Node with record
      BOS    : Nodes.Tokens.Token_Access;
      Root   : Versioned_Nodes.Container;
      EOS    : Nodes.Tokens.Token_Access;
   end record;

   overriding function Is_Token (Self : Ultra_Root) return Boolean;

   overriding function Arity (Self : Ultra_Root) return Natural;

   overriding function Kind (Self : Ultra_Root) return Node_Kind;

   overriding function Child
     (Self  : Ultra_Root;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Child
     (Self  : aliased in out Ultra_Root;
      Index : Positive;
      Value : Node_Access);

   overriding function Nested_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Parent
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Parent
     (Self  : in out Ultra_Root;
      Value : Node_Access) is null;

   overriding function Exists
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Boolean;

   overriding function Get_Flag
     (Self  : Ultra_Root;
      Flag  : Transient_Flags) return Boolean;

   overriding procedure Set_Flag
     (Self  : in out Ultra_Root;
      Flag  : Transient_Flags;
      Value : Boolean := True) is null;

   overriding procedure On_Commit
     (Self   : in out Ultra_Root;
      Parent : Node_Access);

   overriding procedure On_Nested_Changes
     (Self : in out Ultra_Root;
      Diff : Integer) is null;

   overriding function Span
     (Self : aliased in out Ultra_Root;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural;

   overriding procedure Discard (Self  : in out Ultra_Root);

   overriding procedure Discard_Parent (Self : in out Ultra_Root) is null;

   overriding function Local_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Nested_Errors
     (Self : Ultra_Root;
      Time : Version_Trees.Version) return Boolean;

   overriding function Local_Errors
     (Self : Ultra_Root;
      Unused : Version_Trees.Version) return Boolean is (False);

   overriding procedure Set_Local_Errors
     (Self  : in out Ultra_Root;
      Value : Boolean := True) is null;

end Incr.Nodes.Ultra_Roots;
