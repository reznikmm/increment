--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Incr.Nodes.Joints is

   type Joint (Document : Documents.Document_Access;
               Arity    : Natural) is new Node with private;

   type Joint_Access is access all Joint'Class;

   package Constructors is
      procedure Initialize
        (Self     : aliased out Joint'Class;
         Kind     : Node_Kind;
         Children : Node_Array);

      procedure Initialize_Ancient
        (Self   : out Joint'Class;
         Parent : Node_Access);

   end Constructors;

private

   type Versioned_Node_Array is array (Positive range <>) of
     Versioned_Nodes.Container;

   type Cached_Integer is record
      Value : Integer := -1;  --  -1 means invalid cache
      Time  : Version_Trees.Version;
   end record;

   type Cached_Spans is array (Span_Kinds) of Cached_Integer;

   type Joint (Document : Documents.Document_Access;
               Arity    : Natural) is
     new Node_With_Parent (Document) with
   record
      Kind       : Node_Kind;
      Kids       : Versioned_Node_Array (1 .. Arity);
      NC         : Versioned_Booleans.Container;
      NE         : Versioned_Booleans.Container;
      Span_Cache : Cached_Spans;
   end record;

   overriding function Arity (Self : Joint) return Natural;

   overriding function Is_Token (Self : Joint) return Boolean;

   overriding function Kind (Self : Joint) return Node_Kind;

   overriding function Child
     (Self  : Joint;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access;

   overriding procedure Set_Child
     (Self  : aliased in out Joint;
      Index : Positive;
      Value : Node_Access);

   overriding function Nested_Changes
     (Self : Joint;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean;

   overriding function Nested_Errors
     (Self : Joint;
      Time : Version_Trees.Version) return Boolean;

   overriding function Span
     (Self : aliased in out Joint;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural;

   overriding procedure Discard (Self  : in out Joint);

   overriding procedure On_Commit
     (Self   : in out Joint;
      Parent : Node_Access);

end Incr.Nodes.Joints;
