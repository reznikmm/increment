--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Nodes.Tokens;
with Incr.Nodes.Ultra_Roots;

package body Incr.Documents is

   package body Constructors is
      procedure Initialize
        (Self : aliased in out Document'Class;
         Root : Nodes.Node_Access)
      is
         Child : constant Nodes.Ultra_Roots.Ultra_Root_Access :=
           new Nodes.Ultra_Roots.Ultra_Root (Self'Unchecked_Access);
      begin
         Nodes.Ultra_Roots.Constructors.Initialize (Child.all, Root);
         Self.Ultra_Root := Child;
      end Initialize;
   end Constructors;

   ------------
   -- Commit --
   ------------

   not overriding procedure Commit (Self : in out Document) is
      Next : Version_Trees.Version;
   begin
      Self.Ultra_Root.On_Commit (null);
      Self.History.Start_Change (Self.History.Changing, Next);
   end Commit;

   -------------------
   -- End_Of_Stream --
   -------------------

   function End_Of_Stream
     (Self : Document) return Nodes.Tokens.Token_Access
   is
      Result : constant Nodes.Node_Access :=
        Self.Ultra_Root.Child (Index => 3, Time => Self.History.Changing);
   begin
      return Nodes.Tokens.Token_Access (Result);
   end End_Of_Stream;

   ---------------------
   -- Start_Of_Stream --
   ---------------------

   function Start_Of_Stream
     (Self : Document) return Nodes.Tokens.Token_Access
   is
      Result : constant Nodes.Node_Access :=
        Self.Ultra_Root.Child (Index => 1, Time => Self.History.Changing);
   begin
      return Nodes.Tokens.Token_Access (Result);
   end Start_Of_Stream;

   ----------------
   -- Ultra_Root --
   ----------------

   not overriding function Ultra_Root
     (Self : Document) return Nodes.Node_Access is
   begin
      return Self.Ultra_Root;
   end Ultra_Root;

end Incr.Documents;
