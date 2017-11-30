--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Nodes.Tokens;

package body Incr.Nodes is

   To_Diff  : constant array (Boolean) of Integer :=
     (False => -1, True => 1);

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Self : aliased in out Node_With_Parent'Class) is
      begin
         Versioned_Booleans.Initialize (Self.Exist, False);
         Versioned_Booleans.Initialize (Self.LC, False);
         Versioned_Booleans.Initialize (Self.LE, False);
         Versioned_Nodes.Initialize (Self.Parent, null);
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self    : aliased in out Node_With_Parent'Class;
         Parent  : Node_Access) is
      begin
         Versioned_Booleans.Initialize (Self.Exist, True);
         Versioned_Booleans.Initialize (Self.LC, False);
         Versioned_Booleans.Initialize (Self.LE, False);
         Versioned_Nodes.Initialize (Self.Parent, Parent);
      end Initialize_Ancient;

   end Constructors;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Self  : Node_With_Exist;
      Time  : Version_Trees.Version) return Boolean is
   begin
      return Versioned_Booleans.Get (Self.Exist, Time);
   end Exists;

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index
     (Self  : Node'Class;
      Child : Constant_Node_Access;
      Time  : Version_Trees.Version) return Natural is
   begin
      for J in 1 .. Self.Arity loop
         if Constant_Node_Access (Self.Child (J, Time)) = Child then
            return J;
         end if;
      end loop;

      return 0;
   end Child_Index;

   --------------------
   -- Discard_Parent --
   --------------------

   overriding procedure Discard_Parent (Self : in out Node_With_Parent) is
      Changed : Boolean;
      Ignore  : Integer := 0;
      Now     : constant Version_Trees.Version :=
        Self.Document.History.Changing;
   begin
      Changed := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Changed then
         Self.Propagate_Nested_Changes (-1);
      end if;

      Versioned_Nodes.Discard (Self.Parent, Now, Ignore);

      if Changed then
         Self.Propagate_Nested_Changes (1);
      end if;
   end Discard_Parent;

   -----------------
   -- First_Token --
   -----------------

   function First_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version)
      return Tokens.Token_Access
   is
      Child : Node_Access;
   begin
      if Self.Arity > 0 then
         Child := Self.Child (1, Time);

         if Child.Is_Token then
            return Tokens.Token_Access (Child);
         else
            return Child.First_Token (Time);
         end if;
      elsif Self.Is_Token then
         return Tokens.Token'Class (Self)'Access;
      else
         return null;
      end if;
   end First_Token;

   --------------
   -- Get_Flag --
   --------------

   overriding function Get_Flag
     (Self  : Node_With_Exist;
      Flag  : Transient_Flags) return Boolean is
   begin
      return Self.Flag (Flag);
   end Get_Flag;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token
     (Self : aliased in out Node'Class;
      Time : Version_Trees.Version) return Tokens.Token_Access
   is
      Child : Node_Access;
   begin
      if Self.Arity > 0 then
         Child := Self.Child (Self.Arity, Time);

         if Child.Is_Token then
            return Tokens.Token_Access (Child);
         else
            return Child.Last_Token (Time);
         end if;
      elsif Self.Is_Token then
         return Tokens.Token'Class (Self)'Access;
      else
         return null;
      end if;
   end Last_Token;

   -------------------
   -- Local_Changes --
   -------------------

   overriding function Local_Changes
     (Self : Node_With_Exist;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      use type Version_Trees.Version;

      Time : Version_Trees.Version := To;
   begin
      if Self.Document.History.Is_Changing (To) then
         --  Self.LC doesn't contain Local_Changes for Is_Changing version yet
         --  Take it from Self.Nested_Changes
         if Self.Local_Changes > 0 then
            return True;
         elsif Time = From then
            return False;
         end if;

         Time := Self.Document.History.Parent (Time);
      end if;

      while Time /= From loop
         if Versioned_Booleans.Get (Self.LC, Time) then
            return True;
         end if;

         Time := Self.Document.History.Parent (Time);
      end loop;

      return False;
   end Local_Changes;

   ------------------
   -- Local_Errors --
   ------------------

   overriding function Local_Errors
     (Self : Node_With_Exist;
      Time : Version_Trees.Version) return Boolean is
   begin
      return Versioned_Booleans.Get (Self.LE, Time);
   end Local_Errors;

   ------------------
   -- Next_Subtree --
   ------------------

   function Next_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access
   is
      Node   : Constant_Node_Access := Self'Unchecked_Access;
      Parent : Node_Access := Node.Parent (Time);
      Child  : Node_Access;
   begin
      while Parent /= null loop
         declare
            J : constant Natural := Parent.Child_Index (Node, Time);
         begin
            if J in 1 .. Parent.Arity - 1 then
               for K in J + 1 .. Parent.Arity loop
                  Child := Parent.Child (K, Time);

                  if Child /= null then
                     return Child;
                  end if;
               end loop;
            end if;
         end;

         Node := Constant_Node_Access (Parent);
         Parent := Node.Parent (Time);
      end loop;

      return null;
   end Next_Subtree;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit
     (Self   : in out Node_With_Exist;
      Parent : Node_Access)
   is
      Now   : constant Version_Trees.Version := Self.Document.History.Changing;
      This  : constant Node_Access := Self'Unchecked_Access;
      Child : Node_Access;
      Diff  : Integer := 0;  --  Ignore this diff
   begin
      pragma Assert (Node'Class (Self).Parent (Now) = Parent);
      Versioned_Booleans.Set (Self.LC, Self.Local_Changes > 0, Now, Diff);
      Self.Nested_Changes := 0;
      Self.Local_Changes := 0;
      Self.Flag := (others => False);

      for J in 1 .. This.Arity loop
         Child := This.Child (J, Now);

         if Child /= null then
            Child.On_Commit (Self'Unchecked_Access);
         end if;
      end loop;
   end On_Commit;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : Node_With_Parent;
      Time  : Version_Trees.Version)
      return Node_Access is
   begin
      return Versioned_Nodes.Get (Self.Parent, Time);
   end Parent;


   ----------------------
   -- Previous_Subtree --
   ----------------------

   function Previous_Subtree
     (Self : Node'Class;
      Time : Version_Trees.Version) return Node_Access
   is
      Node   : Constant_Node_Access := Self'Unchecked_Access;
      Parent : Node_Access := Node.Parent (Time);
      Child  : Node_Access;
   begin
      while Parent /= null loop
         declare
            J : constant Natural := Parent.Child_Index (Node, Time);
         begin
            if J in 2 .. Parent.Arity then
               for K in reverse 1 .. J - 1 loop
                  Child := Parent.Child (K, Time);

                  if Child /= null then
                     return Child;
                  end if;
               end loop;
            end if;
         end;

         Node := Constant_Node_Access (Parent);
         Parent := Node.Parent (Time);
      end loop;

      return null;
   end Previous_Subtree;

   ------------------------------
   -- Propagate_Nested_Changes --
   ------------------------------

   procedure Propagate_Nested_Changes
     (Self : in out Node'Class;
      Diff : Integer)
   is
      Parent : constant Node_Access :=
        Self.Parent (Self.Document.History.Changing);
   begin
      if Parent /= null then
         Parent.On_Nested_Changes (Diff);
      end if;
   end Propagate_Nested_Changes;

   ------------------------------
   -- Propagate_Nested_Changes --
   ------------------------------

   overriding procedure On_Nested_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := Self.Local_Changes > 0 or Self.Nested_Changes > 0;
      Self.Nested_Changes := Self.Nested_Changes + Diff;
      After := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Before /= After then
         Self.Propagate_Nested_Changes (To_Diff (After));
      end if;
   end On_Nested_Changes;

   --------------
   -- Set_Flag --
   --------------

   overriding procedure Set_Flag
     (Self  : in out Node_With_Exist;
      Flag  : Transient_Flags;
      Value : Boolean := True)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := (Self.Flag and Local_Changes_Mask) /= No_Flags;
      Self.Flag (Flag) := Value;
      After := (Self.Flag and Local_Changes_Mask) /= No_Flags;

      if Before /= After then
         Self.Update_Local_Changes (To_Diff (After));
      end if;
   end Set_Flag;

   ----------------------
   -- Set_Local_Errors --
   ----------------------

   overriding procedure Set_Local_Errors
     (Self  : in out Node_With_Exist;
      Value : Boolean := True)
   is
      Now : constant Version_Trees.Version := Self.Document.History.Changing;
      Diff : Integer := 0;
   begin
      Versioned_Booleans.Set (Self.LE, Value, Now, Diff);
      Self.Update_Local_Changes (Diff);
   end Set_Local_Errors;

   ----------------
   -- Set_Parent --
   ----------------

   overriding procedure Set_Parent
     (Self  : in out Node_With_Parent;
      Value : Node_Access)
   is
      Changed : Boolean;
      Ignore  : Integer := 0;
      Now     : constant Version_Trees.Version :=
        Self.Document.History.Changing;
   begin
      Changed := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Changed then
         Self.Propagate_Nested_Changes (-1);
      end if;

      Versioned_Nodes.Set (Self.Parent, Value, Now, Ignore);

      if Changed then
         Self.Propagate_Nested_Changes (1);
      end if;
   end Set_Parent;

   --------------------------
   -- Update_Local_Changes --
   --------------------------

   not overriding procedure Update_Local_Changes
     (Self : in out Node_With_Exist;
      Diff : Integer)
   is
      Before : Boolean;
      After  : Boolean;
   begin
      Before := Self.Local_Changes > 0 or Self.Nested_Changes > 0;
      Self.Local_Changes := Self.Local_Changes + Diff;
      After := Self.Local_Changes > 0 or Self.Nested_Changes > 0;

      if Before /= After then
         Self.Propagate_Nested_Changes (To_Diff (After));
      end if;
   end Update_Local_Changes;

end Incr.Nodes;
