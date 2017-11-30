--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Nodes.Joints;

package body Incr.Nodes.Ultra_Roots is

   -----------
   -- Arity --
   -----------

   overriding function Arity (Self : Ultra_Root) return Natural is
      pragma Unreferenced (Self);
   begin
      return 3;
   end Arity;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self  : Ultra_Root;
      Index : Positive;
      Time  : Version_Trees.Version)
      return Node_Access
   is
   begin
      case Index is
         when 1 =>
            return Node_Access (Self.BOS);
         when 2 =>
            return Versioned_Nodes.Get (Self.Root, Time);
         when 3 =>
            return Node_Access (Self.EOS);
         when others =>
            raise Constraint_Error;
      end case;
   end Child;

   -------------
   -- Discard --
   -------------

   overriding procedure Discard (Self  : in out Ultra_Root) is
   begin
      raise Program_Error with "Unimplemented";
   end Discard;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Boolean
   is
      pragma Unreferenced (Time, Self);
   begin
      return True;
   end Exists;

   --------------
   -- Get_Flag --
   --------------

   overriding function Get_Flag
     (Self  : Ultra_Root;
      Flag  : Transient_Flags) return Boolean is
      pragma Unreferenced (Self, Flag);
   begin
      return True;
   end Get_Flag;

   --------------
   -- Is_Token --
   --------------

   overriding function Is_Token (Self : Ultra_Root) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Token;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Self : Ultra_Root) return Node_Kind is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Kind;

   -------------------
   -- Local_Changes --
   -------------------

   overriding function Local_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      use type Version_Trees.Version;
      Next : Version_Trees.Version := To;
      Prev : Version_Trees.Version;
   begin
      loop
         Prev := Self.Document.History.Parent (Next);

         if Self.Child (2, Prev) /= Self.Child (2, Next) then
            return True;
         end if;

         exit when From = Prev;

         Next := Prev;
      end loop;

      return False;
   end Local_Changes;

   -------------------
   -- Nested_Errors --
   -------------------

   overriding function Nested_Errors
     (Self : Ultra_Root;
      Time : Version_Trees.Version) return Boolean
   is
      Child : Nodes.Node_Access;
   begin
      for J in 1 .. 3 loop
         Child := Self.Child (J, Time);

         if Child /= null then
            if Child.Local_Errors (Time) or Child.Nested_Errors (Time) then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Nested_Errors;

   --------------------
   -- Nested_Changes --
   --------------------

   overriding function Nested_Changes
     (Self : Ultra_Root;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      pragma Unreferenced (Self, From, To);
   begin
      return True;
   end Nested_Changes;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit
     (Self   : in out Ultra_Root;
      Parent : Node_Access)
   is
      Root : constant Node_Access :=
        Versioned_Nodes.Get (Self.Root, Self.Document.History.Changing);
   begin
      pragma Assert (Parent = null);
      Mark_Deleted_Children (Self);
      Self.BOS.On_Commit (Self'Unchecked_Access);

      if Root /= null then
         Root.On_Commit (Self'Unchecked_Access);
      end if;

      Self.EOS.On_Commit (Self'Unchecked_Access);
   end On_Commit;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : Ultra_Root;
      Time  : Version_Trees.Version) return Node_Access
   is
      pragma Unreferenced (Self, Time);
   begin
      return null;
   end Parent;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self  : aliased in out Ultra_Root;
      Index : Positive;
      Value : Node_Access)
   is
      Now : constant Version_Trees.Version := Self.Document.History.Changing;
      Old : constant Node_Access := Self.Child (Index, Now);

      Ignore : Integer := 0;
   begin
      if Index /= 2 then
         raise Constraint_Error;
      elsif Old /= null then
         Old.Set_Parent (null);
      end if;

      Versioned_Nodes.Set (Self.Root, Value, Now, Ignore);

      if Value /= null then
         Value.Set_Parent (Self'Unchecked_Access);
      end if;
   end Set_Child;

   ----------
   -- Span --
   ----------

   overriding function Span
     (Self : aliased in out Ultra_Root;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural
   is
      Root_Span : constant Natural := Self.Child (2, Time).Span (Kind, Time);
   begin
      case Kind is
         when Text_Length =>
            return Root_Span + 1;  --  including end_of_stream character
         when Token_Count =>
            return Root_Span + 2;  --  including sentinel tokens
         when Line_Count =>
            return Root_Span + 1;  --  including end_of_stream character
      end case;
   end Span;

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self  : out Ultra_Root'Class;
         Root  : Nodes.Node_Access) is
      begin
         Self.BOS := new Nodes.Tokens.Token (Self.Document);
         Nodes.Tokens.Constructors.Initialize_Ancient
           (Self   => Self.BOS.all,
            Parent => Self'Unchecked_Access,
            Back   => 0);

         Self.EOS := new Nodes.Tokens.Token (Self.Document);
         Nodes.Tokens.Constructors.Initialize_Ancient
           (Self   => Self.EOS.all,
            Parent => Self'Unchecked_Access,
            Back   => 1);

         if Root /= null then
            Nodes.Joints.Constructors.Initialize_Ancient
              (Self   => Nodes.Joints.Joint (Root.all),
               Parent => Self'Unchecked_Access);
         end if;

         Versioned_Nodes.Initialize (Self.Root, Root);
      end Initialize;

   end Constructors;

end Incr.Nodes.Ultra_Roots;
