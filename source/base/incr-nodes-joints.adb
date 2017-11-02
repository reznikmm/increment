package body Incr.Nodes.Joints is

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self     : aliased out Joint'Class;
         Kind     : Node_Kind;
         Children : Node_Array)
      is
         Now  : constant Version_Trees.Version :=
           Self.Document.History.Changing;
         Diff : Integer := 0;
      begin
         Self.Kind := Kind;

         Nodes.Constructors.Initialize (Self);
         Versioned_Booleans.Initialize (Self.NC, False);
         Versioned_Booleans.Initialize (Self.NE, False);
         Versioned_Booleans.Set (Self.Exist, True, Now, Diff);

         for J in Children'Range loop
            Versioned_Nodes.Initialize (Self.Kids (J), null);
            Versioned_Nodes.Set (Self.Kids (J), Children (J), Now, Diff);

            if Children (J) /= null then
               Children (J).Set_Parent (Self'Unchecked_Access);
            end if;
         end loop;

         Self.Update_Local_Changes (Diff);
      end Initialize;

      ------------------------
      -- Initialize_Ancient --
      ------------------------

      procedure Initialize_Ancient
        (Self   : out Joint'Class;
         Parent : Node_Access) is
      begin
         Nodes.Constructors.Initialize_Ancient (Self, Parent);
         Versioned_Booleans.Initialize (Self.NC, False);
         Versioned_Booleans.Initialize (Self.NE, False);
      end Initialize_Ancient;

   end Constructors;

   -----------
   -- Arity --
   -----------

   overriding function Arity (Self : Joint) return Natural is
   begin
      return Self.Arity;
   end Arity;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Self  : Joint;
      Index : Positive;
      Time  : Version_Trees.Version) return Node_Access is
   begin
      return Versioned_Nodes.Get (Self.Kids (Index), Time);
   end Child;

   -------------
   -- Discard --
   -------------

   overriding procedure Discard (Self  : in out Joint) is
      Now  : constant Version_Trees.Version := Self.Document.History.Changing;
      Diff : Integer := 0;
   begin
      Versioned_Booleans.Discard (Self.Exist, Now, Diff);

      for J in Self.Kids'Range loop
         Versioned_Nodes.Discard (Self.Kids (J), Now, Diff);
         Self.Child (J, Now).Discard_Parent;
      end loop;

      Self.Update_Local_Changes (Diff);
   end Discard;

   --------------
   -- Is_Token --
   --------------

   overriding function Is_Token (Self : Joint) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Token;

   ----------
   -- Kind --
   ----------

   overriding function Kind (Self : Joint) return Node_Kind is
   begin
      return Self.Kind;
   end Kind;

   --------------------
   -- Nested_Changes --
   --------------------

   overriding function Nested_Changes
     (Self : Joint;
      From : Version_Trees.Version;
      To   : Version_Trees.Version) return Boolean
   is
      use type Version_Trees.Version;

      Time : Version_Trees.Version := To;
   begin
      if Self.Document.History.Is_Changing (To) then
         if Self.Nested_Changes > 0 then
            return True;
         elsif Time = From then
            return False;
         end if;

         Time := Self.Document.History.Parent (Time);
      end if;

      while Time /= From loop
         if Versioned_Booleans.Get (Self.NC, Time) then
            return True;
         end if;

         Time := Self.Document.History.Parent (Time);
      end loop;

      return False;
   end Nested_Changes;

   -------------------
   -- Nested_Errors --
   -------------------

   overriding function Nested_Errors
     (Self : Joint;
      Time : Version_Trees.Version) return Boolean is
   begin
      return Versioned_Booleans.Get (Self.NE, Time);
   end Nested_Errors;

   ---------------
   -- On_Commit --
   ---------------

   overriding procedure On_Commit (Self : in out Joint) is
      Now  : constant Version_Trees.Version := Self.Document.History.Changing;
      Child  : Nodes.Node_Access;
      Errors : Boolean := False;
      Ignore : Integer := 0;
   begin
      Versioned_Booleans.Set
        (Self    => Self.NC,
         Value   => Self.Nested_Changes > 0,
         Time    => Self.Document.History.Changing,
         Changes => Ignore);

      Node_With_Parent (Self).On_Commit;

      for J in Self.Kids'Range loop
         Child := Self.Child (J, Now);

         if Child.Nested_Errors (Now) or else Child.Local_Errors (Now) then
            Errors := True;
            exit;
         end if;
      end loop;

      Versioned_Booleans.Set (Self.NE, Errors, Now, Ignore);
   end On_Commit;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self  : aliased in out Joint;
      Index : Positive;
      Value : Node_Access)
   is
      Diff : Integer := 0;
      Now  : constant Version_Trees.Version := Self.Document.History.Changing;
   begin
      Versioned_Nodes.Set (Self.Kids (Index), Value, Now, Diff);
      Self.Update_Local_Changes (Diff);

      if Value /= null then
         Value.Set_Parent (Self'Unchecked_Access);
      end if;
   end Set_Child;

   ----------
   -- Span --
   ----------

   overriding function Span
     (Self : aliased in out Joint;
      Kind : Span_Kinds;
      Time : Version_Trees.Version) return Natural
   is
      use type Version_Trees.Version;

      function Get_Span return Natural;

      --------------
      -- Get_Span --
      --------------

      function Get_Span return Natural is
         Result : Natural := 0;
      begin
         for J in 1 .. Self.Arity loop
            Result := Result + Self.Child (J, Time).Span (Kind, Time);
         end loop;

         return Result;
      end Get_Span;

      Cached : Cached_Integer renames Self.Span_Cache (Kind);
   begin
      if Cached.Value = -1 or else Cached.Time /= Time then
         Cached.Value := Get_Span;
         Cached.Time := Time;
      end if;

      return Cached.Value;
   end Span;

end Incr.Nodes.Joints;
