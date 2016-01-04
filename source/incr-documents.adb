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

with Incr.Nodes.Tokens;
with Incr.Nodes.Ultra_Roots;

package body Incr.Documents is

   package body Constructors is
      procedure Initialize (Self  : aliased in out Document'Class) is
         Child : constant Nodes.Ultra_Roots.Ultra_Root_Access :=
           new Nodes.Ultra_Roots.Ultra_Root (Self'Unchecked_Access);
      begin
         Nodes.Ultra_Roots.Constructors.Initialize (Child.all);
         Self.Ultra_Root := Child;
      end Initialize;
   end Constructors;

   ------------
   -- Commit --
   ------------

   not overriding procedure Commit (Self : in out Document) is
      Next : Version_Trees.Version;
   begin
      Self.History.Start_Change (Self.History.Changing, Next);
   end Commit;

   -------------------
   -- End_Of_Stream --
   -------------------

   function End_Of_Stream
     (Self : Document) return Nodes.Tokens.Token_Access
   is
      Result : constant Nodes.Node_Access :=
        Self.Ultra_Root.Child (Index => 3, Time => Self.History.Prehistoric);
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
        Self.Ultra_Root.Child (Index => 1, Time => Self.History.Prehistoric);
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
