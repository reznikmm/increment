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

with Incr.Nodes.Joints;

package body Tests.Parser_Data is

   -------------
   -- Actions --
   -------------

   overriding function Actions
     (Self : Provider) return P.Action_Table_Access is
   begin
      return P.Action_Table_Access (Self.Actions);
   end Actions;

   package body Constructors is
      function Create
        (Document  : Incr.Documents.Document_Access;
         NT        : Node_Kind_Array;
         Parts     : P.Parts_Count_Table;
         Names     : League.String_Vectors.Universal_String_Vector;
         Max_State : P.Parser_State;
         Max_Term  : Incr.Nodes.Token_Kind) return Provider
      is
         use type Incr.Nodes.Node_Kind;
      begin
         return Result : Provider
           (Document)
         do
            Result.Max_Term := Max_Term;
            Result.Max_NT := Max_Term;
            Result.Names := Names;
            Result.Actions := new P.Action_Table
              (1 .. Max_State, 1 .. Result.Max_NT);
            Result.States := new P.State_Table
              (1 .. Max_State, Max_Term + 1 .. Result.Max_NT);
            Result.NT := new Node_Kind_Array'(NT);
            Result.Parts := new P.Parts_Count_Table'(Parts);
         end return;
      end Create;
   end Constructors;

   -----------------
   -- Create_Node --
   -----------------

   overriding procedure Create_Node
     (Self     : aliased in out Provider;
      Prod     : Incr.Parsers.Incremental.
        Parser_Data_Providers.Production_Index;
      Children : Incr.Nodes.Node_Array;
      Node     : out Incr.Nodes.Node_Access;
      Kind     : out Incr.Nodes.Node_Kind)
   is
      Result : Incr.Nodes.Joints.Joint_Access;
   begin
      Kind := Self.NT (Prod);

      if Children'Length = 0 then
         Node := null;
         return;
      end if;

      Result := new Incr.Nodes.Joints.Joint (Self.Document, Children'Length);
      Incr.Nodes.Joints.Constructors.Initialize (Result.all, Kind, Children);

      Node := Incr.Nodes.Node_Access (Result);
   end Create_Node;

   ----------------
   -- Kind_Image --
   ----------------

   overriding function Kind_Image
     (Self : Provider;
      Kind : Incr.Nodes.Node_Kind) return Wide_Wide_String
   is
      use type Incr.Nodes.Node_Kind;
   begin
      if Kind = 0 then
         return "EOF";
      elsif Positive (Kind) <= Self.Names.Length then
         return Self.Names (Positive (Kind)).To_Wide_Wide_String;
      else
         return "unknown";
      end if;
   end Kind_Image;

   ------------
   -- States --
   ------------

   overriding function States (Self : Provider) return P.State_Table_Access is
   begin
      return P.State_Table_Access (Self.States);
   end States;

   -----------------
   -- Part_Counts --
   -----------------

   overriding function Part_Counts
     (Self : Provider) return P.Parts_Count_Table_Access is
   begin
      return P.Parts_Count_Table_Access (Self.Parts);
   end Part_Counts;

end Tests.Parser_Data;
