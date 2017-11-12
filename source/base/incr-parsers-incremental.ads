--  Copyright (c) 2015-2017 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Incr.Documents;
with Incr.Lexers.Incremental;
with Incr.Nodes;
with Incr.Version_Trees;

package Incr.Parsers.Incremental is
   --  @summary
   --  Incremental Parser
   --
   --  @description
   --  This package provides incremental syntactical analyser.
   --
   --  The package uses three versions of the document:
   --
   --  * Reference - version when last analysis completed
   --  * Changing - current version under construction
   --  * Previous - last read-olny version of document to be analyzed
   --

   package Parser_Data_Providers is
      type Parser_State is new Natural;
      type Production_Index is new Natural;

      type Action_Kinds is (Shift, Reduce, Error, Finish);

      type Action (Kind : Action_Kinds := Error) is record
         case Kind is
            when Shift =>
               State : Parser_State;
            when Reduce =>
               Prod  : Production_Index;
            when Error | Finish =>
               null;
         end case;
      end record;

      type Action_Table is array
        (Parser_State range <>, Nodes.Token_Kind range <>) of Action;
      type Action_Table_Access is access constant Action_Table;

      type State_Table is array
        (Parser_State range <>, Nodes.Node_Kind range <>) of Parser_State;
      type State_Table_Access is access constant State_Table;

      type Parts_Count_Table is array (Production_Index range <>) of Natural;
      type Parts_Count_Table_Access is access constant Parts_Count_Table;

      type Parser_Data_Provider is limited interface;
      type Parser_Data_Provider_Access is
        access all Parser_Data_Provider'Class;

      not overriding function Actions
        (Self : Parser_Data_Provider) return Action_Table_Access is abstract;

      not overriding function States
        (Self : Parser_Data_Provider) return State_Table_Access is abstract;

      not overriding function Part_Counts
        (Self : Parser_Data_Provider) return Parts_Count_Table_Access is
           abstract;

      not overriding function Kind_Image
        (Self : Parser_Data_Provider;
         Kind : Nodes.Node_Kind) return Wide_Wide_String is abstract;

      type Node_Factory is limited interface;
      type Node_Factory_Access is access all Node_Factory'Class;

      not overriding procedure Create_Node
        (Self     : aliased in out Node_Factory;
         Prod     : Production_Index;
         Children : Nodes.Node_Array;
         Node     : out Nodes.Node_Access;
         Kind     : out Nodes.Node_Kind) is abstract;

   end Parser_Data_Providers;

   type Incremental_Parser is tagged limited private;
   --  Type to perform incremental lexical analysis

   type Incremental_Parser_Access is access Incremental_Parser;

   procedure Run
     (Self      : Incremental_Parser;
      Lexer     : Incr.Lexers.Incremental.Incremental_Lexer_Access;
      Provider  : Parser_Data_Providers.Parser_Data_Provider_Access;
      Factory   : Parser_Data_Providers.Node_Factory_Access;
      Document  : Documents.Document_Access;
      Reference : Version_Trees.Version);
   --  Start analysis by looking for tokens where re-lexing should be start.
   --  Mark them with Need_Analysis flag.

private

   type Incremental_Parser is tagged limited record
      null;
   end record;

end Incr.Parsers.Incremental;
