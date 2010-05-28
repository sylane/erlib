%% ===========================================================================
%% @doc        Tree structure with a cursor pointing to a node, where moving
%%             the cursor to the current node parent or childs, updating
%%             current node value and adding child nodes are O(1) operations.
%% @since      May 15, 2010
%% @version    1.0
%% @copyright  (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% @authors    Sebastien Merle <s.merle@gmail.com>
%% @end
%%
%% Copyright (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   * Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%   * Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%   * Neither the name of "erlib" nor the names of its contributors may be
%%     used to endorse or promote products derived from this software without
%%     specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% ===========================================================================

-module(navtree).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("navtree.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/1,
         is_root/1,
         is_leaf/1,
         get_child_count/1,
         get_path/1,
         go_path/2,
         go_top/1,
         go_up/1,
         go_down/2,
         get_current/1,
         update_current/2,
         add_child/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Tree's state record name
-define(St, ?NAVTREE_RECNAME).


%% ===================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a new tree with the specified root node.
-spec new(Root::term()) -> Tree::navtree().
%% --------------------------------------------------------------------
new(Root) -> #?St{current = Root, childs = {}, indexes = [], branches = []}.

%% --------------------------------------------------------------------
%% @doc Tells if the cursor is on the tree root.
-spec is_root(Tree::navtree()) -> IsRoot::boolean().
%% --------------------------------------------------------------------
is_root(?NAVTREE_MATCH_ROOT()) -> true;
is_root(_)                     -> false.

%% --------------------------------------------------------------------
%% @doc Tells if the cursor is on a leaf.
-spec is_leaf(Tree::navtree()) -> IsLeaf::boolean().
%% --------------------------------------------------------------------
is_leaf(?NAVTREE_MATCH_LEAF()) -> true;
is_leaf(_)                     -> false.

%% --------------------------------------------------------------------
%% @doc Gives the number of child of the current node.
-spec get_child_count(Tree::navtree()) -> Count::pos_integer().
%% --------------------------------------------------------------------
get_child_count(#?St{childs = Childs}) -> size(Childs).

%% --------------------------------------------------------------------
%% @doc Gives the path to the current node.
-spec get_path(Tree::navtree()) -> Path::navtree_path().
%% --------------------------------------------------------------------
get_path(#?St{indexes = Indexes}) -> lists:reverse(Indexes).

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the tree root.
-spec go_top(Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_top(?NAVTREE_MATCH_ROOT() = Tree) -> Tree;
go_top(Tree)                         -> go_top(go_up(Tree)).

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the current node parent.
-spec go_up(Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_up(?NAVTREE_MATCH_ROOT()) ->
    erlang:error(no_parent);
go_up(Tree) ->
    #?St{current = Curr, childs = Childs,
         indexes = [I |Is], branches = [{E, C} |Bs]} = Tree,
    NewChilds = erlang:setelement(I, C, {Curr, Childs}),
    Tree#?St{current = E, childs = NewChilds, indexes = Is, branches = Bs}.

%% --------------------------------------------------------------------
%% @doc Moves the cursor to one of the current node child.
-spec go_down(Index::navtree_index(), Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_down(_Index, ?NAVTREE_MATCH_LEAF()) ->
    erlang:error(no_child);
go_down(Index, Tree) ->
    #?St{current = Curr, childs = Childs, indexes = Is, branches = Bs} = Tree,
    {NewCurr, NewChilds} = element(Index, Childs),
    Tree#?St{current = NewCurr, childs = NewChilds,
             indexes = [Index |Is], branches = [{Curr, Childs} |Bs]}.

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the specified path.
-spec go_path(Path::navtree_path(), Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_path(Path, Tree) -> intern_go(go_top(Tree), Path).

%% --------------------------------------------------------------------
%% @doc Gives the current node value.
-spec get_current(Tree::navtree()) -> NodeValue::term().
%% --------------------------------------------------------------------
get_current(?NAVTREE_MATCH_CURRENT(Curr)) -> Curr.

%% --------------------------------------------------------------------
%% @doc Updates the current node value.
-spec update_current(NewNodeValue::term(), Tree::navtree()) ->
        {OldNodeValue::term(), NewTree::navtree()}.
%% --------------------------------------------------------------------
update_current(NewNode, #?St{current = OldNode} = Tree) ->
    {OldNode, Tree#?St{current = NewNode}}.

%% --------------------------------------------------------------------
%% @doc Adds a child node to the current node.
-spec add_child(ChildNode::term(), Tree::navtree()) ->
        {ChildIndex::navtree_index(), NewTree::navtree()}.
%% --------------------------------------------------------------------
add_child(Child, #?St{childs = Childs} = Tree) ->
    NewChilds = erlang:append_element(Childs, {Child, {}}),
    {size(NewChilds), Tree#?St{childs = NewChilds}}.


%% ====================================================================
%% Local Functions
%% ====================================================================

intern_go(Tree, [])       -> Tree;
intern_go(Tree, [I |Rem]) -> intern_go(go_down(I, Tree), Rem).
