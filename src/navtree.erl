%% ===========================================================================
%% @doc        Tree structure with a cursor pointing to a node, where moving
%%             the cursor to the node node parent or children, updating
%%             node node value and adding child nodes are O(1) operations.
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
-export([new/1, new/2,
         is_root/1,
         is_leaf/1,
         get_count/1,
         get_path/1,
         go_path/2,
         go_top/1,
         go_up/1,
         go_down/2,
         get_id/1,
         get_node/1,
         update_node/2,
         add_child/2, add_child/3,
         remove_child/2]).

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
new(Root) ->
    new(root, Root).

%% --------------------------------------------------------------------
%% @doc Creates a new tree with the specified root node.
-spec new(Id::navtree_id(), Root::term()) -> Tree::navtree().
%% --------------------------------------------------------------------
new(Id, Root) ->
    #?St{id=Id, node = Root, children = [], ids = [], stack = []}.

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
%% @doc Gives the number of child of the node node.
-spec get_count(Tree::navtree()) -> Count::pos_integer().
%% --------------------------------------------------------------------
get_count(#?St{children = Children}) -> length(Children).

%% --------------------------------------------------------------------
%% @doc Gives the path to the node node.
-spec get_path(Tree::navtree()) -> Path::navtree_path().
%% --------------------------------------------------------------------
get_path(#?St{id = Id, ids = Ids}) -> tl(lists:reverse([Id |Ids])).

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the tree root.
-spec go_top(Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_top(?NAVTREE_MATCH_ROOT() = Tree) -> Tree;
go_top(Tree)                         -> go_top(go_up(Tree)).

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the node node parent.
-spec go_up(Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_up(?NAVTREE_MATCH_ROOT()) ->
    erlang:error(no_parent);
go_up(Tree) ->
    #?St{id = Id, node = Node, children = Children,
         ids = [ParentId |Is], stack = [{Parent, PChildren} |Bs]} = Tree,
    NewChildren = lists:keyreplace(Id, 1, PChildren, {Id, Node, Children}),
    Tree#?St{id = ParentId, node = Parent, children = NewChildren,
             ids = Is, stack = Bs}.

%% --------------------------------------------------------------------
%% @doc Moves the cursor to one of the node node child.
-spec go_down(Id::navtree_id(), Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_down(_Id, ?NAVTREE_MATCH_LEAF()) ->
    erlang:error(no_child);
go_down(Id, Tree) ->
    #?St{id = CurrId, node = CurrNode, children = CurrChildren,
         ids = Is, stack = Bs} = Tree,
    {NewId, NewNode, NewChildren} = lists:keyfind(Id, 1, CurrChildren),
    Tree#?St{id = NewId, node = NewNode, children = NewChildren,
             ids = [CurrId |Is], stack = [{CurrNode, CurrChildren} |Bs]}.

%% --------------------------------------------------------------------
%% @doc Moves the cursor to the specified path.
-spec go_path(Path::navtree_path(), Tree::navtree()) -> NewTree::navtree().
%% --------------------------------------------------------------------
go_path(Path, Tree) -> intern_go(go_top(Tree), Path).

%% --------------------------------------------------------------------
%% @doc Gives the node node value.
-spec get_id(Tree::navtree()) -> NodeId::navtree_id().
%% --------------------------------------------------------------------
get_id(#?St{id = Id}) -> Id.

%% --------------------------------------------------------------------
%% @doc Gives the node node value.
-spec get_node(Tree::navtree()) -> NodeValue::term().
%% --------------------------------------------------------------------
get_node(?NAVTREE_MATCH_CURRENT(Curr)) -> Curr.

%% --------------------------------------------------------------------
%% @doc Updates the node node value.
-spec update_node(NewNodeValue::term(), Tree::navtree()) ->
          NewTree::navtree().
%% --------------------------------------------------------------------
update_node(NewNode, Tree) ->
    Tree#?St{node = NewNode}.

%% --------------------------------------------------------------------
%% @doc Adds a child node to the current node using erlang:make_ref()
%%      to create a unique identifier.
-spec add_child(Node::term(), Tree::navtree()) ->
          {ChildId::navtree_id(), NewTree::navtree()}.
%% --------------------------------------------------------------------
add_child(Node, Tree) ->
    add_child(erlang:mak_ref(), Node, Tree).

%% --------------------------------------------------------------------
%% @doc Adds a child node with specified identifier.
-spec add_child(Id::term(), Node::term(), Tree::navtree()) ->
          {ChildId::navtree_id(), NewTree::navtree()}.
%% --------------------------------------------------------------------
add_child(Id, Node, #?St{children = Children} = Tree) ->
    {Id, Tree#?St{children = [{Id, Node, []} |Children]}}.

%% --------------------------------------------------------------------
%% @doc Removes the child node with specified identifier.
-spec remove_child(Id::term(), Tree::navtree()) ->
          NewTree::navtree().
%% --------------------------------------------------------------------
remove_child(Id, #?St{children = Children} = Tree) ->
    NewChildren = lists:keydelete(Id, 1, Children),
    Tree#?St{children = NewChildren}.


%% ====================================================================
%% Local Functions
%% ====================================================================

intern_go(Tree, [])       -> Tree;
intern_go(Tree, [I |Rem]) -> intern_go(go_down(I, Tree), Rem).
