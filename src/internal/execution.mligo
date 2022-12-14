(* MIT License
   Copyright (c) 2022 Marigold <contact@marigold.dev>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "../common/errors.mligo" "Errors"
#import "../common/constants.mligo" "Constants"
#import "../common/util.mligo" "Util"
#import "proposal_content.mligo" "Proposal_content"
#import "./storage.mligo" "Storage"
#import "conditions.mligo" "Conditions"

type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

let send_by (type a) (parameter: a) (target : address) (amount : tez) : operation =
    [@no_mutation]
    let contract_opt : a contract option = Tezos.get_contract_opt target in
    let contract = Option.unopt_with_error contract_opt Errors.unknown_contract in
    Tezos.transaction parameter amount contract

let send (type a) (content : a proposal_content) (storage : a storage_types) : (operation option * a storage_types) =
    match content with
    | Transfer tx -> (Some (send_by tx.parameter tx.target tx.amount), storage)
    | Execute tx -> (Some (send_by tx.parameter tx.target tx.amount), storage)
    | Execute_lambda e -> (Some (e.lambda ()), storage)
    | Adjust_threshold t -> (None, Storage.Op.adjust_threshold t storage)
    | Add_owners s -> (None, Storage.Op.add_owners s storage)
    | Remove_owners s -> (None, Storage.Op.remove_owners s storage)

let perform_operations (type a) (proposal: a storage_types_proposal) (storage : a storage_types) : operation list * a storage_types =
    let batch (type a) ((ops, s), c : (operation list * a storage_types) * a proposal_content) : (operation list * a storage_types) =
      let (opt_op, new_s) = send c s in
      match opt_op with
      | Some op -> op::ops, new_s
      | None -> ops, new_s
    in
    if proposal.state = (Executed : storage_types_proposal_state)
    then List.fold_left batch (Constants.no_operation, storage) proposal.contents
    else (Constants.no_operation, storage)
