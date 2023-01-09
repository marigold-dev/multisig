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
#import "../common/util.mligo" "Util"
#import "proposal_content.mligo" "Proposal_content"
#import "parameter.mligo" "Parameter"
#import "storage.mligo" "Storage"

type storage_types = Storage.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_state = Storage.Types.proposal_state
type proposal_content = Proposal_content.Types.t

[@inline]
let only_owner (type a) (storage : a storage_types) : unit =
    assert_with_error (Set.mem (Tezos.get_sender ()) storage.owners) Errors.only_owner

[@inline]
let amount_must_be_zero_tez (an_amout : tez) : unit =
    assert_with_error (an_amout = 0tez) Errors.amount_must_be_zero_tez

[@inline]
let unsigned (type a) (proposal : a storage_types_proposal) : unit =
    assert_with_error (not Map.mem (Tezos.get_sender ()) proposal.signatures) Errors.has_already_signed

[@inline]
let ready_to_execute (state : storage_types_proposal_state) : unit =
    assert_with_error (not (state = (Proposing : storage_types_proposal_state))) Errors.no_enough_signature_to_resolve

[@inline]
let unresolved (state : storage_types_proposal_state) : unit =
    assert_with_error (state = (Proposing : storage_types_proposal_state)) Errors.unresolved

[@inline]
let check_proposal (type a) (content: a proposal_content) : unit =
    match content with
    | Transfer t ->
        assert_with_error (not (t.amount = 0tez)) Errors.amount_is_zero
    | Execute _ -> ()
    | Execute_lambda _ -> ()
    | Adjust_threshold t ->
        assert_with_error (t > 0n) Errors.invalidated_threshold
    | Add_owners s ->
        assert_with_error (Set.cardinal s > 0n) Errors.no_owners
    | Remove_owners s ->
        assert_with_error (Set.cardinal s > 0n) Errors.no_owners

[@inline]
let not_empty_content (type a) (proposals_content: (a proposal_content) list) : unit =
    let () = assert_with_error ((List.length proposals_content) > 0n) Errors.no_proposal in
    List.iter check_proposal proposals_content

[@inline]
let check_setting (type a) (storage : a storage_types) : unit =
    let () = assert_with_error (Set.cardinal storage.owners > 0n) Errors.no_owner  in
    let () = assert_with_error (Set.cardinal storage.owners >= storage.threshold) Errors.no_enough_owner in
    let () = assert_with_error (storage.threshold > 0n) Errors.invalidated_threshold in
    ()

[@inline]
let check_proposals_content (type a) (bytes : bytes) (proposals_content: (a proposal_content) list) : unit =
  let pack_proposals_content = Bytes.pack proposals_content in
  assert_with_error (bytes = pack_proposals_content) Errors.not_the_same_content
